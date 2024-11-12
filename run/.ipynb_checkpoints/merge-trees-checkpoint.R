
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

library(lidR)
library(MeanShiftR)
library(data.table)
library(sf)


print(paste("input:",input_file))

# use all available processor cores
set_lidr_threads(0)

#####
##   Begin function definitions for merging
#####

compute_min_centroid_dist <- function(cm){
    
	# Initialize a vector to store the minimum distance for each centroid
	centroids <- cm
    min_distances <- numeric(length(centroids))

  # Loop through each centroid and compute distances to all other centroids
  for (i in 1:length(centroids)) {

    # Get the centroid at index i
    current_centroid <- centroids[i, ]

    # Compute distances from current centroid to all other centroids
    distances <- st_distance(current_centroid, centroids)

    # Set the distance to itself (index i) to a very large number to exclude it
    distances[i] <- Inf

    # Get the minimum distance to another centroid
    min_distances[i] <- min(distances)
	}

    return(min_distances)
}

## Regression equation of field sampled tree height-to-crown-width ratio. 
## MeanShift segmentation only allows us to define one static h2cw ratio.
## In reality, it is a function of height. Here we define that function
## And use it as a threshold below which we want to think about merging trees

h2cw_ratio <- function(height){
	return(10^(-0.1*(height^0.61)))
}

## RMSE of the regression. Unused right now.
RMSE <- 1.1231

# gets the first neighbor inside the regression radius based on the X-Y centroid.
# in the future should probably evaluate candidates based on Z centroid too.

getMergeID <- function(treeID, centroid, regRadius, cm){
	returnID <- NULL
	for(tree in cm$ID){
		if(tree == treeID){
		## do not test itself
		  next
		}

		tree <- cm[ID == tree,,]
		distance <- as.numeric(st_distance(centroid, tree$centroid.geometry))
		if(distance < regRadius){
			returnID <- tree$ID
			break
		}
	}
	return(returnID)
	
}

mergeTreesByID <- function(currentID, neighborID, las_dt){

	## assign all points with currentID to have neighborID
	las_dt[ID == currentID, ID := neighborID]
	
	return(las_dt)
}

## update cm to do the following:
	## remove currentID entry
	## re-compute the centroid, area, etc for neighborID

update_crown_metrics <- function(head, las_dt, cm, currentID, neighborID){

		newPts <- las_dt[ID == neighborID,,]
		cm_new <- crown_metrics(lidR::LAS(newPts, head), geom = "concave", func = NULL, attribute = "ID")
     		cm_new$area <- st_area(cm_new)
     		cm_new$centroid <- st_centroid(cm_new)
		currTree <- cm[ID == currentID,,]
		newTree <- cm[ID == neighborID,,]
		newMaxZ <-  max(currTree$maxZ, newTree$maxZ)
		cm_new$maxZ <- newMaxZ
    		cm_new$minDiam <- (h2cw_ratio(cm_new$maxZ)*cm_new$maxZ)
     		cm_new$minArea <- ((cm_new$minDiam/2)^2)*pi
		#print("new formatted row: ")
		#print(cm_new)
		#print("old formatted row: ")
		#print(newTree)
		#remove current IDs
		cm <- cm[ID != currentID]
		cm_new <- as.data.table(cm_new)
		#add this new version of the merged tree as a row to cm and return it
		cm[ID == neighborID, names(cm_new) := cm_new]
		return(cm)

}

mergeBelowThreshold <- function(head, las, heightThreshold){
    
     # save NAs separately to add back later
     las_nas <- filter_poi(las, ID == 99999)
     
     # remove NAs
     las <- filter_poi(las, ID != 99999)

     las_dt <- lidR::payload(las) %>% as.data.table

     #filter to only include trees below the height threshold
     
     maxHeightByID <- las_dt[, .(max(Z)), by = ID]
     treesBelowThreshold <- maxHeightByID[V1 < heightThreshold,,]
     las_subset <- filter_poi(las, ID %in% treesBelowThreshold$ID)

     #compute X-Y polygon of each tree, get its area and centroid for merging ops, and add the max-height
     cm <- crown_metrics(las_subset, attribute = "ID", geom = "concave", func = NULL)
     cm$area <- st_area(cm)
     cm$centroid <- st_centroid(cm)
     cm <- cm %>% as.data.table	
     cm[treesBelowThreshold, maxZ := V1, on = "ID"]
     cm$minDiam <- (h2cw_ratio(cm$maxZ)*cm$maxZ)
     cm$minArea <- ((cm$minDiam/2)^2)*pi
    # print(cm)
     ##
     ##  Wrap everything above here into a separate function

     ## add the min area as a column

     dynamic_trees <- lidR::payload(las) %>% as.data.table
     originalIDs <- cm$ID

     for (tree in 1:length(originalIDs)){
	tree <- originalIDs[tree]
	treeStats <- cm[ID == tree,,]
	## merge if the segmented area is less than the area of the regression equation	
	if(as.numeric(treeStats$area) < as.numeric(treeStats$minArea)){
	## merging
 	mergeID <-getMergeID(tree, treeStats$centroid.geometry, treeStats$minDiam/2, cm)

	if(is.null(mergeID)){
		print(paste0('no merge candidate found for tree ID: ',tree))
		next
	}
	print(paste0('treeID: ',tree,' merge ID found:',mergeID))
	las_dt <- mergeTreesByID(tree, mergeID, las_dt)
	#print('finished mergeTreesByID')
	cm <- update_crown_metrics(head, las_dt, cm, tree, mergeID)
	next
	}
	else{ 
	  next
	}

     }
	## join back as LAS
     	# join las_dt and las_nas, then use LAS()
     	las_nas <- as.data.table(payload(las_nas))
     	# because the header already has 99999 set as the NA value, it seems to be excluding those points.
        # here we assign NA to the column, which will be replaced with 99999 by the LAS() function.
        las_nas[, ID := NA]
        joined <- rbind(las_nas,las_dt)
	#print(las_nas)
	flas <- LAS(joined, head)
	return(flas)
	#	saveRDS(las_dt, "merge_test_043_full.rds")
}


#####
##   End function definitions for merging
#####

# Merge centroids
tryCatch({
 
                           
     flas <- readLAS(input_file)
     f_head <- readLASheader(input_file)
    
     # merge trees from segmented las
     mergedLAS <- mergeBelowThreshold(f_head, flas, 100)
    
     print("saving merged file")
     # overwrite the earlier segmentation with our merged result
     writeLAS(mergedLAS, output_file)
     },
     error = function(e){
     	print("error in file:")
	print(e)
     	skipped[input_file]
     })
