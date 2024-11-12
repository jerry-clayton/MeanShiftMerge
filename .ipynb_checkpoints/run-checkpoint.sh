#!/bin/bash

#get command arguments to pass to rscript

# Activate the 'ms-env' conda environment
basedir=$( cd "$(dirname "$0")" ; pwd -P)

source $(conda info --base)/etc/profile.d/conda.sh
eval "$(conda shell.bash hook)"
conda activate ms-env

echo "Current Conda environment: $(conda info --envs | grep '*' | awk '{print $1}')"

echo "Basedir: ${basedir}"
echo "Working directory: $(pwd)"
# Check if the conda environment was activated
if [[ $? -ne 0 ]]; then
  echo "Failed to activate the conda environment 'ms-env'."
  exit 1
fi

mkdir -p output

parent_path=$(realpath .)
infile=$(ls input | head -n 1)


input_path="$parent_path/input/$infile"
outfile="segmented_merged_${new_infile}"
testout="test_output_${new_infile}"
output_path="$parent_path/output/$outfile"
test_output_path="$parent_path/output/$testout"

#cp "$input_path" "$test_output_path"
echo "input path: $input_path"
echo "output path: $output_path"
# echo "Running r package install script" 
# # Run script to install r-only packages
#Rscript --verbose ${basedir}/run/create-ms-env.R

# # Check if the R script ran successfully
# if [[ $? -ne 0 ]]; then
#   echo "Failed to run the R script."
#   exit 1
# fi

# echo "R package install script ran succedssfully."
echo "Running  MeanShift segmentation"
# Run the R script
Rscript --verbose ${basedir}/run/merge-trees.R "$input_path" "$output_path" 

# Check if the R script ran successfully
if [[ $? -ne 0 ]]; then
  echo "Failed to run the R script."
  exit 1
fi

echo "R package install script ran succedssfully."

# Deactivate the conda environment (optional)
conda deactivate