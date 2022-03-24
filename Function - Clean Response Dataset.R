#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~  FUNCTION - CLEANING THE RESPONSE DATASET TO HAVE 10 HEALTH BIOMARKERS  ~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function will clean the response dataset by extracting the 10 health biomarkers of interest
#          The 10 health biomarkers are Serum C-Reactive Protein, Serum Albumin Body Mass Index, Systolic Blood 
#          Pressure, Diastolic Blood Pressure, Serum HDL Cholesterol, Serum Total Cholesterol, Serum Total 
#          Triglycerides, 60 second pulse, and Serum creatinine.
# Inputs: unclean_response_dataset - the dataset that contains excess variables. This should be the result 
#                                    of the compile function that extracted the individual response datasets
#                                    and merged all individual response datasets across the cycles
# Outputs: returns a dataframe with the 10 health biomarkers. 

clean_response_dataset <- function(unclean_response_dataset)
{
  codenames_keep <- c("SEQN"
                      # , "BMXBMI"
                      , "BMXHT"
                      # , "BPXPLS"
                      # , "BPXSY1"
                      # , "BPXDI1"
                      # , "LBXCRP"
                      # , "LBXTC"
                      # , "LBDHDL"
                      # , "LBDHDD"
                      # , "LBXHDD"
                      # , "LBXTR"
                      , "LBXSAL"
                      # , "LBXSTR"
                      , "LBXSCR"
                      # , "study_year"
                      )
  cleaned_response_dataset <- unclean_response_dataset[,codenames_keep]
  return(cleaned_response_dataset)
}
