#############################################################################################################
###################  MAIN SCRIPT - AUTOMATED EXTRACTING AND COMPILING THE NHANES DATABASE  ##################
#############################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Install and Upload Any Necessary Packages  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Install and upload nhanesA package to obtain access to all NHANES datasets 
# https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
install.packages("nhanesA")
library("nhanesA")

# Upload the dplyr package to use full_join to merge the all merged chemical datasests by columns across 
# the cycles 
install.packages("dplyr")
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Compile and Clean the NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Set the working directory that contains the individual chemical datasets
chemical_datasets_directory <- "~/Desktop/Sarah - Kidney Health/Chemical Datasets"

# Extract the individual chemical datasets and compile them into the unclean chemicals dataset
chemicals_unclean <- compile_chemicals_dataset(chemical_datasets_directory)

# Using the unclean chemicals dataset, this function will clean the chemical dataset while forming the
# comments dataset (a dataset of indicators to show whether a participant's measurement were above or below 
# the LOD) and the unfinished weights dataset (a dataset containing the survey weights)
# All of these datasets are stored in a list
nhanes_clean_list <- clean_chemicals_dataset(chemicals_unclean)

# Extract the clean chemical dataset from the list 
chemicals_clean <- nhanes_clean_list$chemicals_dataset

# Extract the unfinished dataset from the list
# This dataset is unfinished, since it does not containing the weights pertaining to the interview or MEC
# examinations. These weights are in the demographics dataset
weights_unfinished <- nhanes_clean_list$weights_dataset

# Set the working directory that contains the individual demographic datasets
demographics_dataset_directory <- "M:/NHANES/DEMOGRAPHICS/Demographics Datasets"

# Extract the individual demographics datasets and compile them into the unclean demographics dataset
demographics_unclean <- compile_demographics_dataset(demographics_dataset_directory)

# Using the unclean demographics datasets, this function will clean the demographics dataset while forming
# the merged chemicals and demographics dataset and the finished weight dataset
nhanes_new_list <- 
  clean_demographics_dataset_and_merge_with_chemicals_and_weights_datasets(demographics_unclean_dataset = 
                                                                             demographics_unclean
                                                                           , chemicals_clean_dataset = 
                                                                             chemicals_clean
                                                                           , weights_unfinished_dataset = 
                                                                             weights_unfinished)

# Set the working directory that contains the individual response datasets
response_datasets_directory <- "M:/NHANES/CHEMRESPONSES/Response Datasets"

responses_unclean <- compile_response_dataset(response_datasets_directory)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Extract the Clean Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Extract the comments dataset, which contains indicators to show whether a participant measurements are 
# below or above LOD
comments_clean <- nhanes_clean_list$comments_dataset

# Extract the clean chemical dataset, which contains biomarkers that are indicative of chemical exposure 
# along with albumin and creatinine
chemicals_clean <- nhanes_clean_list$chemicals_dataset

# Extract the clean demographics dataset
demographics_clean <- nhanes_new_list$demographics_dataset  

# Extract the merged chemical and demographic datasets
# Note that participants who have chemical measurements are in this dataset
chem_demo <- nhanes_new_list$chemicals_demographics_dataset

# Extract the finished weights dataset, which contains the weights for the subsample and those for the interview
# and MEC examinations 
weights_finished <- nhanes_new_list$weights_finished_dataset

responses_clean <- clean_response_dataset(responses_unclean)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Extract the First Two Cycle  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

index_cycle_1_2 <- which(chem_demo$SDDSRVYR == 1 | chem_demo$SDDSRVYR == 2)

chem_demo_cycle_1_2 <- chem_demo[index_cycle_1_2,]

chem_demo_response_cycle_1_2 <- merge(chem_demo_cycle_1_2
                                      , responses_clean
                                      , by = 'SEQN'
                                      , all.x = TRUE)

first_chem_variable <- which(colnames(chem_demo_response_cycle_1_2) == "LBXV4C")
last_chem_variable <- which(colnames(chem_demo_response_cycle_1_2) == "LBXVVB")


chem_stats_dataset <- sapply(chem_demo_response_cycle_1_2[,first_chem_variable:last_chem_variable],
                             FUN = display_chem_statistics)
chem_stats_dataset <- data.frame(chem_stats_dataset)

View(chem_stats_dataset)

clean_chem_stat_index <- which(chem_stats_dataset["perc_with_measurements",] > 0)

clean_chem_stat_dataset <- chem_stats_dataset[, clean_chem_stat_index]

chem_codenames_include <- colnames(clean_chem_stat_dataset)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Calculating the GFR for participants  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

chem_demo_response_cycle_1_2_updated <- calculate_gfr(merged_dataset = chem_demo_response_cycle_1_2)



