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

install.packages("reshape2")
library("reshape2")

install.packages("readxl")
library("readxl")

install.packages("xlsx")
library("xlsx")

install.packages("ggplot2")
library("ggplot2")

install.packages("survival")
library("survival")

install.packages("survminer")
library("survminer")

setwd("~/Downloads/Exposure Project")

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

chem_demo_response<- merge(chem_demo
                           , responses_clean
                           , by = 'SEQN'
                           , all.x = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~  Upload Dataset of Chemical Identifiers & Properties  ~~~~~~~~~~~~~~=~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

chem_identi_props <- read_xlsx(path = "NHANES - Dataset of Chemical Identifiers, Restriction Dates, and Half-Lives 1b.xlsx",
                                sheet = 3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Extract the First Two Cycle  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

chem_demo <- chem_demo[,]


chem_stats_dataset <- sapply(chem_demo_response[,first_chem_variable:last_chem_variable],
                             FUN = display_chem_statistics)
chem_stats_dataset <- data.frame(chem_stats_dataset)

#View(chem_stats_dataset)

clean_chem_stat_index <- which(chem_stats_dataset["perc_with_measurements",] > 0)

clean_chem_stat_dataset <- chem_stats_dataset[, clean_chem_stat_index]

chem_codenames_include <- colnames(clean_chem_stat_dataset)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Standardizing Creatinine for Cycle 1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

columns_cycle_one <- c(which(chem_demo_response$SDDSRVYR == 1))


scr_vector <- c(chem_demo_response[columns_cycle_one, "LBXSCR"])


chem_demo_response[columns_cycle_one, "LBXSCR"] <- 1.103 * (scr_vector) + 0.147

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Calculating the GFR for participants  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

chem_demo_response_updated <- calculate_gfr(merged_dataset = chem_demo_response)


chem_demo_response_updated <- calculate_acr(merged_dataset = chem_demo_response_updated)

chem_demo_response_new <- merge(chem_demo_response_updated
                            , mortality_clean
                            , by = 'SEQN'
                            , all.x = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  cleaning data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


sample_df <- read.csv(file="SAMPLE_SIZE_UPDATED.csv", header=TRUE, sep=",")

chem_names_na_removed <- sample_df[,1]

#rows below lower quartile
num_low_quart <- which(sample_df[,"GFR"] < 3343)

#names of chemicals needed to be removed 
lower_quartile <- sample_df[num_low_quart, 1]

#set of clean chemicals to ass
clean_chem_col <- chem_names_na_removed[-c(lower_quartile)]

column_names <- colnames(chem_demo_response_new)

response_col <- setdiff(column_names, chem_codename_list)




chem_demo_response_clean <- merge(chem_demo_response_new[,response_col]
                                 , chem_demo_response_new[,clean_chem_col]
                                 , by = 'SEQN'
                                 , all.x = TRUE)
#find chemicals that were removed, change notes 


 