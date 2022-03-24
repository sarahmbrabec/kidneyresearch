#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  FUNCTION - COMPILE THE NHANES RESPONSE DATASET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: extract the individual response dataset for each cycle and merge into one combined response dataset
# Inputs: main_directory - the working directory of the folder that contains the folders for each cycle. Each 
#                          cycle-specific folder contains the file names for each the response dataset
# Outputs: returns a dataframe with the merged response dataset 

compile_response_dataset <- function(main_directory)
{
  # Establish the working directory to be the folder that contains the folders of the cycle-specific response
  # datasets
  setwd(main_directory)
  
  # Obtain a vector of folder names, one for each cycle
  nhanes_dataset_by_cycle <- list.files()

  # Determine the number of cycles 
  num_cycles <- length(nhanes_dataset_by_cycle)
  
  # Initialize a list to store a dataframe of biomarker measurements for each cycle
  all_nhanes_datasets_by_cycle <- list()
  
  # For each cycle in NHANES, go into a cycle-specific folder and extract the corresponding files to form a 
  # merged chemicals datasest for an ith cycle
  for(i in seq(num_cycles))
  {
    # Define the directory of the ith cycle-specific folder
    cycle_specific_nhanes_dataset_directory <- paste(main_directory
                                                     , nhanes_dataset_by_cycle[i]
                                                     , sep = "/")
    print(cycle_specific_nhanes_dataset_directory)
    
    # Establish the working directory for the ith cycle-specific folder
    setwd(cycle_specific_nhanes_dataset_directory)
    
    # Determine a vector of file names in the ith cycle-specific folder
    files_names.xpt <- list.files()
    # print(files_names.xpt)
    
    # Determine the number of files in the ith cycle-specific folder
    num_files_in_cycle_specific_folder <- length(files_names.xpt)

    # Determine the name of the first file in the folder
    first_file <- files_names.xpt[1]
    # Replace the ".XPT" in the file name with ""
    first_file <- gsub(".XPT"
                       , ""
                       , first_file)

    # Use the updated file name to extract the appropriate dataset from the nhanesA package
    # Store the first chemical dataset for the ith cycle into cycle_specific_datasest, so that subsequent
    # chemical datasets can be merged with the first one
    cycle_specific_dataset <- nhanes(first_file)

    # For the 2nd file and beyond (jth) in the cycle-specific folder, the corresponding chemical dataset will
    # be merged with the first chemical dataset by SEQN to form the cycle-specific dataset of chemical
    # measurements
    for(j in 2:num_files_in_cycle_specific_folder)
    {
      # Determine the name of the jth file in the folder and replace the ".XPT" in the file name with ""
      file_name_j <- gsub(".XPT"
                          , ""
                          , files_names.xpt[j])
      # Message to know which chemical dataset is being extracted
      # print(file_name_j)

      # Store jth chemical dataset into temp_file
      temp_file <- nhanes(file_name_j)

      # Merge the jth chemical dataset with the previous chemicals datset by SEQN
      cycle_specific_dataset <- merge(cycle_specific_dataset
                                      , temp_file
                                      , all = TRUE
                                      , by = "SEQN")
    }
    
    # Determine the number of participants in the ith cycle
    num_participants_cycle_i <- dim(cycle_specific_dataset)[1]
    # Define the study year that each participant belongs to
    study_year <- rep(i, num_participants_cycle_i)
    
    # Store the merged chemical datset for the ith cycle into a list
    all_nhanes_datasets_by_cycle[[i]] <- data.frame(cycle_specific_dataset
                                                    , study_year)
    
  }
  

  # Rename the list containing all the cycle-specific chemicals datasets
  list_merged_nhanes_datasets <- all_nhanes_datasets_by_cycle
  # return(list_merged_nhanes_datasets)

  # # Determine the number of cycles or technically, determine the number of dataframes stored in this list
  num_elements_in_list <- length(lengths(list_merged_nhanes_datasets))


  for(m in 2:num_elements_in_list)
  {
    list_merged_nhanes_datasets[[m]][,"LBXSNASI"] <- as.numeric(list_merged_nhanes_datasets[[m]][,"LBXSNASI"])
    list_merged_nhanes_datasets[[m]][,"LBXSCLSI"] <- as.numeric(list_merged_nhanes_datasets[[m]][,"LBXSCLSI"])
    list_merged_nhanes_datasets[[m-1]][,"LBDSTPSI"] <- as.numeric(list_merged_nhanes_datasets[[m-1]][,"LBDSTPSI"])
  }

  list_merged_nhanes_datasets[[1]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[1]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[4]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[4]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[5]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[5]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[6]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[6]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[7]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[7]][,"LBXPLTSI"])
  list_merged_nhanes_datasets[[8]][,"LBXPLTSI"] <- as.numeric(list_merged_nhanes_datasets[[8]][,"LBXPLTSI"])


  merged_nhanes_datasets <- list_merged_nhanes_datasets[[1]]

  # For subsequent cycles, the kth cycle chemical dataset will be merged with the previous merged chemical
  # dataset
  for(k in 2:num_elements_in_list)
  {
    # Perform the merge by the codenames
    merged_nhanes_datasets <- full_join(merged_nhanes_datasets
                                        , list_merged_nhanes_datasets[[k]]
                                        , by = NULL)
    # Message to relay which cycle has been merged in
    print(paste("Merge in Cycle ", k, sep = ""))
  }

  # Return the merged chemical biomarker dataset
  return(merged_nhanes_datasets)
  
}
