#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####################  FUNCTION TO VECTORIZE DATAFRAME OF LINEAR REGRESSION STATISTICS  ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: For a covariate of interest in the linear regression result, this function will extract the 
#          coefficient, standard error, t-score, and p-value of that covariate. 
# Inputs: LR_result - results of a multivariate linear regression 
#         pattern_motif - pattern that is used to look for the covariate of interest
#           Ex. If you're interested in extracting the age covariate, use the pattern "age_covariate"
#         name_motif - pattern that is used to provide a short name for the covariate 
#           Ex. "age"
# Outputs: return a list that contains the coefficient, standard error, t-score, and p-value of the covariate

LR_coefficient_assignment <- function(LR_result
                                      , pattern_motif
                                      , name_motif
                                      , is_empty = FALSE)
{
  # Determine the row of the covariate in the dataset containing the results  of linear regression 
  if(is_empty == TRUE)
  {
    index = NULL
    
  } else {
    
    index <- grep(pattern_motif, row.names(LR_result$coefficients))
    
  }
  
  # Creates a new name for the following variable 
  # Ex. age coviariate 
  estimate_name <- paste(name_motif,"coefficient", sep = "_") # age_coefficient
  percent_name <- paste(name_motif,"percent_change", sep = "_") # age_percent_change
  error_name <- paste(name_motif,"std_error", sep = "_") # age_std_error
  tscore_name <- paste(name_motif,"t_score", sep = "_") # age_t_score
  pvalue_name <- paste(name_motif,"p_value", sep = "_") # age_p_value
  
  # Initialize an empty list to hold the coefficients of the covariate 
  result_list <- list()
  
  # There may be calls in which the coefficients do not exist for the covariate
  # This statement accounts for those calls
  if( length(index) == 0 ) {
    
    coefficient = NA
    percent_change = NA
    st_error = NA
    t_score = NA
    p_value = NA
    
    # If coefficients do exist for the covariate, 
    # then the following statement will assign the values to the corresponding coefficient
  } else {
    
    coefficient <- summary_lm_albumin$coefficients[index,"Estimate"]
    st_error <- summary_lm_albumin$coefficients[index,"Std. Error"]
    t_score <- summary_lm_albumin$coefficient[index, "t value"]
    p_value <- summary_lm_albumin$coefficient[index, "Pr(>|t|)"]
    
    if(name_motif == "PIR"){
      
      quantile_range <- quantile(chem_demo_response_cycle_1_2_updated$INDFMPIR,0.975,na.rm=TRUE)-
        quantile(chem_demo_response_cycle_1_2_updated$INDFMPIR,0.025,na.rm=TRUE)
      percent_change <- 100*(10^(coefficient*quantile_range) - 1)
      
    } else if (name_motif == "cotinine"){
      
      quantile_range <- log10(quantile(chem_demo_response_cycle_1_2_updated$LBXCOT,0.975,na.rm=TRUE))-
        log10(quantile(chem_demo_response_cycle_1_2_updated$LBXCOT,0.025,na.rm=TRUE))
      percent_change <- 100*(10^(coefficient*quantile_range) - 1)
      
    }  else {
      
      percent_change <- 100*(10^coefficient - 1)
    }
  }
  
  # Populate the empty list with the short name variable and their corresponding coefficients
  result_list[estimate_name] <- coefficient
  result_list[percent_name] <- percent_change
  result_list[error_name] <- st_error
  result_list[tscore_name] <- t_score
  result_list[pvalue_name] <- p_value
  
  # Return the list of coefficients to the function that calls this function.
  return(result_list)
}
