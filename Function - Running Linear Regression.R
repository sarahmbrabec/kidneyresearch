#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~FUNCTION - RUNNING LINEAR REGRESSION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


run_linear_regression <- function(dataset_chem_demo_response,
                                  chem_include)
{
  
  num_chemicals <- length(chem_include)
  kidney_lr_results <- data.frame(matrix(ncol = 200, nrow = num_chemicals))
  
  rownames(kidney_lr_results) <- chem_include
  
  list_covar_names<- c("intercept",
                       "chem", 
                       "gender", 
                       "race_1_mexican",
                       "race_2_other_hispanic",
                       "race_4_black", 
                       "race_5_other", 
                       "PIR", 
                       "cotinine",
                       "age")
  
  covar_codenames <- c("Intercept", 
                       "chem",
                       "RIAGENDR", 
                       ", ref = 3)1$", 
                       ", ref = 3)2$", 
                       ", ref = 3)4$", 
                       ", ref = 3)5$", 
                       "INDFMPIR", 
                       "LBXCOT", 
                       "RIDAGEYR" )
  
  covariate_vector <- c()
  
  
  #k <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10)
  
  # rows_not_fold <- c(1,3,4,5,6,8,9,10,11,13,14,15,16,18,19,20,21,23,24,25,26,28,29,30,31,33,34,35,36,38,39,40,41,43,44,45)
  rows_not_fold <- which(!grepl("fold",colnames(kidney_lr_results))==TRUE)
  
  summary_rows <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
  
  
  
  
  for(n in list_covar_names){
    
    covariate_label <- paste(n,
                             c("coefficient", "fold_diff", "std_error","t_value","p_value")
                             ,sep = "_")
    
    covariate_vector <- append(covariate_vector, covariate_label)

  }
  
  outcome_vector <-  c("albumin", "gfr", "cystatin", "creatinine")
  
  regression_stats <- c()
  
  for (n in outcome_vector)
  {
    covariate_label <- paste(n, covariate_vector,  sep ="_")
    # print(covariate_label)
    
    regression_stats <- append(regression_stats, covariate_label)
  }
  
  colnames(kidney_lr_results) <- regression_stats
  
  m <- 1 
  g <- 2 
  
  kidney_lr_results[,]<-NA
  
  #first_chem : last_chem
  
  for(chem in dataset_chem_demo_response[,chem_include]){
    
    chem_title <-  attr(chemicals_clean[ ,i],"label")
    
    # linear_mod_label <- paste(linear_mod,
    # attr(chemicals_clean[,chem],"label"),
    # sep =  "_")
    chem_colname <- colnames(dataset_chem_demo_response[,chem_include])[i]
    
    print(paste(i, 
                attr(chem,"label"), 
                chem_colname))
    
    if(identical(chem,dataset_chem_demo_response[,"URXUCD"]) == "TRUE"){
      # print('cadmium')
      cadmium_zero <-  which(chem == 0)
      chem[cadmium_zero] <- NA
    }
    
    if(all(is.na(chem))){
      #   print("empty")
    } else{
      
      try_albumin <- class(try(lm(data = dataset_chem_demo_response, log10(URXUMA) ~ (chem) +
                                    factor(RIAGENDR) +
                                    relevel(factor(RIDRETH1), ref = 3) +
                                    INDFMPIR +
                                    log10(LBXCOT)+
                                    RIDAGEYR)))
      
      
    }
    
    
    
    # print(try_albumin)
    if(try_albumin == "try-error")
    {
      # print(chem_colname)
      linear_Mod_albumin <- lm(data = dataset_chem_demo_response,
                               log10(URXUMA) ~ chem +
                                 relevel(factor(RIDRETH1),
                                         ref = 3) +
                                 INDFMPIR +
                                 log10(LBXCOT)+
                                 RIDAGEYR)
      summary_lm_albumin <- summary(linear_Mod_albumin)
      LR_result <- summary_lm_albumin
      
      #print(summary_lm_albumin)

    }
    else{
      linear_Mod_albumin <- lm(data = dataset_chem_demo_response, log10(URXUMA) ~ (chem) +
                                 factor(RIAGENDR) +
                                 relevel(factor(RIDRETH1), ref = 3) +
                                 INDFMPIR +
                                 log10(LBXCOT)+
                                 RIDAGEYR)
      summary_lm_albumin <- summary(linear_Mod_albumin)
      
      #print(summary_lm_albumin)
      
      LR_result <- summary_lm_albumin
      
      
    }
    
    print(LR_result)
    
    albumin_list <- c()
    for(n in seq(length(list_covar_names)))
    {
      covar_list <-LR_coefficient_assignment(LR_result, 
                                             covar_codenames[n], 
                                             list_covar_names[n], 
                                             chem)
      albumin_list <- c(albumin_list, covar_list)
      
    }
    
    # print(albumin_list)
    albumin_col <- grep("albumin", colnames(kidney_lr_results))
    
    kidney_lr_results[i,albumin_col] <- albumin_list
    
    
    
    
    # coefficients_row_num <- dim(summary_lm_albumin$coefficients)[1]
    # coefficients_col_num <- dim(summary_lm_albumin$coefficients)[2]
    
    # coefficients_col_seq <- rep(seq(coefficients_col_num), coefficients_row_num)
    # coefficients_row_seq <- sort(rep(seq(coefficients_row_num), coefficients_col_num))
    # 
    # 
    # 
    # coefficients_vector <- as.vector(summary_lm_albumin$coefficients)
    # 
    # if(coefficients_row_num == 9){
    #   c(coefficients_vector[1:])
    # }
    # 
    # print()
    # m <- 1
    # for(n in rows_not_fold[1:40]){
    # 
    #   print(summary_lm_albumin$coefficients[coefficients_row_seq[m],coefficients_col_seq[m]])
    #   kidney_lr_results[i,n] <- summary_lm_albumin$coefficients[coefficients_row_seq[m],
    #                                                             coefficients_col_seq[m]]
    m <- m + 1
    #}
    
    


    try_GFR <- class(try(lm(data = dataset_chem_demo_response, log10(GFR) ~ (chem) +
                              factor(RIAGENDR) +
                              relevel(factor(RIDRETH1), ref = 3) +
                              INDFMPIR +
                              log10(LBXCOT)+
                              RIDAGEYR)))
    # print(try_GFR)
    if(try_GFR == "try-error"){
      no_overlap <- length(which(!is.na(dataset_chem_demo_response$GFR)
                                 & !is.na(dataset_chem_demo_response$URXUUR)))
      # If there is overlap between the chemical and GFR
      if(no_overlap > 0){

        linear_Mod_GFR <- lm(data = dataset_chem_demo_response,
                             log10(GFR) ~ (chem) +
                               relevel(factor(RIDRETH1),
                                       ref = 3) +
                               INDFMPIR +
                               log10(LBXCOT)+
                               RIDAGEYR)
        summary_lm_GFR <- summary(linear_Mod_GFR)


        LR_result <- summary_lm_GFR
        #print(LR_result)

        intercept_list <- LR_coefficient_assignment(LR_result, "Intercept", "cont_intercept")


        #There is no overlap
      } else{
        print("no overlap")
      }
    }else{
      linear_Mod_GFR <- lm(data = dataset_chem_demo_response, log10(GFR) ~ (chem) +
                             factor(RIAGENDR) +
                             relevel(factor(RIDRETH1), ref = 3) +
                             INDFMPIR +
                             log10(LBXCOT)+
                             RIDAGEYR)
      summary_lm_GFR <- summary(linear_Mod_GFR)


      LR_result <- summary_lm_GFR
      #print(LR_result)


    }
    print(LR_result)
    
    gfr_list <- c()
    for(n in seq(length(list_covar_names)))
    {
      covar_list <-LR_coefficient_assignment(LR_result, 
                                             covar_codenames[n], 
                                             list_covar_names[n], 
                                             chem)
      gfr_list <- c(gfr_list, covar_list)
      
    }
    
    gfr_col <- grep("gfr", colnames(kidney_lr_results))

    kidney_lr_results[i, gfr_col] <- gfr_list


    try_cystatin <- class(try(lm(data = dataset_chem_demo_response, log10(SSCYPC) ~ (chem) +
                                   factor(RIAGENDR) +
                                   relevel(factor(RIDRETH1), ref = 3) +
                                   INDFMPIR +
                                   log10(LBXCOT)+
                                   RIDAGEYR)))
    # print(try_cystatin)
    if(try_cystatin == "try-error"){
      no_overlap <- length(which(!is.na(dataset_chem_demo_response$SSCYPC)
                                 & !is.na(dataset_chem_demo_response$URXUUR)))
      # If there is overlap between the chemical and cystatin
      if(no_overlap > 0){

        linear_Mod_cystatin <- lm(data = dataset_chem_demo_response,
                                  log10(SSCYPC) ~ (chem) +
                                    relevel(factor(RIDRETH1),
                                            ref = 3) +
                                    INDFMPIR +
                                    log10(LBXCOT)+
                                    RIDAGEYR)
        summary_lm_cystatin <- summary(linear_Mod_cystatin)
        LR_result <- summary_lm_cystatin
        intercept_list <- LR_coefficient_assignment(LR_result, "Intercept", "cont_intercept")
        # print("overlap")
        # print(summary_lm_cystatin)

        # There is no overlap
      } else{
        print("no overlap")
      }
    } else{
      linear_Mod_cystatin <- lm(data = dataset_chem_demo_response, log10(SSCYPC) ~ (chem) +
                                  factor(RIAGENDR) +
                                  relevel(factor(RIDRETH1), ref = 3) +
                                  INDFMPIR +
                                  log10(LBXCOT)+
                                  RIDAGEYR)
      summary_lm_cystatin <- summary(linear_Mod_cystatin)
      LR_result <- summary_lm_cystatin

    }
    
    print(LR_result)
    
    cystatin_list <- c()
    for(n in seq(length(list_covar_names)))
    {
      covar_list <- LR_coefficient_assignment(LR_result, 
                                             covar_codenames[n], 
                                             list_covar_names[n], 
                                             chem)
      cystatin_list <- c(cystatin_list, covar_list)
      
    }
    
    
    cystatin_col <- grep("cystatin", colnames(kidney_lr_results))

    kidney_lr_results[i, cystatin_col] <- cystatin_list
    

    try_creatinine <- class(try(lm(data = dataset_chem_demo_response, log10(LBXSCR) ~ (chem) +
                                     factor(RIAGENDR) +
                                     relevel(factor(RIDRETH1), ref = 3) +
                                     INDFMPIR +
                                     log10(LBXCOT)+
                                     RIDAGEYR)))
    # print(try_creatinine)
    if(try_creatinine == "try-error"){
      no_overlap <- length(which(!is.na(dataset_chem_demo_response$LBXSCR)
                                 & !is.na(dataset_chem_demo_response$URXUUR)))
      # If there is overlap between the chemical and cystatin
      if(no_overlap > 0){

        linear_Mod_creatinine <- lm(data = dataset_chem_demo_response,
                                    log10(LBXSCR) ~ (chem) +
                                      relevel(factor(RIDRETH1),
                                              ref = 3) +
                                      INDFMPIR +
                                      log10(LBXCOT)+
                                      RIDAGEYR)
        summary_lm_creatinine <- summary(linear_Mod_creatinine)
        LR_result <- summary_lm_creatinine
        #print("overlap")
        #print(summary_lm_creatinine)

        # There is no overlap
      } else{
        print("no overlap")
      }
    } else{
      linear_Mod_creatinine <- lm(data = dataset_chem_demo_response, log10(LBXSCR) ~ (chem) +
                                    factor(RIAGENDR) +
                                    relevel(factor(RIDRETH1), ref = 3) +
                                    INDFMPIR +
                                    log10(LBXCOT)+
                                    RIDAGEYR)
      summary_lm_creatinine <- summary(linear_Mod_creatinine)
      LR_result <- summary_lm_creatinine
    
    }
    
    print(LR_result)
    
    creatinine_list <- c()
    
    for(n in seq(length(list_covar_names)))
    {
      covar_list <-LR_coefficient_assignment(LR_result, 
                                             covar_codenames[n], 
                                             list_covar_names[n], 
                                             chem)
      creatinine_list <- c(creatinine_list, covar_list)
      
    }
    
    creatinine_col <- grep("creatinine", colnames(kidney_lr_results))

    kidney_lr_results[i, creatinine_col] <- creatinine_list
    i <<- i + 1
  }

  return(kidney_lr_results)
  
}



