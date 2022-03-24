# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  FUNCTION - RUNNING LINEAR MODELS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

try_linearMod <- function(chem) 
{
  
  chem_title <-  attr(chemicals_clean[ ,i],"label")
  
  chem_colname <- colnames(chem_demo_response_cycle_1_2_updated[,first_chem : last_chem])[i]
  print(paste(i, 
              attr(chem,"label"), 
              chem_colname))
  
  linearMod_albumin <- lm(data = chem_demo_response_cycle_1_2_updated, log10(URXUMA) ~ log10(chem) + 
                            relevel(factor(RIDRETH1), ref = 3) +
                            INDFMPIR +
                            LBXCOT+ 
                            RIDAGEYR)
  
  summary_lm_albumin <- summary(linearMod_albumin)
  
  return(summary_lm_albumin)
 

