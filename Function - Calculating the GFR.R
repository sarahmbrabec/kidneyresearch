# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # #~~~~~~~~~~~~~~~~~~~~~  FUNCTION - CALCULATING THE GLOMERCULAR FILTRATION RATE  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # 
 calculate_gfr <- function(merged_dataset)
{
   num_of_participants <- dim(merged_dataset)[1]



  participants_sex <- merged_dataset[ ,"RIAGENDR"]

  #print(participants_sex)

  participants_race <- merged_dataset[ , "RIDRETH1"]

  #print(participants_race)

  participants_scr <- merged_dataset[ , "LBXSCR"]

  #print(participants_scr)

  participants_age <- merged_dataset[ , "RIDAGEYR"]

  #print(participants_age)

  participants_height <- merged_dataset[, "BMXHT"]
  
  merged_dataset <- data.frame(merged_dataset, GFR_ADULT = NA)
  
  merged_dataset <- data.frame(merged_dataset, GFR_CHILD = NA)
# 
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ADULT GFR EQUATION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   
#   
  print(participants_age)
  
 
   for(participant in seq(num_of_participants)){
  
    respondent_sex <- participants_sex[participant]
  
    respondent_race <- participants_race[participant]
  
    respondent_scr <- participants_scr[participant]

    respondent_age <- participants_age[participant]
    
    respondent_height <- participants_height[participant]


    
     if(respondent_age >= 18){

       #print(participant)





       if(respondent_sex == 2){

         k = 0.7

         a = -0.329
       }
       else{

         k = 0.9

         a = -0.411
       }

       GFR_ADULT = 141 * (min(respondent_scr/k,1)^a) * (max(respondent_scr/k,1)^-1.209) * (0.993^respondent_age)

       if(respondent_sex == 2){

         GFR_ADULT <- GFR_ADULT*(1.0189)
       }

       if(respondent_race == 4){

         GFR_ADULT <- GFR_ADULT*(1.159)
         
         

       
       }
       
      merged_dataset[participant, "GFR_Adult"] <- GFR_ADULT
     }

     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CHILD GFR EQUATION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

     if(respondent_age < 18){


      
       GFR_CHILD = (0.41*respondent_height)/respondent_scr

       #print(GFR_child)
       
       
       
       #gfr_child_dataset <<- data.frame(GFR_child)
       
       #print(GFR_child)
      
       
        merged_dataset[participant, "GFR_Child"] <- GFR_CHILD

     }



 
   
  }

 
 
  merged_dataset <- data.frame(merged_dataset, GFR_CHILD = NA)
  
  merged_dataset <- data.frame(merged_dataset, GFR_ADULT = NA)

  return(merged_dataset)

}
 
 