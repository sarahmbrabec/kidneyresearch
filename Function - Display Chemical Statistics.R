display_chem_statistics <- function(x)
{
  mean_chem <- mean(x, na.rm = TRUE)
  
  median_chem <- median(x, na.rm = TRUE)
  
  quantile_chem <- quantile(x, na.rm = TRUE, probs = c(0.05, 0.95))
  
  total_len <- length(x)
  
  na_len <- length(which(is.na(x)))
  
  not_na_len <- length(which(!is.na(x)))
  
  percent_na <- (na_len/total_len)*100
  
  percent_not_na <- (not_na_len/total_len)*100
  
  return_dataset <- data.frame("Mean" = mean_chem,
                               "Median" = median_chem,
                               quantile_5th = quantile_chem[1],
                               quantile_95th =  quantile_chem[2],
                               perc_without_measurements = percent_na,
                               perc_with_measurements = percent_not_na,
                               stringsAsFactors = FALSE)
  
  return(return_dataset)
}