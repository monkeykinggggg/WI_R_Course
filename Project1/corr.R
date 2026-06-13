corr <- function( directory , threshold = 0){
  source("complete.R")
  nobs_df <- complete(directory)
  all_correlations = numeric()
  ids_above_threshold <- nobs_df$id[nobs_df$nobs > threshold]
  for(i in ids_above_threshold){
    curr_df = read.csv(get_file_name_for_id(directory, i))
    curr_df = curr_df[complete.cases(curr_df),]
    curr_cor = cor(curr_df$sulfate,curr_df$nitrate)
    all_correlations = c(all_correlations, curr_cor)
  }
  all_correlations
}