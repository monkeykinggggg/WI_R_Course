complete <- function( directory , id = 1:332 ){
  comp <- data.frame(id=numeric(), nobs=numeric())
  for(i in id){
    df = read.csv(get_file_name_for_id(directory, i))
    curr_comp = sum(complete.cases(df))
    comp = rbind(comp, c(id = i, nobs = curr_comp))
  }
  colnames(comp) = c("id", "nobs")
  comp
}