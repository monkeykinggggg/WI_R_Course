library(data.table)


get_file_name_for_id <- function(directory, id){
  id_char = as.character(id)
  zeros = paste(rep("0", (3-nchar(id_char))), collapse = "")
  filename = paste(zeros,id_char,".csv", sep="")
  paste(directory, filename, sep="/")
}

pollutantmean <- function(directory, pollutant, id = 1:332 ) {
  whole_df = NULL
  for(i in id){
    df = data.frame(fread(get_file_name_for_id(directory, i)))
    whole_df = rbind(whole_df, df)
  }
  values_vec = whole_df[[pollutant]]
  not_na_list = !is.na(values_vec)
  mean(values_vec[not_na_list])
}
