library(dplyr)
library(tidyr)
setClass("subject", slots = list( data = "data.frame", id = "numeric"))
setClass("visit", slots = list(data = "data.frame",id = "numeric",visit = "numeric"))
setClass("room", slots = list(data = "data.frame", id = "numeric", visit = "numeric", room = "character"))
setClass("LongitudinalData", slots = list(data = "data.frame"))

make_LD <- function(dataframe){
  new("LongitudinalData", data = as.data.frame(dataframe))
}

setMethod("show", "LongitudinalData", function(obiekt){
  n<-length(unique(obiekt@data$id))
  cat("Longitudinal dataset with", n, "subjects\n")
})

setMethod("show", "subject", function(obiekt){
  if(nrow(obiekt@data)==0){
    print(NULL)
  }
  else{
    cat("Subject ID: ", obiekt@id, "\n")
  }
})

setMethod("show", "visit", function(obiekt){
  if(nrow(obiekt@data)==0){
    print(NULL)
  }
  else{
    cat("ID:", obiekt@id, "\n")
  }
  cat("Visit:", obiekt@visit, "\n")
})

setMethod("show", "room", function(obiekt){
  if(nrow(obiekt@data)==0){
    print(NULL)
  }
  else{
    cat("ID:", obiekt@id, "\n")
  }
  cat("Visit:", obiekt@visit, "\n")
  cat("Room:", obiekt@room, "\n")
})

setGeneric("subject", function(obiekt, id) standardGeneric("subject"))
setMethod("subject", "LongitudinalData", function(obiekt, id){
  person_rows = obiekt@data[obiekt@data$id==id,]
  if(nrow(person_rows)==0){
    return(NULL)
  }
  else{
    new("subject",  data=person_rows, id=id)
  }
})

setMethod("summary", "subject", function(object){
  statistics <- object@data %>%
    group_by(visit, room) %>%
    summarise(avg_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = room, values_from = avg_val)
  statistics <- statistics[, c("visit", sort(setdiff(names(statistics), "visit")))]
  cat("ID:", object@id, "\n")
  return(as.data.frame(statistics))
})

setGeneric("visit", function(obiekt, id) standardGeneric("visit"))
setMethod("visit", "subject", function(obiekt, id){
  visit_rows<-obiekt@data[obiekt@data$visit==id,]
  if(nrow(visit_rows)==0)
    return(NULL)
  else{
    new("visit",  data=visit_rows,id=obiekt@id, visit=id)
  }
})

setGeneric("room", function(obiekt, roomname) standardGeneric("room"))
setMethod("room", "visit", function(obiekt, roomname){
  roomrows<-obiekt@data[obiekt@data$room==roomname,]
  if(nrow(roomrows)==0)
    return(NULL)
  else{
    new("room",  data=roomrows, id=obiekt@id,visit=obiekt@visit,room=roomname )
  }
})

setMethod("summary", "room", function(object){
  roomdata<-object@data
  roomdata<-roomdata[["value"]]
  cat("ID:", object@id, "\n")
  return(summary(roomdata))
})


