
#Nombre: Estefany Paredes

# Convierte un data_frame en un objeto "LongitudinalData"
make_LD <- function(x){
  structure(x, class = "LongitudinalData")
}

# Para extraer informacion subject-specific 
subject <- function(x, i) UseMethod("subject")

subject.LongitudinalData <- function(x, i) {
  
  index <- which(x$id %in% i)
  x <- lapply(x, function(x) x[index])
  
  structure(x, class = "LongitudinalData")
}

# Para extraer informacion extracting visit-specific 
visit <- function(i, j) UseMethod("visit")

visit.LongitudinalData <- function(x, j) {
  
  index <- which(x$visit %in% j)
  x <- lapply(x, function(x) x[index])
  
  structure(x, class = "LongitudinalData")
  
}

# Para extraer informacion room-specific 
room <- function(i, j) UseMethod("room")

room.LongitudinalData <- function(x, k) {
  
  index <- which(x$room == k)
  x <- lapply(x, function(x) x[index])
  
  structure(x, class = "LongitudinalData")
  
}

# Imprimir un objeto  "LongitudinalData"
print.LongitudinalData <- function(x, ...) {
  
  if(length(unique(x$id)) == 1){
    cat("Subject ID: ", unique(x$id), "\n")  
  } else if(length(unique(x$id)) == 0) {
    cat("NULL")
  } else {
    cat("Longitudinal dataset with", length(unique(x$id)), "subjects")
  }
  if(length(unique(x$visit)) == 1){
    cat("Visit: ", unique(x$visit), "\n")
  }
  if(length(unique(x$room)) == 1){
    cat("Room: ", unique(x$room))
  }
  invisible(x)
}

# Summary function
summary.LongitudinalData <- function(object, ...) {
  object <- list(
    summary.id = unique(object$id),
    summary.data = data.frame(
      visit = object$visit,
      room = object$room,
      value = object$value
    ) 
  )
  if(length(unique(object$summary.data$visit)) == 1 & 
     length(unique(object$summary.data$room)) == 1){
    
    object$summary.data <- summary(object$summary.data$value)
    
  } else {
    
    object$summary.data <- object$summary.data %>% 
      aggregate(value ~ visit + room, FUN = mean, data = .) %>%
      spread(room, value)
    
  }
  
  class(object) <- "summary_LongitudinalData"
  object
}

# Print summary method for "LongitudinalData" object
print.summary_LongitudinalData <- function(x, ...) {
  cat("Subject ID: ", x$summary.id, "\n")
  print(x$summary.data)
  invisible(x)
}

#####################################################

library(readr)
library(magrittr)
library(tidyr)
source("oop_code.R")

data <- read_csv("data/MIE.csv")

x <- make_LD(data)

print(class(x))
print(x)

out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54)
summary(out)

out <- subject(x, 14)
summary(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
summary(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room")
summary(out)