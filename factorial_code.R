
#Nombre: Estefany Paredes

############################
#PRIMERA FORMA
############################
#Factorial_loop

Factorial_loop<-function(x){
  if(x<0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  if(x == 0){
    return(1)
  } else{
    y <- 1
    for(i in 1:x){
      y <- y * ((1:x)[i])
    }
    return(y)
  }
}

############################
#sEGUNDA FORMA
############################
#Factorial_reduce

Factorial_reduce <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  # ensure purrr package is installed
  if (!require('purrr', quietly = TRUE)) {
    stop('Please install the purrr package')
  }
  
  if(x == 0){
    return(1)
  } else{
    reduce(as.numeric(1:x), `*`) %>% return()
  }
}

############################
#TERCERA FORMA
############################
#Factorial_func

Factorial_func <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to, or greater than, zero")
  }
  
  if (x == 0){
    return (1)
  } else{
    return (x * Factorial_func(x-1))
  }          
}

############################
#CUARTA FORMA
############################
#Factorial_mem

memoization <- function(){
  
  values <- 1
  
  Factorial_mem <- function(x){
    
    if(x < 0){
      stop("Factorials can only be computed when x is equal to, or greater than, zero")
    }
    
    if (x == 0 | x == 1){
      return(1)
    } 
    
    if (length(values) < x){
      values <<- `length<-`(values, x)
    }
    
    if (!is.na(values[x])){
      return(values[x])
    }
    #calculate new values
    values[x] <<- x * factorial(x-1)
    values[x]
  }
  Factorial_mem
}

Factorial_mem <- memoization()

# benchmarking these four functions
library(microbenchmark)
microbenchmark(
  Factorial_loop(5),
  Factorial_reduce(5),
  Factorial_func(5),
  Factorial_mem(5)
)

microbenchmark(
  Factorial_loop(15),
  Factorial_reduce(15),
  Factorial_func(15),
  Factorial_mem(15)
)

microbenchmark(
  Factorial_loop(50),
  Factorial_reduce(50),
  Factorial_func(50),
  Factorial_mem(50)
)

