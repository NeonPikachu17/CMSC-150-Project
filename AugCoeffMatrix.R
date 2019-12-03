# Augmented Coefficient Matrix
checkFunctions <- function (equation){               # checks the length of each equation
  func = splitFunction(equation[[1]], 1)[1]
  func = substring(func, 11)
  func = strsplit(func, ", ")
  func = substring(func[[length(func)]], 1, 3)
  
  return(func)
}

splitFunction <- function (string, num){          # splits the function using deparse
  if(num == 1)
    return(deparse(string, width.cutoff = 500L)[1])
  else
    return(deparse(string, width.cutoff = 500L)[2])
} 

createMatrix <- function (equation){         # creates a matrix based off the function with the most parameters
  dem = list(c(1:length(functions)), c(checkFunctions(functions), "RHS"))
  matrix1 = matrix(0, nrow = length(equation), ncol = (length(checkFunctions(equation)) + 1), dimnames = dem)
  return(matrix1)
}



