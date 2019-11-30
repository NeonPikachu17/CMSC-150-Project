# Augmented Coefficient Matrix
checkFunctions <- function (equation){               # checks the length of each equation
  func = splitFunction(equation[[1]], 1)[1]
  func = substring(func, 11)
  func = strsplit(func, ", ")
  func = substring(func[[length(func)]], 1, 2)
  
  return(func)
}

splitFunction <- function (string, num){          # splits the function using deparse
  if(num == 1)
    return(deparse(string)[1])
  else
    return(deparse(string)[2])
} 

createMatrix <- function (equation){         # creates a matrix based off the function with the most parameters
  dem = list(c(1:length(equation)), c(checkFunctions(equation), "RHS"))
  matrix1 = matrix(0, nrow = length(checkFunctions(equation)), ncol = (length(checkFunctions(equation)) + 1), dimnames = dem)
  return(matrix1)
}

checkArguments <- function (equation, array){         # Checks each argument and places it into an array for easier placement in matrices
  count = 1
  count2 = 1
  array2 = c()
  
  form1 = strsplit(splitFunction(equation, 2), "\\+ ")[[1]]      # splices the function into parts when it sees a "+ "
  while (count2 <= length(form1)){                      
    form2 = strsplit(strsplit(form1[count2], "\\* ")[[1]], " ") # splices the function into parts when it sees a "* "
    while (count <= length(array)){
      if (grepl(form2[2], array[count])){
        array2[count] = as.double(form2[1])             # places values when value is found to be in a value (x1 = x1)
      } else{
        array2[length(array)] = as.double(form2[1]) * -1     # places value to array when it is the RHS
      }
      count = count + 1
    }
    count = 1
    count2 = count2 + 1
  }
  return(array2)                                 # returns the array
}

placeArgument <- function (equation, matrix, array){         # Places the variables into the matrix that was created
  point = 1
  matrix2 = matrix
  
  while (matrix2[point, point] != 0){
    point = point + 1
  }
  matrix2[point, ] = array
  return(matrix2)                              # returns the matrix
}

AugCoeffMatrix <- function (equation){         # the main function
  
  pen = createMatrix(equation)
  for(point in equation){
    meh = checkArguments(point, c(checkFunctions(equation), "RHS"))
    pen = placeArgument(equation, pen, meh)
  }
  dam <- list(variables = checkFunctions(equation), augcoeffmatrix = pen)  # names the matrix
  return(dam)             # returns the final matrix
}