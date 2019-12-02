# Gauss - Jordan Elimination
source("GaussJordan.R")

# Start of Exer 6
checkVariables <- function (matrix){      # checks each variable and returns the variables to an array
  array = c()                             # creates an empty array to put items for naming of matrix
  count = 0
  while (count < (matrix+1)){             # main checker of variables
    array = c(array, paste("x", as.character(count), sep = "^"))
    count = count + 1
  }
  return(array)                           # returns completed array
}

solveMatrix <- function(matrix, x, y, degree){  # Sets up the Augmented Coefficient Matrix of the problem
  mat = matrix
  row = 1
  col = 0
  count = 0
  
  while(row <= nrow(matrix)){                 # My formula for solving the Augmented coefficient matrix
    vector1 = c()
    while (col < ncol(matrix)){
      if(col<nrow(matrix)){
        m = sum(x^(col+count))
      } else{
        m = sum(x^(count)*y)
      }
      vector1 = c(vector1, (m))
      col = col + 1
    }
    mat[row, ] = vector1
    col = 0
    count = count + 1
    row = row + 1
  }
  return(mat)                         # Returns the Matrix
}

stringManipulation <- function(solution_set){           # Function which uses the solution set and makes a Function that is a string
  count = 1
  while (count <= length(solution_set)){            # creates the variables part of the function/equation
    if (count == 1){
      men = paste(paste(solution_set[count], sep = ""),sep = "")    # For variable without degree (x0)
    } else{
      men = paste(men, " + ", paste(solution_set[count])," * ", paste(names(solution_set[count]), sep = "^"), sep = "") # For variable with degrees (x1, x2, .., etc)
    }
    count = count + 1
  } 
  
  ten = paste("function (", paste(substring(names(solution_set[1]), 1,1), collapse = ", "), ") ", sep = "") # creates the "function ( )" part of the equation
  wen = paste(ten, men, sep="")        # Combines the two strings together
  return(wen)                # Returns the String which contains the function
}

stringFunction <- function(string){        # Function which creates the functionalized function
  return(eval(parse(text = string)))       # returns the functionalized function thru eval(parse())
}

PolynomialRegression <- function(x, y, degree){       # Code in which uses all the other codes // main code
  
  matrix = matrix(0, (degree+1), (degree+2), dimnames = list(c(1:(degree+1)), c(checkVariables(degree), "RHS")))   # creates the matrix where the variables will be placed
  if(degree > 0){
    if (length(x) == length(y)){                                  # Checks if it is possible to use the function
      well <- list(augcoeffmatrix = solveMatrix(matrix, x, y, degree))    # creates a labelled list which contains the augcoeffmatrix
      solution_set = GaussJordan(well)                         # Uses Gauss Jordan to solve the solution set of the missig variables
      names(solution_set) = checkVariables(degree)             # Puts names to Solution Set
      well <- list(augcoeffmatrix = solveMatrix(matrix, x, y, degree), solution_set = solution_set,  # Puts final touches to the program by adding everything into one singular labelled list
                   function_string = stringManipulation(solution_set), function_function = stringFunction(stringManipulation(solution_set)))
    } else{
      return("NA") # Results to NA if problem's x length is not equal to y length
    }
  } else{
    return("NA")   # Results to NA if problem's degree to find is less than 1
  }
  return(well) # Retruns the finished labelled list
}
