#Author: Marveen Antonio S. Bernabe
#Section CMSC 150 B-5L
#Program -> Gaussian and Gauss-Jordan Elimination

# Start of exercise 4
# Gauss - Jordan Elimination
swapping <- function (vector, count){           # Swaps the rows and removes the columns based on count given
  var = c()                                     # To know which row to swap
  count4 = 1                        
  while (count4 < count){                       
    var = c(var, count4)
    count4 = count4 + 1
  }
  if (count == 1){
    vector2 = max(abs(vector[,count])) 
  } else{
    vector2 = max(abs(vector[-(var), count]))
  }
  print(vector2)
  maxRow = c()                                  # Gets the maxRow 
  temp = c()              
  count2 = count
  
  while (count2 < ncol(vector)){                # Swaps the vectors when it finds the maximum number of the three
    if (abs(vector[count2,count]) == vector2){
      #print(vector[count2, count])
      temp = vector[count, ]
      maxRow = vector[count2, ]
      vector[count, ] = maxRow
      vector[count2, ] = temp
      break
    }
    count2 = count2 + 1
  }
  return(vector)
}

gjelim <- function(matrix, verbose = T){            # The Gauss - Jordan Elimination Itself
  pivot_row = c()   
  pivot_element = 0
  count = 1
  count2 = 2
  vector3 = c()
  
  print("Aug Coeff Matrix")
  print(matrix)                                     # Shows the Matrix
  while (count < ncol(matrix)){                     # For iteration of rows
    if (verbose){
      print(paste("Normalized Row", count, sep = " "))
      matrix = swapping(matrix, count)              # Swaps the matrix
      print(matrix)
      pivot_row = matrix[count, ]       
      pivot_element = matrix[count,count]           # Gets Pivot Row and Pivot Element             
      normalized_row = pivot_row/pivot_element      # Gets Normalized Row
      print(normalized_row)                         # Shows the components of the matrix
      matrix[count, ] = normalized_row
      while (count2 <= nrow(matrix)){               # Iteration for normalizing equations
        if(count2 != count){
          vector3 = normalized_row * matrix[count2, count]  # Multiplies the first element of the matrix to the normalized row
          matrix[count2, ] = matrix[count2, ] - vector3     # Subtracts the normalized equation to the matrix row itself
        } else {
          print("SKIPPING")             # Skips when count == count2
        }
        count2 = count2 + 1
      }
      count2 = 1
      count  = count + 1
    }
  }
  print("GAUSS - JORDAN ELIMINATION")
  print(matrix)
  return(matrix)                    # Returns the Matrix Itself
}

GaussJordan <- function(vector, verbose = T){
  if (vector$augcoeffmatrix[1,1] == 0){
    print("Infinite Solutions, Cannot be solved")       # Checks if solution is Infinite or not
  } else{
    new_matrix = gjelim(vector$augcoeffmatrix)            # Solves the equation
    new_solution = new_matrix[, ncol(new_matrix)]
    potato = new_solution   
    return(potato)                                    # Returns the final matrix
  }
}
