#Quadratic Spline Method
source("GaussJordan.R")

#Gets the variables of the matrix
createVariables <- function(matrix){
  var = nrow(matrix)-1
  vars = c()
  vars2 = c("a", "b", "c")
  for(p in c(1:var)){
    for(z in vars2){
      if(z == "a" && p == 1){}
      else vars = c(vars, paste(z, p, sep = ""))
    }
  }
  vars = c(vars, "RHS")
  return(vars)
}

#Generates the Matrix
generateMatrix <- function(matrix){
  arr = createVariables(matrix)
  points = nrow(matrix)
  n = points-1
  counter = 1
  number_equations = 3*n
  i = 2
  meh = c("a","b","c")
  x = matrix[,1]
  y = matrix[,2]
  count = 1
  
  qMatrix = matrix(0, number_equations-1, number_equations)
  colnames(qMatrix) = arr
  a1 = 0
  for(p in c(2:n)){
    a = x[p]^2
    b = x[p]
    c = 1
    for(m in c((p-1):p)){ # For condition 1 Equation
      for(i in meh){   #Don't mind the triple for loop thankies
        if(i == "a" && m == 1){
        } else{
          index = match(paste(i, m, sep = ""), arr)
          if(i == "a") qMatrix[count, index] = a
          else if(i == "b") qMatrix[count, index] = b
          else if(i == "c") qMatrix[count, index] = c
        }
      }
      qMatrix[count, ncol(qMatrix)] = y[p]
      count = count + 1
    }
  }
  
  for(z in c(1,points)){ # For condition 2 Equation
    a = x[z]^2
    b = x[z]
    c = 1
    o = z
    for(g in meh){
      if(g == "a" && z == 1){
      } else{
        if(z == points){
          z = z - 1
        }
        index = match(paste(g, z, sep = ""), arr)
        if(g == "a") qMatrix[count, index] = a
        else if(g == "b") qMatrix[count, index] = b
        else if(g == "c") qMatrix[count, index] = c            
      }
    }
    print(paste(o, "hehe"))
    qMatrix[count, ncol(qMatrix)] = y[o]
    count = count + 1
  }
  
  for(sea in c(2:n)){
    men = c("a","b")
    mun = 1
    for(po in c((sea-1), sea)){   #Don't mind the triple for-loop
      for(pot in men){
        if(pot == "a" && po == 1){}
        else{
          index = match(paste(pot, po, sep=""), arr)
          if(pot == "a") meow = 2*(x[sea])
          else meow = 1
          if(mun%%2 == 0) meow = meow * -1
          qMatrix[count, index] = meow
        }
      }
      mun = mun + 1
    }
    count = count + 1
  }
  return(qMatrix)
}

#Quadratic Spline Method
QuadraticSpline <- function(matrix){
  arr = createVariables(matrix)
  meow <- list(augcoeffmatrix = generateMatrix(matrix))
  wew = GaussJordan(meow)
  wew = c(0, wew)
  names(wew) = names = c("a1", arr[-length(arr)])
  return(wew)
}
