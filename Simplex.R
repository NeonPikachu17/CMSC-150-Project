#Simplex Method
source("AugCoeffMatrix.R")
checkCol <- function(mat){
  max = 0
  row = 0
  checker = mat[nrow(mat),]
  
  #checker of column
  for(i in checker){
    if(i < 0 && abs(i) > abs(max)){
      max = i
    }
  }
  index = match(max, checker)
  
  #Checker of row
  colum = mat[-nrow(mat),]
  test = colum[,ncol(colum)]/colum[,index]
  row = test[test>0]
  row = min(row[is.finite(row)], na.rm = TRUE)
  
  rown = match(row,test)
  
  j = list(x = rown, y = index)
  return(j)
}

createSlacks <- function(mat){
  vec = mat[nrow(mat),]
  RHS = mat[,ncol(mat)]
  vec2 = vec[-length(vec)]
  vec2 = vec2*(-1)
  matr1 = mat[,-ncol(mat)]
  matr = matrix(0, nrow(mat), ( ncol(mat) + nrow(mat)) )
  pot = mat[,-ncol(mat)]
  for(i in c(1:nrow(matr))){
    vim = matrix(0,1, nrow(matr))
    vim = as.vector((vim))
    vim[i] = 1
    if(i != nrow(matr)) matr[i,] = c(pot[i,], vim, mat[i,ncol(mat)])
    else matr[i,] = c(vec2, vim, 0)
  }
  return(matr)
}

Simplex <- function(mat){
  # Dual Problem
  print("Initial Matrix")
  print(mat)
  mat = t(mat)
  mat = createSlacks(mat)
  print("Transposed")
  print(mat)
  while(1){
    if(all(mat[nrow(mat),] >= 0)) break 
    lis = checkCol(mat)
    row = lis$x
    col = lis$y
    pivot_row = mat[row,]
    pivot_element = mat[row,col]
    print(paste("Pivot Element: ", pivot_element, sep = ""))
    if(pivot_element == 0) return(NULL)
    normalized_row = pivot_row/pivot_element
    print(paste("Normalized Row: ", normalized_row, sep = ""))
    mat[row,] = normalized_row
    for(i in c(1:nrow(mat))){
      if(all(mat[i, ] == normalized_row)) next
      vec = normalized_row * mat[i, col]
      mat[i,] = mat[i,] - vec
    }
    print(mat)
  }
  return(mat)
}

