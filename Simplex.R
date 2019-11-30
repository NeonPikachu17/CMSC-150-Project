#Simplex Method

create <- function(){
  v1 = c(200,5,6,7,8,9)
  v2 = c(10,8,0,1,0,0,0,80)
  v3 = c(1,0,0,0,1,0,0,9)

  li <- list(v1,v2,v3,v4,v5)
  
  mat = matrix(0, 5, 8)
  mat[1,] = v1
  mat[2,] = v2
  mat[3,] = v3
  mat[4,] = v4
  mat[5,] = v5
  return(mat)
}

create2 <- function(){
  v1 = c(1,1,1,1,1,500)
  v2 = c(.1,.4,.6,.5,.8,225)
  v3 = c(1,-2,0,0,0,0)
  v4 = c(0,0,-3,0,1,0)
  v5 = c(0,-1,-1,1,0,0)
  li <- list(v1,v2,v3,v4,v5)
  
  mat = matrix(0, 5, 6)
  mat[1,] = v1
  mat[2,] = v2
  mat[3,] = v3
  mat[4,] = v4
  mat[5,] = v5
  return(mat)
}

create3 <- function(){
  v1 = c(5,4,1,0,0,32)
  v2 = c(1,2,0,1,0,10)
  v3 = c(-2,-3,0,0,1,0)

  li <- list(v1,v2,v3)
  
  mat = matrix(0, 3, 6)
  mat[1,] = v1
  mat[2,] = v2
  mat[3,] = v3
  return(mat)
}

create4 <- function(){
  v1 = c(1,2,4)
  v2 = c(7,6,20)
  v3 = c(14,20,1)
  mat = matrix(0, 3, 3)
  mat[1,] = v1
  mat[2,] = v2
  mat[3,] = v3
  return(mat)
}

c

mem = create4()

#Simplex Method
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
  row = min(test[test>0], na.rm = TRUE)
  
  rown = match(row,test)
  
  j = list(x = rown, y = index)
  return(j)
}

createSlacks <- function(mat){
  matr = matrix(0, nrow(mat), (2* ncol(mat)))
  matri = mat[,ncol(mat)]
  matri = matri[-ncol(mat)]
  for(i in c(1:ncol(mat))){
    vec = seq(0, 0, length.out=ncol(mat))
    sol = mat[i,-ncol(mat)]
    if(i == ncol(mat)) sol = sol * -1   
    vec[i] = 1
    if (i == 3) arr = c(sol,vec,0)
    else arr = c(sol, vec, matri[i])
    print(arr)
    matr[i,] = arr
  }
  return(matr)
}

Simplex <- function(mat){
  # Dual Problem
  mat = t(mat)
  mat = createSlacks(mat)
  while(1){
    if(all(mat[nrow(mat),] >= 0)) break 
    print(mat)
    lis = checkCol(mat)
    row = lis$x
    col = lis$y
    pivot_row = mat[row,]
    pivot_element = mat[row,col]
    normalized_row = pivot_row/pivot_element
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


Simplex(mem)
