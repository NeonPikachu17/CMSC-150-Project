#Simplex Method

create <- function(){
  v1 = c(7,11,1,0,0,0,0,77)
  v2 = c(10,8,0,1,0,0,0,80)
  v3 = c(1,0,0,0,1,0,0,9)
  v4 = c(0,1,0,0,0,1,0,6)
  v5 = c(-150,-175,0,0,0,0,1,0)
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

mem = create2()

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

Simplex <- function(mat){
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
