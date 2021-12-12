gauss_jordan = function(a,b){
  
  replace_row = function(m, r1, r2, k){
    m[r1,] = m[r1,] + m[r2,]*k
    return(m)
  }
  
  swap_row = function(m, r1, r2){
    temp <- m[r2,]
    m[r2,] <- m[r1,]
    m[r1,] <- temp
    return(m)
  }
  
  m = matrix(c(a,b), nrow=nrow(a))
  b = nrow(m)
  colx = 1
  
  for(i in 1:b){
    if(m[i,i] == 0){
      pos = i
      repeat{
        pos = pos + 1
        if(pos > b){
          break 
        }else if(m[pos,i] != 0 && rowSums(m)[i] != 0){
          m = swap_row(m, i, pos)
        }else if(rowSums(m)[i] == 0){
          for(j in 1:(b-i)){
            m = swap_row(m, i+j-1, i+j)
          }
        }
      }
    }
  }
  
  while(b != 1){
    for(i in 1:(b-1)){
      m = replace_row(m, i+colx, colx, -m[i+colx,colx]/m[colx,colx])
    }
    b = b-1
    colx = colx+1 
  }
  
  b = nrow(m)
  colx = nrow(m)
  while(b != 1){
    for(i in 1:(b-1)){
      m = replace_row(m, colx-i, colx, -m[colx-i,colx]/m[colx,colx])
    }
    m[b,] = m[b,]/m[colx,colx]
    b = b-1
    colx = colx-1 
  }
  m[1,] = m[1,]/m[1,1]
  return(m)
}

#_______________________________________________________________________
swap_row = function(m, r1, r2){
  temp <- m[r2,]
  m[r2,] <- m[r1,]
  m[r1,] <- temp
  return(m)
}

scale_row = function(m, r, k){
  m[r,] = m[r,]*k
  return(m)
}

replace_row = function(m, r1, r2, k){
  m[r1,] = m[r1,] + m[r2,]*k
  return(m)
}

#_______________________________________________________________________
swap = function(m){
  b= nrow(m)
  
  for(i in 1:b){
    if(m[i,i] == 0){
      pos = i
      repeat{
        pos = pos + 1
        if(pos > b){
         break 
        }else if(m[pos,i] != 0 && rowSums(m)[i] != 0){
          m = swap_row(m, i, pos)
        }else if(rowSums(m)[i] == 0){
          for(j in 1:(b-i)){
           m = swap_row(m, i+j-1, i+j)
          }
        }
      }
    }
  }
  return(m)
}
#-----------------------------------------------------------------------
m = matrix(c(1,2,3,-1,1,1,-1,2,0,-1,-1,3,3,1,2,1), nrow=4)
n = matrix(c(4,1,-3,4))

gauss_jordan(m,n)

a = matrix(c(1,-3,1,
             2,3,-1,
             3,-2,-2), nrow=3, byrow = TRUE)
b = matrix(c(8,1,7))

i = matrix(c(1,0,0,
             0,1,0,
             0,0,1), nrow=3, byrow = TRUE)
gauss_jordan(a,i)

x = matrix(c(0,6,0,1,0,9,1,0,0,3,1,0,1,4,0,0), nrow=4)
x
swap(x)

x = matrix(c(3,2,-1,3,2,-2,0,3,4,3,1,-3,-2,1,0,2,4,1,0,1,-1,-1,2,1,0), 5,5, byrow = T)
x
det(x)
gauss_jordan(x)
