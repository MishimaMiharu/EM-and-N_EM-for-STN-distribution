cf = function(c,k){
  result = c*mulp(0,k)
  for (i in 1:k) {
    result = result - c(mulp(i,k),0)
  }
  return(result)
}
