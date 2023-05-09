mulp = function(i,k){
  if(k>i){
    temp = mulp(i,k-1)
    result = c(0,temp)+c((k-1)*temp,0)
  }
  else if(k==i&i!=0){
    result = mulp(0,k-1)
  }
  else{
    result = 1
  }
  return(result)
}
