divisors = function(x, r){
div = numeric(0)
div[1] = 1
for(i in 2:x) {
  if((x %% i) <= r) {
    div = c(div, i)
  }
}
  return(div)
}