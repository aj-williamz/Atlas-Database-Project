cutoff = function(x, L){
  return(cor(x[1:(length(x) - L)], x[(L+1):length(x)]))
}