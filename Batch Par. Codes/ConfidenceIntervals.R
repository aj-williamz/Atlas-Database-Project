ConfidenceIntervals = function(AvgQueue, AvgNode, t, tcurr, i, k, alpha){
  
  b = length(t)%/%k
  s = seq(from = 1, to = length(t), by = b)
  L1 = list(diff(AvgQueue[s])/b, diff(AvgNode[s])/b)
  L2 = list((i/tcurr)*L1[[1]], (i/tcurr)*L1[[2]],(i/tcurr)*(L1[[2]]-L1[[1]]))
  names(L1) = c("AvgDelays", "AvgWaits")
  names(L2) = c("AvgQueueLength", "AvgNodeNum", "AvgUtil")
  L = c(L1, L2)
  names(L) = c(names(L1), names(L2))
  
  m = s = e = numeric(0)
  Conf = Cut = C = list(0)
  
  for(i in 1:5){
    Cut[[i]] = c(names(L)[i], cutoff(L[[i]], 2))
  }
  
  for(i in 1:5){
    m[i] = mean(L[[i]])
    s[i] = sd(L[[i]])
    e[i] = qt(1 - alpha,df = k-1)*s[i]/sqrt(k)
    Conf[[i]] = c(names(L)[i], m[i]-e[i], m[i]+e[i])
  }
  
  C = list(Cut, Conf)
  names(C) = c("CuttoffLag", "Confidence")
  return(C)
}

