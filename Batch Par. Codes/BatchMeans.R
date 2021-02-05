BatchMeans = function(k, ArrivalTimes, ServiceTimes){
  
  
  BatchStats = Myssq3Batch(ArrivalTimes, ServiceTimes)
  
  Batches = seq(to = 1, from = length(BatchStats[[6]]), by = - k) #ignore the last bit, rather than the first bit
  
  NodeMeans = BatchStats[[6]][Batches]
  
  QueueMeans = BatchStats[[7]][Batches]
  
  ServerMeans = BatchStats[[8]][Batches]
  
  Times = BatchStats[[9]][Batches]
  
  BatchDelays = (QueueMeans/k)#ignore the first index
     
  BatchWaits = (NodeMeans/k)
    
  BatchNodeNum = (NodeMeans/Times)
    
  BatchQNums = (QueueMeans/Times)
  
  BatchUtils = (ServerMeans/Times)
    
  
  #To make sure even batch size, some data has to be ignored, I reveresed the 
  #sequence named Batches in order to ignore the arrivals at the beginning 
  #rather than the end
  
  Ignores = (length(NodeMeans)-1)%%k  
  
  #[[1]] avg Delay of each batch
  #[[2]] avg wait time of each batch
  #[[3]] avg number in node of each batch
  #[[4]] avg number in queue of each batch
  #[[5]] avg system utilization of each batch
  #[[6]] Time elapsed between the beginning and end of each batch
  
  
  return(list(BatchDelays[-length(BatchDelays)] , BatchWaits[-length(BatchDelays)] , BatchNodeNum[-length(BatchDelays)] , BatchQNums[-length(BatchDelays)] , BatchUtils[-length(BatchDelays)] , Ignores))
  
}
