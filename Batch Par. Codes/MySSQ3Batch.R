Myssq3Batch = function(ArrivalTimes, ServiceTimes, k){
  ti = 0.0 #initial time 
  tf = ComparrivalTimes[length(ComparrivalTimes)] 
  Inft = (100*tf) #Some arbitrary large number 
  
  #Initialize job statistics 
  
  tinn = 0  #time integrated number in node 
  tinq = 0  #time integrated number in queue 
  tins = 0  #time integrated number in service 
  
  #Initialize Time Statistics 
  
  tarr = -1       #next arrival time 
  tcomp = -1    #next completion time 
  tcurr = -1    #current time 
  tnext = -1    #next event time 
  tlast = -1    #last arrival time
  
  #initialize vector of stored departure times
  
  DepTimes = t = AvgQueue =  AvgNode = AvgServ = numeric(0)
  
  #Initialize Counters for departed jobs and number of jobs in node 
  i = n = 0 
  ind = 1
  t[1] = AvgQueue[1] = AvgNode[1] = AvgServ[1] = 0 
  
  #Set the first clock
  tcurr = ti                  #set the clock
  tarr = ArrivalTimes[1]#Schedule the first arrival 
  tcomp = Inft                #The first event can't be a completion
  
  
  
  while(tarr < tf || n > 0){
    
    tnext = min(tarr, tcomp)  #grab the next event, either arrival or completion 
    if(n > 0){
      tinn = tinn + (tnext - tcurr)*n
      tinq = tinq + (tnext - tcurr)*(n-1)
      tins = tins + (tnext - tcurr)
      
      if((i+n+1)%%k == 0){
        ind = ind + 1
        t[ind] = tnext
        AvgNode[ind] = tinn 
        AvgQueue[ind] = tinq
        AvgServ[ind] = tins 
      }
    }
    
    tcurr = tnext #advance the clock 
    
    if(tcurr == tarr){  #if we have an arrival,
      n = n + 1         #add one to the node 
      tarr = ArrivalTimes[n+i+1] #get the next interarrivalarrival times
      
      if(is.na(tarr) || is.na(tf)){break}
      if(tarr > tf){    #do not process jobs after the door has closed 
        tlast = tcurr   #end the clock as the last job arrives 
        tarr = Inft     #the next event must be a completion (which can happen after doors closed)
      }
      
      if(n == 1){
        tcomp = tcurr + ServiceTimes[i+1]  #get completion time for first person into system
      }
      
    }else{
      i = i + 1
      if(tcomp < Inft){DepTimes[i] = tcomp} #store the dep
      n = n-1
      if(n > 0){
        tcomp = tcurr + ServiceTimes[i+1]
      }else{
        tcomp = Inft
      }
    }
  }
  
  
  L = list((diff(AvgQueue)/k)[-1], (diff(AvgNode)/k)[-1], 
           (diff(AvgNode)/diff(t))[-1], (diff(AvgQueue)/diff(t))[-1], 
           (diff(AvgServ)/diff(t))[-1])
  names(L) = c("BatchDelays", "BatchWaits", 
               "BatchNodeNum", "BatchQueue", 
               "BatchUtil")
  return(L)
}
