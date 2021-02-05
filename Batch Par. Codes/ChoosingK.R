
#Choose the utilization
Util = 0.6


mean = TraceSim[[4]]
OGUtil = TraceSim[[2]]
L = length(AdjCompService)
TraceBatch = LongRun(ComparrivalTimes, (Util/OGUtil)*AdjCompService)
ExpBatch = LongRun(ComparrivalTimes, rexp(L, 1/((Util/OGUtil)*mean)))

AvgQueueT = TraceBatch[[1]]
AvgNodeT = TraceBatch[[2]]
tT = TraceBatch[[3]]
iT = TraceBatch[[5]]
tcurrT = TraceBatch[[4]]
alpha = 0.05

AvgQueueE = ExpBatch[[1]]
AvgNodeE = ExpBatch[[2]]
tE = ExpBatch[[3]]
iE = ExpBatch[[5]]
tcurrE = ExpBatch[[4]]



kk = numeric(0)
IntTrace = IntExp = list(0)
for(j in 1:11){
kk[j] = 32*2^(j-1)
IntTrace[[j]] = ConfidenceIntervals(AvgQueueT, AvgNodeT, tT, tcurrT, iT, kk[j], alpha)
IntExp[[j]] = ConfidenceIntervals(AvgQueueE, AvgNodeE, tE, tcurrE, iE, kk[j], alpha)
}


#kk[4] is optimal for all but Util, k = 32 is optimal For Utilization

