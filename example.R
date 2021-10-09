#### example
library(survival)

#### data
data=pbc
D=data[,c(2,3,5)]
D$time=D$time/365
D$status=as.numeric(D$status==2)
colnames(D)=gsub('time','Time',colnames(D))
colnames(D)=gsub('status','Delta',colnames(D))

out=L1_rmst_ph(D, tau=10 , re=100 )