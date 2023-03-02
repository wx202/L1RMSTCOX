L1_rmst_ph=function( D, tau=10 , re=100 ){
  
n=nrow(D)
set.seed(12345)
index=sample(n, floor(n/2), replace = FALSE)
datav=D[index,]
datat=D[-index,]
nv=nrow(datav)
nt=nrow(datat)

#### parameter estimation using datav
## Null model
temp=survfit(Surv(datav$Time,datav$Delta)~1)
ind=which.min(abs(temp$time-tau))
rmst=sum( ((temp$time-c(0,temp$time[-length(temp$time)]))*temp$surv)[1:ind]  )

## 
modfit=coxph(as.formula(paste("Surv(Time,Delta)~", paste(colnames(D)[-(1:2)], collapse = "+ "))),datav)

#### statistics using datat
## L1
y=apply(cbind(datat$Time,tau),1,min)
delta=apply(cbind(datat$Delta,datat$Time>tau),1,max)

temp=survfit(Surv(datat$Time,1-datat$Delta)~1)
tempind=c(sapply(1:nt, function(kk){which.min(abs(y[kk]-temp$time))}))
Ghat=temp$surv[tempind]
w=delta/Ghat

L1_nullmod=mean( w*abs(y-rmst) )/mean(w)
  
temp=survfit(modfit,newdata=datat)
ind=which.min(abs(temp$time-tau))
yhat=apply( ((temp$time-c(0,temp$time[-length(temp$time)]))*temp$surv)[1:ind,],2,sum )
L1_mod=mean( w*abs(y-yhat) )/mean(w)
 
## variance estimation
vv=gen.bootstrap.weights(1234,nt,re)
temp=apply(vv,2,resam,D=datat,tau,y,delta,modfit,rmst) 
ese=apply(temp,1,sd)
CI=apply(temp, 1, quantile, c(0.025,0.975))
 

out=list('L1_nullmod'=L1_nullmod,'L1_mod'=L1_mod,
               'L1_nullmod_ese'=ese[1],'L1_mod_ese'=ese[2],'CI.quantile'=CI)
}


gen.bootstrap.weights=function(data.num, n, num.perturb=500){
  set.seed(data.num)
  sapply(1:num.perturb,function(x) sample(1:n,n,replace=T))
}


resam<- function(vv,D,tau,y,delta,modfit,rmst){
  data=D[vv,]
  y=y[vv]
  delta=delta[vv]
  n=nrow(data)
  
  ##
  temp=survfit(Surv(data$Time,1-data$Delta)~1)
  tempind=c(sapply(1:n, function(kk){which.min(abs(y[kk]-temp$time))}))
  Ghat=temp$surv[tempind]
  w=delta/Ghat
  
  L1_nullmod=mean( w*abs(y-rmst) )/mean(w)
  
  temp=survfit(modfit,newdata=data)
  ind=which.min(abs(temp$time-tau))
  yhat=apply( ((temp$time-c(0,temp$time[-length(temp$time)]))*temp$surv)[1:ind,],2,sum )
  L1_mod=mean( w*abs(y-yhat) )/mean(w)
  
  out=c(L1_nullmod=L1_nullmod,L1_mod=L1_mod)
  
}

