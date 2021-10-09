L1_rmst_ph=function( D, tau=10 , re=100 ){
  
  n=nrow(D)
  
  ## Null model
  temp=survfit(Surv(D$Time,D$Delta)~1)
  # plot(temp$time,temp$surv,type='l',xlab = 'year',ylab = 'Survival probability',ylim = c(0,1))
  ind=which.min(abs(temp$time-tau))
  rmst=sum( ((temp$time-c(0,temp$time[-length(temp$time)]))*temp$surv)[1:ind]  )
  
  ## 
  modfit=coxph(as.formula(paste("Surv(Time,Delta)~", paste(colnames(D)[-(1:2)], collapse = "+ "))),D)
  
  #### L1
  y=apply(cbind(D$Time,tau),1,min)
  delta=apply(cbind(D$Delta,D$Time>tau),1,max)
  
  temp=survfit(Surv(D$Time,1-D$Delta)~1)
  tempind=c(sapply(1:n, function(kk){which.min(abs(y[kk]-temp$time))}))
  Ghat=temp$surv[tempind]
  w=delta/Ghat
  
  L1_nullmod=mean( w*abs(y-rmst) )/mean(w)
  
  temp=survfit(modfit,newdata=D)
  ind=which.min(abs(temp$time-tau))
  yhat=apply( ((temp$time-c(0,temp$time[-length(temp$time)]))*temp$surv)[1:ind,],2,sum )
  L1_mod=mean( w*abs(y-yhat) )/mean(w)
  
  ## variance estimation
  vv=gen.bootstrap.weights(1234,n,re)
  temp=apply(vv,2,resam,D,tau,y,delta,modfit,rmst) 
  ese=apply(temp,1,sd)
  CI=apply(temp, 1, quantile, c(0.025,0.975))
  
  
  out=data.frame('L1_nullmod'=L1_nullmod,'L1_mod'=L1_mod,
                 'L1_nullmod_ese'=ese[1],'L1_mod_ese'=ese[2])
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

