############## NHANES toothwear data application #################
############ using age as the ranking variable #######################
library(dplyr)
library(MASS)
library(pscl)

#read in data
nhanes = read.csv("nhanes.ex1.csv")

###population hurdle model
dat.fit=zeroinfl(count~MCQ010+as.numeric(RIDAGEYR)+RIAGENDR+HID040, data=nhanes, dist="negbin")
truemod=summary(dat.fit)


#Sampling
## N=500 is sample size, r=5 is the set size, and m=100 is the cycle size, Nr is number of draws needed for ERSS
N=500
r=5
m=N/r
Nr=m*r^2  #or Nr=N*r  


srs=c()
rss=c()
bias_rss=c()
bias_srs=c()
mse_srs=c()
mse_rss=c()
pvrss=c()
pvsrs=c()
rssv=c()
srsv=c()
rss.cp=c()
srs.cp=c()


ii=1
iitrack=0
iter<-500

jobID=as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
jobID=1

while (ii <=iter){
  iitrack=iitrack+1
  set.seed(500*jobID+iitrack*3)
  
  nhanes$rowind=seq(1,nrow(nhanes),length=nrow(nhanes))
  srsdat <- nhanes[sample(1:nrow(nhanes), N,replace=FALSE),]
  srsy=srsdat$count
  
  #erss
  rss0 <- sample(nhanes$rowind,Nr,replace=FALSE)
  rss0=array(rss0,c(r,r,m))                          
  
  #ranking the sample#
  y1rs=array(NA, dim=c(r,ncol(nhanes),m))
  for (mi in 1:m)   ### cycle
  {
    y1rind=matrix(NA, r, ncol(nhanes))
    for (ri in 1:r)
    {
      y1kdat=nhanes[rss0[,ri,mi],]
      
      y1rind [ri,] = as.matrix(y1kdat[which.max(y1kdat$RIDAGEYR),] )
    }
    y1rs[,,mi] = y1rind
  } # M1
  
  varnames=colnames(nhanes)
  y1rst=matrix(NA, N, ncol(nhanes))
  for (ci in 1:ncol(nhanes))   ### cycle
  {
    y1temp=c()
    for (mi in 1:m)
    {
      y1temp=c(y1temp,y1rs[,ci,mi])
    }
    y1rst[,ci]=y1temp
  }
  colnames(y1rst)=colnames(nhanes)  
  rssdat=as.data.frame(y1rst) # binds on last dimension
  
  ###SRS model  
  fitSRS<-zeroinfl(count~MCQ010+as.numeric(RIDAGEYR)+RIAGENDR+HID040, data=srsdat, dist="negbin")
  mymod0=summary(fitSRS) 
  
  ### ERSS model
  fitRSS<-zeroinfl(count~MCQ010+as.numeric(RIDAGEYR)+RIAGENDR+HID040, data=rssdat, dist="negbin")
  mymod1=summary(fitRSS)
  
  #estimate of beta from count model
  srs[ii]= mymod0$coefficients$count[2,1]
  rss[ii]= mymod1$coefficients$count[2,1]
  
  #bias for count model= estimate of beta in count - coeff from truemod
  bias_srs[ii]=mymod0$coefficients$count[2,1] - truemod$coefficients$count[2,1]
  bias_rss[ii]=mymod1$coefficients$count[2,1] - truemod$coefficients$count[2,1]
  
  #standard errors  
  srsv[ii]=mymod0$coefficients$count[2,2]
  rssv[ii]=mymod1$coefficients$count[2,2]
  
  #mse
  mse_srs[ii]=(bias_srs[ii])^2 + (srsv[ii])^2
  mse_rss[ii]=(bias_rss[ii])^2 + (rssv[ii])^2 
  
  #pvalue
  pvrss[ii]=mymod1$coefficients$count[2,4]
  pvsrs[ii]=mymod0$coefficients$count[2,4]
  
  #coverage probability
  ##########################################
  rss.ci=c(confint(fitRSS, "count_MCQ010"))
  srs.ci=c(confint(fitSRS, "count_MCQ010"))
  rss.cp[ii]=(rss.ci[2]>=truemod$coefficients$count[2,1])*(rss.ci[1]<=truemod$coefficients$count[2,1])
  srs.cp[ii]=(srs.ci[2]>=truemod$coefficients$count[2,1])*(srs.ci[1]<=truemod$coefficients$count[2,1])
  
  ii=ii+1
  
}

result=round(cbind(srs,rss,bias_srs,bias_rss,mse_srs,mse_rss,pvsrs,pvrss),8)
result


mean(rss)
mean(srs)
mean(rssv)
mean(srsv)
mean(bias_rss)
mean(bias_srs)
mean(mse_srs)
mean(mse_rss)
eff=mean(mse_srs)/mean(mse_rss)
mean(pvrss)
mean(pvsrs)
mean(rss.cp)
mean(srs.cp)

