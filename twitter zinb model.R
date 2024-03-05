library(MASS)
library(pscl)


# load data
flu = read.csv("./twitter.ex2.csv")


# the second example using the binary variable, 'user_verified' as predictor, while adjusting for 'user_friends_count'
## predictors are largely dispersed and were log transformed in model
### model before applying any sampling method

flufit2=zeroinfl(tweet_retweet_count~user_verified+scale(log(user_friends_count+1)), data=flu, dist="negbin")    
truemod=summary(flufit2)


# Sampling
## N=200 is sample size, r=5 is the set size, and m=40 is the cycle size, Nr is number of draws needed for ERSS
N=200
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
  set.seed(1500*jobID+iitrack*2)
  
  flu$rowind=seq(1,nrow(flu),length=nrow(flu))
  srsdat <- flu[sample(1:nrow(flu), N,replace=FALSE),]
  srsy=srsdat$tweet_retweet_count
  
  #erss
  rss0 <- sample(flu$rowind,Nr,replace=FALSE)
  rss0=array(rss0,c(r,r,m))                          
  
  #ranking the sample#
  y1rs=array(NA, dim=c(r,ncol(flu),m))
  for (mi in 1:m)   ### cycle
  {
    y1rind=matrix(NA, r, ncol(flu))
    for (ri in 1:r)
    {
      y1kdat=flu[rss0[,ri,mi],]
      y1rind [ri,] = as.matrix(y1kdat[which.max(y1kdat$user_friends_count),] )
    }
    y1rs[,,mi] = y1rind
  } # M1
  
  varnames=colnames(flu)
  
  y1rst=matrix(NA, N, ncol(flu))
  for (ci in 1:ncol(flu))   ### cycle
  {
    y1temp=c()
    for (mi in 1:m)
    {
      y1temp=c(y1temp,y1rs[,ci,mi])
    }
    y1rst[,ci]=y1temp
  }
  colnames(y1rst)=colnames(flu)  
  rssdat=as.data.frame(y1rst) # binds on last dimension
  
  
  ### SRS model
  fitSRS<-zeroinfl(tweet_retweet_count~user_verified+scale(log(as.numeric(as.character(user_friends_count+1))+1)), data=srsdat, dist="negbin")
  mymod0=summary(fitSRS) 
  
  
  ### ERSS model
  #mymod1=summary(fitRSS)
  fitRSS<-zeroinfl(as.numeric(as.character(tweet_retweet_count))~user_verified+scale(log(as.numeric(as.character(user_friends_count))+1)), data=rssdat, dist="negbin")
  mymod1=summary(fitRSS)
  
  
  #estimate of beta from count model
  srs[ii]= mymod0$coefficients$count[2,1]
  rss[ii]= mymod1$coefficients$count[2,1]
  
  #bias for count model= estimate of beta in count - coeff from truemod
  bias_srs[ii]=abs(mymod0$coefficients$count[2,1] - truemod$coefficients$count[2,1])
  bias_rss[ii]=abs(mymod1$coefficients$count[2,1] - truemod$coefficients$count[2,1])
  
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
  rss.ci=c(confint(fitRSS, "count_user_verifiedTRUE"))
  srs.ci=c(confint(fitSRS, "count_user_verifiedTRUE"))
  rss.cp[ii]=(rss.ci[2]>=truemod$coefficients$count[2,1])*(rss.ci[1]<=truemod$coefficients$count[2,1])
  srs.cp[ii]=(srs.ci[2]>=truemod$coefficients$count[2,1])*(srs.ci[1]<=truemod$coefficients$count[2,1])
  
  
  ii=ii+1
  
}

result=round(cbind(srs,rss,bias_srs,bias_rss,mse_srs,mse_rss,pvsrs,pvrss),8)
result


mean(rss)
mean(srs)
mean(bias_rss)
mean(bias_srs)
mean(mse_srs, na.rm = T)
mean(mse_rss, na.rm = T)
eff=mean(mse_srs, na.rm = T)/mean(mse_rss, na.rm = T)
mean(pvrss, na.rm = T)
mean(pvsrs, na.rm = T)
mean(rss.cp, na.rm = T)
mean(srs.cp, na.rm = T)
mean(rssv, na.rm = T)
mean(srsv, na.rm = T)

