library(MASS)
library(pscl)

# load data
flu = read.csv("./twitter.ex2.csv")

# histogram of variables 
hist(flu$tweet_retweet_count)
table(flu$user_followers_count)
table(flu$user_favorites_count)


# the first example using the continuous variable, 'user_followers_count' as predictor, while adjusting for 'user_favorites_count'
## predictors are largely dispersed and were log transformed in model
### model before applying any sampling method

flufit1=hurdle(tweet_retweet_count~log1p(user_followers_count)+log1p(user_favorites_count), data=flu, dist="negbin")    
truemod=summary(flufit1)


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
  set.seed(500*jobID+iitrack*3)
  
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
      
      y1rind [ri,] = as.matrix(y1kdat[which.max(y1kdat$user_favorites_count),] )
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
  
  #Hurdle models 
  ### with srs data
  fitSRS<-hurdle(tweet_retweet_count~log1p(as.numeric(as.character(user_followers_count)))+log1p(as.numeric(as.character(user_favorites_count))), data=srsdat, dist="negbin")
  mymod0=summary(fitSRS) 
  
  
  ### with erss data
  fitRSS<-hurdle(tweet_retweet_count~log1p(as.numeric(as.character(user_followers_count)))+log1p(as.numeric(as.character(user_favorites_count))), data=rssdat, dist="negbin")
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
  rss.ci=c(confint(fitRSS, "count_log1p(as.numeric(as.character(user_followers_count)))"))
  srs.ci=c(confint(fitSRS, "count_log1p(as.numeric(as.character(user_followers_count)))"))
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
mean(mse_srs)
mean(mse_rss)
eff=mean(mse_srs)/mean(mse_rss)
mean(pvrss)
mean(pvsrs)
mean(rss.cp)
mean(srs.cp)
mean(rssv, na.rm = T)
mean(srsv, na.rm = T)
