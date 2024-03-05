library(MASS)
library(pscl)

######################################################
r<-2 #set size
mv=c(50,70,100) #cycle
rhov=c(0.2, 0.4, 0.6, 0.8) #rho

beta0<-0
betax<-.5
betaz<-.5
betac<-0.02
sig_c<-matrix(0,10,10)
diag(sig_c) <- 1

betax<-1
betaz<-1

########################################################
bias_rss=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)) )
bias_srs=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))  
mse_rss=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100))) 
mse_srs=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))  
power_rss=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100))) 
power_srs=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))  
cp_rss=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))  
cp_srs=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))  
effv=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))
se_rss=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))
se_srs=array(,dim=c(4,3),dimnames = list(rho=c(0.2,0.4,0.6,0.8),cycle=c(50,70,100)))


################## Simulation ########################

for (p in 1:4) {
  rho=rhov[p]
  
  for (s in 1:3) {
    m=mv[s]
    
    rss=c()   
    srs=c() 
    rssv=c()  
    srsv=c()
    psrs=c()  
    prss=c()
    mserss=c() 
    msesrs=c()
    rss.cp=c()
    srs.cp=c()
    
    sig<-cbind(c(1,rho),c(rho,1))
    RSSx<-matrix(0,nrow=m*r,ncol=2)
    covx<-matrix(0, nrow=m*r, ncol=10)
    RSSdata<-matrix(0,nrow=m*r,ncol=13)
    
    ii=1
    iitrack=0
    iter<-1000
    
    jobID=as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
    jobID=1
    
    while (ii <=iter){
      tryCatch({
        
        iitrack=iitrack+1
        set.seed(500*jobID+iitrack*3)
        
        x<-mvrnorm(n=r*m,mu=c(0,0),Sigma=sig)
        
        c<-mvrnorm(n=r*m,mu=c(0,0,0,0,0,0,0,0,0,0), Sigma=sig_c)
        
        N = r*m              
        
        # to generate count outcome, Y with excess zero, first simulate U from binomial distribution
        U <- rbinom(N, 1, 0.6)
        
        #Variable to store the samples from the mixture distribution                                             
        y = rep(NA,N)
        
        #Simulate y from Poisson distribution
        y = rpois(n=N,lambda=exp(beta0+betax*x[,1]+betaz*x[,2]+
                                   betac*c[,1]+betac*c[,2]+
                                   betac*c[,3]+betac*c[,4]+
                                   betac*c[,5]+betac*c[,6]+
                                   betac*c[,7]+betac*c[,8]+
                                   betac*c[,9]+betac*c[,10]))
        
        #if U=0 then y=0, otherwise y remains count outcome from Poisson distribution
        y1 <- ifelse(U == 0, 0, y)
        SRSdata<-cbind(y1,x,c)     #this is SRS data
        
        #ranking the sample
        for(k in 1:m){
          for(j in 1:r){
            x<-mvrnorm(n=r,mu=c(0,0),Sigma=sig)
            
            RSSx[r*(k-1)+j,]<-x[which.max(x[,2]),]   # ranking based on max z-value
          }
        }
        
        y<-rpois(n=r*m,lambda=exp(beta0+betax*RSSx[,1]+betaz*RSSx[,2]+
                                    betac*c[,1]+betac*c[,2]+
                                    betac*c[,3]+betac*c[,4]+
                                    betac*c[,5]+betac*c[,6]+
                                    betac*c[,7]+betac*c[,8]+
                                    betac*c[,9]+betac*c[,10]))
        y2 <- ifelse(U == 0, 0, y)
        RSSdata<-cbind(y2,RSSx,c)     #this is ERSS data
        
        
        # Models for SRSdata#
        fitSRS<-hurdle(SRSdata[,1]~SRSdata[,2]+SRSdata[,3]+
                         SRSdata[,4]+SRSdata[,5]+
                         SRSdata[,6]+SRSdata[,7]+
                         SRSdata[,8]+SRSdata[,9]+
                         SRSdata[,10]+SRSdata[,11]+
                         SRSdata[,12]+SRSdata[,13],dist="poisson")
        mymod0=summary(fitSRS)
        
        # Models for RSSdata#
        fitRSS<-hurdle(RSSdata[,1]~RSSdata[,2]+RSSdata[,3]+
                         RSSdata[,4]+RSSdata[,5]+
                         RSSdata[,6]+RSSdata[,7]+
                         RSSdata[,8]+RSSdata[,9]+
                         RSSdata[,10]+RSSdata[,11]+
                         RSSdata[,12]+RSSdata[,13],dist="poisson")
        mymod1=summary(fitRSS)
        
        #bias for count model= estimate of beta in count - Bx
        srs[ii]=abs(mymod0$coefficients$count[2,1] - betax)
        rss[ii]=abs(mymod1$coefficients$count[2,1] - betax)
        
        #standard errors 
        ######################################## 
        srsv[ii]=mymod0$coefficients$count[2,2]
        rssv[ii]=mymod1$coefficients$count[2,2] 
        
        
        #power
        ##########################################
        
        if (mymod0$coefficients$count[2,4]<.05) {psrs[ii] =1} else {psrs[ii] =0}
        if (mymod1$coefficients$count[2,4]<.05) {prss[ii] =1} else {prss[ii] =0}
        
        
        
        #mse
        ##########################################
        mserss[ii]=(rss[ii])^2 + (rssv[ii])^2        # MSE count model
        msesrs[ii]=(srs[ii])^2 + (srsv[ii])^2
        
        
        #coverage probability
        ##########################################
        rss.ci=c(confint(fitRSS, "count_RSSdata[, 2]"))
        srs.ci=c(confint(fitSRS, "count_SRSdata[, 2]"))
        rss.cp[ii]=(rss.ci[2]>=betax)*(rss.ci[1]<=betax)
        srs.cp[ii]=(srs.ci[2]>=betax)*(srs.ci[1]<=betax)
        
        
        ii=ii+1
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
      
    }
    
    bias_rss[p,s]=mean(rss)
    bias_srs[p,s]=mean(srs)
    mse_rss[p,s]=mean(mserss)
    mse_srs[p,s]=mean(msesrs)
    power_rss[p,s]=mean(prss)
    power_srs[p,s]=mean(psrs)
    cp_rss[p,s]=mean(rss.cp)
    cp_srs[p,s]=mean(srs.cp)
    effv[p,s]=mean(msesrs)/mean(mserss)
    se_rss[p,s]=mean(rssv)
    se_srs[p,s]=mean(srsv)
    
  } #s
} #p

sim.res=list(bias_rss,bias_srs,mse_rss,mse_srs,power_rss,power_srs,cp_rss,cp_srs,effv,se_rss,se_srs)
names(sim.res) <- c("bias_rss","bias_srs","mse_rss","mse_srs","power_rss","power_srs","cp_rss", "cp_srs", "effv", "se_rss", "se_srs")
