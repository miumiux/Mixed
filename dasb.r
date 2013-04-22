
dasb <- read.csv("C:/Users/yl2820/Dropbox/GOALS/Fp/Data/DASB_fp_diag.csv")

require(lme4)
require(nlme)
require(reshape2)
require(sm)

#|-----------------------------------------------------------------------
#|DATA PREPARISON
#|-----------------------------------------------------------------------

dat1<-dasb
#Exclude those scans with all entries are NA
dat<-subset(dat1,!is.na(dat1$FFS1)|!is.na(dat1$FFS2)|!is.na(dat1$FFS3))  ###10 subjects excluded

dat<-dat[!is.na(dat$DIAG),]
dat$JJM<-as.factor(dat$JJM)

#Number of diagnosis
table(dat$DIAG)
#A merged version of diagnosis
diag1<-substring(dat$DIAG,1,3)
table(diag1)
dat<-data.frame(dat,diag1)

#Long format data
fdat <- melt(dat,
             # ID variables - all the variables to keep but not split apart on
             id.vars=c("Study.Number","JJM","DIAG","diag1"),
             # The source columns
             measure.vars=c("FFS1","FFS2","FFS3"),
             # Name of the destination column that will identify the original
             # column that the measurement came from
             variable.name="FFS.num",  #FFS1,FFS2,FFS3
             value.name="FFS"
)

dat<-dat[order(dat$JJM,dat$Study.Number),]
#check for data balance 
temp<-split(dat$Study.Number,dat$JJM,drop=T)
temp2<-sapply(temp,function(z){sum(!is.na(z))})
table(temp2)


#|-----------------------------------------------------------------------
#|ANALYSIS
#|-----------------------------------------------------------------------

#-----------------------With single scans-------------------------#

FFS100<-fdat$FFS*100
fdat1<-cbind(fdat,FFS100); fdat1<-fdat1[!is.na(fdat1$FFS100),]
summary(aov(fdat1$FFS100~fdat1$DIAG+fdat1$JJM+fdat1$Study.Number))

#Reduce Diagnosis into more general groups. Not much change in estimation

summary(fm21<-lmer(FFS100~1+diag1+(1|JJM)+(1|Study.Number),data=fdat1,REML=F,na.action=na.omit))
summary(fm11<-lmer(FFS100~1+diag1+(1|Study.Number),data=fdat1,REML=F,na.action=na.omit))
summary(fm0<-lm(FFS~diag1,data=fdat1))

#Test for random effect
anova(fm21,fm11)
anova(fm1,fm0)
intervals(fm2) #C.I for random effect


#-----------------------Multiple scans-------------------------#
#Below subset JJMs with two or more scans, dataset fdat2
temp1<-split(fdat,fdat$JJM)
a<-sapply(temp1,function(z){length(z$FFS)>3}) #Scan # greater than 3, Logic return
temp22<-temp1[a] #Subseting
#List to data frame
fdat2<-data.frame()
for (i in 1:length(temp22))
  fdat2<-rbind(fdat2,temp22[[i]])

#Unit=>percentage
FFS100<-fdat2$FFS*100
#Extract complete data
fdat2<-cbind(fdat2,FFS100); 
fdat2<-fdat2[!is.na(fdat2$FFS100),]

#Model with random subject
summary(fm2<-lmer(FFS100~1+diag1+(1|JJM)+(1|Study.Number),data=fdat2,REML=F,na.action=na.omit))
# summary(fm2<-lme(FFS100~1+diag1,random=~1|JJM/Study.Number,data=fdat2,method="ML",na.action=na.omit))

#Model with random scan
summary(fm1<-lmer(FFS100~1+diag1+(1|Study.Number),data=fdat2,REML=F,na.action=na.omit))

#Nested model
summary(fm0<-lm(FFS100~1+diag1,data=fdat2))


##Test for random effect (combination of chisq distribution)
anova(fm2,fm1)
anova(fm1,fm0)


#|-----------------------------------------------------------------------
#|SIMULATION TO CHECK MODEL STATBILITY
#|-----------------------------------------------------------------------
#Effective to lmer

set.seed(1243)
#-----------------------Multiple scans-------------------------#
remat <- summary(fm2)@REmat
var<-data.frame(as.numeric(remat[,4])) ###SD actually
row.names(var)<-c("rscan","rjjm","rnoise")

sim.std<-matrix(0,100,3)
sim.fix<-matrix(0,100,length(fm2@fixef))
p<-NULL
for (i in 1:100)
{
  #Simulate random effect for scan
  sp.JJM<-split(fdat2,fdat2$JJM,drop=T)
  sim.JJMl<-lapply(sp.JJM,function(z){cbind(z,rep(rnorm(1,0,var["rjjm",]),nrow(z)))})
  sim.JJM<-unsplit(sim.JJMl,fdat2$JJM,drop=T)
  
  #Simulate random effect for scan
  sp.SN<-split(fdat2,fdat2$Study.Number,drop=T)
  sim.scanl<-lapply(sp.SN,function(z){cbind(z,rep(rnorm(1,0,var["rscan",]),nrow(z)))})
  sim.scan<-unsplit(sim.scanl,fdat2$Study.Number,drop=T)
  
  #Simulate noise
  sim.noise<-rnorm(nrow(fdat2),0,var["rnoise",])
  
  #fitted Fixed effect
  fixef<-as.matrix(fm2@X)%*%as.matrix(fm2@fixef)
  #rjjm,rscan,rnoise,fixef
  sim<-sim.JJM[,8]+sim.scan[,8]+sim.noise+fixef
  
  new<-cbind(fdat2[,1:5],sim)
  sim.fm<-lmer(sim~1+diag1+(1|JJM)+(1|Study.Number),data=new,REML=F,na.action=na.omit)
  sim.fm0<-lmer(sim~1+diag1+(1|Study.Number),data=new,REML=F,na.action=na.omit)
  p[i]<-anova(sim.fm0,sim.fm)$"Pr(>Chisq)"[2]
  sim.std[i,]<-as.numeric(summary(sim.fm)@REmat[,4])
  sim.fix[i,]<-sim.fm@fixef
}


#-----------------------With single scans-------------------------#

remat1 <- summary(fm21)@REmat
var1<-data.frame(as.numeric(remat1[,4])) ###SD actually
row.names(var1)<-c("rscan","rjjm","rnoise")

sim.std1<-matrix(0,100,3)
sim.fix1<-matrix(0,100,length(fm21@fixef))
p1<-NULL
for (i in 1:100)
{
  #Simulate random effect for scan
  sp.JJM<-split(fdat1,fdat1$JJM,drop=T)
  sim.JJMl<-lapply(sp.JJM,function(z){cbind(z,rep(rnorm(1,0,var1["rjjm",]),nrow(z)))})
  sim.JJM<-unsplit(sim.JJMl,fdat1$JJM,drop=T)
  
  #Simulate random effect for scan
  sp.SN<-split(fdat1,fdat1$Study.Number,drop=T)
  sim.scanl<-lapply(sp.SN,function(z){cbind(z,rep(rnorm(1,0,var1["rscan",]),nrow(z)))})
  sim.scan<-unsplit(sim.scanl,fdat1$Study.Number,drop=T)
  
  #Simulate noise
  sim.noise<-rnorm(nrow(fdat1),0,var1["rnoise",])
  
  #fitted Fixed effect
  fixef<-as.matrix(fm21@X)%*%as.matrix(fm21@fixef)
  #rjjm,rscan,rnoise,fixef
  sim<-sim.JJM[,8]+sim.scan[,8]+sim.noise+fixef
  
  
  new<-cbind(fdat1[,1:5],sim)
  sim.fm<-lmer(sim~1+diag1+(1|JJM)+(1|Study.Number),data=new,REML=F,na.action=na.omit)
  sim.fm0<-lmer(sim~1+diag1+(1|Study.Number),data=new,REML=F,na.action=na.omit)
  p1[i]<-anova(sim.fm0,sim.fm)$"Pr(>Chisq)"[2]
  sim.std1[i,]<-as.numeric(summary(sim.fm)@REmat[,4])
  sim.fix1[i,]<-sim.fm@fixef
}

sim.std0<-as.data.frame(rbind(cbind(sim.std1,rep(0,100)),cbind(sim.std,rep(1,100))))
colnames(sim.std0)<-c("rscan","rjjm","rnoise","l")
# create value labels 
jjm.f <- factor(sim.std0$l, levels= c(0,1),
                labels = c("Complete data", "Multi-scans")) 


#|-----------------------------------------------------------------------
#|PARAMETER ESTIMATE DENSITY FROM 100 SIMULATION
#|-----------------------------------------------------------------------
name<-c("Scan","JJM","Noise")
par(mfrow=c(3,1))

for(i in 1:3)
{
  sm.density.compare(sim.std0[,i],sim.std0$l, xlab="Estimate of RE")
  title(main=paste("Simulation of random effect estimate for" ,name[i] ,"(Kernal density)"))
 
  abline(v=as.numeric(remat1[i,4]),col="red")
  abline(v=as.numeric(remat[i,4]),col="green")
  colfill<-c(2:(2+length(levels(jjm.f)))) 
  legend("topright",levels(jjm.f), fill=colfill)
}

#|-----------------------------------------------------------------------
#|P-VALUE HISTOGRAM FOR TESTING RANDOM SUBJECT FROM 100 SIMULATION
#|-----------------------------------------------------------------------
par(mfrow=c(2,1))
hist(p1,freq=F,main="P-value for test RE of JJM(Complete dataset)")
lines(density(p1),col=2)

hist(p,freq=F,main="P-value for test RE of JJM(Multi-scans)")
lines(density(p),col=2)

#####################################################################

###plot check for constant variance assump#
par(mfrow=c(1,2))
str(summary(lm(varb~meanb)))

#Complete data
aa<-split(fdat1,fdat1$Study.Number,drop=T)
which.max(sapply(aa,function(z){var(z$FFS100)}))
meana<-as.numeric(sapply(aa,function(z){mean(z$FFS100)})[-183])
vara<-as.numeric(sapply(aa,function(z){var(z$FFS100)})[-183])

plot(meana,vara,main="Mean vs. Variability. Complete data")
abline(lm(vara~meana),col=2)

#Multiscan data
bb<-split(fdat2,fdat2$Study.Number,drop=T)
which.max(sapply(bb,function(z){var(z$FFS100)}))
meanb<-as.numeric(sapply(bb,function(z){mean(z$FFS100)})[-10])
varb<-as.numeric(sapply(bb,function(z){var(z$FFS100)})[-10])

plot(meanb,varb,main="Mean vs. Variability. Multi-scan")
abline(lm(varb~meanb),col=2)
