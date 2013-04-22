Goals <- read.csv("C:/Users/miu/Dropbox/GOALS/data/Goals.csv")
#Sergievsky 
Goals <- read.csv("C:/Users/yl2820/Dropbox/GOALS/data/Goals.csv")
library(lme4)
library(taRifx) #Sorting
library(nlme)
library(ez)

#Check for missing
temp<-split(Goals$ProgCode,Goals$FellCode)
temp2<-sapply(temp,function(z){sum(!is.na(z))})
#Response frequency
table(temp2)

dat<-Goals[Goals$ProcCode %in% c(2,4,6),]
dat1<-subset(dat,FellCode!=1.2)
temp_dat<-split(dat1$ProgCode,dat1$FellCode)
temp2_dat<-sapply(temp_dat,function(z){sum(!is.na(z))})
#Response frequency for procedure 246
table(temp2_dat)

#take out 1.2, called temp1

length(unique(dat1$FellCode))


#format and extract time
mydate2 <- as.Date(dat1$ProcDate, format = "%m/%d/%Y")
mydate1<- as.Date(c("2010-07-01")) #Enter time
period<-as.numeric(mydate2-mydate1) %% 365  #Take mod to obtain exact period from enter the program
Qtr<-ceiling(period/91.25)
table(Qtr)
temp1<-cbind(dat1,period,Qtr)
#Time vs score
plot(temp1$period,temp1$Average,ylim=c(0,5),col=as.factor(temp1$FellCode))
par(new=F)
temp1<-temp1[order(temp1$FellCode,temp1$period),]
#split by fellCode with period
temp3<-split(temp1,temp1$FellCode)



#Spahgetti plot against period(raw)
ymax<-max(temp1$Average)
ymin<-0
xmax<-max(temp1$period)
xmin<-0
plot(temp3[[1]]$period,temp3[[1]]$Average,type="o",col=1,
     xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     xlab="Period in the fellowship",ylab="Average score of each procedure")

for(i in 2:34)
{
  lines(temp3[[i]]$period,temp3[[i]]$Average,type="o",col=i)
}

#######################################################


#Final model with Period as primary predictor and adjust for AdvPrev
par(mfrow=c(1,1))
log.period<-log(temp1$period+5)
temp1<-cbind(temp1,log.period)
#######################################################
#Univariately look at impact of each factor on the average performance score
a<-split(temp1,Qtr)
table(Qtr)
lapply(a,function(z){c<-c(mean(z$Autonom),sd(z$Autonom))})
#AdvPrev
###Check for transformation
b<-0
for(i in 1:100)
{
plot(log(temp1$NumbPrev+i),temp1$Average)
a<-summary(lm(temp1$Average~I(log(temp1$NumbPrev+1))))
b[i]<-a$r.squared
}

summary(r.period<-lme(Average~AdvPrev,random=~1|FellCode,data=temp1,method="ML"))
#NumbPrev
summary(r.period<-lme(Average~NumbPrev,random=~1|FellCode,data=temp1,method="ML"))
#Difficulty
summary(r.perio<d-lme(Average~log.period,random=~1|FellCode,data=temp1,method="ML"))

##Overall##
summary(r.period<-lmer(Average~Difficulty+log.period+AdvPrev+I(log(temp1$NumbPrev+1))+(1+log.period|FellCode),data=temp1,method="ML"))
1-pnorm(1.62)

##Depth##
summary(r.Depth<-lmer(Depth~Difficulty+log.period+AdvPrev+NumbPrev+(1+log.period|FellCode),data=temp1,method="ML"))

##Bimanual##
summary(r.Bimanual<-lmer(Bimanual~Difficulty+log.period+AdvPrev+I(log(temp1$NumbPrev+1))+(1+log.period|FellCode),data=temp1,method="ML"))

##Efficient##
summary(r.Efficient<-lmer(Efficient~Difficulty+log.period+AdvPrev+NumbPrev+(1+log.period|FellCode),data=temp1,method="ML"))

##Tissue##
summary(r.Tissue<-lmer(Tissue~Difficulty+log.period+AdvPrev+I(log(temp1$NumbPrev+1))+(1+log.period|FellCode),data=temp1,method="ML"))

##Autonomy##
summary(r.Autonomy<-lmer(Autonom~Difficulty+log.period+AdvPrev+NumbPrev+(1+log.period|FellCode),data=temp1,method="ML"))

#################################################################
summary(r.period1<-lme(Average~I(sqrt(period))+AdvPrev,random=~1+I(sqrt(period))|FellCode,data=temp1,method="ML"))
plot(fitted(r.period),fitted(r.period1),xlab="fitted value from log(x+5) transforamtin",ylab="Fitted from quare root transformation")

summary(r.period0<-lme(Average~log.period+AdvPrev,random=~1|FellCode,data=temp1,method="ML"))
anova(r.period,r.period0)
intervals(r.period)
fitted<-fitted(r.period)
#This is the dataset conbine with the data and the fitted value
fit.dat<-cbind(temp1,fitted)
temp3<-split(fit.dat,temp1$FellCode)


######PLOT#####

par(mfcol=c(1,2))
plot(fit.dat$period,fitted,main="Overall")
#Spahgetti plot against period
ymax<-max(fit.dat$fitted)
ymin<-min(fit.dat$fitted)
xmax<-max(fit.dat$period)
xmin<-0
plot(temp3[[1]]$period,temp3[[1]]$fitted,type="o",col=1,
     xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     xlab="Period in the fellowship",ylab="Average score of each procedure",
     main="Fitted value for all procedure, by each fellow")

for(i in 2:34)
{
  lines(temp3[[i]]$period,temp3[[i]]$fitted,type="o",col=i)
}

#####More smooth version of predicted value
log.period_pred<-log(seq(1,365,by=1)+5)
period_pred<-seq(1,365,by=1)
mean(temp1$Difficulty) #3.0
split(temp1,Qtr)
NumbPrev_Qtr<-sapply(split(temp1,Qtr),function(z){mean(z$NumbPrev)})
AdvPrev_Qtr<-sapply(split(temp1,Qtr),function(z){mean(z$AdvPrev)})
AdvPrev_pred<-rep(AdvPrev_Qtr,c(91,91,91,92))
NumbPrev_pred<-rep(NumbPrev_Qtr,c(91,91,91,92))
Difficulty_Pred<-rep(3.0,365)
FellCode_Pred<-rep(unique(temp1$FellCode),each=365)
plot(temp1$period,temp1$AdvPrev)

#####Linear fit of NUMBPREV and AdvPrev########
summary(a<-lm(I(log(temp1$NumbPrev+1)~temp1$period)))
NumbPrev_Pred<-exp(rep(a$coefficient[1],365)+a$coefficient[2]*seq(1,365,by=1))-2
summary(b<-lm(I(log(temp1$AdvPrev+1)~temp1$period)))
AdvPrev_Pred<-exp(rep(b$coefficient[1],365)+b$coefficient[2]*seq(1,365,by=1))-2

predict.x<-data.frame(cbind(log.period_pred,AdvPrev_Qtr,NumbPrev_Qtr,FellCode_Pred))
#r.pred<-predict(r.period,predict.x,level=1)

###############Prediction based on fixed effect####################
par(mfrow=c(3,2))
#####PLEASE CHANGE THIS INTO A FUNCTION!!!!!!!#####################
tot_fitted<-rep(r.period@fixef["(Intercept)"],365)+AdvPrev_Pred*rep(r.period@fixef["AdvPrev"],365)+NumbPrev_Pred*rep(r.period@fixef["NumbPrev"],365)+Difficulty_Pred*rep(r.period@fixef["Difficulty"],365)+log.period_pred*rep(r.period@fixef["log.period"],365)
plot(period_pred,tot_fitted,ylim=c(1,5),col=2
     ,main="Learning curve for AVERAGE score",
      ylab="Performance score(GOALS)",xlab="Time in the program")
points(temp1$period,temp1$Average)
abline(h = 1:5, v = seq(1,364,by=91), col = "gray", lty=3)
legend("bottomright",c("p-value:", "<0.01**"),col=3)

Depth_fitted<-rep(r.Depth@fixef["(Intercept)"],365)+AdvPrev_Pred*rep(r.Depth@fixef["AdvPrev"],365)+NumbPrev_Pred*rep(r.Depth@fixef["NumbPrev"],365)+Difficulty_Pred*rep(r.Depth@fixef["Difficulty"],365)+log.period_pred*rep(r.Depth@fixef["log.period"],365)
plot(period_pred,Depth_fitted,ylim=c(1,5),col=3
     ,main="Learning curve for DEPTH domain",
     ylab="Performance score(GOALS)",xlab="Time in the program")
points(temp1$period,temp1$Depth)
abline(h = 1:5, v = seq(1,364,by=91), col = "gray", lty=3)
legend("bottomright",c("p-value:","0.20"),col=3)

Bimanual_fitted<-rep(r.Bimanual@fixef["(Intercept)"],365)+AdvPrev_Pred*rep(r.Bimanual@fixef["AdvPrev"],365)+NumbPrev_Pred*rep(r.Bimanual@fixef["NumbPrev"],365)+Difficulty_Pred*rep(r.Bimanual@fixef["Difficulty"],365)+log.period_pred*rep(r.Bimanual@fixef["log.period"],365)
plot(period_pred,Bimanual_fitted,ylim=c(1,5),col=4
     ,main="Learning curve for BIMANUAL domain",
     ylab="Performance score(GOALS)",xlab="Time in the program")
points(temp1$period,temp1$Bimanual)
abline(h = 1:5, v = seq(1,364,by=91), col = "gray", lty=3)
legend("bottomright",c("p-value:", "<0.01**"),col=3)

Efficient_fitted<-rep(r.Efficient@fixef["(Intercept)"],365)+AdvPrev_Pred*rep(r.Efficient@fixef["AdvPrev"],365)+NumbPrev_Pred*rep(r.Efficient@fixef["NumbPrev"],365)+Difficulty_Pred*rep(r.Efficient@fixef["Difficulty"],365)+log.period_pred*rep(r.Efficient@fixef["log.period"],365)
plot(period_pred,Efficient_fitted,ylim=c(1,5),col=5
     ,main="Learning curve for EFFICIENT domain",
     ylab="Performance score(GOALS)",xlab="Time in the program")
points(temp1$period,temp1$Efficient)
abline(h = 1:5, v = seq(1,364,by=91), col = "gray", lty=3)
legend("bottomright",c("p-value:", "<0.01**"),col=3)

Tissue_fitted<-rep(r.Tissue@fixef["(Intercept)"],365)+AdvPrev_Pred*rep(r.Tissue@fixef["AdvPrev"],365)+NumbPrev_Pred*rep(r.Tissue@fixef["NumbPrev"],365)+Difficulty_Pred*rep(r.Tissue@fixef["Difficulty"],365)+log.period_pred*rep(r.Tissue@fixef["log.period"],365)
plot(period_pred,Tissue_fitted,ylim=c(1,5),col=6
     ,main="Learning curve for TISSUE domain",
     ylab="Performance score(GOALS)",xlab="Time in the program")
points(temp1$period,temp1$Tissue)
abline(h = 1:5, v = seq(1,364,by=91), col = "gray", lty=3)
legend("bottomright",c("p-value:", "0.95"),col=3)


Autonomy_fitted<-rep(r.Autonomy@fixef["(Intercept)"],365)+AdvPrev_Pred*rep(r.Autonomy@fixef["AdvPrev"],365)+NumbPrev_Pred*rep(r.Autonomy@fixef["NumbPrev"],365)+Difficulty_Pred*rep(r.Autonomy@fixef["Difficulty"],365)+log.period_pred*rep(r.Autonomy@fixef["log.period"],365)
plot(period_pred,Autonomy_fitted,ylim=c(1,5),col=9
     ,main="Learning curve for AUTONOMY domain",
     ylab="Performance score(GOALS)",xlab="Time in the program")
points(temp1$period,temp1$Autonom)
abline(h = 1:5, v = seq(1,364,by=91), col = "gray", lty=3)
legend("bottomright",c("p-value:", "<0.01**"),col=3)


############################Individual level, USE lme##########
plot(seq(0,365,by=1),r.pred,type="o",col=1,
     xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     xlab="Period in the fellowship",ylab="Average score of each procedure",main="Predicted learning curve")

points(seq(0,365,by=1),r.pred,type="o",col=6, 
     xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     xlab="Period in the fellowship",ylab="Average score of each procedure",main="Predicted learning curve")

#####Residual plot
plot(temp1$period,residuals(r.period),main="Residual plot")
abline(0,0,col=2)



#####################################################3
#Separate by procedure
pro245<-NULL
pro136<-NULL
pro1<-NULL
pro3<-NULL
pro6<-NULL
pro<-rep("0",nrow(temp1))
for (i in 1:nrow(temp1))
{
  if( !is.na(temp1[i,"Depth"]))
    {pro245<-rbind(pro245,temp1[i,])
    pro[i]<-"245"}
  else if( !is.na(temp1[i,"Trocars"])) 
  #3=Laparoscopic Incisional Hernia Repair
  {pro3<-rbind(pro3,temp1[i,])
   pro[i]<-"3"}
  else if(!is.na(temp1[i,"GroinTrocar"])) 
  # Laparoscopic Inguinal Hernia Repair
  {pro6<-rbind(pro6,temp1[i,]) 
  pro[i]<-"6"}
  else 
  #Laparoscopic Colectomy
  {pro1<-rbind(pro1,temp1[i,])
  pro[i]<-"1"}
}

temp1<-data.frame(temp1,pro)
par(mfrow=c(1,1))
a<-boxplot(Average~pro,data=temp1,notch=T,col=c(2,4),main="Box plot for different procedure")
class(temp1$pro)
summary(aov(Average~pro,data=temp1))

temp3<-split(temp1,temp1$FellCode)
summary(pro.period<-lmer(Average~log.period+AdvPrev+(1+log.period|FellCode),data=temp1,method="REML"))
#split by fellCode with period

str(pro.period)
temp245<-split(pro245,FellCode)


#Spahgetti plot against period
ymax<-max(pro245$Average)
ymin<-0
xmax<-max(pro245$period)
xmin<-0
plot(temp245[[1]]$period,temp245[[1]]$Average,type="o",col=1,
     xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     xlab="Period in the fellowship",ylab="Average score of each procedure",
     main="Procedures 2,4,5")

for(i in 2:34)
{
  lines(temp3[[i]]$period,temp3[[i]]$Average,type="o",col=i)
}


#split by fellCode with period
temp3<-split(pro136,FellCode)

#Spahgetti plot against period
ymax<-max(pro136$Average)
ymin<-0
xmax<-max(pro136$period)
xmin<-0
plot(temp3[[1]]$period,temp3[[1]]$Average,type="o",col=1,
     xlim=c(xmin,xmax),ylim=c(ymin,ymax),
     xlab="Period in the fellowship",ylab="Average score of each procedure",
     main="Procedures 1,3,6")

for(i in 2:34)
{
  lines(temp3[[i]]$period,temp3[[i]]$Average,type="o",col=i)
}

