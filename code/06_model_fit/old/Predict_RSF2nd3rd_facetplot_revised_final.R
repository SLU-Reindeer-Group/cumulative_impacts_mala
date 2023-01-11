library(ggplot2)
library(lme4)
source("~/Dropbox/Vindval/R_revised2/summary.R")
#load("H:/Documents/Reindeer/COLAND/Mala/2ndRSF_kalv0816VJune8.vari.RData")
### 2nd-order ######
load("~/Dropbox/Vindval/R_GPS_shed/2ndRSF_kalv0816.vari.RData")
#load("~/Dropbox/Vindval/R_GPS_shed/glmer_mala2ndJune08.RData")
#rm(glmer_mala1, glmer_mala2, glmer_mala3, glmer_mala4, glmer_mala5, glmer_mala6, glmer_mala7)
save(glmer_mala8, glmer_ind5, file="Final_models_shed.Rdata")

# best model
kalvRSF.vari$viewsmd<-factor(kalvRSF.vari$viewsmd,levels =c("Viewcover","Viewopen","Noview") )
glmer_mala8 <- glmer(formula = resp ~ scale(dem) + scale(slope) + 
                       beforeafter*viewsmd*I(exp(-0.0002*distvindkraft)) + I(exp(-0.002*waterdist))  +
                       I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +(1|id),
                     family = binomial,   data = kalvRSF.vari, control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(glmer_mala8)

startN<- getME(glmer_mala8,c("theta","fixef"))
#### Update old estimate with new starting values, and increased maximum functional evaluation
glmer_mala8<-update(glmer_mala8,start=startN,control=glmerControl(optCtrl=list(maxfun=2e4)))
## Newdata 2nd #####
newdata2nd <- expand.grid(smd50upveg4 = c("Forest","Heath", "Clear", "Young", "Mire"),
                          viewsmd= c("Viewcover","Viewopen","Noview"), 
                          beforeafter = c("before","during","after"),n=1:100)
#distvindkraft= mean(kalvRSF.vari$distvindkraft),
newdata2nd$distvindkraft= seq(from= min(kalvRSF.vari$distvindkraft), to=10000, length.out=100)
newdata2nd$waterdist = mean (kalvRSF.vari$waterdist)
newdata2nd$distroad=mean(kalvRSF.vari$distroad)
newdata2nd$slope=mean(kalvRSF.vari$slope)
newdata2nd$distpowma=mean(kalvRSF.vari$distpowma)
newdata2nd$dem=mean(kalvRSF.vari$dem)
newdata2nd$vrm=0
newdata2nd$resp=c(0,1)
newdata2nd$id=NA
#newdata2nd$dem<-0
#newdata2nd$slope<-0
####### 2nd Viewopen #######################################################
newdata2ndO <- newdata2nd[ newdata2nd$viewsmd=="Viewopen",]
newdata2ndO$viewsmd<-factor(newdata2ndO$viewsmd,levels =c("Viewcover","Viewopen","Noview") )
#compute fitted values and  SE
pred2ndO <- predict(glmer_mala8, newdata=newdata2ndO,type="response", re.form=NA,na.action=na.omit)

mf<-model.frame(formula = resp ~ scale(dem,center = mean(kalvRSF.vari$dem),scale=sd(kalvRSF.vari$dem)) + 
                  scale(slope,center = mean(kalvRSF.vari$slope),scale=sd(kalvRSF.vari$slope)) + 
                  beforeafter*viewsmd*I(exp(-0.0002*distvindkraft)) + I(exp(-0.002*waterdist))  +
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)),
                data = newdata2ndO,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_mala8,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_mala8)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,1500)
for(i in 1:1500){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}

newdata2ndO$plo <- (plogis(eta-1.96*SEp))
newdata2ndO$phi <- (plogis(eta+1.96*SEp))
newdata2ndO$Pred <-predO ## prediction from Moudud code
#newdata2ndO$Pred2 <-pred2ndO ## prediction from predict.merMod
str(newdata2ndO)

####### 2nd Viewcover #######################################################
newdata2ndC <- newdata2nd[ newdata2nd$viewsmd=="Viewcover",]
newdata2ndC$viewsmd<-factor(newdata2ndC$viewsmd,levels =c("Viewcover","Viewopen","Noview") )
#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(kalvRSF.vari$dem),scale=sd(kalvRSF.vari$dem)) + 
                  scale(slope,center = mean(kalvRSF.vari$slope),scale=sd(kalvRSF.vari$slope)) + 
                  beforeafter*viewsmd*I(exp(-0.0002*distvindkraft)) + I(exp(-0.002*waterdist))  +
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)),
                data = newdata2ndC,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_mala8,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_mala8)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,1500)
for(i in 1:1500){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata2ndC$plo <- (plogis(eta-1.96*SEp))
newdata2ndC$phi <- (plogis(eta+1.96*SEp))
newdata2ndC$Pred <-predO
#str(newdata2ndC)

###### 2nd Noview ########################################################
newdata2ndN <- newdata2nd[ newdata2nd$viewsmd=="Noview",]
newdata2ndN$viewsmd<-factor(newdata2ndN$viewsmd,levels =c("Viewcover","Viewopen","Noview") )
#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(kalvRSF.vari$dem),scale=sd(kalvRSF.vari$dem)) + 
                  scale(slope,center = mean(kalvRSF.vari$slope),scale=sd(kalvRSF.vari$slope)) + 
                  beforeafter*viewsmd*I(exp(-0.0002*distvindkraft)) + I(exp(-0.002*waterdist))  +
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)),
                data = newdata2ndN,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_mala8,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_mala8)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,1500)
for(i in 1:1500){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata2ndN$plo <- (plogis(eta-1.96*SEp))
newdata2ndN$phi <- (plogis(eta+1.96*SEp))
newdata2ndN$Pred <-predO
#str(newdata2ndN)

####### combine all 2nd ################################
newdata2nd1 <- rbind(newdata2ndN,newdata2ndC, newdata2ndO)
newdata2nd1$Scale <- rep("2nd", nrow=newdata2nd1)
str(newdata2nd1)
levels(newdata2nd1$viewsmd) <- c( "In sight cover", "In sight open" ,"Out of sight"  )
newdata2nd1$viewsmd <- relevel(newdata2nd1$viewsmd, ref= "In sight open")
newdata2nd1$viewsmd <- relevel(newdata2nd1$viewsmd, ref="Out of sight" )


p_2nd <- ggplot(newdata2nd1, aes(x = distvindkraft, y = Pred)) + 
  facet_grid(. ~ viewsmd) +
  geom_ribbon(aes(ymin = plo,ymax = phi, fill =beforeafter), alpha = 0.4) + 
  geom_line(aes(linetype= beforeafter,colour = beforeafter), size = 0.5)+
  geom_hline(yintercept=0.50,colour = "grey60",alpha=0.5 ) +
  ylim(0,1)+
  xlim(0,10000)+
  labs(x = "Distance to wind turbines (m)", y ="Predicted probability ± 95% CI" )+
  scale_linetype_manual(values=c(3,2,1), name="Phase",
                        breaks=c("before","during","after"), # Specify both colour and shape
                        labels=c( "Before construction","Construction", "Operation")) + 
  scale_colour_manual(values=c("black", "black", "black"),name="Phase", 
                      breaks=c("before","during","after"), # Specify both colour and shape
                      labels=c( "Before construction","Construction", "Operation"))+
  scale_shape_manual(values=c(3,2,1),name="Phase",
                     breaks=c("before","during","after"), # Specify both colour and shape
                     labels=c( "Before construction","Construction", "Operation"))+
  scale_fill_manual(values=c("gray80", "gray45", "gray0"),name="Phase",
                    breaks=c("before","during","after"), # Specify both colour and shape
                    labels=c( "Before construction","Construction", "Operation"))+
  #theme(axis.ticks = element_line(size = 0.3, colour="black")) +
  theme (panel.border = element_rect(colour="gray", linetype="solid", fill=NA))+
  theme(strip.background = element_rect(colour = "white", fill = "white"))+
  theme(strip.text.x = element_text(colour = "black", size=11))+
  theme(strip.text.y = element_text(colour = "black", size=11))+
  theme(panel.background=element_blank())+
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour= "gray96")) +
  theme(axis.line=element_line(colour="black")) +
  theme(axis.title.x = element_text(size=13),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=9, colour="black"))+
  theme(axis.title.y = element_text(size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=9, colour="black"))+
  theme(legend.justification=c(0.9,0.1), legend.position=c(0.98,0.1))
p_2nd


################ 3rd order ########################################
load("~/Dropbox/Vindval/R_GPS_shed/3rdRSFBBMM_kalv0816.vari.RData")
load("~/Dropbox/Vindval/R_GPS_shed/glmer_ind3rd_June11.RData")
rm(glmer_ind1,glmer_ind2,glmer_ind3, glmer_ind4, glmer_ind6, glmer_ind7)

#glmer_ind5 <- glmer(formula = resp ~ scale(dem) + scale(vrm) + smd50upveg4*beforeafter*I(exp(-0.0005*distvindkraft)) +
#                      I(exp(-0.002*waterdist)) + I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +I(exp(-0.002*distsmall))
#                    +(1|id), family = binomial,control=glmerControl(optCtrl=list(maxfun=2e4)),
#                   data = indkalvRSF.vari)
summary(glmer_ind5)
# Newdata 3rd ####
newdata3rd <- expand.grid(smd50upveg4 = c("Forest","Heath", "Clear", "Young", "Mire"),
                          beforeafter = c("before","during","after"),n=1:100)
newdata3rd$distvindkraft= seq(from= min(indkalvRSF.vari$distvindkraft), to=10000, length.out=100)
newdata3rd$waterdist = mean (indkalvRSF.vari$waterdist)
newdata3rd$distroad=mean(indkalvRSF.vari$distroad)
newdata3rd$slope=mean(indkalvRSF.vari$slope)
newdata3rd$distpowma=mean(indkalvRSF.vari$distpowma)
newdata3rd$distsmall=mean(indkalvRSF.vari$distsmall)
newdata3rd$dem=mean(indkalvRSF.vari$dem)
newdata3rd$vrm=mean(indkalvRSF.vari$vrm)
newdata3rd$resp=c(0,1)
newdata3rd$id=NA

####### 3rd Forest #######################################################
newdata3rdF <- newdata3rd[ newdata3rd$smd50upveg4=="Forest",]
newdata3rdF$smd50upveg4<-factor(newdata3rdF$smd50upveg4,levels =c("Forest","Heath", "Clear", "Young", "Mire") )
table(newdata3rdF$viewsmd)
table(kalvRSF.vari$smd50upveg4)
table(newdata3rdF$beforeafter)
pred3rdF <- predict(glmer_ind5, newdata=newdata3rdF,type="response", re.form=NA,na.action=na.omit)

#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(indkalvRSF.vari$dem),scale=sd(indkalvRSF.vari$dem)) + 
                  scale(vrm,center = mean(indkalvRSF.vari$vrm),scale=sd(indkalvRSF.vari$vrm)) + 
                  smd50upveg4*beforeafter*I(exp(-0.0005*distvindkraft)) + I(exp(-0.002*waterdist)) + 
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +I(exp(-0.002*distsmall)),
                data = newdata3rdF,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_ind5,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_ind5)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,300)
for(i in 1:300){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata3rdF$plo <- (plogis(eta-1.96*SEp))
newdata3rdF$phi <- (plogis(eta+1.96*SEp))
newdata3rdF$Pred <-predO
#newdata3rdF$Pred2 <-pred3rdF
#str(newdata3rdF)
####### 3rd Clear #######################################################
newdata3rdC <- newdata3rd[ newdata3rd$smd50upveg4=="Clear",]
newdata3rdC$smd50upveg4<-factor(newdata3rdC$smd50upveg4,levels =c("Forest","Heath", "Clear", "Young", "Mire") )
table(newdata3rdC$smd50upveg4)

#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(indkalvRSF.vari$dem),scale=sd(indkalvRSF.vari$dem)) + 
                  scale(vrm,center = mean(indkalvRSF.vari$vrm),scale=sd(indkalvRSF.vari$vrm)) + 
                  smd50upveg4*beforeafter*I(exp(-0.0005*distvindkraft)) + I(exp(-0.002*waterdist)) + 
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +I(exp(-0.002*distsmall)),
                data = newdata3rdC,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_ind5,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_ind5)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,300)
for(i in 1:300){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata3rdC$plo <- (plogis(eta-1.96*SEp))
newdata3rdC$phi <- (plogis(eta+1.96*SEp))
newdata3rdC$Pred <-predO
#str(newdata3rdC)

####### 3rd Clear #######################################################
newdata3rdY <- newdata3rd[ newdata3rd$smd50upveg4=="Young",]
newdata3rdY$smd50upveg4<-factor(newdata3rdY$smd50upveg4,levels =c("Forest","Heath", "Clear", "Young", "Mire") )
table(newdata3rdY$smd50upveg4)

#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(indkalvRSF.vari$dem),scale=sd(indkalvRSF.vari$dem)) + 
                  scale(vrm,center = mean(indkalvRSF.vari$vrm),scale=sd(indkalvRSF.vari$vrm)) + 
                  smd50upveg4*beforeafter*I(exp(-0.0005*distvindkraft)) + I(exp(-0.002*waterdist)) + 
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +I(exp(-0.002*distsmall)),
                data = newdata3rdY,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_ind5,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_ind5)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,300)
for(i in 1:300){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata3rdY$plo <- (plogis(eta-1.96*SEp))
newdata3rdY$phi <- (plogis(eta+1.96*SEp))
newdata3rdY$Pred <-predO
#str(newdata3rdY)


####### 3rd Heath #######################################################
newdata3rdH <- newdata3rd[ newdata3rd$smd50upveg4=="Heath",]
newdata3rdH$smd50upveg4<-factor(newdata3rdH$smd50upveg4,levels =c("Forest","Heath", "Clear", "Young", "Mire") )
#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(indkalvRSF.vari$dem),scale=sd(indkalvRSF.vari$dem)) + 
                  scale(vrm,center = mean(indkalvRSF.vari$vrm),scale=sd(indkalvRSF.vari$vrm)) + 
                  smd50upveg4*beforeafter*I(exp(-0.0005*distvindkraft)) + I(exp(-0.002*waterdist)) + 
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +I(exp(-0.002*distsmall)),
                data = newdata3rdH,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_ind5,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_ind5)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,300)
for(i in 1:300){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata3rdH$plo <- (plogis(eta-1.96*SEp))
newdata3rdH$phi <- (plogis(eta+1.96*SEp))
newdata3rdH$Pred <-predO
#str(newdata3rdH)

####### 3rd Mire #######################################################
newdata3rdM <- newdata3rd[ newdata3rd$smd50upveg4=="Mire",]
newdata3rdM$smd50upveg4<-factor(newdata3rdM$smd50upveg4,levels =c("Forest","Heath", "Clear", "Young", "Mire") )
#predSE_M <-predictSE(glmer_ind5, newdata3rdM, se.fit = F, print.matrix =
#                       FALSE, level = 0, type = "response")
#compute fitted values and  SE
mf<-model.frame(formula = resp ~ scale(dem,center = mean(indkalvRSF.vari$dem),scale=sd(indkalvRSF.vari$dem)) + 
                  scale(vrm,center = mean(indkalvRSF.vari$vrm),scale=sd(indkalvRSF.vari$vrm)) + 
                  smd50upveg4*beforeafter*I(exp(-0.0005*distvindkraft)) + I(exp(-0.002*waterdist)) + 
                  I(exp(-0.002*distpowma)) + I(exp(-0.002*distroad)) +I(exp(-0.002*distsmall)),
                data = newdata3rdM,drop.unused.levels = F)
X0<-model.matrix(attr(mf,"terms") ,data=mf)
fitBeta<-getME(glmer_ind5,"fixef") # fixed effects
fitBeta<-as.numeric(fitBeta)
eta<-X0%*%fitBeta
predO<-as.numeric(plogis(eta))
vcovB<-vcov(glmer_ind5)
vcovB<-as.matrix(vcovB)
SEp<-rep(NA,300)
for(i in 1:300){
  x1<-as.numeric(X0[i,])
  SEp[i]<-sqrt(t(x1)%*%vcovB%*%x1) 
}
newdata3rdM$plo <- (plogis(eta-1.96*SEp))
newdata3rdM$phi <- (plogis(eta+1.96*SEp))
newdata3rdM$Pred <-predO
#str(newdata3rdM)

newdata3rd1 <- rbind(newdata3rdM,newdata3rdH, newdata3rdY,newdata3rdC,newdata3rdF)
newdata3rd1$Scale <- rep("3rd", nrow=newdata3rd1)
#newdata <- rbind(newdata2nd, newdata3rd)
table(newdata3rd1$smd50upveg4)
table(newdata3rd1$beforeafter)
levels(newdata3rd1$smd50upveg4) <- c("Forest","Heath", "Clear cut", "Young forest", "Mire"  )


###### ggplot2 facet #########
p_3rd <- ggplot(newdata3rd1, aes(x = distvindkraft, y = Pred)) + 
  facet_wrap( ~ smd50upveg4, ncol=3)+
  #facet_grid(~ smd50upveg4) +
  geom_ribbon(aes(ymin = plo,ymax = phi, fill =beforeafter), alpha = 0.4) + 
  geom_line(aes(linetype= beforeafter,colour = beforeafter), size = 0.5)+
  geom_hline(yintercept=0.50,colour = "grey60",alpha=0.5 ) +
  ylim(0,1)+
  xlim(0,10000)+
  labs(x = "Distance to wind turbines (m)", y ="Predicted probability ± 95% CI" )+
  scale_linetype_manual(values=c(3,2,1), name="Phase",
                        breaks=c("before","during","after"), # Specify both colour and shape
                        labels=c( "Before construction","Construction", "Operation")) + 
  scale_colour_manual(values=c("black", "black", "black"),name="Phase", 
                      breaks=c("before","during","after"), # Specify both colour and shape
                      labels=c( "Before construction","Construction", "Operation"))+
  scale_shape_manual(values=c(3,2,1),name="Phase",
                     breaks=c("before","during","after"), # Specify both colour and shape
                     labels=c( "Before construction","Construction", "Operation"))+
  scale_fill_manual(values=c("gray80", "gray45", "gray0"),name="Phase",
                    breaks=c("before","during","after"), # Specify both colour and shape
                    labels=c( "Before construction","Construction", "Operation"))+
  #theme(axis.ticks = element_line(size = 0.3, colour="black")) +
  theme (panel.border = element_rect(colour="gray", linetype="solid", fill=NA))+
  theme(strip.background = element_rect(colour = "white", fill = "white"))+
  theme(strip.text.x = element_text(colour = "black", size=11))+
  theme(strip.text.y = element_text(colour = "black", size=11))+
  theme(panel.background=element_blank())+
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_line(colour= "gray96")) +
  theme(axis.line=element_line(colour="black")) +
  theme(axis.title.x = element_text(size=13),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=9, colour="black"))+
  theme(axis.title.y = element_text(size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=9, colour="black"))+
  theme(legend.justification=c(0.9,0.1), legend.position=c(0.88,0.2))
p_3rd

save(p_3rd, newdata3rd1, p_2nd,newdata2nd1,file="Preddata2nd3rdRSF_juli8.RData") 


############################################################

# percent decrease in Pred per km ####
newdata2nd1$distvindclass <- cut(newdata2nd1$distvindkraft, breaks= c(0, 200,950,1056, 1950, 2061,2963, 3066,3968,4071,4973, 5076,10000),
                                label= c("200","950","1056","1950","2061","2963", "3066","3968","4071","4973", "5076","10000"), 
                                include.lowest=TRUE)
sumdist2nd <-summarySE(newdata2nd1, measurevar="Pred", groupvars=c("Scale", "distvindclass", "viewsmd", "beforeafter") )
sumdist2nd
write.table(sumdist2nd, file="Predmargvalues_2nd.txt")



newdata3rd1$distvindclass <- cut(newdata3rd1$distvindkraft,  breaks= c(0, 200,950,1056, 1950, 2061,2963, 3066,3968,4071,4973, 5076,10000),
                                label= c("200","950","1056","1950","2061","2963", "3066","3968","4071","4973", "5076","10000"), 
                                 include.lowest=TRUE)
sumdist3rd <-summarySE(newdata3rd1, measurevar="Pred", groupvars=c("distvindclass", "smd50upveg4", "beforeafter") )
sumdist3rd

write.table(sumdist3rd, file="Predmargvalues_3rd.txt")


############################################################

#newdata2ndOex <- newdata2ndO
#PredOpen<- predictSE(glmer_mala8, newdata=newdata2ndO, se.fit=F, type="response")
#newdata2ndOex$PredOpen <- PredOpen
#PredCover<- predictSE(glmer_mala8, newdata=newdata2ndC, se.fit=F, type="response")
#newdata2ndOex$PredCover <- PredCover

ggplot(newdata3rdF, aes(x = distvindkraft, y = Pred)) + 
  #facet_grid(. ~ viewsmd) +
  geom_ribbon(aes(ymin = plo,ymax = phi, fill =beforeafter), alpha = 0.4) + 
  geom_line(aes(linetype= beforeafter,colour = beforeafter), size = 0.5)+
  geom_hline(yintercept=0.50,colour = "grey60",alpha=0.5 ) +
  ylim(0,1)+
  xlim(0,10000)

#newdata3rdY$plo <- predSE_Y$fit-1.96*predSE_Y$se.fit
#newdata3rdY$phi <- predSE_Y$fit+1.96*predSE_Y$se.fit
#newdata3rdY$Pred <- predSE_Y$fit