# In this script we shall take a look at the test set of
# the original paper on RSI
dat<-read.csv("eschrich_2009.csv",header=T)
plot(dat$Predicted,dat$Observed,xlim=c(0,1),
     ylim=c(0,1),pch=19,xlab="Observed SF2",
     ylab="Predicted SF2")
abline(a=0,b=1,col=2,lty=2)
# Wow thats really bad!
cor.test(dat$Predicted,dat$Observed,method="spearman")
# in fact the correlation is negative! -0.61 p = 0.03
# oh my!  Hmmm! So if it predicts anything its the other way round!

# Let's compare to random guessing
cs<-cor(dat$Observed,dat$Predicted,method="spearman")
set.seed(2021)
rcs<-array(NA,dim=c(1000000,1))
for (ii in 1:1000000){
  dummy<-runif(length(dat$Observed))
  rcs[ii]<-cor(dat$Observed,dummy,method="spearman")
}
hist(rcs,main="",xlab="Spearman's Rho")
abline(v=cs,col=2)
sum(rcs>=cs)/1000000
# 98% of random gusses show a better correlation to SF at 2 Gy than RSI!!!