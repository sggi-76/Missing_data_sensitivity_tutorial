## ----setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(arm)
library(mice)
library(ggplot2)
library(car)
library(dplyr)
library(forcats)
library(purrr)
library(gridExtra)
setwd("~/Dropbox (LSE Statistics)/Sentencing Disparities/Methods festival")


## ----------------------------------------------------------------------------------------------

ReMF.dat<- read.csv("ReMF_original.csv",header=T,stringsAsFactors = T)

#head(ReMF.dat)

ReMF.dat <- ReMF.dat %>% mutate(Prev_Convictions = 
          fct_relevel(Prev_Convictions, c("None","1 to 3","4 to 9","10 or more")))
#some house-keeping.

#Only those with sentence length available
ReMF.dat <- subset(ReMF.dat,Custody==1)
ReMF.dat <- dplyr::select(ReMF.dat,-c(Custody))

#centre age
ReMF.dat <- ReMF.dat %>%
  mutate(Age_Cont = Age_Cont - mean(Age_Cont))



## ----------------------------------------------------------------------------------------------
Sen_Len.benchmark <- lm(Sentence_Length~.,data=ReMF.dat)

#summary(Sen_Len.benchmark)


## ----------------------------------------------------------------------------------------------
ci.tab<-function(vec,Miss=1,SL=0,Ethn=0,Inter=0){
  #default values are miss=1, outcome=1 is custody, 
  #Cust=Ethn=Inter=0 means there is no parameter for the missingness model
 ret.tab <- c(vec[1]-2*vec[2],vec[1],
      vec[1]+2*vec[2])
 ret.tab <-c(ret.tab,c(Miss, SL, Ethn, Inter))
 ret.tab <- as.data.frame(t(ret.tab))
  colnames(ret.tab) = c("Lower 95% CI", "coefficient","Upper 95% CI",
                        "Missingness","SL","Ethn","Inter")
  ret.tab
}
#Miss=1, no missing
#Miss=2, MCAR
#Miss=3, MAR
#Miss=4, MNAR


## ----------------------------------------------------------------------------------------------
Ethn.SL.benchmark <- summary(Sen_Len.benchmark)$coefficients[4,1:2]
#the 4th row corresponds to ethnicity and the 1,2 columns are the estimate and its sd
Ethn.SL.benchmark.tab <- ci.tab(Ethn.SL.benchmark)
#Ethn.SL.benchmark.tab


## ----------------------------------------------------------------------------------------------
#parameter that governs the percentage of missingness
per.miss <- 0.3

#generate the missing data indicator
MCAR <- rbinom(n=nrow(ReMF.dat),size=1,prob=c(per.miss))
#sum(MCAR)/nrow(ReMF.dat) #can use to check that the % of missing is approximately correct

#now add NAs to create a missingness pattern
MCAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MCAR==0,Ethnicity, NA))


## ----------------------------------------------------------------------------------------------
SL.MCAR <- summary(lm(Sentence_Length~.,data=MCAR_ReMF.dat))
Ethn.SL.MCAR <-SL.MCAR$coefficients[4,1:2]

Ethn.SL.MCAR.tab <- ci.tab(Ethn.SL.MCAR, Miss=2)

rbind(Ethn.SL.benchmark.tab,Ethn.SL.MCAR.tab)


## ----------------------------------------------------------------------------------------------
MM.Age_Cont <- -0.02
MM.MF_Remorse <- -0.2

#vector of probabilities that are associated with Age and remorse
pMAR <- invlogit(-0.8 + MM.Age_Cont*ReMF.dat$Age_Cont + MM.MF_Remorse*ReMF.dat$MF_Remorse)

#missing data indicator
MAR <- rbinom(n = nrow(ReMF.dat), size = 1, prob = pMAR)
sum(MAR)/nrow(ReMF.dat) 
#check that this is approx 0.2

MAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MAR==0,Ethnicity,NA))


## ----------------------------------------------------------------------------------------------
Sen_Len.MAR <- summary(lm(Sentence_Length~.,data=MAR_ReMF.dat))
Ethn.SL.MAR <- Sen_Len.MAR$coefficients[4,1:2]

Ethn.SL.MAR.tab <- ci.tab(Ethn.SL.MAR, Miss=3)


rbind(Ethn.SL.benchmark.tab,Ethn.SL.MCAR.tab, Ethn.SL.MAR.tab)


## ----------------------------------------------------------------------------------------------
MM.Age_Cont <- -0.01
MM.MF_Remorse <- -0.1
MM.Ethn <- 0.05
MM.SL <- 0.0025
MM.Ethn.SL <- 0.0025
MNAR_intercept <- -1.5

pMNAR <- with(ReMF.dat,
             invlogit(MNAR_intercept + MM.Age_Cont*Age_Cont + MM.MF_Remorse*MF_Remorse +
                   MM.SL*Sentence_Length + MM.Ethn*Ethnicity +
                     MM.Ethn.SL*Ethnicity*Sentence_Length))

#missing data indicator
MNAR <- rbinom(n = nrow(ReMF.dat), size = 1, prob = pMNAR)
sum(MNAR, na.rm=T)/nrow(subset(ReMF.dat))

MNAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MNAR==0,Ethnicity,NA))



## ----------------------------------------------------------------------------------------------
Sen_Len.MNAR <- summary(lm(Sentence_Length~.,data=MNAR_ReMF.dat))
Ethn.SL.MNAR <- Sen_Len.MNAR$coefficients[4,1:2]

Ethn.SL.MNAR.tab <- ci.tab(Ethn.SL.MNAR,Miss=4, SL = MM.SL, Ethn = MM.Ethn, Inter = MM.Ethn.SL)

rbind(Ethn.SL.benchmark.tab,Ethn.SL.MCAR.tab, 
      Ethn.SL.MAR.tab,Ethn.SL.MNAR.tab)


## ----------------------------------------------------------------------------------------------

Sen_Len.all.results <- rbind(Ethn.SL.benchmark.tab,Ethn.SL.MCAR.tab,Ethn.SL.MAR.tab,Ethn.SL.MNAR.tab)
rownames(Sen_Len.all.results) <- c("True","MCAR","MAR","MNAR")
Sen_Len.all.results



## ----------------------------------------------------------------------------------------------
MM.Ethn.SL.Vec <- seq(0.00,0.01,by=0.001)

#to store the percentage missing so this remains "plausible"
store.per.miss <- rep(NA, length(MM.Ethn.SL.Vec))

#to store the estimate of ethnicity
store.coeff.ethn <- c()

for(i in 1:length(MM.Ethn.SL.Vec)){
MM.Ethn.SL <- MM.Ethn.SL.Vec[i]
pMNAR <- with(ReMF.dat,
             invlogit(MNAR_intercept + MM.Age_Cont*Age_Cont + MM.MF_Remorse*MF_Remorse +
                   MM.SL*Sentence_Length + MM.Ethn*Ethnicity +
                     MM.Ethn.SL*Ethnicity*Sentence_Length))

#missing data indicator
MNAR <- rbinom(n = nrow(ReMF.dat), size = 1, prob = pMNAR)
store.per.miss[i]<-sum(MNAR, na.rm=T)/nrow(subset(ReMF.dat))

MNAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MNAR==0,Ethnicity,NA))

Sen_Len.MNAR <- summary(lm(Sentence_Length~.,data=MNAR_ReMF.dat))
Ethn.SL.MNAR <- Sen_Len.MNAR$coefficients[4,1:2]

store.coeff.ethn <- rbind(store.coeff.ethn,ci.tab(Ethn.SL.MNAR, Miss=4,
SL = MM.SL, Ethn = MM.Ethn, Inter = MM.Ethn.SL))

}


## ----------------------------------------------------------------------------------------------
Quartiles.SL <- with(ReMF.dat, quantile(Sentence_Length)[2:4]) #25th, median and 75%

plot.coeff.miss <- ggplot(data=data.frame(Per.miss=store.per.miss,store.coeff.ethn),
                          aes(x=Per.miss)) +
                  geom_ribbon(aes(ymin=Lower.95..CI,
                                  ymax=Upper.95..CI), fill = "grey70")+
  geom_line(aes(y=coefficient)) +
  geom_hline(yintercept=Ethn.SL.benchmark.tab$coefficient) +
  geom_hline(yintercept=0, linetype="dotted") + 
  geom_vline(xintercept=c(0.28,0.32),linetype="dashed")
  

prob.Ethn.SL.miss = with(store.coeff.ethn,
                      invlogit(MNAR_intercept+MM.SL*Quartiles.SL[2]+
                                 MM.Ethn+Inter*Quartiles.SL[2])) 


plot.per.miss <- ggplot(data=data.frame(prob.Ethn.SL.miss=prob.Ethn.SL.miss,
                                        Per.miss=store.per.miss),
                        aes(y=prob.Ethn.SL.miss,x=Per.miss))+geom_line() +
                        geom_vline(xintercept=c(0.28,0.32),linetype="dashed")

grid.arrange(plot.coeff.miss, plot.per.miss, nrow = 2)

