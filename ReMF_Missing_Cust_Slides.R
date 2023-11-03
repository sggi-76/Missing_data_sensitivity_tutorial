## ----setup, include=FALSE----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(arm)
library(mice)
library(ggplot2)
library(car)
library(dplyr)
library(purrr)
library(forcats)
library(gridExtra)
setwd("~/Dropbox (LSE Statistics)/Sentencing Disparities/Methods festival")


## ----------------------------------------------------------------------------------------------
ReMF.dat<- read.csv("ReMF_original.csv",
                       header=T,stringsAsFactors = T)
#head(ReMF.dat)

ReMF.dat <- ReMF.dat %>% mutate(Prev_Convictions = 
          fct_relevel(Prev_Convictions, 
          c("None","1 to 3","4 to 9","10 or more")))
#some house-keeping.


## ----------------------------------------------------------------------------------------------
ReMF.dat <- ReMF.dat %>%
  mutate(Age_Cont = Age_Cont - mean(Age_Cont))


## ----------------------------------------------------------------------------------------------
ReMF.dat <- dplyr::select(ReMF.dat,-c(Sentence_Length))


## ----------------------------------------------------------------------------------------------
Cust.benchmark <- glm(Custody~.,data=ReMF.dat)

#summary(Cust.benchmark)


## ----------------------------------------------------------------------------------------------
ci.tab<-function(vec,Miss=1,Cust=0,Ethn=0,Inter=0){
  #default values are miss=1, 
  #Cust=Ethn=Inter=0 means there is 
  #no parameter for the missingness model
 ret.tab <- c(vec[1]-2*vec[2],vec[1],
      vec[1]+2*vec[2])
 ret.tab <- exp(ret.tab)
 #If you want output on the log(OR) scale, comment the line above
 ret.tab <-c(ret.tab,c(Miss, Cust, Ethn, Inter))
 ret.tab <- as.data.frame(t(ret.tab))
  colnames(ret.tab) = c("Lower 95% CI", "Est","Upper 95% CI",
                        "Miss","Cust","Ethn","Inter")
  ret.tab
}
#Miss=1, no missing
#Miss=2, MCAR
#Miss=3, MAR
#Miss=4, MNAR


## ----------------------------------------------------------------------------------------------
Ethn.Cust.benchmark <- 
  summary(Cust.benchmark)$coefficients[4,1:2] 
#the 4th row corresponds to ethnicity and 
#the 1,2 columns are the estimate and its sd

Ethn.Cust.benchmark.tab <- ci.tab(Ethn.Cust.benchmark)
#Ethn.Cust.benchmark.tab



## ----------------------------------------------------------------------------------------------
#parameter that governs the percentage of missingness
per.miss <- 0.2

#generate the missing data indicator
MCAR <- rbinom(n=nrow(ReMF.dat),size=1,prob=c(per.miss))
#sum(MCAR)/nrow(ReMF.dat) 
#should check that the % of missing is approximately correct

#now add NAs to create a missingness pattern
MCAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MCAR==1,NA,Ethnicity))


## ----------------------------------------------------------------------------------------------
Cust.MCAR <- summary(glm(Custody~.,data=MCAR_ReMF.dat))
Ethn.Cust.MCAR <- Cust.MCAR$coefficients[4,1:2]

Ethn.Cust.MCAR.tab <- ci.tab(Ethn.Cust.MCAR, Miss=2)

#to display
#rbind(Ethn.Cust.benchmark.tab,Ethn.Cust.MCAR.tab)


## ----------------------------------------------------------------------------------------------
MM.Age_Cont <- -0.02
MM.MF_Remorse <- -0.20


#vector of probablities that are associated with Age and remorse
pMAR <- invlogit(-1.3 + MM.Age_Cont*ReMF.dat$Age_Cont +
                   MM.MF_Remorse*ReMF.dat$MF_Remorse)

#missing data indicator
MAR <- rbinom(n = nrow(ReMF.dat), size = 1, prob = pMAR)
#sum(MAR)/nrow(ReMF.dat) 
#check that this is approx 0.2

MAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MAR==0,Ethnicity,NA))


## ----------------------------------------------------------------------------------------------
Cust.MAR <- summary(glm(Custody~.,data=MAR_ReMF.dat))
Ethn.Cust.MAR <- Cust.MAR$coefficients[4,1:2]

Ethn.Cust.MAR.tab <- ci.tab(Ethn.Cust.MAR, Miss=3)


## ----------------------------------------------------------------------------------------------
rbind(Ethn.Cust.benchmark.tab,Ethn.Cust.MCAR.tab,
      Ethn.Cust.MAR.tab)


## ----------------------------------------------------------------------------------------------
MM.Ethn <- 0.2
MM.Cust <- -0.05
MM.Ethn.Cust <- 0.9


## ----------------------------------------------------------------------------------------------
MNAR_intercept <- -1.55
#Ethn=1, Cust=1
invlogit(MNAR_intercept+MM.Cust+MM.Ethn+MM.Ethn.Cust)
#Ethn=1, Cust=0
invlogit(MNAR_intercept+MM.Ethn)
#Ethn=0, Cust=1
invlogit(MNAR_intercept+MM.Cust)
#Ethn=0, Cust=0
invlogit(MNAR_intercept)


## ----------------------------------------------------------------------------------------------

pMNAR <- with(ReMF.dat,
             invlogit(MNAR_intercept + MM.Age_Cont*Age_Cont + 
                   MM.MF_Remorse*MF_Remorse +
                   MM.Cust*Custody + 
                   MM.Ethn*Ethnicity + 
                   MM.Ethn.Cust*Custody*Ethnicity))

#missing data indicator
MNAR <- rbinom(n = nrow(ReMF.dat), size = 1, prob = pMNAR)
sum(MNAR)/nrow(ReMF.dat)

MNAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MNAR==0,Ethnicity,NA))



## ----------------------------------------------------------------------------------------------
Cust.MNAR <- summary(glm(Custody~.,data=MNAR_ReMF.dat))
Ethn.Cust.MNAR <- Cust.MNAR$coefficients[4,1:2]

Ethn.Cust.MNAR.tab <- ci.tab(Ethn.Cust.MNAR, Miss=4, 
Cust = MM.Cust, Ethn = MM.Ethn, Inter = MM.Ethn.Cust)


## ----------------------------------------------------------------------------------------------
rbind(Ethn.Cust.benchmark.tab,Ethn.Cust.MCAR.tab,
      Ethn.Cust.MAR.tab,Ethn.Cust.MNAR.tab)


## ----------------------------------------------------------------------------------------------
Cust.all.results <- rbind(Ethn.Cust.benchmark.tab,Ethn.Cust.MCAR.tab,
                          Ethn.Cust.MAR.tab,Ethn.Cust.MNAR.tab)
rownames(Cust.all.results) <- c("True","MCAR","MAR","MNAR")
Cust.all.results


## ----echo=FALSE--------------------------------------------------------------------------------
MM.Ethn.Cust.Vec <- seq(0,2,by=0.05)
MM.Intercept <- (seq())

#to store the percentage missing so this remains "plausible"
store.per.miss <- rep(NA, length(MM.Ethn.Cust.Vec))

store.coeff.ethn <- c()

for(i in 1:length(MM.Ethn.Cust.Vec)){
  MM.Ethn.Cust <- MM.Ethn.Cust.Vec[i]
 pMNAR <- with(ReMF.dat,
             invlogit(MNAR_intercept + MM.Age_Cont*Age_Cont + 
                     MM.MF_Remorse*MF_Remorse +
                     MM.Cust*Custody + 
                     MM.Ethn*Ethnicity + 
                     MM.Ethn.Cust*Custody*Ethnicity))

#missing data indicator
MNAR <- rbinom(n = nrow(ReMF.dat), size = 1, prob = pMNAR)
store.per.miss[i] <- sum(MNAR)/nrow(ReMF.dat)

MNAR_ReMF.dat <- ReMF.dat %>%
  mutate(Ethnicity=ifelse(MNAR==0,Ethnicity,NA))

Cust.MNAR <- summary(glm(Custody~.,data=MNAR_ReMF.dat))
Ethn.Cust.MNAR <- Cust.MNAR$coefficients[4,1:2]

store.coeff.ethn <- rbind(store.coeff.ethn,ci.tab(Ethn.Cust.MNAR, Miss=4, 
                Cust = MM.Cust, Ethn = MM.Ethn, Inter = MM.Ethn.Cust))
}

##PLOTS

plot.coeff.miss <- ggplot(data=data.frame(Per.miss=store.per.miss,store.coeff.ethn),
                          aes(x=Per.miss)) +
                  geom_ribbon(aes(ymin=Lower.95..CI,
                                  ymax=Upper.95..CI), fill = "grey70")+
  geom_line(aes(y=Est)) +
  geom_hline(yintercept=Ethn.Cust.benchmark.tab$coefficient) +
  geom_vline(xintercept=c(0.18,0.22),linetype="dashed")
  

prob.Ethn.Cust.miss = with(store.coeff.ethn,
                      invlogit(MNAR_intercept+MM.Cust+
                                 MM.Ethn+Inter)) 


plot.per.miss <- ggplot(data=data.frame(prob.Ethn.Cust.miss=prob.Ethn.Cust.miss,
                                        Per.miss=store.per.miss),
                        aes(y=prob.Ethn.Cust.miss,x=Per.miss))+geom_line() +
                        geom_vline(xintercept=c(0.18,0.22),linetype="dashed")

grid.arrange(plot.coeff.miss, plot.per.miss, nrow = 2)

