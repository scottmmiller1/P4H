###################################
##    Impact Trip Evaluation     ##
###################################

setwd("/Users/scottmiller/Desktop/ImpactTrip_Winter_19/")
dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/Analysis/ImpactTrip/ImpactTrip_UF_Haiti_ALL/ImpactTrip_UF_Haiti_ALL.csv")
library(tidyverse)


#dta <- filter(dta, dta$Trip == "2019 Winter") # change for each school
dta <- dta[,-c(1:2)]


## Trip Process ##

#overall
eval <- matrix(0,6,5)

eval.vars <- c(1:6)

for (i in trip.vars) {
  for (j in 1:5) {
    eval[i,j] <- length(which(dta[,i]==j))
  }
}


#percentage
evalpct <- matrix(0,6,5)

for (i in eval.vars) {
  for (j in 1:5) {
    evalpct[i,j] <- length(which(dta[,i]==j))/length(dta[,i]==j)
  }
}


# meetings missed
summary(dta$X7)


## Impact ##

## before / after table
BA <- matrix(0,18,5)

ba.vars <- c(8:25)

for (i in ba.vars) {
  for (j in 1:5) {
    BA[i-7,j] <- length(which(dta[,i]==j))
  }
}


## difference table

diff <- matrix(0,18,9)

diff.vars <- seq(9,25,by=2)

for (i in diff.vars) {
  for (j in -4:4) {
    diff[i-8,j+5] <- length(which(dta[,i]-dta[,i-1]==j))
  }
}
diff <- diff[-c(seq(2,18,by=2)),]


## Average before/after/difference

avg <- matrix(0,18,3)

#before
for (i in diff.vars) {
  avg[i-8,1] <- mean(dta[,i-1],na.rm = T) 
  #after
  avg[i-8,2] <- mean(dta[,i],na.rm = T) 
  #difference
  avg[i-8,3] <- mean(dta[,i]-dta[,i-1],na.rm = T) 
}
avg <- avg[-c(seq(2,18,by=2)),]


## Demographics ##

dem <- matrix(0,8,8)

dem.vars <- c(26:32)

for (i in dem.vars) {
  for (j in 0:7) {
    dem[i-25,j+1] <- length(which(dta[,i]==j))
  }
}


## Statistical Tests ##

test <- matrix(0,18,2)

for (i in seq(1,17,by=2)) {
  test[i,1] <- chisq.test(dta[,i+9], dta[,i+8])$p.value
  test[i,2] <- cor.test(dta[,i+9], dta[,i+8], "greater", "spearman")$p.value
}
