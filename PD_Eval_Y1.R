
#library(tidyverse)

setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/Analysis/PD/PD_19/Year 1/Data/Evals/")
dta <- read.csv("PD_19 Evals Full.csv")
#dta <- filter(dta, dta$School == "Sonje") # change for each school


## Part 1

## before / after table
BA1 <- matrix(0,12,5)
BA.questions <- c(3:14) 

for (i in BA.questions) {
  for (j in 1:5) {
    BA1[i-2,j] <- length(which(dta[,i]==j))
  }
}


## difference table
diff <- matrix(0,12,9)
A.questions <- seq(4,14,by=2)

for (i in A.questions) {
  for (j in -4:4) {
    diff[i-2,j+5] <- length(which(dta[,i]-dta[,i-1]==j))
  }
}
diff <- diff[-c(seq(1,11,by=2)),]


## Average before/after/difference
avg1 <- matrix(0,12,3)
B.questions <- seq(3,13,by=2)

#before
for (i in B.questions) {
  avg1[i-2,1] <- mean(dta[,i],na.rm = T) 
}
#after
for (i in A.questions) {
  avg1[i-3,2] <- mean(dta[,i],na.rm = T) 
}
#difference
for (i in A.questions) {
  avg1[i-3,3] <- mean(dta[,i]-dta[,i-1],na.rm = T) 
}
avg1 <- avg1[-c(seq(2,12,by=2)),]


#-----------------------------------------
## Part 2

#overall
eval <- matrix(0,4,5)
p2.questions <- c(15:18)

for (i in p2.questions) {
  for (j in 1:5) {
    eval[i-14,j] <- length(which(dta[,i]==j))
  }
}


#percentage
evalpct <- matrix(0,6,5)

for (i in p2.questions) {
  for (j in 1:5) {
    evalpct[i-14,j] <- length(which(dta[,i]==j))/length(dta[,i]==j)
  }
}


#-----------------------------------------
## Part 3

dem <- matrix(0,8,7)  
p3.questions <- c(19:26)

for (i in p3.questions) {
  for (j in 0:6) {
    dem[i-18,j+1] <- length(which(dta[,i]==j))
  }
}


# Experience
mean(dta$X14, na.rm = T)

# # of students
mean(dta$X15, na.rm = T)

# Students per class
mean(dta$X16, na.rm = T)


