
setwd("/Users/scottmiller/Desktop/PD_18")
dta <- read.csv("Exam Analysis/PD_18 Exam Data Full.csv")
library(tidyverse)


dta1 <- filter(dta, dta$Complete !="i") # deletes observations that did not take both exams
dta1 <- dta1[,-c(22:23)] # new dataset with missing rows / cols removed
dta1 <- filter(dta1, dta1$School == "JOUISSANT") # change for each school

num <- matrix(0,nrow(dta1),17)
num[,1] <- dta1[,1]

# label answer choices
for (i in 1:nrow(num)) {
  for (j in 6:21) {
    if (dta1[i,j] == "v") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "f") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "bon") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "pa bon") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "a") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "b") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "c") {
      num[i,j-4] <- 3
    } else if (dta1[i,j] == "d") {
      num[i,j-4] <- 4
    } else if (dta1[i,j] == "e") {
      num[i,j-4] <- 5
    }
  }
}


# -------------------------------------------------------

#overall
answers <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    answers[i,j+1] <- length(which(num[,i+1]==j))
  }
}

# -------------------------------------------------------

#Pre
pre_num <- num[-c(seq(2,nrow(num),by=2)),]

pre <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    pre[i,j+1] <- length(which(pre_num[,i+1]==j))
  }
}


# Pre percent
pre_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    pre_pct[i,j+1] <- length(which(pre_num[,i+1]==j))/length(pre_num[,i+1]==j)
  }
}


# -------------------------------------------------------

#Post
post_num <- num[-c(1,seq(1,nrow(num)-1,by=2)),]

post <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    post[i,j+1] <- length(which(post_num[,i+1]==j))
  }
}


# post percent
post_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    post_pct[i,j+1] <- length(which(post_num[,i+1]==j))/length(post_num[,i+1]==j)
  }
}
