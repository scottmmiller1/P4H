install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)


setwd("/Users/scottmiller/Desktop/PD_18")
dta <- read.csv("PD Evals.csv")

###################
#    Combined     #
###################

## Part 1

## before / after table
BA1 <- matrix(0,12,5)

for (i in 2:13) {
  for (j in 1:5) {
    BA1[i-1,j] <- length(which(dta[,i]==j))
  }
}

View(BA1)
#write.csv(BA1, "BA1.csv")


## difference table

diff1 <- matrix(0,12,9)

for (i in seq(3,13,by=2)) {
  for (j in -4:4) {
    diff1[i-2,j+5] <- length(which(dta[,i]-dta[,i-1]==j))
  }
}
diff1 <- diff1[-c(seq(2,18,by=2)),]

View(diff1)
#write.csv(diff1, "diff1.csv")


## Average before/after/difference

avg1 <- matrix(0,12,3)

#before
for (i in seq(2,12,by=2)) {
  avg1[i,1] <- mean(dta[,i],na.rm = T) 
}
#after
for (i in seq(3,13,by=2)) {
  avg1[i-1,2] <- mean(dta[,i],na.rm = T) 
}
#difference
for (i in seq(3,13,by=2)) {
  avg1[i-1,3] <- mean(dta[,i]-dta[,i-1],na.rm = T) 
}
avg1 <- avg1[-c(seq(1,17,by=2)),]
View(avg1)
#write.csv(avg1, "avg1.csv")

#-----------------------------------------

## Part 2

#overall
eval <- matrix(0,4,5)

for (i in 14:17) {
  for (j in 1:5) {
    eval[i-13,j] <- length(which(dta[,i]==j))
  }
}

View(eval) 
#write.csv(eval, "eval.csv")

#percentage
evalpct <- matrix(0,6,5)

for (i in 14:17) {
  for (j in 1:5) {
    evalpct[i-13,j] <- length(which(dta[,i]==j))/length(dta[,i]==j)
  }
}

View(evalpct) 
#write.csv(evalpct, "evalpct.csv")


#-----------------------------------------

## Part 3

dem1 <- matrix(0,8,7)  

for (i in 18:25) {
  for (j in 0:6) {
    dem1[i-17,j+1] <- length(which(dta[,i]==j))
  }
}

View(dem1) 
#write.csv(dem1, "dem1.csv")

# Experience
mean(dta$X14, na.rm = T)

# # of students
mean(dta$X15, na.rm = T)

# Students per class
mean(dta$X16, na.rm = T)



#######################
#   Mission of Hope   #
#######################

MoH <- dplyr::filter(dta, dta$school == "MoH") 

## Part 1

## before / after table
BA1 <- matrix(0,12,5)

for (i in 2:13) {
  for (j in 1:5) {
    BA1[i-1,j] <- length(which(MoH[,i]==j))
  }
}

View(BA1)
#write.csv(BA1, "BA1.csv")


## difference table

diff1 <- matrix(0,12,9)

for (i in seq(3,13,by=2)) {
  for (j in -4:4) {
    diff1[i-2,j+5] <- length(which(MoH[,i]-MoH[,i-1]==j))
  }
}
diff1 <- diff1[-c(seq(2,18,by=2)),]

View(diff1)
#write.csv(diff1, "diff1.csv")


## Average before/after/difference

avg1 <- matrix(0,12,3)

#before
for (i in seq(2,12,by=2)) {
  avg1[i,1] <- mean(MoH[,i],na.rm = T) 
}
#after
for (i in seq(3,13,by=2)) {
  avg1[i-1,2] <- mean(MoH[,i],na.rm = T) 
}
#difference
for (i in seq(3,13,by=2)) {
  avg1[i-1,3] <- mean(MoH[,i]-MoH[,i-1],na.rm = T) 
}
avg1 <- avg1[-c(seq(1,17,by=2)),]
View(avg1)
#write.csv(avg1, "avg1.csv")

#-----------------------------------------

## Part 2

#overall
eval <- matrix(0,4,5)

for (i in 14:17) {
  for (j in 1:5) {
    eval[i-13,j] <- length(which(MoH[,i]==j))
  }
}

View(eval) 
#write.csv(eval, "eval.csv")

#percentage
evalpct <- matrix(0,6,5)

for (i in 14:17) {
  for (j in 1:5) {
    evalpct[i-13,j] <- length(which(MoH[,i]==j))/length(MoH[,i]==j)
  }
}

View(evalpct) 
#write.csv(evalpct, "evalpct.csv")


#-----------------------------------------

## Part 3

dem1 <- matrix(0,8,7)  

for (i in 18:25) {
  for (j in 0:6) {
    dem1[i-17,j+1] <- length(which(MoH[,i]==j))
  }
}

View(dem1) 
#write.csv(dem1, "dem1.csv")

# Experience
mean(MoH$X14, na.rm = T)

# # of students
mean(MoH$X15, na.rm = T)

# Students per class
mean(MoH$X16, na.rm = T)


##############
#   Harvey   #
##############

Harvey <- dplyr::filter(dta, dta$school == "Harvey") 

## Part 1

## before / after table
BA1 <- matrix(0,12,5)

for (i in 2:13) {
  for (j in 1:5) {
    BA1[i-1,j] <- length(which(Harvey[,i]==j))
  }
}

View(BA1)
write.csv(BA1, "BA1.csv")


## difference table

diff1 <- matrix(0,12,9)

for (i in seq(3,13,by=2)) {
  for (j in -4:4) {
    diff1[i-2,j+5] <- length(which(Harvey[,i]-Harvey[,i-1]==j))
  }
}
diff1 <- diff1[-c(seq(2,18,by=2)),]

View(diff1)
write.csv(diff1, "diff1.csv")


## Average before/after/difference

avg1 <- matrix(0,12,3)

#before
for (i in seq(2,12,by=2)) {
  avg1[i,1] <- mean(Harvey[,i],na.rm = T) 
}
#after
for (i in seq(3,13,by=2)) {
  avg1[i-1,2] <- mean(Harvey[,i],na.rm = T) 
}
#difference
for (i in seq(3,13,by=2)) {
  avg1[i-1,3] <- mean(Harvey[,i]-Harvey[,i-1],na.rm = T) 
}
avg1 <- avg1[-c(seq(1,17,by=2)),]
View(avg1)
write.csv(avg1, "avg1.csv")

#-----------------------------------------

## Part 2

#overall
eval <- matrix(0,4,5)

for (i in 14:17) {
  for (j in 1:5) {
    eval[i-13,j] <- length(which(Harvey[,i]==j))
  }
}

View(eval) 
write.csv(eval, "eval.csv")

#percentage
evalpct <- matrix(0,6,5)

for (i in 14:17) {
  for (j in 1:5) {
    evalpct[i-13,j] <- length(which(Harvey[,i]==j))/length(Harvey[,i]==j)
  }
}

View(evalpct) 
write.csv(evalpct, "evalpct.csv")


#-----------------------------------------

## Part 3

dem1 <- matrix(0,8,7)  

for (i in 18:25) {
  for (j in 0:6) {
    dem1[i-17,j+1] <- length(which(Harvey[,i]==j))
  }
}

View(dem1) 
write.csv(dem1, "dem1.csv")

# Experience
mean(Harvey$X14, na.rm = T)

# # of students
mean(Harvey$X15, na.rm = T)

# Students per class
mean(Harvey$X16, na.rm = T)


#############
#    ESM    #
#############

ESM <- dplyr::filter(dta, dta$school == "ESM") 

## Part 1

## before / after table
BA1 <- matrix(0,12,5)

for (i in 2:13) {
  for (j in 1:5) {
    BA1[i-1,j] <- length(which(ESM[,i]==j))
  }
}

View(BA1)
write.csv(BA1, "BA1.csv")


## difference table

diff1 <- matrix(0,12,9)

for (i in seq(3,13,by=2)) {
  for (j in -4:4) {
    diff1[i-2,j+5] <- length(which(ESM[,i]-ESM[,i-1]==j))
  }
}
diff1 <- diff1[-c(seq(2,18,by=2)),]

View(diff1)
write.csv(diff1, "diff1.csv")


## Average before/after/difference

avg1 <- matrix(0,12,3)

#before
for (i in seq(2,12,by=2)) {
  avg1[i,1] <- mean(ESM[,i],na.rm = T) 
}
#after
for (i in seq(3,13,by=2)) {
  avg1[i-1,2] <- mean(ESM[,i],na.rm = T) 
}
#difference
for (i in seq(3,13,by=2)) {
  avg1[i-1,3] <- mean(ESM[,i]-ESM[,i-1],na.rm = T) 
}
avg1 <- avg1[-c(seq(1,17,by=2)),]
View(avg1)
write.csv(avg1, "avg1.csv")

#-----------------------------------------

## Part 2

#overall
eval <- matrix(0,4,5)

for (i in 14:17) {
  for (j in 1:5) {
    eval[i-13,j] <- length(which(ESM[,i]==j))
  }
}

View(eval) 
write.csv(eval, "eval.csv")

#percentage
evalpct <- matrix(0,6,5)

for (i in 14:17) {
  for (j in 1:5) {
    evalpct[i-13,j] <- length(which(ESM[,i]==j))/length(ESM[,i]==j)
  }
}

View(evalpct) 
write.csv(evalpct, "evalpct.csv")


#-----------------------------------------

## Part 3

dem1 <- matrix(0,8,7)  

for (i in 18:25) {
  for (j in 0:6) {
    dem1[i-17,j+1] <- length(which(ESM[,i]==j))
  }
}

View(dem1) 
write.csv(dem1, "dem1.csv")

# Experience
mean(ESM$X14, na.rm = T)

# # of students
mean(ESM$X15, na.rm = T)

# Students per class
mean(ESM$X16, na.rm = T)




