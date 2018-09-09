####################
##    EPDC 2017   ##
####################

setwd("/Users/Scott/Desktop/P4H Global/Evaluation/Evaluation Analysis/EPDC_17/")
s2 <- read.csv("EPDC_17_s2.csv")
s2[s2 == 88] <- NA

## Demographics ##

#sex,age,edu,schtype,exp,stdno,clsz,level,training,p4htrain

dem2 <- matrix(0,10,7)  

for (i in 29:38) {
  for (j in 0:6) {
    dem2[i-28,j+1] <- length(which(s2[,i]==j))
  }
}

View(dem2) 
write.csv(dem2, "dem2.csv")

#student number
mean(s2$X24_Students,na.rm = T); sd(s2$X24_Students,na.rm = T); range(s2$X24_Students,na.rm = T)

#class size
mean(s2$X25_StudentsInClasses,na.rm = T); sd(s2$X25_StudentsInClasses,na.rm = T); range(s2$X25_StudentsInClasses,na.rm = T)

# --------------------------------------------------------

## Conference Evaluation ##

eval2 <- matrix(0,9,5)

for (i in 20:28) {
  for (j in 1:5) {
    eval2[i-19,j] <- length(which(s2[,i]==j))
  }
}

View(eval2)
write.csv(eval2, "eval2.csv")

evalpct2 <- matrix(0,9,5)

for (i in 20:28) {
  for (j in 1:5) {
    evalpct2[i-19,j] <- length(which(s2[,i]==j))/length(s2[,i])
  }
}

View(evalpct2)
write.csv(evalpct2, "evalpct2.csv")

# --------------------------------------------------------

## Conference Impact ##

## before / after table
BA2 <- matrix(0,18,5)

for (i in 2:19) {
  for (j in 1:5) {
    BA2[i-1,j] <- length(which(s2[,i]==j))
  }
}

View(BA2)
write.csv(BA2, "BA2.csv")


## difference table

diff2 <- matrix(0,18,9)

for (i in seq(3,19,by=2)) {
  for (j in -4:4) {
    diff2[i-2,j+5] <- length(which(s2[,i]-s2[,i-1]==j))
    }
  }
      diff2 <- diff2[-c(seq(2,18,by=2)),]

View(diff2)
write.csv(diff2, "diff2.csv")


## Average before/after/difference

avg2 <- matrix(0,18,3)

  #before
for (i in seq(2,18,by=2)) {
  avg2[i,1] <- mean(s2[,i],na.rm = T) 
}
  #after
for (i in seq(3,19,by=2)) {
  avg2[i-1,2] <- mean(s2[,i],na.rm = T) 
}
  #difference
for (i in seq(3,19,by=2)) {
  avg2[i-1,3] <- mean(s2[,i]-s2[,i-1],na.rm = T) 
}
    avg2 <- avg2[-c(seq(1,17,by=2)),]
View(avg2)
write.csv(avg2, "avg2.csv")


# --------------------------------------------------------

#tapply(x1,x1,mean)








