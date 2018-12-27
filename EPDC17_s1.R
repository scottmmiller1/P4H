####################
##    EPDC 2017   ##
####################

setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/Analysis/EPDC_17/")
s1 <- read.csv("EPDC_17_S1.csv")
s1[s1 == 88] <- NA

## Demographics ##

#sex,age,edu,schtype,exp,stdno,clsz,level,training,p4htrain

dem1 <- matrix(0,10,7)  

for (i in 29:38) {
  for (j in 0:6) {
    dem1[i-28,j+1] <- length(which(s1[,i]==j))
  }
}

View(dem1) 
write.csv(dem1, "dem1.csv")

#student number
mean(s1$studentno,na.rm = T); sd(s1$studentno,na.rm = T); range(s1$studentno,na.rm = T)

#class size
mean(s1$class_size,na.rm = T); sd(s1$class_size,na.rm = T); range(s1$class_size,na.rm = T)

# --------------------------------------------------------

## Conference Evaluation ##

eval1 <- matrix(0,9,5)

for (i in 20:28) {
  for (j in 1:5) {
    eval1[i-19,j] <- length(which(s1[,i]==j))
  }
}

View(eval1)
#write.csv(eval1, "eval1.csv")

evalpct1 <- matrix(0,9,5)

for (i in 20:28) {
  for (j in 1:5) {
    evalpct1[i-19,j] <- length(which(s1[,i]==j))/length(s1[,i])
  }
}

View(evalpct1)
#write.csv(evalpct1, "evalpct1.csv")

# --------------------------------------------------------

## Conference Impact ##

## before / after table
BA1 <- matrix(0,18,5)

for (i in 2:19) {
  for (j in 1:5) {
    BA1[i-1,j] <- length(which(s1[,i]==j))
  }
}

View(BA1)
#write.csv(BA1, "BA1.csv")


## difference table

diff1 <- matrix(0,18,9)

for (i in seq(3,19,by=2)) {
  for (j in -4:4) {
    diff1[i-2,j+5] <- length(which(s1[,i]-s1[,i-1]==j))
    }
  }
      diff1 <- diff1[-c(seq(2,18,by=2)),]

View(diff1)
#write.csv(diff1, "diff1.csv")


## Average before/after/difference

avg1 <- matrix(0,18,3)

  #before
for (i in seq(2,18,by=2)) {
  avg1[i,1] <- mean(s1[,i],na.rm = T) 
}
  #after
for (i in seq(3,19,by=2)) {
  avg1[i-1,2] <- mean(s1[,i],na.rm = T) 
}
  #difference
for (i in seq(3,19,by=2)) {
  avg1[i-1,3] <- mean(s1[,i]-s1[,i-1],na.rm = T) 
}
    avg1 <- avg1[-c(seq(1,17,by=2)),]
View(avg1)
#write.csv(avg1, "avg1.csv")


# --------------------------------------------------------


test <- matrix(0,9,2)

for (i in seq(3,19,by=2)) {
  test[1,i] <- chisq.test(s1[,i], s1[,i-1])$p.value
  test[2,i] <- cor.test(s1[,i], s1[,i-1], "greater", "spearman")$p.value
    }
      View(test)


for (i in seq(3,19,by=2)) {
  print(cor.test(s1[,i], s1[,i-1], "greater", "spearman"))
    }

for (i in seq(3,19,by=2)) {
  print(chisq.test(s1[,i], s1[,i-1]))
    }



