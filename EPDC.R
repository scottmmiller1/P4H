################
##    EPDC    ##
################

setwd("/Users/scottmiller/Desktop/EPDC_19/")
dta <- read.csv("EPDC2019.csv", header = T)


## Demographics ##

#sex,age,edu,schtype,exp,stdno,clsz,level,training,p4htrain

dem <- matrix(0,10,7)  

q.dem <- c(20:29)

for (i in q.dem) {
  for (j in 0:6) {
    dem[i-19,j+1] <- length(which(dta[,i]==j))
  }
}

#experience
mean(dta[,24],na.rm = T); sd(dta[,24],na.rm = T); range(dta[,24],na.rm = T)
#student no.
mean(dta[,25],na.rm = T); sd(dta[,25],na.rm = T); range(dta[,25],na.rm = T)
#class size
mean(dta[,26],na.rm = T); sd(dta[,26],na.rm = T); range(dta[,26],na.rm = T)

# --------------------------------------------------------

## Conference Evaluation ##

eval <- matrix(0,7,5)
q.eval <- c(13:19)

for (i in q.eval) {
  for (j in 1:5) {
    eval[i-12,j] <- length(which(dta[,i]==j))
  }
}

evalpct <- matrix(0,9,5)

for (i in q.eval) {
  for (j in 1:5) {
    evalpct[i-12,j] <- length(which(dta[,i]==j))/length(dta[,i])
  }
}

# --------------------------------------------------------

## Conference Impact ##

## before / after table
BA <- matrix(0,12,5)
q.BA <- c(1:12)

for (i in q.BA) {
  for (j in 1:5) {
    BA[i,j] <- length(which(dta[,i]==j))
  }
}

## difference table

diff <- matrix(0,12,9)
q.diff <- seq(2,12,by=2)

for (i in q.diff) {
  for (j in -4:4) {
    diff[i-1,j+5] <- length(which(dta[,i]-dta[,i-1]==j))
  }
}
diff <- diff[-q.diff,]

## Average before/after/difference

avg <- matrix(0,12,3)

#before
q.before <- seq(1,11,by=2)
for (i in q.before) {
  avg[i,1] <- mean(dta[,i],na.rm = T) 
}
#after
q.after <- seq(2,12,by=2)
for (i in q.after) {
  avg[i-1,2] <- mean(dta[,i],na.rm = T) 
}
#difference
for (i in q.after) {
  avg[i-1,3] <- mean(dta[,i]-dta[,i-1],na.rm = T) 
}
avg <- avg[-q.after,]


# --------------------------------------------------------


test <- matrix(0,12,2)

for (i in q.diff) {
  test[i,1] <- chisq.test(dta[,i], dta[,i-1])$p.value
  test[i,2] <- cor.test(dta[,i], dta[,i-1], "greater", "spearman")$p.value
}
test <- test[-seq(1,11,by=2),]
