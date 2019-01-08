####################################
##    Impact Trip 2018 - Summer   ##
####################################

setwd("/Users/scottmiller/Desktop/ImpactTrip_Summer_18/")
dta <- read.csv("2018 Summer Impact Trip Data.csv")


## Trip Process ##

#overall
trip <- matrix(0,6,5)

trip.vars <- c(1:6)

for (i in trip.vars) {
  for (j in 1:5) {
    trip[i,j] <- length(which(dta[,i]==j))
  }
}


#percentage
trip1pct <- matrix(0,6,5)

for (i in trip.vars) {
  for (j in 1:5) {
    trip1pct[i,j] <- length(which(dta[,i]==j))/length(dta[,i]==j)
  }
}


# meetings missed
summary(dta$X7)


## Impact ##

## before / after table
BA1 <- matrix(0,18,5)

ba.vars <- c(8:25)

for (i in ba.vars) {
  for (j in 1:5) {
    BA1[i-7,j] <- length(which(dta[,i]==j))
  }
}


## difference table

diff1 <- matrix(0,18,9)

diff.vars <- seq(9,25,by=2)

for (i in diff.vars) {
  for (j in -4:4) {
    diff1[i-8,j+5] <- length(which(dta[,i]-dta[,i-1]==j))
  }
}
diff1 <- diff1[-c(seq(2,18,by=2)),]


## Average before/after/difference

avg1 <- matrix(0,18,3)

#before
for (i in diff.vars) {
  avg1[i-8,1] <- mean(dta[,i-1],na.rm = T) 
  #after
  avg1[i-8,2] <- mean(dta[,i],na.rm = T) 
  #difference
  avg1[i-8,3] <- mean(dta[,i]-dta[,i-1],na.rm = T) 
}
avg1 <- avg1[-c(seq(2,18,by=2)),]


## Demographics ##

dem <- matrix(0,8,8)

dem.vars <- c(26:33)

for (i in dem.vars) {
  for (j in 0:7) {
    dem1[i-25,j+1] <- length(which(dta[,i]==j))
  }
}


## Statistical Tests ##

test <- matrix(0,18,2)

for (i in seq(1,17,by=2)) {
  test[i,1] <- chisq.test(dta[,i+9], dta[,i+8])$p.value
  test[i,2] <- cor.test(dta[,i+9], dta[,i+8], "greater", "spearman")$p.value
}
View(test)


for (i in seq(1,17,by=2)) {
  print(chisq.test(dta[,i+9], dta[,i+8]))
}

# Not statistically significant:


for (i in seq(1,17,by=2)) {
  print(cor.test(dta[,i+9], dta[,i+8], "greater", "spearman"))
}

# Not statistically significant:

