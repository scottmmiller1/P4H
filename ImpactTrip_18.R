###########################
##    Impact Trip 2018   ##
###########################

setwd("/Users/scottmiller/Desktop/ImpactTrip_18/")
dta <- read.csv("ImpactTrip_18.csv")


## Trip Process ##

# ID, prep, tinfo, cinfo, nightly, fsafety, support, nmissed

trip <- matrix(0,57,6)

for (i in 1:57) {
  for (j in 2:7) {
    if (dta[i,j] == "Strongly Disagree") {
      trip[i,j-1] <- 1
    } else if (dta[i,j] == "Disagree") {
      trip[i,j-1] <- 2
    } else if (dta[i,j] == "Neutral") {
      trip[i,j-1] <- 3
    } else if (dta[i,j] == "Agree") {
      trip[i,j-1] <- 4
    } else if (dta[i,j] == "Strongly Agree") {
      trip[i,j-1] <- 5
    }
  }
}

#overall
trip1 <- matrix(0,6,5)

for (i in 1:6) {
  for (j in 1:5) {
    trip1[i,j] <- length(which(trip[,i]==j))
  }
}

View(trip1) 
#write.csv(trip1, "trip1.csv")

#percentage
trip1pct <- matrix(0,6,5)

for (i in 1:6) {
  for (j in 1:5) {
    trip1pct[i,j] <- length(which(trip[,i]==j))/length(trip[,i]==j)
  }
}

View(trip1pct) 
#write.csv(trip1pct, "trip1pct.csv")

mean(dta$nmissed)


## Impact ##

## before / after table
BA1 <- matrix(0,18,5)

for (i in 1:18) {
  for (j in 1:5) {
    dta[,i+8] <- floor(as.numeric(dta[,i+8]))
    BA1[i,j] <- length(which(dta[,i+8]==j))
  }
}

View(BA1)
#write.csv(BA1, "BA1.csv")

## difference table

diff1 <- matrix(0,18,9)

for (i in seq(1,17,by=2)) {
  for (j in -4:4) {
    diff1[i,j+5] <- length(which(dta[,i+9]-dta[,i+8]==j))
  }
}
diff1 <- diff1[-c(seq(2,19,by=2)),]

View(diff1)
#write.csv(diff1, "diff1.csv")

## Average before/after/difference

avg1 <- matrix(0,18,3)

#before
for (i in seq(1,17,by=2)) {
  avg1[i,1] <- mean(dta[,i+8],na.rm = T) 
  #after
  avg1[i,2] <- mean(dta[,i+9],na.rm = T) 
  #difference
  avg1[i,3] <- mean(dta[,i+9]-dta[,i+8],na.rm = T) 
}
avg1 <- avg1[-c(seq(2,18,by=2)),]
View(avg1)
#write.csv(avg1, "avg1.csv")


## Demographics ##

dem <- matrix(0,57,7)

for (i in 1:57) {
    if (dta[i,29] == "Male") {
      dem[i,1] <- 1  } 
    if (dta[i,30] == "FR") {
      dem[i,2] <- 1
    } else if (dta[i,30] == "SO") {
      dem[i,2] <- 2
    } else if (dta[i,30] == "JR") {
      dem[i,2] <- 3
    } else if (dta[i,30] == "SR") {
      dem[i,2] <- 4
    } else if (dta[i,30] == "Graduate") {
      dem[i,2] <- 5
    } else if (dta[i,30] == "Faculty") {
      dem[i,2] <- 6 }
    if (dta[i,31] == "Hispanic or Latino") {
      dem[i,3] <- 1 }
    if (dta[i,32] == "American Indian or Alaska Native") {
      dem[i,4] <- 1
    } else if (dta[i,32] == "Asian") {
      dem[i,4] <- 2
    } else if (dta[i,32] == "African American or Black") {
      dem[i,4] <- 3
    } else if (dta[i,32] == "White") {
      dem[i,4] <- 4
    } else if (dta[i,32] == "Native Hawaiian") {
      dem[i,4] <- 5
    } else if (dta[i,32] == "Other Pacific Islander") {
      dem[i,4] <- 6 }
    if (dta[i,35] == "Health Education") {
      dem[i,5] <- 1
    } else if (dta[i,35] == "Youth Leadership") {
      dem[i,5] <- 2
    } else if (dta[i,35] == "VBS") {
      dem[i,5] <- 3  }
    dem[i,6] <- as.numeric(dta[i,36])-1
    if (dta[i,37] == "0") {
      dem[i,7] <- 0
    } else if (dta[i,37] == "1") {
      dem[i,7] <- 1
    } else if (dta[i,37] == "2") {
      dem[i,7] <- 2
    } else if (dta[i,37] == "3") {
      dem[i,7] <- 3
    } else if (dta[i,37] == "4") {
      dem[i,7] <- 4
    }
}

View(dem)

dem1 <- matrix(0,7,7)

for (i in 1:7) {
  for (j in 0:6) {
    dem1[i,j+1] <- length(which(dem[,i]==j))
  }
}

View(dem1) 
#write.csv(dem1, "dem1.csv")


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

# Not statistically significant: Perform, Career, 


for (i in seq(1,17,by=2)) {
  print(cor.test(dta[,i+9], dta[,i+8], "greater", "spearman"))
}

# Not statistically significant: Perform

