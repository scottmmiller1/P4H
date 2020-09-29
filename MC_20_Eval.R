
#################################################################
##  Analyzes Master Class eval data and generates              ##
##  corresponding charts for P4H Global MC evaluation reports. ##
#################################################################


setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/MC/MC_20/Sessions/S1/Rplots")
dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/MC/MC_20/Data/MC_20_Eval.csv")

## packages
library(tidyverse)
library(ggplot2)
# element count function
source("/Users/scottmiller/GitHub/P4H/elem_count.R")


dta <- filter(dta, dta$Complete !="i") # deletes observations that did not take both exams
dta1 <- filter(dta, dta$First !="Brenda")

num <- matrix(0,nrow(dta1),19)


# label answer choices
for (i in 1:nrow(num)) {
  for (j in 5:23) {
    if (dta1[i,j] == "Strongly Disagree") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "Disagree") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "Undecided") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "Agree") {
      num[i,j-4] <- 3
    } else if (dta1[i,j] == "Strongly Agree") {
      num[i,j-4] <- 5
    }
  }
}


##########################
##      Eval Charts     ##
##########################

eval <- elem_count(quest = 19, choices = 5, data = num, percent = FALSE)
evalpct <- elem_count(quest = 19, choices = 5, data = num, percent = TRUE)

eval <- eval[,-1]

colnames(eval) <- c("Strongly Disagree","Disagree","Undecided","Agree","Strongly Agree")

# 1

png("e1.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e1 <- barplot((eval[1,]/sum(eval[1,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e1, (eval[1,]/sum(eval[1,]))*100 + 7, paste(round((eval[1,]/sum(eval[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2

png("e2.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e2 <- barplot((eval[2,]/sum(eval[2,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e2, (eval[2,]/sum(eval[2,]))*100 + 7, paste(round((eval[2,]/sum(eval[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3

png("e3.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e3 <- barplot((eval[3,]/sum(eval[3,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e3, (eval[3,]/sum(eval[3,]))*100 + 7, paste(round((eval[3,]/sum(eval[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4

png("e4.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e4 <- barplot((eval[4,]/sum(eval[4,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e4, (eval[4,]/sum(eval[4,]))*100 + 7, paste(round((eval[4,]/sum(eval[4,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 5

png("e5.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e5 <- barplot((eval[5,]/sum(eval[5,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e5, (eval[5,]/sum(eval[5,]))*100 + 7, paste(round((eval[5,]/sum(eval[5,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 6

png("e6.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e6 <- barplot((eval[6,]/sum(eval[6,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e6, (eval[6,]/sum(eval[6,]))*100 + 7, paste(round((eval[6,]/sum(eval[6,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 7

png("e7.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e7 <- barplot((eval[7,]/sum(eval[7,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e7, (eval[7,]/sum(eval[7,]))*100 + 7, paste(round((eval[7,]/sum(eval[7,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 8

png("e8.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e8 <- barplot((eval[8,]/sum(eval[8,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e8, (eval[8,]/sum(eval[8,]))*100 + 7, paste(round((eval[8,]/sum(eval[8,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 9

png("e9.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e9 <- barplot((eval[9,]/sum(eval[9,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e9, (eval[9,]/sum(eval[9,]))*100 + 7, paste(round((eval[9,]/sum(eval[9,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 10

png("e10.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e10 <- barplot((eval[10,]/sum(eval[10,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e10, (eval[10,]/sum(eval[10,]))*100 + 7, paste(round((eval[10,]/sum(eval[10,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 11

png("e11.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e11 <- barplot((eval[11,]/sum(eval[11,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e11, (eval[11,]/sum(eval[11,]))*100 + 7, paste(round((eval[11,]/sum(eval[11,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 12

png("e12.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e12 <- barplot((eval[12,]/sum(eval[12,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e12, (eval[12,]/sum(eval[12,]))*100 + 7, paste(round((eval[12,]/sum(eval[12,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 13

png("e13.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e13 <- barplot((eval[13,]/sum(eval[13,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e13, (eval[13,]/sum(eval[13,]))*100 + 7, paste(round((eval[13,]/sum(eval[13,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 14

png("e14.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e14 <- barplot((eval[14,]/sum(eval[14,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e14, (eval[14,]/sum(eval[14,]))*100 + 7, paste(round((eval[14,]/sum(eval[14,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 15

png("e15.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e15 <- barplot((eval[15,]/sum(eval[15,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e15, (eval[15,]/sum(eval[15,]))*100 + 7, paste(round((eval[15,]/sum(eval[15,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 16

png("e16.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e16 <- barplot((eval[16,]/sum(eval[16,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e16, (eval[16,]/sum(eval[16,]))*100 + 7, paste(round((eval[16,]/sum(eval[16,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 17

png("e17.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e17 <- barplot((eval[17,]/sum(eval[17,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e17, (eval[17,]/sum(eval[17,]))*100 + 7, paste(round((eval[17,]/sum(eval[17,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 18

png("e18.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e18 <- barplot((eval[18,]/sum(eval[18,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e18, (eval[18,]/sum(eval[18,]))*100 + 7, paste(round((eval[18,]/sum(eval[18,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 19

png("e19.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e19 <- barplot((eval[19,]/sum(eval[19,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,115), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e19, (eval[19,]/sum(eval[19,]))*100 + 7, paste(round((eval[19,]/sum(eval[19,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 

# ----------------------------------------------

# eval
for (i in 1:19) {
  n.obs[i,3] <- sum(eval[i,])
}

# obs data
write.table(n.obs,"Values/nobs.txt",sep=",",row.names=FALSE,col.names =FALSE)

evalpct <- evalpct[,-1]

# eval data
evalpct <- round(evalpct*100,digits = 0)
write.table(evalpct,"Values/eval.txt",sep=",",row.names=FALSE,col.names =FALSE)


