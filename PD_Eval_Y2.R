
################################################################
##  Analyzes Year 1 PD eval data and generates corresponding  ##
##  charts for P4H Global PD evaluation reports.              ##
################################################################


#library(tidyverse)

setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_20/Year 2/")
dta <- read.csv("Data/PD_20_Y2 Evals Full.csv")
dta <- filter(dta, dta$School == "JOSEPH SCHOOL") # change for each school


## Part 1

## before / after table
BA <- matrix(0,12,5)
BA.questions <- c(3:14) 

for (i in BA.questions) {
  for (j in 1:5) {
    BA[i-2,j] <- length(which(dta[,i]==j))
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
avg <- matrix(0,12,3)
B.questions <- seq(3,13,by=2)

#before
for (i in B.questions) {
  avg[i-2,1] <- mean(dta[,i],na.rm = T) 
}
#after
for (i in A.questions) {
  avg[i-3,2] <- mean(dta[,i],na.rm = T) 
}
#difference
for (i in A.questions) {
  avg[i-3,3] <- mean(dta[,i]-dta[,i-1],na.rm = T) 
}
avg <- avg[-c(seq(2,12,by=2)),]


#-----------------------------------------
## Part 2

#overall
eval <- matrix(0,6,5)
p2.questions <- c(15:20)

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

dem <- matrix(0,9,7)  
p3.questions <- c(22:30)

for (i in p3.questions) {
  for (j in 0:6) {
    dem[i-21,j+1] <- length(which(dta[,i]==j))
  }
}


# Experience
mean(dta$X14, na.rm = T)

# # of students
mean(dta$X15, na.rm = T)

# Students per class
mean(dta$X16, na.rm = T)





##########################
##      Eval Charts     ##
##########################

## Run PD_Eval_19 prior to this section

setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_20/Year 2/JS/Rplots")

# Summary

# avg. growth
avg <- round(avg, digits = 2)
rownames(avg) <- c("Evaluation Methods","Evaluation Use","Lesson Plans","Objective Writing","Self-Reflection","Critical Thinking")

png("avg_growth.png", width = 700) 
op <- par(mar = c(4,11,4,4) + 1)
avg_growth <- barplot(avg[,3], names.arg = rownames(avg), las=1, cex.names= 1.5, xlim = c(0,2.5),
                      font.lab=1, font.main = 1, font.axis = 1, horiz = T, space = 1,
                      col= c("royalblue3") , border="white", main = "Avg. Growth", cex.main=2) 
text(y = avg_growth, avg[,3] + .2, paste(avg[,3]), cex=1.5) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth

pos <- c(rep(0,6))

for (i in 1:6) { 
  pos[i] <- round(1 - ((diff[i,3] + diff[i,4] + diff[i,5]) / sum(diff[i,])), digits = 2)*100
}

png("pos_growth.png", width = 700) 
op <- par(mar = c(4,4,6,1) + 1)
pos_growth <- barplot(pos, names.arg = rownames(avg), las=1, cex.names= .85, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 1, space = 1,
                      col= c("royalblue3") , border="white", main = "% with Positive Growth", cex.main=2) 
text(pos_growth, pos + 6, paste(pos,"%",sep=""), cex=1.5) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth adjusted

v.h <- c(rep(0,12))
for (i in seq(1,11, by = 2)) {
  v.h[i] <- round(BA1[i,5] / sum(BA1[i,]), digits = 2)*100
}
v.h <- v.h[-seq(2,12, by = 2)]

adj.tot <- c(rep(0,6))
for (i in 1:6) {
  if (pos[i] == 100) {
    v.h[i] <- 0
  }
  adj.tot[i] <- pos[i] + v.h[i] 
}

adj <- rbind(pos, v.h)
colnames(adj) <- c("Evaluation Methods","Evaluation Use","Lesson Plans","Objective Writing","Self-Reflection","Critical Thinking")

png("adj_growth.png", width = 700) 
op <- par(mar = c(4,4,6,1) + 1)
pos_growth <- barplot(adj, names.arg = colnames(adj), las=1, cex.names= .85, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 1, space = 1,
                      col= c("royalblue3", "firebrick2") , border="white", main = "Adj. % with Positive Growth", cex.main=2) 
text(pos_growth, adj.tot + 6, paste(adj.tot,"%",sep=""), cex=1.5) 
par(op) ## reset
dev.off() 


# BA Charts

d <- matrix(0,6,6)
for (i in 1:6) {
  d[i,] <- (diff[i,4:9]/sum(diff[i,4:9]))*100
}
colnames(d) <- c("-1 Unit","0 Units","1 Unit","2 Units","3 Units","4 Units")

# 1

B_A1 <- BA1[1:2,]
colnames(B_A1) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A1) <- c("Before","After")

png("b1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b1 <- barplot((B_A1/sum(B_A1))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Evaluation Methods", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,50), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
#text(b1, B_A1 + 5, paste(B_A1,"%",sep="") ,cex=1.9) 
legend("top", legend = rownames(B_A1), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c1.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c1 <- barplot((d[1,]/sum(d[1,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Evaluation Methods", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,60), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c1, (d[1,]/sum(d[1,]))*100 + 4, paste(round((d[1,]/sum(d[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
#legend("top", legend = rownames(d1), 
#       fill = c("royalblue3","firebrick2"),
#       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

sum(diff[1,])

# 2

B_A2 <- BA1[3:4,]
colnames(B_A2) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A2) <- c("Before","After")

png("b2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b2 <- barplot((B_A2/sum(B_A2))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Evaluation Use", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,40), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A2), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c2.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c2 <- barplot((d[2,]/sum(d[2,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Evaluation Use", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,70), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c2, (d[2,]/sum(d[2,]))*100 + 4, paste(round((d[2,]/sum(d[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[2,])

# 3

B_A3 <- BA1[5:6,]
colnames(B_A3) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A3) <- c("Before","After")

png("b3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b3 <- barplot((B_A3/sum(B_A3))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Lesson Plans", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,50), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A3), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c3.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c3 <- barplot((d[3,]/sum(d[3,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Lesson Plans", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c3, (d[3,]/sum(d[3,]))*100 + 4, paste(round((d[3,]/sum(d[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[3,])

# 4

B_A4 <- BA1[7:8,]
colnames(B_A4) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A4) <- c("Before","After")

png("b4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b4 <- barplot((B_A4/sum(B_A4))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Objective Writing Ability", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,50), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A4), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c4 <- barplot((d[4,]/sum(d[4,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Objective Writing Ability", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,60), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c4, (d[4,]/sum(d[4,]))*100 + 4, paste(round((d[4,]/sum(d[4,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[4,])

# 5

B_A5 <- BA1[9:10,]
colnames(B_A5) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A5) <- c("Before","After")

png("b5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b5 <- barplot((B_A5/sum(B_A5))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Understanding of Self-Reflection", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,50), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A5), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c5 <- barplot((d[5,]/sum(d[5,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Understanding of Self-Reflection", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,70), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c5, (d[5,]/sum(d[5,]))*100 + 4, paste(round((d[5,]/sum(d[5,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[5,])

#6

B_A6 <- BA1[11:12,]
colnames(B_A6) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A6) <- c("Before","After")

png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot((B_A6/sum(B_A6))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Critical Thinking", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,40), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A6), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot((d[6,]/sum(d[6,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Critical Thinking", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, (d[6,]/sum(d[6,]))*100 + 4, paste(round((d[6,]/sum(d[6,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[6,])


# Eval Charts

colnames(eval) <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

# 1

png("e1.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e1 <- barplot((eval[1,]/sum(eval[1,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e1, (eval[1,]/sum(eval[1,]))*100 + 5, paste(round((eval[1,]/sum(eval[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2

png("e2.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e2 <- barplot((eval[2,]/sum(eval[2,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e2, (eval[2,]/sum(eval[2,]))*100 + 5, paste(round((eval[2,]/sum(eval[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3

png("e3.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e3 <- barplot((eval[3,]/sum(eval[3,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e3, (eval[3,]/sum(eval[3,]))*100 + 5, paste(round((eval[3,]/sum(eval[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4

png("e4.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e4 <- barplot((eval[4,]/sum(eval[4,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e4, (eval[4,]/sum(eval[4,]))*100 + 5, paste(round((eval[4,]/sum(eval[4,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 5

png("e5.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e5 <- barplot((eval[5,]/sum(eval[5,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e5, (eval[5,]/sum(eval[5,]))*100 + 5, paste(round((eval[5,]/sum(eval[5,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 6

png("e6.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e6 <- barplot((eval[6,]/sum(eval[6,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e6, (eval[6,]/sum(eval[6,]))*100 + 5, paste(round((eval[6,]/sum(eval[6,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 




# ----------------------------------------------
# BA 
for (i in 1:6) {
  n.obs[i,2] <- sum(diff[i,])
}
# eval
for (i in 1:4) {
  n.obs[i,3] <- sum(eval[i,])
}
# dem
for (i in c(1:8)) {
  n.obs[i,4] <- sum(dem[i,])
}

# obs data
write.table(n.obs,"Values/nobs.txt",sep=",",row.names=FALSE,col.names =FALSE)


# difference data
d <- round(d,digits = 0)
write.table(d,"Values/diff.txt",sep=",",row.names=FALSE,col.names =FALSE)

# eval data
avg <- round(avg[,3],digits = 2)
write.table(avg,"Values/avg.txt",sep=",",row.names=FALSE,col.names =FALSE)

# eval data
evalpct <- round(evalpct*100,digits = 0)
write.table(evalpct,"Values/eval.txt",sep=",",row.names=FALSE,col.names =FALSE)

# dem data
for (i in 1:8) {
  dem[i,] <- round((dem[i,]/sum(dem[i,]))*100,digits = 0)
}
dem[5,1:3] <- round(rbind(mean(dta$X14, na.rm = T),mean(dta$X15, na.rm = T),mean(dta$X16, na.rm = T)),digits=1)
write.table(dem,"Values/dem.txt",sep=",",row.names=FALSE,col.names =FALSE)



