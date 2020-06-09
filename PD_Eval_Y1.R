
################################################################
##  Analyzes Year 1 PD eval data and generates corresponding  ##
##  charts for P4H Global PD evaluation reports.              ##
################################################################


setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_20/Year 1/MENFP/Rplots")
dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_20/Year 1/Data/PD_20_Y1 Evals Full.csv")
dta <- filter(dta, dta$School == "MENFP") # change for each school

# Call element count function
source("/Users/scottmiller/GitHub/P4H/elem_count.R")


## Part 1

## before / after table
BA.data <- dta[,3:14] 
BA1 <- elem_count(quest = 12, choices = 5, data = BA.data, percent = FALSE)


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
p2.data <- dta[,15:18]
eval <- elem_count(quest = 4, choices = 5, data = p2.data, percent = FALSE)
evalpct <- elem_count(quest = 4, choices = 5, data = p2.data, percent = TRUE)



#-----------------------------------------
## Part 3
p3.data <- dta[,19:26] 
dem <- elem_count(quest = 8, choices = 6, data = p3.data, percent = FALSE)


# Experience
mean(dta$X14, na.rm = T)

# # of students
mean(dta$X15, na.rm = T)

# Students per class
mean(dta$X16, na.rm = T)





##########################
##      Eval Charts     ##
##########################

# Summary

# avg. growth
avg <- round(avg1, digits = 2)
rownames(avg) <- c("Discipline","Student-Centered","Manage","Collaborate","Role","Theories")

png("avg_growth.png", width = 700) 
op <- par(mar = c(4,11,4,4) + 1)
avg_growth <- barplot(avg[,3], names.arg = rownames(avg), las=1, cex.names= 1.5, xlim = c(0,2.5),
                      font.lab=1, font.main = 1, font.axis = 1, horiz = T, space = 1,
                      col= c("royalblue3") , border="white", main = "Avg. Growth", cex.main=2) 
text(y = avg_growth, avg[,3] + .12, paste(avg[,3]), cex=1.5) 
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
colnames(adj) <- c("Discipline","Student-Centered","Manage","Collaborate","Role","Theories")

png("adj_growth.png", width = 700) 
op <- par(mar = c(4,4,6,1) + 1)
pos_growth <- barplot(adj, names.arg = colnames(adj), las=1, cex.names= .85, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 1, space = 1,
                      col= c("royalblue3", "firebrick2") , border="white", main = "Adj. % with Positive Growth", cex.main=2) 
text(pos_growth, adj.tot + 6, paste(adj.tot,"%",sep=""), cex=1.5) 
par(op) ## reset
dev.off() 


# BA Charts

# convert to percentages
for (i in 1:12) {
  BA1[i,] <- (BA1[i,]/sum(BA1[i,]))*100
}
colnames(BA1) <- c("Very Low","Low","Medium","High","Very High")
rownames(BA1) <- rep(c("Before","After"),6)

d <- matrix(0,6,6)
for (i in 1:6) {
  d[i,] <- (diff[i,4:9]/sum(diff[i,4:9]))*100
}
colnames(d) <- c("-1 Unit","0 Units","1 Unit","2 Units","3 Units","4 Units")


# 1
png("b1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b1 <- barplot(BA1[1:2,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Disciplinary Interventions", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,90), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
#text(b1, B_A1 + 5, paste(B_A1,"%",sep="") ,cex=1.9) 
legend("top", legend = rownames(BA1[1:2,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c1.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c1 <- barplot(d[1,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Disciplinary Interventions", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c1, d[1,] + 5, paste(round(d[1,],digits = 0),"%",sep="") ,cex=1.9) 
#legend("top", legend = rownames(d1), 
#       fill = c("royalblue3","firebrick2"),
#       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 2
png("b2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b2 <- barplot(BA1[3:4,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Student-Centered Classrooms", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,90), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA1[3:4,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c2.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c2 <- barplot(d[2,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Student-Centered Classrooms", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c2, d[2,] + 5, paste(round(d[2,],digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3
png("b3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b3 <- barplot(BA1[5:6,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Capacity to Manage Classroom", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,90), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA1[5:6,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c3.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c3 <- barplot(d[3,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Capacity to Manage Classroom", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c3, d[3,] + 5, paste(round(d[3,],digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4
png("b4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b4 <- barplot(BA1[7:8,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Collaborative Learning Strategies", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,90), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA1[7:8,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c4 <- barplot(d[4,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Collaborative Learning Strategies", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c4, d[4,] + 5, paste(round(d[4,],digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 5
png("b5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b5 <- barplot(BA1[9:10,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Role", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,90), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA1[9:10,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c5 <- barplot(d[5,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Role", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c5, d[5,] + 5, paste(round(d[5,],digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


#6
png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot(BA1[11:12,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Learning Theories", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,90), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA1[11:12,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot(d[6,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Learning Theories", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, d[6,] + 5, paste(round(d[6,],digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 



# Eval Charts

colnames(eval) <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

# 1

png("e1.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e1 <- barplot((eval[1,]/sum(eval[1,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e1, (eval[1,]/sum(eval[1,]))*100 + 7, paste(round((eval[1,]/sum(eval[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2

png("e2.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e2 <- barplot((eval[2,]/sum(eval[2,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e2, (eval[2,]/sum(eval[2,]))*100 + 7, paste(round((eval[2,]/sum(eval[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3

png("e3.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e3 <- barplot((eval[3,]/sum(eval[3,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e3, (eval[3,]/sum(eval[3,]))*100 + 7, paste(round((eval[3,]/sum(eval[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4

png("e4.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e4 <- barplot((eval[4,]/sum(eval[4,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e4, (eval[4,]/sum(eval[4,]))*100 + 7, paste(round((eval[4,]/sum(eval[4,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


## demographics

# Gender
gender <- round(c(dem[1,2:3]) / sum(dem[1,2:3]), digits = 2)*100 

png("dem1.png", width = 700) 
pie(gender, labels = paste(gender,"%",sep=""), col= c("royalblue3","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Male","Female"), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 


# Age
age <- round(c(dem[2,2:7]) / sum(dem[2,2:7]), digits = 2)*100 

png("dem2.png", width = 700) 
pie(age, labels = paste(c("",age[2:4],""),c("",rep("%",3),""),sep=""), 
    col= c("darkorchid4","grey","firebrick2","royalblue3","gray48","green4"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("< 18","18-24","25-34","35-44","45-54","55-64"), 
       fill = c("darkorchid4","grey","firebrick2","royalblue3","gray48","green4"),
       horiz = T, cex = 1.2, yjust = 1)
dev.off() 


# Education
edu <- round(c(dem[3,2:5]) / sum(dem[3,2:5]), digits = 2)*100 

png("dem3.png", width = 700) 
pie(edu, labels = paste(edu,"%",sep=""), 
    col= c("grey","royalblue3","gray48","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Primary","Secondary","Some University","University"), 
       fill = c("grey","royalblue3","gray48","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

# Prior Training
train <- round(c(dem[7,2:3]) / sum(dem[7,2:3]), digits = 2)*100 

png("dem6.png", width = 700) 
pie(train, labels = paste(train,"%",sep=""), col= c("royalblue3","firebrick2"), 
    cex = 2, radius = .7, clockwise = T, main = "Prior Training")
legend("top", legend = c("Yes","No"), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 


# P4H Training
p4h <- round(c(dem[8,2:3]) / sum(dem[8,2:3]), digits = 2)*100 

png("dem7.png", width = 700) 
pie(p4h, labels = paste(p4h,"%",sep=""), col= c("royalblue3","firebrick2"), 
    cex = 2, radius = .7, clockwise = T, main = "P4H Training")
legend("top", legend = c("Yes","No"), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
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
avg <- round(avg1[,3],digits = 2)
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


