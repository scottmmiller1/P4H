
setwd("/Users/scottmiller/Desktop/EPDC_19/Rplots")


### Run EPDC_19 prior to this section

# -------------------------------------------------------------------------

##########################
##       Dem Charts     ##
##########################

# Gender
gender <- round(c(dem[1,2:3]) / sum(dem[1,2:3]), digits = 2)*100 

png("dem1.png", width = 700) 
pie(gender, labels = paste(gender,"%",sep=""), col= c("royalblue3","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Male","Female"), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

sum(dem[1,])

# Age
age <- round(c(dem[2,2:7]) / sum(dem[2,2:7]), digits = 2)*100 

png("dem2.png", width = 700) 
pie(age, labels = paste(c("",age[2:5],""),c("",rep("%",4),""),sep=""), 
    col= c("darkorchid4","grey","firebrick2","royalblue3","gray48","green4"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("< 18","18-24","25-34","35-44","45-54","55-64"), 
       fill = c("darkorchid4","grey","firebrick2","royalblue3","gray48","green4"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

sum(dem[2,])

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

sum(dem[3,])

# School Type
schtype <- round(c(dem[4,2:5]) / sum(dem[4,2:5]), digits = 2)*100 

png("dem4.png", width = 700) 
pie(schtype, labels = paste(schtype,"%",sep=""), col= c("grey","royalblue3","gray48","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Public","Private","Both","Not a Teacher"), 
       fill = c("grey","royalblue3","gray48","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

sum(dem[4,])

# Level
level <- round(c(dem[8,2:5]) / sum(dem[8,2:5]), digits = 2)*100 

png("dem5.png", width = 700) 
pie(level, labels = paste(level,"%",sep=""), col= c("grey","royalblue3","gray48","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Primary","Secondary","University","Other"), 
       fill = c("grey","royalblue3","gray48","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

sum(dem[8,])

# Prior Training
train <- round(c(dem[9,2:3]) / sum(dem[9,2:3]), digits = 2)*100 

png("dem6.png", width = 700) 
pie(train, labels = paste(train,"%",sep=""), col= c("royalblue3","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Yes","No"), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

sum(dem[9,])

# P4H Training
p4h <- round(c(dem[10,2:3]) / sum(dem[10,2:3]), digits = 2)*100 

png("dem7.png", width = 700) 
pie(p4h, labels = paste(p4h,"%",sep=""), col= c("royalblue3","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Yes","No"), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.2, yjust = 2)
dev.off() 

sum(dem[10,])

# -------------------------------------------------------------------------


##########################
##       BA Charts      ##
##########################

# Summary

# avg. growth
avg1 <- round(avg, digits = 2)
rownames(avg1) <- c("Reaching Students","Engagement","Collaborate","Assessments","Role","Reading Strategies","Discipline")

png("avg_growth.png", width = 700) 
op <- par(mar = c(4,11,4,4) + 1)
avg_growth <- barplot(avg1[,3], names.arg = rownames(avg1), las=1, cex.names= 1.5, xlim = c(0,1.6),
                      font.lab=1, font.main = 1, font.axis = 1, horiz = T, space = 1,
                      col= c("royalblue3") , border="white", main = "Avg. Growth", cex.main=2) 
text(y = avg_growth, avg1[,3] + .1, paste(avg1[,3]), cex=1.5) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth

pos <- c(rep(0,7))

for (i in 1:7) { 
  pos[i] <- round(1 - (sum(diff[i,1:5]) / sum(diff[i,])), digits = 2)*100
}

png("pos_growth.png", width = 700) 
op <- par(mar = c(10,4,4,1) + 1)
pos_growth <- barplot(pos, names.arg = rownames(avg1), las=1, cex.names= 1.4, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 2, space = 1,
                      col= c("royalblue3") , border="white", main = "% with Positive Growth", cex.main=2) 
text(pos_growth, pos + 7, paste(pos,"%",sep=""), cex=1.9) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth adjusted

v.h <- c(rep(0,14))
for (i in seq(1,13, by = 2)) {
  v.h[i] <- round(BA[i,5]  / sum(BA[i,]), digits = 2)*100
}
v.h <- v.h[-seq(2,14, by = 2)]

adj.tot <- c(rep(0,7))
for (i in 1:7) {
  adj.tot[i] <- pos[i] + v.h[i] 
}

adj <- rbind(pos, v.h)
colnames(adj) <- c("Reaching Students","Engagement","Collaborate","Assessments","Role","Reading Strategies","Discipline")

png("adj_growth.png", width = 700) 
op <- par(mar = c(10,4,4,1) + 1)
pos_growth <- barplot(adj, names.arg = colnames(adj), las=1, cex.names= 1.3, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 2, space = 1,
                      col= c("royalblue3", "firebrick2") , border="white", main = "Adj. % with Positive Growth", cex.main=2) 
text(pos_growth, adj.tot + 7, paste(adj.tot,"%",sep=""), cex=1.9) 
par(op) ## reset
dev.off() 


# BA Charts

  # convert to percentages
  for (i in 1:14) {
    BA[i,] <- (BA[i,]/sum(BA[i,]))*100
  }
    colnames(BA) <- c("Very Low","Low","Medium","High","Very High")
    rownames(BA) <- rep(c("Before","After"),7)
  
  d <- matrix(0,7,6)
  for (i in 1:7) {
    d[i,] <- (diff[i,4:9]/sum(diff[i,4:9]))*100
  }
    colnames(d) <- c("-1 Unit","0 Units","1 Unit","2 Units","3 Units","4 Units")

    

# 1
png("b1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b1 <- barplot(BA[1:2,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Strategies to Reach Students", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
#text(b1, B_A1 + 5, paste(B_A1,"%",sep="") ,cex=1.9) 
legend("top", legend = rownames(BA[1:2,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c1.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c1 <- barplot(d[1,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Strategies to Reach Students", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c1, d[1,] + 5, paste(round((d[1,]/sum(d[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2
png("b2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b2 <- barplot(BA[3:4,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Student-Engagement", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA[3:4,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c2.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c2 <- barplot(d[2,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Student-Engagement", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c2, d[2,] + 5, paste(round((d[2,]/sum(d[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3
png("b3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b3 <- barplot(BA[5:6,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Desire to Collaborate", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA[5:6,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c3.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c3 <- barplot(d[3,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Desire to Collaborate", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c3, d[3,] + 5, paste(round((d[3,]/sum(d[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4
png("b4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b4 <- barplot(BA[7:8,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Effective Assessments", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA[7:8,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c4 <- barplot(d[4,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Effective Assessments", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c4, d[4,] + 5, paste(round((d[4,]/sum(d[4,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 5
png("b5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b5 <- barplot(BA[9:10,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Role", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA[9:10,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c5 <- barplot(d[5,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Role", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c5, d[5,] + 5, paste(round((d[5,]/sum(d[5,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 6
png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot(BA[11:12,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Reading Strategies", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA[11:12,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot(d[6,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Reading Strategies", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, d[6,] + 5, paste(round((d[6,]/sum(d[6,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 7
png("b7.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b7 <- barplot(BA[13:14,], col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Discipline", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(BA[13:14,]), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c7.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c7 <- barplot(d[7,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Discipline", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,80), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c7, d[7,] + 5, paste(round((d[7,]/sum(d[7,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 



# -------------------------------------------------------------------------


##########################
##      Eval Charts     ##
##########################

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


# 7
png("e7.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e7 <- barplot((eval[7,]/sum(eval[7,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e7, (eval[7,]/sum(eval[7,]))*100 + 5, paste(round((eval[7,]/sum(eval[7,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 8
png("e8.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e8 <- barplot((eval[8,]/sum(eval[8,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e8, (eval[8,]/sum(eval[8,]))*100 + 5, paste(round((eval[8,]/sum(eval[8,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 


# 9
png("e9.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e9 <- barplot((eval[9,]/sum(eval[9,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e9, (eval[9,]/sum(eval[9,]))*100 + 5, paste(round((eval[9,]/sum(eval[9,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 



# pre-load data

# Obs data

# BA 
n.obs <- matrix(0,9,3)
for (i in 1:7) {
  n.obs[i,1] <- sum(diff[i,])
}

# eval
for (i in 1:9) {
  n.obs[i,2] <- sum(eval[i,])
}

# dem
for (i in c(1:9)) {
  n.obs[i,3] <- sum(dem[i,])
}


# obs data
write.table(n.obs,"nobs.txt",sep=",",row.names=FALSE,col.names =FALSE)

# difference data
d <- round(d,digits = 0)
write.table(d,"diff.txt",sep=",",row.names=FALSE,col.names =FALSE)

# eval data
avg <- round(avg[,3],digits = 2)
write.table(avg,"avg.txt",sep=",",row.names=FALSE,col.names =FALSE)


# eval data
evalpct <- round(evalpct*100,digits = 0)
write.table(evalpct,"eval.txt",sep=",",row.names=FALSE,col.names =FALSE)




