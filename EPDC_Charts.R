
setwd("/Users/scottmiller/Desktop/ImpactTrip_Summer_18/Rplots")


##########################
##      Dem Charts      ##
##########################

### Run ImpactTrip_18_S prior to this section

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
rownames(avg1) <- c("Strategies","Engagement","Collaborate","Lesson Plans","Role","Theories")

png("avg_growth.png", width = 700) 
op <- par(mar = c(4,11,4,4) + 1)
avg_growth <- barplot(avg1[,3], names.arg = rownames(avg1), las=1, cex.names= 1.5, xlim = c(0,1.6),
                      font.lab=1, font.main = 1, font.axis = 1, horiz = T, space = 1,
                      col= c("royalblue3") , border="white", main = "Avg. Growth", cex.main=2) 
text(y = avg_growth, avg1[,3] + .1, paste(avg1[,3]), cex=1.5) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth

pos <- c(rep(0,6))

for (i in 1:6) { 
  pos[i] <- round(1 - (sum(diff[i,1:5]) / sum(diff[i,])), digits = 2)*100
}

png("pos_growth.png", width = 700) 
op <- par(mar = c(4,4,6,1) + 1)
pos_growth <- barplot(pos, names.arg = rownames(avg1), las=1, cex.names= 1.1, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 1, space = 1,
                      col= c("royalblue3") , border="white", main = "% with Positive Growth", cex.main=2) 
text(pos_growth, pos + 5, paste(pos,"%",sep=""), cex=1.9) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth adjusted

v.h <- c(rep(0,12))
for (i in seq(1,11, by = 2)) {
  v.h[i] <- round(BA[i,5]  / sum(BA[i,]), digits = 2)*100
}
v.h <- v.h[-seq(2,12, by = 2)]

adj.tot <- c(rep(0,6))
for (i in 1:6) {
  adj.tot[i] <- pos[i] + v.h[i] 
}

adj <- rbind(pos, v.h)
colnames(adj) <- c("Discipline","Student-Centered","Manage","Collaborate","Role","Theories")

png("adj_growth.png", width = 700) 
op <- par(mar = c(4,4,6,1) + 1)
pos_growth <- barplot(adj, names.arg = colnames(adj), las=1, cex.names= 1.1, ylim = c(0,110),
                      font.lab=1, font.main = 1, font.axis = 1, las = 1, space = 1,
                      col= c("royalblue3", "firebrick2") , border="white", main = "Adj. % with Positive Growth", cex.main=2) 
text(pos_growth, adj.tot + 6, paste(adj.tot,"%",sep=""), cex=1.9) 
par(op) ## reset
dev.off() 


# BA Charts

d <- diff[,4:9]
colnames(d) <- c("-1 Unit","0 Units","1 Unit","2 Units","3 Units","4 Units")

# 1

B_A1 <- BA[1:2,]
colnames(B_A1) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A1) <- c("Before","After")

png("b1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b1 <- barplot(B_A1, col= c("royalblue3","firebrick2"), border="white", 
              main = "Strategies to Reach Students", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
#text(b1, B_A1 + 5, paste(B_A1,"%",sep="") ,cex=1.9) 
legend("top", legend = rownames(B_A1), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c1.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c1 <- barplot(d[1,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Strategies to Reach Students", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c1, d[1,] + 10, paste(d[1,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[1,])

# 2

B_A2 <- BA[3:4,]
colnames(B_A2) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A2) <- c("Before","After")

png("b2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b2 <- barplot(B_A2, col= c("royalblue3","firebrick2"), border="white", 
              main = "Student-Engagement", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A2), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c2.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c2 <- barplot(d[2,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Student-Engagement", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c2, d[2,] + 10, paste(d[2,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[2,])

# 3

B_A3 <- BA[5:6,]
colnames(B_A3) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A3) <- c("Before","After")

png("b3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b3 <- barplot(B_A3, col= c("royalblue3","firebrick2"), border="white", 
              main = "Willingness to Collaborate", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A3), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c3.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c3 <- barplot(d[3,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Willingness to Collaborate", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c3, d[3,] + 10, paste(d[3,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[3,])

# 4

B_A4 <- BA[7:8,]
colnames(B_A4) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A4) <- c("Before","After")

png("b4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b4 <- barplot(B_A4, col= c("royalblue3","firebrick2"), border="white", 
              main = "Lesson Plans", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A4), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c4 <- barplot(d[4,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Lesson Plans", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c4, d[4,] + 10, paste(d[4,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[4,])

# 5

B_A5 <- BA[9:10,]
colnames(B_A5) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A5) <- c("Before","After")

png("b5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b5 <- barplot(B_A5, col= c("royalblue3","firebrick2"), border="white", 
              main = "Role", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A5), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c5 <- barplot(d[5,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Role", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c5, d[5,] + 10, paste(d[5,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[5,])

#6

B_A6 <- BA[11:12,]
colnames(B_A6) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A6) <- c("Before","After")

png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot(B_A6, col= c("royalblue3","firebrick2"), border="white", 
              main = "Learning Theories", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A6), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot(d[6,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Learning Theories", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,200), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, d[6,] + 10, paste(d[6,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff[6,])


# Eval Charts

colnames(eval) <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

# 1

png("e1.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e1 <- barplot(eval[1,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,210), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e1, eval[1,] + 10, paste(eval[1,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[1,])

# 2

png("e2.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e2 <- barplot(eval[2,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,210), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e2, eval[2,] + 10, paste(eval[2,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[2,])

# 3

png("e3.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e3 <- barplot(eval[3,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,210), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e3, eval[3,] + 10, paste(eval[3,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[3,])

# 4

png("e4.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e4 <- barplot(eval[4,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,260), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e4, eval[4,] + 10, paste(eval[4,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[4,])

# 5

png("e5.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e5 <- barplot(eval[5,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,210), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e5, eval[5,] + 10, paste(eval[5,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[5,])

# 6

png("e6.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e6 <- barplot(eval[6,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,220), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e6, eval[6,] + 10, paste(eval[6,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[6,])

# 7

png("e7.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e7 <- barplot(eval[7,], names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,220), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e7, eval[7,] + 10, paste(eval[7,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(eval[7,])
