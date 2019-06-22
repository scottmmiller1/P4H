
setwd("/Users/scottmiller/Desktop/ImpactTrip_19_Nica/Rplots")


### Run ImpactTrip.R prior to this section

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


# College Status
college <- round(c(dem[2,2:7]) / sum(dem[2,2:7]), digits = 2)*100 

png("dem2.png", width = 700) 
pie(college, labels = paste(college,"%",sep=""), 
    col= c("darkorchid4","grey","firebrick2","royalblue3","gray48","green4"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Freshman","Sophomore","Junior","Senior","Gradute","Faculty"),
       fill = c("darkorchid4","grey","firebrick2","royalblue3","gray48","green4"),
       horiz = T, cex = 1,yjust = 1)
dev.off() 


# Race
race <- round(c(dem[4,3:6]) / sum(dem[4,3:6]), digits = 2)*100 

png("dem3.png", width = 700) 
pie(race, labels = paste(race,"%",sep=""), 
    col= c("grey","royalblue3","gray48","firebrick2"), 
    cex = 2, radius = .7, clockwise = T)
legend("top", legend = c("Asian","African American / Black","White","Other"), 
       fill = c("grey","royalblue3","gray48","firebrick2"),
       horiz = T, cex = .9)
dev.off() 


# Travel
travel <- round(c(dem[7,1:7]) / sum(dem[7,1:7]), digits = 2)*100 

png("dem4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
dem4 <- barplot(travel, names.arg = c("0","1","2","3","4","5","6"), las=1, col= c("royalblue3"), border="white", 
              main = "International Travel", cex.main = 2,
              font.main = 1, font.axis=1, space = 1,
              ylim = c(0,72), font.lab=1, cex.axis = 1.5, cex.names = 1.5)
text(dem4, travel + 4, paste(travel,"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# P4H Travel
p4h <- round(c(dem[8,1:5]) / sum(dem[8,1:5]), digits = 2)*100 

png("dem5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
dem5 <- barplot(p4h, names.arg = c("0","1","2","3","4"), las=1, col= c("royalblue3"), border="white", 
                main = "P4H Travel", cex.main = 2,
                font.main = 1, font.axis=1, space = 1,
                ylim = c(0,60), font.lab=1, cex.axis = 1.5, cex.names = 1.5)
text(dem5, p4h + 5, paste(p4h,"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

# -------------------------------------------------------------------------


##########################
##       BA Charts      ##
##########################

# Summary

# avg. growth
avg1 <- round(avg1, digits = 2)
rownames(avg1) <- c("Culture","Aid","Poverty","Operate","Critical Thinking","Career","Perform","Team Connection","Local Connection")

png("avg_growth.png", width = 700) 
op <- par(mar = c(4,11,4,4) + 1)
avg_growth <- barplot(avg1[,3], names.arg = rownames(avg1), las=1, cex.names= 1.5, xlim = c(0,2.7),
                      font.lab=1, font.main = 1, font.axis = 1, horiz = T, space = 1,
                      col= c("royalblue3") , border="white", main = "Avg. Growth", cex.main=2) 
text(y = avg_growth, avg1[,3] + .13, paste(avg1[,3]), cex=1.5) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth

pos <- c(rep(0,6))

for (i in 1:9) { 
  pos[i] <- round(1 - (sum(diff1[i,1:5]) / sum(diff1[i,])), digits = 2)*100
}

png("pos_growth.png", width = 700) 
op <- par(mar = c(10,4,2,1) + 1)
pos_growth <- barplot(pos, names.arg = rownames(avg1), las=2, cex.names= 1.4, ylim = c(0,115),
                      font.lab=1, font.main = 1, font.axis = 1, space = 1,
                      col= c("royalblue3") , border="white", main = "% with Positive Growth", cex.main=2) 
text(pos_growth, pos + 8, paste(pos,"%",sep=""), cex=1.9) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth adjusted

v.h <- c(rep(0,18))
for (i in seq(1,17, by = 2)) {
  v.h[i] <- round(BA1[i,5]  / sum(BA1[i,]), digits = 2)*100
}
v.h <- v.h[-seq(2,18, by = 2)]

adj.tot <- c(rep(0,9))
for (i in 1:9) {
  adj.tot[i] <- pos[i] + v.h[i] 
}

adj <- rbind(pos, v.h)
colnames(adj) <- c("Culture","Aid","Poverty","Operate","Critical Thinking","Career","Perform","Team Connection","Local Connection")

png("adj_growth.png", width = 700) 
op <- par(mar = c(10,4,2,1) + 1)
pos_growth <- barplot(adj, names.arg = colnames(adj), las=2, cex.names= 1.4, ylim = c(0,115),
                      font.lab=1, font.main = 1, font.axis = 1, space = 1,
                      col= c("royalblue3", "firebrick2") , border="white", main = "Adj. % with Positive Growth", cex.main=2) 
text(pos_growth, adj.tot + 8, paste(adj.tot,"%",sep=""), cex=1.9) 
par(op) ## reset
dev.off() 


# ---------------------------------------------------
# BA Charts

d <- diff1[,4:9]
colnames(d) <- c("-1 Unit","0 Units","1 Unit","2 Units","3 Units","4 Units")

# 1

B_A1 <- BA1[1:2,]
colnames(B_A1) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A1) <- c("Before","After")

png("b1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b1 <- barplot(B_A1, col= c("royalblue3","firebrick2"), border="white", 
              main = "Haitian History & Culture", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A1), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c1.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c1 <- barplot(d[1,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Haitian History & Culture", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c1, d[1,] + 2, paste(d[1,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2

B_A2 <- BA1[3:4,]
colnames(B_A2) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A2) <- c("Before","After")

png("b2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b2 <- barplot(B_A2, col= c("royalblue3","firebrick2"), border="white", 
              main = "Effective Aid Practices", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A2), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c2.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c2 <- barplot(d[2,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Effective Aid Practices", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c2, d[2,] + 2, paste(d[2,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3

B_A3 <- BA1[5:6,]
colnames(B_A3) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A3) <- c("Before","After")

png("b3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b3 <- barplot(B_A3, col= c("royalblue3","firebrick2"), border="white", 
              main = "Understanding of Poverty", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A3), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c3.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c3 <- barplot(d[3,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Understanding of Poverty", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c3, d[3,] + 2, paste(d[3,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4

B_A4 <- BA1[7:8,]
colnames(B_A4) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A4) <- c("Before","After")

png("b4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b4 <- barplot(B_A4, col= c("royalblue3","firebrick2"), border="white", 
              main = "Ability to Operate", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A4), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c4 <- barplot(d[4,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Ability to Operate", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c4, d[4,] + 2, paste(d[4,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 5

B_A5 <- BA1[9:10,]
colnames(B_A5) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A5) <- c("Before","After")

png("b5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b5 <- barplot(B_A5, col= c("royalblue3","firebrick2"), border="white", 
              main = "Critical Thinking", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A5), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c5 <- barplot(d[5,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Critical Thinking", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c5, d[5,] + 2, paste(d[5,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


#6

B_A6 <- BA1[11:12,]
colnames(B_A6) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A6) <- c("Before","After")

png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot(B_A6, col= c("royalblue3","firebrick2"), border="white", 
              main = "Career Trajectory", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A6), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot(d[6,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Career Trajectory", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, d[6,] + 2, paste(d[6,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


#6

B_A6 <- BA1[11:12,]
colnames(B_A6) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A6) <- c("Before","After")

png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot(B_A6, col= c("royalblue3","firebrick2"), border="white", 
              main = "Career Trajectory", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A6), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot(d[6,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Career Trajectory", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, d[6,] + 2, paste(d[6,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


#7

B_A7 <- BA1[11:12,]
colnames(B_A7) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A7) <- c("Before","After")

png("b7.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b7 <- barplot(B_A7, col= c("royalblue3","firebrick2"), border="white", 
              main = "Ability to Perform", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A7), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c7.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c7 <- barplot(d[7,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Ability to Perform", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c7, d[7,] + 2, paste(d[7,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


#8

B_A8 <- BA1[11:12,]
colnames(B_A8) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A8) <- c("Before","After")

png("b8.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b8 <- barplot(B_A8, col= c("royalblue3","firebrick2"), border="white", 
              main = "Team Connection", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A8), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c8.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c8 <- barplot(d[8,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Team Connection", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c8, d[8,] + 2, paste(d[8,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


#9

B_A9 <- BA1[11:12,]
colnames(B_A9) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A9) <- c("Before","After")

png("b9.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b9 <- barplot(B_A9, col= c("royalblue3","firebrick2"), border="white", 
              main = "Haitian Connection", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A9), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c9.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c9 <- barplot(d[9,], names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Haitian Connection", cex.main = 2,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c9, d[9,] + 2, paste(d[9,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 



###################
##  Eval Charts  ##
###################

colnames(trip) <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

# 1

png("e1.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e1 <- barplot(trip[1,], names.arg = colnames(trip), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e1, trip[1,] + 1, paste(trip[1,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2

png("e2.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e2 <- barplot(trip[2,], names.arg = colnames(trip), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e2, trip[2,] + 1, paste(trip[2,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3

png("e3.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e3 <- barplot(trip[3,], names.arg = colnames(trip), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,20), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e3, trip[3,] + 1, paste(trip[3,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4

png("e4.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e4 <- barplot(trip[4,], names.arg = colnames(trip), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,30), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e4, trip[4,] + 2, paste(trip[4,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 5

png("e5.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e5 <- barplot(trip[5,], names.arg = colnames(trip), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,25), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e5, trip[5,] + 1, paste(trip[5,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 6

png("e6.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e6 <- barplot(trip[6,], names.arg = colnames(trip), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,25), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e6, trip[6,] + 2, paste(trip[6,],sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 




# --------------

# Number of Responses

# demographics
for (i in 1:5) {
  print(sum(dem[i,]))
}

# demographics
for (i in 1:9) {
  print(sum(diff1[i,]))
}

# demographics
for (i in 1:6) {
  print(sum(trip[i,]))
}




