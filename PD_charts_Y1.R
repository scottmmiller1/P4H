
setwd("/Users/scottmiller/Desktop/PD_19/CFC - G/Rplots")


##########################
##      Exam Charts     ##
##########################

### Run PD_18 prior to this section

# -------------------------------------------------------------------------
# Growth %

n.quest <- 16
growth <- t(c(rep(0,n.quest)))

# group questions by correct answer
true.A <- c(5,7,9)
false.B <- c(1:4,6,8)
C <- c(13,14)
D <- c(10:12,15)
E <- c(16)

# true / A
for (i in true.A) {
  growth[i] <- round(post_pct[i,2] - pre_pct[i,2], digits = 2)*100
}
# false / B
for (i in false.B) {
  growth[i] <- round(post_pct[i,3] - pre_pct[i,3], digits = 2)*100
}
# C
for (i in C) {
  growth[i] <- round(post_pct[i,4] - pre_pct[i,4], digits = 2)*100
}
# D
for (i in D) {
  growth[i] <- round(post_pct[i,5] - pre_pct[i,5], digits = 2)*100
}
# E
for (i in E) {
  growth[i] <- round(post_pct[i,6] - pre_pct[i,6], digits = 2)*100
}


colnames(growth) <- c("Knowledge Source", "Physical Punishment", "Student Leadership", "Teacher-Centered", 
              "Classroom Activities", "Classroom Management", "Rules & Procedures", "Behaviorism",
              "Cognitivism", "Constructivism", "Student-Centered", "Classroom Management (2)", 
              "Student-teacher Relationship", "Collaborative Learning", "Group Work", "Classroom Strategies")


png("growth_pct.png", width = 1000, height = 600) 
op <- par(mar = c(13,4,2,1) + 1)
growth.pct <- barplot(growth, names.arg = colnames(growth), las=2, cex.names=1.2, ylim = c(0,110), 
                      font.lab=1, font.main = 1, font.axis = 1,
                      col= c("royalblue3") , border="white", main = "Growth %", cex.main=2) 
text(growth.pct, growth + 5 , paste(growth,"%",sep="") ,cex=1.3) 
par(op) ## reset
dev.off() 

# -------------------------------------------------------------------------
# 

for (i in 1:16) {
  assign(paste("pre_post", i, sep = ""), rbind(round(pre_pct[i,], digits = 2)*100, round(post_pct[i,], digits = 2)*100)) 
}


# True / False

# 1
pre_post1 <- t(pre_post1[,2:3])
rownames(pre_post1) <- c("True","False")
colnames(pre_post1) <- c("Pre","Post")

png("pp1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp1 <- barplot(pre_post1, col= c("grey","royalblue3"), border="white",
        main = "", font.axis=1, beside=T,
        ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp1, pre_post1 + 5, paste(pre_post1,"%",sep="") ,cex=2) 
legend("top",
       legend = rownames(pre_post1), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 2
pre_post2 <- t(pre_post2[,2:3])
rownames(pre_post2) <- c("True","False")
colnames(pre_post2) <- c("Pre","Post")

png("pp2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp2 <- barplot(pre_post2, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,120), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp2, pre_post2 + 5, paste(pre_post2,"%",sep="") ,cex=2) 
legend("top",
       legend = rownames(pre_post2), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 3
pre_post3 <- t(pre_post3[,2:3])
rownames(pre_post3) <- c("True","False")
colnames(pre_post3) <- c("Pre","Post")

png("pp3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp3 <- barplot(pre_post3, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,120), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp3, pre_post3 + 5, paste(pre_post3,"%",sep="") ,cex=2) 
legend("top",
       legend = rownames(pre_post3), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 4
pre_post4 <- t(pre_post4[,2:3])
rownames(pre_post4) <- c("True","False")
colnames(pre_post4) <- c("Pre","Post")

png("pp4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp4 <- barplot(pre_post4, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,120), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp4, pre_post4 + 5, paste(pre_post4,"%",sep="") ,cex=2) 
legend("top",
       legend = rownames(pre_post4), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 5
pre_post5 <- t(pre_post5[,2:3])
rownames(pre_post5) <- c("True","False")
colnames(pre_post5) <- c("Pre","Post")

png("pp5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp5 <- barplot(pre_post5, col= c("royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,120), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp5, pre_post5 + 5, paste(pre_post5,"%",sep="") ,cex=2) 
legend("top",
       legend = rownames(pre_post5), 
       fill = c("royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 6
pre_post6 <- t(pre_post6[,2:3])
rownames(pre_post6) <- c("True","False")
colnames(pre_post6) <- c("Pre","Post")

png("pp6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp6 <- barplot(pre_post6, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,120), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp6, pre_post6 + 5, paste(pre_post6,"%",sep="") ,cex=2) 
legend("top",
       legend = rownames(pre_post6), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# -------------------------------------------------------------------------
# 

# Multiple Choice

# 8
pre_post8 <- t(pre_post8[,2:5])
rownames(pre_post8) <- c("A","B","C","D")
colnames(pre_post8) <- c("Pre","Post")

png("pp8.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp8 <- barplot(pre_post8, col= c("dimgrey","royalblue3","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,152), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp8, pre_post8 + 10, paste(pre_post8,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post8), 
       fill = c("dimgrey","royalblue3","darkgrey","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 9
pre_post9 <- t(pre_post9[,2:5])
rownames(pre_post9) <- c("A","B","C","D")
colnames(pre_post9) <- c("Pre","Post")

png("pp9.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp9 <- barplot(pre_post9, col= c("royalblue3","dimgrey","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,152), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp9, pre_post9 + 10, paste(pre_post9,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post9), 
       fill = c("royalblue3","dimgrey","darkgrey","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 10

pre_post10 <- t(pre_post10[,2:5])
rownames(pre_post10) <- c("A","B","C","D")
colnames(pre_post10) <- c("Pre","Post")

png("pp10.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp10 <- barplot(pre_post10, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp10, pre_post10 + 7, paste(pre_post10,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post10), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 11

pre_post11 <- t(pre_post11[,2:5])
rownames(pre_post11) <- c("A","B","C","D")
colnames(pre_post11) <- c("Pre","Post")

png("pp11.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp11 <- barplot(pre_post11, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp11, pre_post11 + 7, paste(pre_post11,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post11), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 12

pre_post12 <- t(pre_post12[,2:5])
rownames(pre_post12) <- c("A","B","C","D")
colnames(pre_post12) <- c("Pre","Post")

png("pp12.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp12 <- barplot(pre_post12, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp12, pre_post12 + 7, paste(pre_post12,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post12), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 13

pre_post13 <- t(pre_post13[,2:5])
rownames(pre_post13) <- c("A","B","C","D")
colnames(pre_post13) <- c("Pre","Post")

png("pp13.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp13 <- barplot(pre_post13, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp13, pre_post13 + 7, paste(pre_post13,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post13), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 14

pre_post14 <- t(pre_post14[,2:5])
rownames(pre_post14) <- c("A","B","C","D")
colnames(pre_post14) <- c("Pre","Post")

png("pp14.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp14 <- barplot(pre_post14, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp14, pre_post14 + 7, paste(pre_post14,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post14), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 15

pre_post15 <- t(pre_post15[,2:5])
rownames(pre_post15) <- c("A","B","C","D")
colnames(pre_post15) <- c("Pre","Post")

png("pp15.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp15 <- barplot(pre_post15, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp15, pre_post15 + 7, paste(pre_post15,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post15), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 16

pre_post16 <- t(pre_post16[,2:6])
rownames(pre_post16) <- c("A","B","C","D","E")
colnames(pre_post16) <- c("Pre","Post")

png("pp16.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp16 <- barplot(pre_post16, col= c("dimgrey","darkgrey","grey","lightgrey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp16, pre_post16 + 7, paste(pre_post16,"%",sep="") ,cex=1.9) 
legend("top",
       legend = rownames(pre_post16), 
       fill = c("dimgrey","darkgrey","grey","lightgrey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

# -------------------------------------------------------------------------


##########################
##      Eval Charts     ##
##########################

## Run PD_Eval_19 prior to this section

setwd("/Users/scottmiller/Desktop/PD_19/CFC - G/Rplots")

# Summary

# avg. growth
avg <- round(avg1, digits = 2)
rownames(avg) <- c("Discipline","Student-Centered","Manage","Collaborate","Role","Theories")

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
  pos[i] <- round(1 - ((diff1[i,3] + diff1[i,4] + diff1[i,5]) / sum(diff1[i,])), digits = 2)*100
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

d <- diff1[,4:9]
colnames(d) <- c("-1 Unit","0 Units","1 Unit","2 Units","3 Units","4 Units")

# 1

B_A1 <- BA1[1:2,]
colnames(B_A1) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A1) <- c("Before","After")

png("b1.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b1 <- barplot((BA1/sum(BA1))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Disciplinary Interventions", cex.main = 2,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
#text(b1, B_A1 + 5, paste(B_A1,"%",sep="") ,cex=1.9) 
legend("top", legend = rownames(B_A1), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c1.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c1 <- barplot((d[1,]/sum(d[1,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Disciplinary Interventions", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c1, (d[1,]/sum(d[1,]))*100 + 4, paste(round((d[1,]/sum(d[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
#legend("top", legend = rownames(d1), 
#       fill = c("royalblue3","firebrick2"),
#       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

sum(diff1[1,])

# 2

B_A2 <- BA1[3:4,]
colnames(B_A2) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A2) <- c("Before","After")

png("b2.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b2 <- barplot((BA2/sum(BA2))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Knowledge of Student-Centered Classrooms", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A2), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c2.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c2 <- barplot((d[2,]/sum(d[2,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Knowledge of Student-Centered Classrooms", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c2, (d[2,]/sum(d[2,]))*100 + 4, paste(round((d[2,]/sum(d[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff1[2,])

# 3

B_A3 <- BA1[5:6,]
colnames(B_A3) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A3) <- c("Before","After")

png("b3.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b3 <- barplot((BA3/sum(BA3))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Capacity to Manage Classroom", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A3), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c3.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c3 <- barplot((d[3,]/sum(d[3,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Capacity to Manage Classroom", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c3, (d[3,]/sum(d[3,]))*100 + 4, paste(round((d[3,]/sum(d[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff1[3,])

# 4

B_A4 <- BA1[7:8,]
colnames(B_A4) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A4) <- c("Before","After")

png("b4.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b4 <- barplot((BA4/sum(BA4))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Collaborative Learning Strategies", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A4), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c4.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c4 <- barplot((d[4,]/sum(d[4,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Collaborative Learning Strategies", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c4, (d[4,]/sum(d[4,]))*100 + 4, paste(round((d[4,]/sum(d[4,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff1[4,])

# 5

B_A5 <- BA1[9:10,]
colnames(B_A5) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A5) <- c("Before","After")

png("b5.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b5 <- barplot((BA5/sum(BA5))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Role", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A5), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c5.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c5 <- barplot((d[5,]/sum(d[5,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Role", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c5, (d[5,]/sum(d[5,]))*100 + 4, paste(round((d[5,]/sum(d[5,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff1[5,])

#6

B_A6 <- BA1[11:12,]
colnames(B_A6) <- c("Very Low","Low","Medium","High","Very High")
rownames(B_A6) <- c("Before","After")

png("b6.png", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
b6 <- barplot((BA6/sum(BA6))*100, col= c("royalblue3","firebrick2"), border="white", 
              main = "Learning Theories", cex.main = 1.8,
              font.main = 1, font.axis=1, beside=T,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
legend("top", legend = rownames(B_A6), 
       fill = c("royalblue3","firebrick2"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

png("c6.png", width = 800) 
op <- par(mar = c(4,4,4,2) + 1)
c6 <- barplot((d[6,]/sum(d[6,]))*100, names.arg = colnames(d), las=1, col= c("royalblue3"), border="white", 
              main = "Change in Learning Theories", cex.main = 1.6,
              font.main = 1, font.axis=1, beside=T, space = 1,
              ylim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(c6, (d[6,]/sum(d[6,]))*100 + 4, paste(round((d[6,]/sum(d[6,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 

sum(diff1[6,])


# Eval Charts

colnames(eval) <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

# 1

png("e1.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e1 <- barplot((eval[1,]/sum(eval[1,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,10), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e1, (eval[1,]/sum(eval[1,]))*100 + 5, paste(round((eval[1,]/sum(eval[1,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 2

png("e2.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e2 <- barplot((eval[2,]/sum(eval[2,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,10), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e2, (eval[2,]/sum(eval[2,]))*100 + 5, paste(round((eval[2,]/sum(eval[2,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 3

png("e3.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e3 <- barplot((eval[3,]/sum(eval[3,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,12), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e3, (eval[3,]/sum(eval[3,]))*100 + 5, paste(round((eval[3,]/sum(eval[3,]))*100,digits = 0),"%",sep="") ,cex=1.9) 
par(op) ## reset
dev.off() 


# 4

png("e4.png", width = 800) 
op <- par(mar = c(4,11,2,2) + 1)
e4 <- barplot((eval[4,]/sum(eval[4,]))*100, names.arg = colnames(eval), las=1, col= c("royalblue3"), border="white", 
              main = "", cex.main = 1.6, horiz = T,
              font.main = 1, font.axis=1, space = 1,
              xlim = c(0,15), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(y = e4, (eval[4,]/sum(eval[4,]))*100 + 5, paste(round((eval[4,]/sum(eval[4,]))*100,digits = 0),"%",sep="") ,cex=1.9)
par(op) ## reset
dev.off() 



