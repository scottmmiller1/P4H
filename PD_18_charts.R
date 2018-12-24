
setwd("/Users/scottmiller/Desktop/PD_18/Combined/Rplots")


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
C <- c(13,14,16)
D <- c(10:12,15)

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

colnames(growth) <- c("Knowledge Source", "Physical Punishment", "Student Leadership", "Teacher-Centered", 
              "Classroom Activities", "Classroom Management", "Rules & Procedures", "Behaviorism",
              "Cognitivism", "Constructivism", "Student-Centered", "Classroom Management (2)", 
              "Student-teacher Relationship", "Collaborative Learning", "Group Work", "Classroom Strategies")


jpeg("growth_pct.jpeg", width = 1000, height = 600) 
op <- par(mar = c(10,4,4,1) + 1)
growth.pct <- barplot(growth, names.arg = colnames(growth), las=2, cex.names=0.8, ylim = c(0,100), 
                      font.lab=1, font.main = 1, font.axis = 1,
                      col= c("royalblue3") , border="white", main = "Growth %", cex.main=2) 
text(growth.pct, growth + 4 , paste(growth,"%",sep="") ,cex=1) 
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

jpeg("pp1.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp1 <- barplot(pre_post1, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
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

jpeg("pp2.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp2 <- barplot(pre_post2, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
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

jpeg("pp3.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp3 <- barplot(pre_post3, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
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

jpeg("pp4.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp4 <- barplot(pre_post4, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
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

jpeg("pp5.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp5 <- barplot(pre_post5, col= c("royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
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

jpeg("pp6.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp6 <- barplot(pre_post6, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
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

jpeg("pp8.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp8 <- barplot(pre_post8, col= c("dimgrey","royalblue3","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp8, pre_post8 + 5, paste(pre_post8,"%",sep="") ,cex=1.5) 
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

jpeg("pp9.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp9 <- barplot(pre_post9, col= c("royalblue3","dimgrey","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp9, pre_post9 + 5, paste(pre_post9,"%",sep="") ,cex=1.5) 
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

jpeg("pp10.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp10 <- barplot(pre_post10, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp10, pre_post10 + 5, paste(pre_post10,"%",sep="") ,cex=1.5) 
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

jpeg("pp11.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp11 <- barplot(pre_post11, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp11, pre_post11 + 5, paste(pre_post11,"%",sep="") ,cex=1.5) 
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

jpeg("pp12.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp12 <- barplot(pre_post12, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp12, pre_post12 + 5, paste(pre_post12,"%",sep="") ,cex=1.5) 
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

jpeg("pp13.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp13 <- barplot(pre_post13, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp13, pre_post13 + 5, paste(pre_post13,"%",sep="") ,cex=1.5) 
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

jpeg("pp14.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp14 <- barplot(pre_post14, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp14, pre_post14 + 5, paste(pre_post14,"%",sep="") ,cex=1.5) 
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

jpeg("pp15.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp15 <- barplot(pre_post15, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp15, pre_post15 + 5, paste(pre_post15,"%",sep="") ,cex=1.5) 
legend("top",
       legend = rownames(pre_post15), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 16

pre_post16 <- t(pre_post16[,2:5])
rownames(pre_post16) <- c("A","B","C","D")
colnames(pre_post16) <- c("Pre","Post")

jpeg("pp16.jpeg", width = 800) 
op <- par(mar = c(4,4,4,1) + 1)
pp16 <- barplot(pre_post16, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp16, pre_post16 + 5, paste(pre_post16,"%",sep="") ,cex=1.5) 
legend("top",
       legend = rownames(pre_post16), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

# -------------------------------------------------------------------------


##########################
##      Eval Charts     ##
##########################

## Run PD_Eval_18 prior to this section


# Summary

# avg. growth
avg <- round(avg1, digits = 2)
rownames(avg) <- c("Discipline","Student-Centered","Manage","Collaborate","Role","Theories")

jpeg("avg_growth.jpeg", width = 700) 
op <- par(mar = c(4,11,4,4) + 1)
avg_growth <- barplot(avg[,3], names.arg = rownames(avg), las=1, cex.names= 1.5, xlim = c(0,2),
                      font.lab=1, font.main = 1, font.axis = 1, horiz = T,
                      col= c("royalblue3") , border="white", main = "Avg. Growth", cex.main=2) 
text(y = avg_growth, avg[,3] + .1, paste(avg[,3]), cex=1.5) 
par(op) ## reset
dev.off() 


# pct. w/ positive growth

pos <- c(rep(0,6))

for (i in 1:6) { 
  pos[i] <- round(1 - (diff1[i,5] / sum(diff1[i,])), digits = 2)*100
}

jpeg("pos_growth.jpeg", width = 700) 
op <- par(mar = c(4,4,6,1) + 1)
pos_growth <- barplot(pos, names.arg = rownames(avg), las=1, cex.names= 1, ylim = c(0,105),
                      font.lab=1, font.main = 1, font.axis = 1, las = 1,
                      col= c("royalblue3") , border="white", main = "% with Positive Growth", cex.main=2) 
text(pos_growth, pos + 5, paste(pos,"%",sep=""), cex=1.2) 
par(op) ## reset
dev.off() 



