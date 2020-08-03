
#################################################################
##  Analyzes Master Class exam data and generates              ##
##  corresponding charts for P4H Global MC evaluation reports. ##
#################################################################


setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/MC/MC_20/Sessions/S1/Rplots")
dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/MC/MC_20/Data/MC_20_Exam.csv")

## packages
library(tidyverse)
library(ggplot2)
# element count function
source("/Users/scottmiller/GitHub/P4H/elem_count.R")


dta1 <- filter(dta, dta$complete !="i") # deletes observations that did not take both exams
#dta1 <- filter(dta1, dta1$Session == 1) # change for each school


num <- matrix(0,nrow(dta1),11)
num[,1] <- dta1[,2]

# label answer choices
for (i in 1:nrow(num)) {
  for (j in 6:15) {
      if (dta1[i,j] == 1) {
      num[i,j-4] <- 1
    }  
      else if (dta1[i,j] == "A") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "B") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "C") {
      num[i,j-4] <- 3
    } else if (dta1[i,j] == "D") {
      num[i,j-4] <- 4
    }
  }
}


#overall
answers <- elem_count(data = num[,-1],quest = 10, choices = 4, percent = FALSE)


#Pre
# data set with only pre-test responses
pre_num <- num[-c(seq(2,nrow(num),by=2)),]
pre <- elem_count(data = pre_num[,-1], quest = 10, choices = 4, percent = FALSE)
pre_pct <- elem_count(data = pre_num[,-1], quest = 10, choices = 4, percent = TRUE)


# -------------------------------------------------------

#Post
# data set with only post-test responses
post_num <- num[-c(1,seq(1,nrow(num)-1,by=2)),]                       # filter(dta1, dta1$Pre_Post == "Pre")
post <- elem_count(data = post_num[,-1], quest = 10, choices = 4, percent = FALSE)
post_pct <- elem_count(data = post_num[,-1], quest = 10, choices = 4, percent = TRUE)



##########################
##      Exam Charts     ##
##########################

# -------------------------------------------------------------------------
# Growth %

n.quest <- 10
growth <- matrix(0,10,3)


# group questions by correct answer
true.A <- c(3,4,5)
false.B <- c(7,8,10)
C <- c(1,2,6)
D <- c(9)

# true / A
for (i in true.A) {
  growth[i,1] <- round(pre_pct[i,2], digits = 2)*100
  growth[i,2] <- round(post_pct[i,2], digits = 2)*100
  growth[i,3] <- round(post_pct[i,2] - pre_pct[i,2], digits = 2)*100
}
# false / B
for (i in false.B) {
  growth[i,1] <- round(pre_pct[i,3], digits = 2)*100
  growth[i,2] <- round(post_pct[i,3], digits = 2)*100
  growth[i,3] <- round(post_pct[i,3] - pre_pct[i,3], digits = 2)*100
}
# C
for (i in C) {
  growth[i,1] <- round(pre_pct[i,4], digits = 2)*100
  growth[i,2] <- round(post_pct[i,4], digits = 2)*100
  growth[i,3] <- round(post_pct[i,4] - pre_pct[i,4], digits = 2)*100
}
# D
for (i in D) {
  growth[i,1] <- round(pre_pct[i,5], digits = 2)*100
  growth[i,2] <- round(post_pct[i,5], digits = 2)*100
  growth[i,3] <- round(post_pct[i,5] - pre_pct[i,5], digits = 2)*100
}


names <- c("Spheres of S.D.", "Citizen Participation", "Resources", "Definition of S.D.", 
           "Principles of S.D.", "Asset-Based Development (1)", "Capital", "Savior Role",
           "Resource Role", "Asset-Based Development (2)")

growth <- data.frame(
  names = as.factor(names), Pre = growth[,1], Post = growth[,2], Growth = growth[,3]
)

# growth percentage
ggplot(growth, aes(x = names, y = Growth)) +
  geom_col(fill = "royalblue3") + 
  geom_text(aes(label = paste(Growth,"%",sep=""), y = Growth + 3), size = 5) +
  ggtitle("Growth Percentage") +
  theme_classic() + 
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(angle = 35, vjust = 1, hjust=1, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        plot.margin=unit(c(0.2,0.2,0,1),"cm")
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0,80)) +
  scale_x_discrete(limits = names)
ggsave("growth_pct.png")




# -------------------------------------------------------------------------
# 

for (i in 1:10) {
  assign(paste("pre_post", i, sep = ""), rbind(round(pre_pct[i,], digits = 2)*100, round(post_pct[i,], digits = 2)*100)) 
}

# True / False

# 1
pre_post1 <- t(pre_post1[,2:5])
rownames(pre_post1) <- c("A","B","C","D")
colnames(pre_post1) <- c("Pre","Post")

png("pp1.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp1 <- barplot(pre_post1, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp1, pre_post1 + 6, paste(pre_post1,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post1), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 2
pre_post2 <- t(pre_post2[,2:5])
rownames(pre_post2) <- c("A","B","C","D")
colnames(pre_post2) <- c("Pre","Post")

png("pp2.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp2 <- barplot(pre_post2, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp2, pre_post2 + 6, paste(pre_post2,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post2), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 3
pre_post3 <- t(pre_post3[,1:2])
rownames(pre_post3) <- c("Incorrect","Correct")
colnames(pre_post3) <- c("Pre","Post")

png("pp3.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp3 <- barplot(pre_post3, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp3, pre_post3 + 5, paste(pre_post3,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post3), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 4
pre_post4 <- t(pre_post4[,1:2])
rownames(pre_post4) <- c("Incorrect","Correct")
colnames(pre_post4) <- c("Pre","Post")

png("pp4.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp4 <- barplot(pre_post4, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp4, pre_post4 + 5, paste(pre_post4,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post4), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 5
pre_post5 <- t(pre_post5[,2:5])
rownames(pre_post5) <- c("A","B","C","D")
colnames(pre_post5) <- c("Pre","Post")

png("pp5.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp5 <- barplot(pre_post5, col= c("royalblue3","dimgrey","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp5, pre_post5 + 6, paste(pre_post5,"%",sep="") ,cex=1.9) 
legend(4,125,
       legend = rownames(pre_post5), 
       fill = c("royalblue3","dimgrey","darkgrey","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 

# 6
pre_post6 <- t(pre_post6[,2:5])
rownames(pre_post6) <- c("A","B","C","D")
colnames(pre_post6) <- c("Pre","Post")

png("pp6.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp6 <- barplot(pre_post6, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp6, pre_post6 + 6, paste(pre_post6,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post6), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 7
pre_post7 <- t(pre_post7[,2:5])
rownames(pre_post7) <- c("A","B","C","D")
colnames(pre_post7) <- c("Pre","Post")

png("pp7.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp7 <- barplot(pre_post7, col= c("dimgrey","royalblue3","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp7, pre_post7 + 6, paste(pre_post7,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post7), 
       fill = c("dimgrey","royalblue3","darkgrey","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 8
pre_post8 <- t(pre_post8[,2:5])
rownames(pre_post8) <- c("A","B","C","D")
colnames(pre_post8) <- c("Pre","Post")

png("pp8.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp8 <- barplot(pre_post8, col= c("dimgrey","royalblue3","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp8, pre_post8 + 6, paste(pre_post8,"%",sep="") ,cex=1.9) 
legend(4,120,
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
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp9 <- barplot(pre_post9, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp9, pre_post9 + 6, paste(pre_post9,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post9), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 10

pre_post10 <- t(pre_post10[,2:5])
rownames(pre_post10) <- c("A","B","C","D")
colnames(pre_post10) <- c("Pre","Post")

png("pp10.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp10 <- barplot(pre_post10, col= c("dimgrey","royalblue3","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp10, pre_post10 + 6, paste(pre_post10,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post10), 
       fill = c("dimgrey","royalblue3","darkgrey","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# ----------------------------------------------

n.obs <- matrix(0,29,4)

#Exam 
for (i in 1:29) {
  n.obs[i,1] <- sum(post[i,])
}

# exam data
# pre
pre <- round((pre_pct*100),digits = 0)
write.table(pre,"Values/pre.txt",sep=",",row.names=FALSE,col.names =FALSE)
# pre
post <- round((post_pct*100),digits = 0)
write.table(post,"Values/post.txt",sep=",",row.names=FALSE,col.names =FALSE)
