
################################################################
##  Analyzes Year 1 PD exam data and generates corresponding  ##
##  charts for P4H Global PD evaluation reports.              ##
################################################################


setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_20/Year 1/MENFP/Creole/Rplots")
dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_20/Year 1/Data/PD_20_Y1 Exam Data Full.csv")

## packages
library(tidyverse)
library(ggplot2)
# element count function
source("/Users/scottmiller/GitHub/P4H/elem_count.R")


dta1 <- filter(dta, dta$Complete !="i") # deletes observations that did not take both exams
#dta1 <- dta1[,-c(22:23)] # new dataset with missing rows / cols removed
dta1 <- filter(dta1, dta1$School == "MENFP") # change for each school

# First several schools, #16 = c, remainder #16 = e 
#for (i in 1:598) {
#  if (dta1[i,21]=="c") {
#    dta1[i,21] <- "e"
#  }
#}

num <- matrix(0,nrow(dta1),17)
num[,1] <- dta1[,1]

# label answer choices
for (i in 1:nrow(num)) {
  for (j in 6:21) {
    if (dta1[i,j] == "v") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "f") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "bon") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "pa bon") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "a") {
      num[i,j-4] <- 1
    } else if (dta1[i,j] == "b") {
      num[i,j-4] <- 2
    } else if (dta1[i,j] == "c") {
      num[i,j-4] <- 3
    } else if (dta1[i,j] == "d") {
      num[i,j-4] <- 4
    } else if (dta1[i,j] == "e") {
      num[i,j-4] <- 5
    }
  }
}


#overall
answers <- elem_count(data = num[,-1],quest = 16, choices = 5, percent = FALSE)


#Pre
# data set with only pre-test responses
pre_num <- num[-c(seq(2,nrow(num),by=2)),]
pre <- elem_count(data = pre_num[,-1], quest = 16, choices = 5, percent = FALSE)
pre_pct <- elem_count(data = pre_num[,-1], quest = 16, choices = 5, percent = TRUE)


# -------------------------------------------------------

#Post
# data set with only post-test responses
post_num <- num[-c(1,seq(1,nrow(num)-1,by=2)),]                       # filter(dta1, dta1$Pre_Post == "Pre")
post <- elem_count(data = post_num[,-1], quest = 16, choices = 5, percent = FALSE)
post_pct <- elem_count(data = post_num[,-1], quest = 16, choices = 5, percent = TRUE)



##########################
##      Exam Charts     ##
##########################

# -------------------------------------------------------------------------
# Growth %

n.quest <- 16
growth <- matrix(0,16,3)
correct <- num
correct <- cbind(dta1[,2],correct) 

# group questions by correct answer
true.A <- c(5,7,9)
false.B <- c(1:4,6,8)
C <- c(13,14)
D <- c(10:12,15)
E <- c(16)

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
# E
for (i in E) {
  growth[i,1] <- round(pre_pct[i,6], digits = 2)*100
  growth[i,2] <- round(post_pct[i,6], digits = 2)*100
  growth[i,3] <- round(post_pct[i,6] - pre_pct[i,6], digits = 2)*100
}


names <- c("Sous Konesans yo", "Chatiman Kòporèl", "Lidèchip Elèv", "Santre sou Pwofesè", 
           "Aktivite nan Salklas", "Jesyon Salklas (1)", "Règ ak Pwosedi", "Beyavyoris",
           "Coyitivis", "Konstritivis", "Santre sou Elèv", "Jesyon Salklas (2)", 
           "Relasyon Elèv-Pwofesè", "Aprantisaj Kolaboratif", "Travay an Gwoup", "Estrateji nan Salklas")

growth <- data.frame(
  names = as.factor(names), Pre = growth[,1], Post = growth[,2], Growth = growth[,3]
)

# growth percentage
ggplot(growth, aes(x = names, y = Growth)) +
  geom_col(fill = "royalblue3") + 
  geom_text(aes(label = paste(Growth,"%",sep=""), y = Growth + 3),size=5) +
  ggtitle("Kwasans") +
  theme_classic() + 
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        plot.margin=unit(c(0.2,0.2,0,1),"cm")
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
  scale_x_discrete(limits = names)
ggsave("growth_pct.png")



# growth percentage
#png("growth_pct.png", width = 1000, height = 600) 
#op <- par(mar = c(13,4,2,1) + 1)
#growth.pct <- barplot(growth, names.arg = colnames(growth), las=2, cex.names=1.2, ylim = c(0,110), 
#                      font.lab=1, font.main = 1, font.axis = 1,
#                      col= c("royalblue3") , border="white", main = "Growth %", cex.main=2) 
#text(growth.pct, growth + 5 , paste(growth,"%",sep="") ,cex=1.3) 
#par(op) ## reset
#dev.off() 

# -------------------------------------------------------------------------
# 

for (i in 1:16) {
  assign(paste("pre_post", i, sep = ""), rbind(round(pre_pct[i,], digits = 2)*100, round(post_pct[i,], digits = 2)*100)) 
}

# True / False

# 1
pre_post1 <- t(pre_post1[,2:3])
rownames(pre_post1) <- c("Dakò","Pa Dakò")
colnames(pre_post1) <- c("Pre","Pòs")

png("pp1.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp1 <- barplot(pre_post1, col= c("grey","royalblue3"), border="white",
               main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp1, pre_post1 + 5, paste(pre_post1,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post1), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 2
pre_post2 <- t(pre_post2[,2:3])
rownames(pre_post2) <- c("Dakò","Pa Dakò")
colnames(pre_post2) <- c("Pre","Pòs")

png("pp2.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp2 <- barplot(pre_post2, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp2, pre_post2 + 5, paste(pre_post2,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post2), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 3
pre_post3 <- t(pre_post3[,2:3])
rownames(pre_post3) <- c("Dakò","Pa Dakò")
colnames(pre_post3) <- c("Pre","Pòs")

png("pp3.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp3 <- barplot(pre_post3, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp3, pre_post3 + 5, paste(pre_post3,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post3), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 4
pre_post4 <- t(pre_post4[,2:3])
rownames(pre_post4) <- c("Dakò","Pa Dakò")
colnames(pre_post4) <- c("Pre","Pòs")

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
pre_post5 <- t(pre_post5[,2:3])
rownames(pre_post5) <- c("Dakò","Pa Dakò")
colnames(pre_post5) <- c("Pre","Pòs")

png("pp5.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp5 <- barplot(pre_post5, col= c("royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp5, pre_post5 + 5, paste(pre_post5,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post5), 
       fill = c("royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 6
pre_post6 <- t(pre_post6[,2:3])
rownames(pre_post6) <- c("Dakò","Pa Dakò")
colnames(pre_post6) <- c("Pre","Pòs")

png("pp6.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp6 <- barplot(pre_post6, col= c("grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp6, pre_post6 + 5, paste(pre_post6,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post6), 
       fill = c("grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 7
pre_post7 <- t(pre_post7[,2:3])
rownames(pre_post7) <- c("Dakò","Pa Dakò")
colnames(pre_post7) <- c("Pre","Pòs")

png("pp7.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp7 <- barplot(pre_post7, col= c("royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,110), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp7, pre_post7 + 5, paste(pre_post7,"%",sep="") ,cex=2) 
legend(3,120,
       legend = rownames(pre_post6), 
       fill = c("royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 



# -------------------------------------------------------------------------
# 

# Multiple Choice

# 8
pre_post8 <- t(pre_post8[,2:5])
rownames(pre_post8) <- c("A","B","C","D")
colnames(pre_post8) <- c("Pre","Pòs")

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
colnames(pre_post9) <- c("Pre","Pòs")

png("pp9.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp9 <- barplot(pre_post9, col= c("royalblue3","dimgrey","darkgrey","grey"), border="white", main = "", font.axis=1, beside=T,
               ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp9, pre_post9 + 6, paste(pre_post9,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post9), 
       fill = c("royalblue3","dimgrey","darkgrey","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 10

pre_post10 <- t(pre_post10[,2:5])
rownames(pre_post10) <- c("A","B","C","D")
colnames(pre_post10) <- c("Pre","Pòs")

png("pp10.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp10 <- barplot(pre_post10, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp10, pre_post10 + 6, paste(pre_post10,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post10), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 11

pre_post11 <- t(pre_post11[,2:5])
rownames(pre_post11) <- c("A","B","C","D")
colnames(pre_post11) <- c("Pre","Pòs")

png("pp11.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp11 <- barplot(pre_post11, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp11, pre_post11 + 6, paste(pre_post11,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post11), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 12

pre_post12 <- t(pre_post12[,2:5])
rownames(pre_post12) <- c("A","B","C","D")
colnames(pre_post12) <- c("Pre","Pòs")

png("pp12.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp12 <- barplot(pre_post12, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp12, pre_post12 + 6, paste(pre_post12,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post12), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 13

pre_post13 <- t(pre_post13[,2:5])
rownames(pre_post13) <- c("A","B","C","D")
colnames(pre_post13) <- c("Pre","Pòs")

png("pp13.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp13 <- barplot(pre_post13, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp13, pre_post13 + 6, paste(pre_post13,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post13), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 14

pre_post14 <- t(pre_post14[,2:5])
rownames(pre_post14) <- c("A","B","C","D")
colnames(pre_post14) <- c("Pre","Pòs")

png("pp14.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp14 <- barplot(pre_post14, col= c("dimgrey","darkgrey","royalblue3","grey"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp14, pre_post14 + 6, paste(pre_post14,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post14), 
       fill = c("dimgrey","darkgrey","royalblue3","grey"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 15

pre_post15 <- t(pre_post15[,2:5])
rownames(pre_post15) <- c("A","B","C","D")
colnames(pre_post15) <- c("Pre","Pòs")

png("pp15.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp15 <- barplot(pre_post15, col= c("dimgrey","darkgrey","grey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp15, pre_post15 + 6, paste(pre_post15,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post15), 
       fill = c("dimgrey","darkgrey","grey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 


# 16

pre_post16 <- t(pre_post16[,2:6])
rownames(pre_post16) <- c("A","B","C","D","E")
colnames(pre_post16) <- c("Pre","Pòs")

png("pp16.png", width = 800) 
op <- par(xpd=T, mar=par()$mar+c(0,-2,2,0))
pp16 <- barplot(pre_post16, col= c("dimgrey","darkgrey","grey","lightgrey","royalblue3"), border="white", main = "", font.axis=1, beside=T,
                ylim = c(0,100), font.lab=1, cex.axis = 1.5, cex.names = 1.5) 
text(pp16, pre_post16 + 6, paste(pre_post16,"%",sep="") ,cex=1.9) 
legend(4,120,
       legend = rownames(pre_post16), 
       fill = c("dimgrey","darkgrey","grey","lightgrey","royalblue3"),
       horiz = T, cex = 1.5)
par(op) ## reset
dev.off() 



# ----------------------------------------------

n.obs <- matrix(0,16,4)

#Exam 
for (i in 1:16) {
  n.obs[i,1] <- sum(post[i,])
}

# exam data
# pre
pre <- round((pre_pct*100),digits = 0)
write.table(pre,"Values/pre.txt",sep=",",row.names=FALSE,col.names =FALSE)
# pre
post <- round((post_pct*100),digits = 0)
write.table(post,"Values/post.txt",sep=",",row.names=FALSE,col.names =FALSE)
