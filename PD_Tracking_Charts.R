
setwd("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_Tacking/All/Rplots")
dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_Tacking/All/PD_All_collapse.csv")


train <- c("Jul. 16, 2018","Jul. 23, 2018","Aug. 2, 2018","Sep. 29, 2018","Oct. 5, 2018","Dec. 10, 2018","Dec. 13, 2018",
           "Jan. 22, 2019","Mar 6, 2019","Apr. 15, 2019","May 27, 2019","Jul. 8, 2019","Jul. 29, 2019","Aug. 20, 2019","Aug. 28, 2019")

# Overall Average
dta$Pre[dta$school=="Mission of Hope"] <- .42; dta$Post[dta$school=="Mission of Hope"] <- .83
dta$Pre[dta$school=="Ecole St Marc"] <- .36; dta$Post[dta$school=="Ecole St Marc"] <- .93
dta$Pre[dta$school=="Harvy"] <- .5; dta$Post[dta$school=="Harvy"] <- .87
dta$Pre[dta$school=="HOH"] <- .4; dta$Post[dta$school=="HOH"] <- .94
dta$Pre[dta$school=="CEFCAP"] <- .41; dta$Post[dta$school=="CEFCAP"] <- .88
dta$Pre[dta$school=="RTS"] <- .55; dta$Post[dta$school=="RTS"] <- .99
dta$Pre[dta$school=="JS"] <- .59; dta$Post[dta$school=="JS"] <- .96
dta$Pre[dta$school=="TRESOR"] <- .48; dta$Post[dta$school=="TRESOR"] <- .98
dta$Pre[dta$school=="Sonje"] <- .51; dta$Post[dta$school=="Sonje"] <- .91
dta$Pre[dta$school=="PD Dondon"] <- .42; dta$Post[dta$school=="PD Dondon"] <- .93
dta$Pre[dta$school=="OEDP"] <- .47; dta$Post[dta$school=="OEDP"] <- .94
dta$Pre[dta$school=="LABY"] <- .35; dta$Post[dta$school=="LABY"] <- .94
dta$Pre[dta$school=="JOUISSANT"] <- .49; dta$Post[dta$school=="JOUISSANT"] <- .90
dta$Pre[dta$school=="CFC - K"] <- .24; dta$Post[dta$school=="CFC - K"] <- .96
dta$Pre[dta$school=="CFC - G"] <- .36; dta$Post[dta$school=="CFC - G"] <- .87


# All
png("BA.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 

# 1
png("pp1.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v6Pre[order(dta$nschool)], type="b", 
      bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
      pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v6Post[order(dta$nschool)], 
        col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend(x=1,y=0.6, legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 2
png("pp2.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v7Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v7Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 3
png("pp3.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v8Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v8Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 4
png("pp4.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v9Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v9Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 5
png("pp5.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v10Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v10Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 6
png("pp6.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v11Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v11Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 7
png("pp7.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v12Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v12Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 8
png("pp8.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v13Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v13Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.3,"2018",cex=1.5)
text(9,0.3,"2019",cex=1.5)
# Add a legend
legend(x=2,y=0.4, legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 9
png("pp9.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v14Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v14Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 10
png("pp10.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v15Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v15Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 11
png("pp11.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v16Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v16Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 12
png("pp12.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v17Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v17Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend(2,.15, legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.15, 0.1))
par(op) ## reset
dev.off() 


# 13
png("pp13.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v18Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v18Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 14
png("pp14.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v19Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v19Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 15
png("pp15.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v20Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v20Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend(2,.15, legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 16
png("pp16.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v21Pre[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="Percent Correct" , col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(0,1), xaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v21Post[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,0.1,"2018",cex=1.5)
text(9,0.1,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


avg <- matrix(0,16,2)

for (i in 2:17) {
  avg[i-1,1] <- mean(dta[,i+16])
  avg[i-1,2] <- mean(dta[,i])
}


# ------------------------------------------------------------------------------
# Evals

dta <- read.csv("/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_Tacking/All/Eval_All_collapse.csv")

eval <- c("Very Low","Low","Neutral","High","Very High")


# overall 

for (i in 1:nrow(dta)) {
  dta$b.avg[i] <- mean(dta$v3[i],dta$v5[i],dta$v7[i],dta$v9[i],dta$v11[i],dta$v13[i],dta$v15[i])
  dta$a.avg[i] <- mean(dta$v4[i],dta$v6[i],dta$v8[i],dta$v10[i],dta$v12[i],dta$v14[i],dta$v16[i])
}

png("avg_eval.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$b.avg[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$a.avg[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 

# 1
png("eval1.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v3[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v4[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 2
png("eval2.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v5[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v6[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 3
png("eval3.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v7[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v8[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 4
png("eval4.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v9[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v10[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 5
png("eval5.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v11[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v12[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 


# 6
png("eval6.png", width = 800) 
op <- par(mar = c(7,4,2,1) + 1)
plot(dta$nschool[order(dta$nschool)], dta$v13[order(dta$nschool)], type="b", 
     bty="l", xlab="", ylab="", col="firebrick2" , lwd=3, 
     pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
grid(NA, NULL, lwd = 2)
axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
axis(2, at=1:5, labels=eval, las = 1, cex.lab=2)
lines(dta$nschool[order(dta$nschool)], dta$v14[order(dta$nschool)], 
      col="royalblue3" , lwd=3 , pch=19 , type="b" )
abline(v=7.5,lty=2,lwd=2)
text(6,1.5,"2018",cex=1.5)
text(9,1.5,"2019",cex=1.5)
# Add a legend
legend("bottomleft", legend = c("Pre", "Post"), col = c("firebrick2", "royalblue3"), 
       pch = c(17,19), bty = "n", pt.cex = 2, cex = 1, text.col = "black", horiz = F, 
       inset = c(0.1, 0.1))
par(op) ## reset
dev.off() 



#training eval

t.eval <- c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")

for (i in 1:4) {
  png(paste("e",i,".png", sep = ""), width = 800) 
  op <- par(mar = c(7,7,2,1) + 1)
  plot(dta$nschool[order(dta$nschool)], dta[,i+13][order(dta$nschool)], type="b", 
       bty="l", xlab="", ylab="", col="royalblue3" , lwd=3, 
       pch=17 , ylim=c(1,5), xaxt = "n", yaxt = "n")
  grid(NA, NULL, lwd = 2)
  axis(1, at=1:15, labels=train, las = 2, cex.lab=2)
  axis(2, at=1:5, labels=t.eval, las = 1, cex.lab=2)
  abline(v=7.5,lty=2,lwd=2)
  text(6,1.5,"2018",cex=1.5)
  text(9,1.5,"2019",cex=1.5)
  par(op) ## reset
  dev.off() 
}


