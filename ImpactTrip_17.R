## P4H 2017 Missions Trip Evaluation ##

setwd("/Users/Scott/Desktop/Evaluation Analysis")
data <- read.csv("Missions.csv")
Missions <- data[1:57,]
library(plyr)

#Data cleaning
id <- Missions$ID
prepared <- as.factor(Missions$prepared)
teaminfo <- as.factor(Missions$teaminfo)
coreinfo <- as.factor(Missions$coreinfo)
nightly <- as.factor(Missions$nightly)
fsafety <- as.factor(Missions$fsafety)
support <- as.factor(Missions$support)
nmissed <- Missions$nmissed
b_culture <- as.factor(Missions$b_culture); a_culture <- as.factor(Missions$a_culture); d_culture <- Missions$a_culture - Missions$b_culture
b_sdev <- as.factor(Missions$b_sdev); a_sdev <- as.factor(Missions$a_sdev); d_sdev <- Missions$a_sdev - Missions$b_sdev
b_poverty <- as.factor(Missions$b_poverty); a_poverty <- as.factor(Missions$a_poverty); d_poverty <- Missions$a_poverty - Missions$b_poverty
b_operate <- as.factor(Missions$b_operate); a_operate <- as.factor(Missions$a_operate); d_operate <- Missions$a_operate - Missions$b_operate
b_critical <- as.factor(Missions$b_critical); a_critical <- as.factor(Missions$a_critical); d_critical <- Missions$a_critical - Missions$b_critical
b_diff <- as.factor(Missions$b_diff); a_diff <- as.factor(Missions$a_diff); d_diff <- Missions$a_diff - Missions$b_diff
b_career <- as.factor(Missions$b_career); a_career <- as.factor(Missions$a_career); d_career <- Missions$a_career - Missions$b_career
b_perform <- as.factor(Missions$b_perform); a_perform <- as.factor(Missions$a_perform); d_perform <- Missions$a_perform - Missions$b_perform
b_adapt <- as.factor(Missions$b_adapt); a_adapt <- as.factor(Missions$a_adapt); d_adapt <- Missions$a_adapt - Missions$b_adapt
b_passion <- as.factor(Missions$b_passion); a_passion <- as.factor(Missions$a_passion); d_passion <- Missions$a_passion - Missions$b_passion
b_tconnect <- as.factor(Missions$b_tconnect); a_tconnect <- as.factor(Missions$b_tconnect); d_tconnect <- Missions$a_tconnect - Missions$b_tconnect
b_hconnect <- as.factor(Missions$b_hconnect); a_hconnect <- as.factor(Missions$a_hconnect); d_hconnect <- Missions$a_hconnect - Missions$b_hconnect
gender <- as.factor(Missions$gender)
college <- as.factor(Missions$college)
race <- as.factor(Missions$race)
major <- as.factor(Missions$major)
core <- as.factor(Missions$core)
ntravel <- Missions$ntravel
np4h <- Missions$np4h
countries <- as.factor(Missions$countries)

##Summary
summary(Missions)
summary(teaminfo) 
summary(teaminfo[new==1])
summary(teaminfo[new==0])
summary(coreinfo) 
summary(nightly) 
summary(fsafety)
summary(support) 
summary(nmissed) 
summary(gender) 
summary(college) 
summary(race)
summary(major) 
summary(countries)
summary(as.factor(ntravel))
summary(as.factor(np4h))

##Means-tests
chisq.test(b_culture, a_culture)
cor.test(Missions$a_culture, Missions$b_culture, "greater", "spearman")
chisq.test(b_sdev, a_sdev)
cor.test(Missions$a_sdev, Missions$b_sdev, "greater", "spearman")
chisq.test(b_poverty, a_poverty)
cor.test(Missions$a_poverty, Missions$b_poverty, "greater", "spearman")
chisq.test(b_operate, a_operate)
cor.test(Missions$a_operate, Missions$b_operate, "greater", "spearman")
chisq.test(b_critical, a_critical)
cor.test(Missions$a_critical, Missions$b_critical, "greater", "spearman")
chisq.test(b_diff, a_diff)
cor.test(Missions$a_diff, Missions$b_diff, "greater", "spearman")
chisq.test(b_career, a_career)
cor.test(Missions$a_career, Missions$b_career, "greater", "spearman")
chisq.test(b_perform, a_perform)
cor.test(Missions$a_perform, Missions$b_perform, "greater", "spearman")
chisq.test(b_adapt, a_adapt)
cor.test(Missions$a_adapt, Missions$b_adapt, "greater", "spearman")
chisq.test(b_passion, a_passion)
cor.test(Missions$a_passion, Missions$b_passion, "greater", "spearman")
chisq.test(b_tconnect, a_tconnect)
cor.test(Missions$a_tconnect, Missions$b_tconnect, "greater", "spearman")
chisq.test(b_hconnect, a_hconnect)
cor.test(Missions$a_hconnect, Missions$b_hconnect, "greater", "spearman")

#Disaggregates
#new
np4h[which(is.na(np4h))] <- 0
new <- ifelse(np4h==0,1,0)

table(d_sdev)
table(d_sdev[new==1])/sum(table(d_sdev[new==1]))
table(d_sdev[new==0])/sum(table(d_sdev[new==0]))
table(d_poverty)
table(d_poverty[new==1])/sum(table(d_poverty[new==1]))
table(d_poverty[new==0])/sum(table(d_poverty[new==0]))
table(d_culture[new==1])
table(d_culture[new==0])
table(d_adapt[new==1])
table(d_adapt[new==0])
table(d_career[new==1])
table(d_career[new==0])
table(d_diff[new==1])
table(d_diff[new==0])
table(d_hconnect[new==1])
table(d_hconnect[new==0])
table(d_tconnect[new==1])
table(d_tconnect[new==0])
table(d_operate[new==1])
table(d_operate[new==0])
table(d_passion[new==1])
table(d_passion[new==0])
table(d_perform[new==1])
table(d_perform[new==0])

table(d_sdev[new==1])
100*table(d_sdev[new==1])/sum(table(d_sdev[new==1]))
plot(as.factor(d_sdev[new==1]))


