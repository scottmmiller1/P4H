
setwd("/Users/scottmiller/Desktop/EPD_18")
dta <- read.csv("EPD Exam Data.csv")
#install.packages("dplyr")
#install.packages("tidyverse")
#library(dplyr)
library(tidyverse)

dta1 <- filter(dta, dta$Complete !="i") # deletes observations that did not take both exams
dta1 <- dta1[-c(214:218),-23] # new dataset with missing rows / cols removed

num <- matrix(0,nrow(dta1),17)
num[,1] <- dta1[,1]

for (i in 2:nrow(num)) {
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

# -------------------------------------------------------

###################
#    Combined     #
###################

#overall
answers <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    answers[i,j+1] <- length(which(num[,i+1]==j))
  }
}

View(answers) 
#write.csv(answers, "answers.csv")

# -------------------------------------------------------

#Pre
pre_num <- num[-c(seq(1,nrow(num),by=2)),]

pre <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    pre[i,j+1] <- length(which(pre_num[,i+1]==j))
  }
}

View(pre) 
#write.csv(pre, "pre.csv")

# Pre percent
pre_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    pre_pct[i,j+1] <- length(which(pre_num[,i+1]==j))/length(pre_num[,i+1]==j)
  }
}

View(pre_pct) 
#write.csv(pre_pct, "pre_pct.csv")

# -------------------------------------------------------

#Post
post_num <- num[-c(1,seq(2,nrow(num)-1,by=2)),]

post <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    post[i,j+1] <- length(which(post_num[,i+1]==j))
  }
}

View(post) 
#write.csv(post, "post.csv")

# post percent
post_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    post_pct[i,j+1] <- length(which(post_num[,i+1]==j))/length(post_num[,i+1]==j)
  }
}

View(post_pct) 
#write.csv(post_pct, "post_pct.csv")


#######################
#   Mission of Hope   #
#######################

MoH <- filter(dta1, dta1$School == "Mission of Hope") 

num <- matrix(0,nrow(MoH),17)
num[,1] <- MoH[,1]

for (i in 1:nrow(num)) {
  for (j in 6:21) {
    if (MoH[i,j] == "v") {
      num[i,j-4] <- 1
    } else if (MoH[i,j] == "f") {
      num[i,j-4] <- 2
    } else if (MoH[i,j] == "bon") {
      num[i,j-4] <- 1
    } else if (MoH[i,j] == "pa bon") {
      num[i,j-4] <- 2
    } else if (MoH[i,j] == "a") {
      num[i,j-4] <- 1
    } else if (MoH[i,j] == "b") {
      num[i,j-4] <- 2
    } else if (MoH[i,j] == "c") {
      num[i,j-4] <- 3
    } else if (MoH[i,j] == "d") {
      num[i,j-4] <- 4
    } else if (dta1[i,j] == "e") {
      num[i,j-4] <- 5
    }
  }
}

#overall
MoH_answers <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    MoH_answers[i,j+1] <- length(which(num[,i+1]==j))
  }
}

View(MoH_answers) 
#write.csv(MoH_answers, "MoH_answers.csv")

# -------------------------------------------------------

#Pre
MoH_pre_num <- num[-c(seq(2,nrow(MoH),by=2)),]

MoH_pre <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    MoH_pre[i,j+1] <- length(which(MoH_pre_num[,i+1]==j))
  }
}

View(MoH_pre) 
#write.csv(MoH_pre, "MoH_pre.csv")

# Pre percent
MoH_pre_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    MoH_pre_pct[i,j+1] <- length(which(MoH_pre_num[,i+1]==j))/length(MoH_pre_num[,i+1]==j)
  }
}

View(MoH_pre_pct) 
#write.csv(MoH_pre_pct, "MoH_pre_pct.csv")

# -------------------------------------------------------

#MoH_post
MoH_post_num <- num[-c(seq(1,(nrow(MoH)-1),by=2)),]

MoH_post <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    MoH_post[i,j+1] <- length(which(MoH_post_num[,i+1]==j))
  }
}

View(MoH_post) 
#write.csv(MoH_post, "MoH_post.csv")

# MoH_post percent
MoH_post_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    MoH_post_pct[i,j+1] <- length(which(MoH_post_num[,i+1]==j))/length(MoH_post_num[,i+1]==j)
  }
}

View(MoH_post_pct) 
#write.csv(MoH_post_pct, "MoH_post_pct.csv")



#######################
#   Ecole St Marc   #
#######################

ESM <- filter(dta1, dta1$School == "Ecole St Marc") 

num <- matrix(0,nrow(ESM),17)
num[,1] <- ESM[,1]

for (i in 1:nrow(num)) {
  for (j in 6:21) {
    if (ESM[i,j] == "v") {
      num[i,j-4] <- 1
    } else if (ESM[i,j] == "f") {
      num[i,j-4] <- 2
    } else if (ESM[i,j] == "bon") {
      num[i,j-4] <- 1
    } else if (ESM[i,j] == "pa bon") {
      num[i,j-4] <- 2
    } else if (ESM[i,j] == "a") {
      num[i,j-4] <- 1
    } else if (ESM[i,j] == "b") {
      num[i,j-4] <- 2
    } else if (ESM[i,j] == "c") {
      num[i,j-4] <- 3
    } else if (ESM[i,j] == "d") {
      num[i,j-4] <- 4
    } else if (dta1[i,j] == "e") {
      num[i,j-4] <- 5
    }
  }
}

#overall
ESM_answers <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    ESM_answers[i,j+1] <- length(which(num[,i+1]==j))
  }
}

View(ESM_answers) 
#write.csv(ESM_answers, "ESM_answers.csv")

# -------------------------------------------------------

#Pre
ESM_pre_num <- num[-c(seq(2,nrow(ESM),by=2)),]

ESM_pre <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    ESM_pre[i,j+1] <- length(which(ESM_pre_num[,i+1]==j))
  }
}

View(ESM_pre) 
#write.csv(ESM_pre, "ESM_pre.csv")

# Pre percent
ESM_pre_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    ESM_pre_pct[i,j+1] <- length(which(ESM_pre_num[,i+1]==j))/length(ESM_pre_num[,i+1]==j)
  }
}

View(ESM_pre_pct) 
#write.csv(ESM_pre_pct, "ESM_pre_pct.csv")

# -------------------------------------------------------

#ESM_post
ESM_post_num <- num[-c(seq(1,(nrow(ESM)-1),by=2)),]

ESM_post <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    ESM_post[i,j+1] <- length(which(ESM_post_num[,i+1]==j))
  }
}

View(ESM_post) 
#write.csv(ESM_post, "ESM_post.csv")

# ESM_post percent
ESM_post_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    ESM_post_pct[i,j+1] <- length(which(ESM_post_num[,i+1]==j))/length(ESM_post_num[,i+1]==j)
  }
}

View(ESM_post_pct) 
#write.csv(ESM_post_pct, "ESM_post_pct.csv")



#######################
#       Harvy        #
#######################

Harvy <- filter(dta1, dta1$School == "Harvy") 

num <- matrix(0,nrow(Harvy),17)
num[,1] <- Harvy[,1]

for (i in 1:nrow(num)) {
  for (j in 6:21) {
    if (Harvy[i,j] == "v") {
      num[i,j-4] <- 1
    } else if (Harvy[i,j] == "f") {
      num[i,j-4] <- 2
    } else if (Harvy[i,j] == "bon") {
      num[i,j-4] <- 1
    } else if (Harvy[i,j] == "pa bon") {
      num[i,j-4] <- 2
    } else if (Harvy[i,j] == "a") {
      num[i,j-4] <- 1
    } else if (Harvy[i,j] == "b") {
      num[i,j-4] <- 2
    } else if (Harvy[i,j] == "c") {
      num[i,j-4] <- 3
    } else if (Harvy[i,j] == "d") {
      num[i,j-4] <- 4
    } else if (dta1[i,j] == "e") {
      num[i,j-4] <- 5
    }
  }
}

#overall
Harvy_answers <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    Harvy_answers[i,j+1] <- length(which(num[,i+1]==j))
  }
}

View(Harvy_answers) 
write.csv(Harvy_answers, "Harvy_answers.csv")

# -------------------------------------------------------

#Pre
Harvy_pre_num <- num[-c(seq(2,nrow(Harvy),by=2)),]

Harvy_pre <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    Harvy_pre[i,j+1] <- length(which(Harvy_pre_num[,i+1]==j))
  }
}

View(Harvy_pre) 
write.csv(Harvy_pre, "Harvy_pre.csv")

# Pre percent
Harvy_pre_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    Harvy_pre_pct[i,j+1] <- length(which(Harvy_pre_num[,i+1]==j))/length(Harvy_pre_num[,i+1]==j)
  }
}

View(Harvy_pre_pct) 
write.csv(Harvy_pre_pct, "Harvy_pre_pct.csv")

# -------------------------------------------------------

#Harvy_post
Harvy_post_num <- num[-c(seq(1,(nrow(Harvy)-1),by=2)),]

Harvy_post <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    Harvy_post[i,j+1] <- length(which(Harvy_post_num[,i+1]==j))
  }
}

View(Harvy_post) 
write.csv(Harvy_post, "Harvy_post.csv")

# Harvy_post percent
Harvy_post_pct <- matrix(0,16,6)

for (i in 1:16) {
  for (j in 0:5) {
    Harvy_post_pct[i,j+1] <- length(which(Harvy_post_num[,i+1]==j))/length(Harvy_post_num[,i+1]==j)
  }
}

View(Harvy_post_pct) 
write.csv(Harvy_post_pct, "Harvy_post_pct.csv")




