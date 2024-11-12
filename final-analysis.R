library("stringr")
library("ggplot2")
library("irr")
#setwd("C:/Users/ellaw/OneDrive/Desktop/School/txtlab/detectiveGPT")

### read in csv files for each annotator ----

annotator1.df <- read.csv("Annotation Template NW.csv")[-1, ]
annotator2.df <- read.csv("Surprise Project Annotation Data - AS.csv")[-1, ]
annotator3.df <- read.csv("AnnotationTemplate_EA.csv")[-1, ]
annotator4.df <- read.csv("Anya_s Copy of New Surprise Annotation Template - Sheet1.csv")[-1, ]
allinfo.df <- read.csv("newoutputdf.csv")

critlist <- c("Initiatory", "Deducible", "Post.dictable", "Important", "Valence", "Immutable")

### Combine all annotators into monster.df ----

annotator1.df$Annotator <- 1
annotator2.df$Annotator <- 2
annotator3.df$Annotator <- 3
annotator4.df$Annotator <- 4


### Assign correct model numbers to each stem ----

unscramble <- function(annotator.df){
  ending_unscrambled <- annotator.df$Ending
  
  for (row_num in 1:120){
    stem_num <- as.numeric(annotator.df$Stem[row_num])
    ending_num <- as.numeric(annotator.df$Ending[row_num])
    ending_unscrambled[row_num] <- str_split_1(allinfo.df$endings_order[stem_num], " ")[ending_num]
  }
  
  return (ending_unscrambled)
  
}

annotator1_unscrambled <- unscramble(annotator1.df)
annotator1.df$unscrambled <- annotator1_unscrambled

annotator2_unscrambled <- unscramble(annotator2.df)
annotator2.df$unscrambled <- annotator2_unscrambled

annotator3_unscrambled <- unscramble(annotator3.df)
annotator3.df$unscrambled <- annotator3_unscrambled

annotator4_unscrambled <- unscramble(annotator4.df)
annotator4.df$unscrambled <- annotator4_unscrambled


### combine all this into a 480-row dataframe ----

monster.df <- NULL
monster.df <- rbind(annotator1.df, annotator2.df, annotator3.df, annotator4.df)

monster.df$Performance <- NULL

for (i in 1:480){
  if (monster.df$Most[i]=="x") {
    performance <- "Winner"
  }
  else if (monster.df$Least[i]=="x") {
    performance <- "Loser"
  }
  else {
    performance <- "Neutral"
  }
  monster.df$Performance[i]<-performance
}


### average deviation index - between annotators for each criteria ----
### https://www.researchgate.net/publication/247720828_Estimating_Interrater_Agreement_with_the_Average_Deviation_Index_A_User%27s_Guide

average_deviation <- function(criterion){
  
  criterion.df <- data.frame(matrix(nrow = 120, ncol = 0))
  
  #change each annotation to numeric instead of character
  criterion.df$a1 <- as.numeric(monster.df[[criterion]][monster.df$Annotator == "1"])
  criterion.df$a2 <- as.numeric(monster.df[[criterion]][monster.df$Annotator == "2"])
  criterion.df$a3 <- as.numeric(monster.df[[criterion]][monster.df$Annotator == "3"])
  criterion.df$a4 <- as.numeric(monster.df[[criterion]][monster.df$Annotator == "4"])
  
  #get the mean for each row by only the annotations (not the stem and ending numbers)
  justvals <- criterion.df[c("a1", "a2", "a3", "a4")]
  
  criterion.df$mean <- rowMeans(justvals)
  
  #calculate deviation index using the formula
  total <- 0
  
  for (i in 1:120){ #for each row
    
    rowtotal <- 0
    
    for (val in justvals[i, ]){ #for each annotation value in the row
      rowtotal <- rowtotal + abs(val - criterion.df$mean[i])
    }
    
    total <- total + (rowtotal/4)
    
  }
  
  return ((total/120))
  
  
}

#run the function
for (i in 1:6){
  crit <- toString(critlist[i])
  print(average_deviation(crit))
}


### inter-annotator agreement for best-worst ----

#make appropriate df

performance.df <- NULL
performance.df <- cbind(monster.df[monster.df$Annotator == 1, ]$Performance,
                        monster.df[monster.df$Annotator == 2, ]$Performance,
                        monster.df[monster.df$Annotator == 3, ]$Performance,
                        monster.df[monster.df$Annotator == 4, ]$Performance)
colnames(performance.df) <- c("fst", "snd", "thr", "frt")

#BAD!!
kappam.fleiss(performance.df, detail=TRUE)

#BAD!!
kappam.light(performance.df)

for (a1 in c("fst", "snd", "thr", "frt")){
  for (a2 in c("fst", "snd", "thr", "frt")){
    if (a1 != a2){
      test <- performance.df[, c(a1, a2)]
      print(kappam.fleiss(test)$value)
    }
  }
}

### calculation for win/neutral/lose ratios ----

badphi <- length(monster.df[monster.df$Performance == "Loser" & monster.df$unscrambled == "h", ]$Stem)

### means and medians for each criteria ALL POINTS ----

means_meds_all <- function(criterion.df){
  return (summary(c(criterion.df$a1, criterion.df$a2, criterion.df$a3, criterion.df$a4)))
}

means_meds_all(immutable.df)
means_meds_all(important.df)
means_meds_all(initiatory.df)
means_meds_all(post.dictable.df)
means_meds_all(valence.df)
means_meds_all(deducible.df)

hist(c(immutable.df$a1, immutable.df$a2, immutable.df$a3, immutable.df$a4),
     main="Immutability",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(important.df$a1, important.df$a2, important.df$a3, important.df$a4),
     main="Importance",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(initiatory.df$a1, initiatory.df$a2, initiatory.df$a3, initiatory.df$a4),
     main="Initiatory",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(post.dictable.df$a1, post.dictable.df$a2, post.dictable.df$a3, post.dictable.df$a4),
     main="Postdictability",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(valence.df$a1, valence.df$a2, valence.df$a3, valence.df$a4),
     main="Valence",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(deducible.df$a1, deducible.df$a2, deducible.df$a3, deducible.df$a4),
     main="Deducibility",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

### means and medians for each criteria AVERAGES BY STORY----

#todo row means and repeat summary/histograms


#Make histograms split by model type


monsterz.df <- monster.df[monster.df$unscrambled == "z", ]

hist(c(as.numeric(monsterz.df$Deducible)),
     main="Deducibility for Zeroshot",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

monsterp.df <- monster.df[monster.df$unscrambled == "p", ]

hist(c(as.numeric(monsterp.df$Deducible)),
     main="Deducibility for Phi3",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

monsterf.df <- monster.df[monster.df$unscrambled == "f", ]

hist(c(as.numeric(monsterf.df$Deducible)),
     main="Deducibility for Fewshot",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

monsterh.df <- monster.df[monster.df$unscrambled == "h", ]

hist(c(as.numeric(monsterh.df$Deducible)),
     main="Deducibility for Human",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

initiatory.df <- annotator1.df[c("Stem", "unscrambled")]

monsterh.df <- monster.df[monster.df$unscrambled == "h", ]

hist(c(as.numeric(monsterh.df$Deducible)),
     main="Deducibility for Human",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

#LOOKING AT THE BEST ONES 

monsterWinners.df <- monster.df[monster.df$Most == "x", ]

hist(c(as.numeric(monsterWinners.df$Deducible)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterWinners.df$Initiatory)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterWinners.df$Post.dictable)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)


hist(c(as.numeric(monsterWinners.df$Important)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterWinners.df$Valence)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterWinners.df$Immutable)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

#FOR THE LOSERS

monsterLosers.df <- monster.df[monster.df$Least == "x", ]

hist(c(as.numeric(monsterLosers.df$Deducible)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterLosers.df$Initiatory)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterLosers.df$Post.dictable)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)


hist(c(as.numeric(monsterLosers.df$Important)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterLosers.df$Valence)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

hist(c(as.numeric(monsterLosers.df$Immutable)),
     main="Deducibility for Winners",
     xlab="Val on Likert Scale",
     xlim=c(0, 5),
     freq = TRUE,
     breaks = c(0, 1, 2, 3, 4, 5)
)

### Compare distributions of each criterion for winners, losers, total ----

monsterWinners.df <- monster.df[monster.df$Most == "x", ]
monsterLosers.df <- monster.df[monster.df$Least == "x", ]

###THIS WORKS -- plotting by model
ggplot(monsterWinners.df, aes(x=as.numeric(Important), fill=paste(unscrambled))) +
  geom_histogram(bins=6) +
  xlim(0.5, 5.5) +
  geom_density(color = "blue", linewidth = 2, stat="count", fill=NA)

##trying to plot by winners vs losers
ggplot(monster.df, aes(x=as.numeric(Initiatory), fill=Performance)) +
  geom_histogram(bins=6, alpha=0.6, position = "identity") +
  xlim(0.5, 5.5) +
  geom_density(color = "blue", linewidth = 2, stat="count", fill=monster.df$Performance, alpha=0.6)

ggplot(data = monster.df, aes(x = as.numeric(Immutable), after_stat(density), fill = Performance)) +
  geom_histogram(bins=6, alpha = 0.8, position = 'identity') +
  geom_density(alpha = 0.30) +
  xlim(0.5,5.5)
  
###THIS DOES NOT WORK
ggplot(monsterWinners.df, aes(x=x)) +
  geom_histogram(data=data.frame(as.numeric(monsterWinners.df$Important)), bins=6, fill="pink") +
  xlim(0.5, 5.5) + 
  #geom_density(color = "blue", linewidth = 2, stat="count")


ggplot(monsterWinners.df, aes(x = Important, y = after_stat(density))) +
  geom_histogram(stat="count") +
  geom_density(color = "green", linewidth = 2, stat="count")

