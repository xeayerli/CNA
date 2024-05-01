# load packages
library(cna) 
library(dplyr)
library(summarytools) #for frequency descriptives for binary variables
library(ggplot2)
library(QCA)
library(QCApro)
library(SetMethods)
#library(frscore)
 
# Reading the CSV file into R
data <- read.csv("/Users/ariel/Desktop/Spring24/548_CNA/CNA_project/PISAdata_LingLab.csv")

#setting up some variables 
gender <- data$Gender
sibling <- data$OnlyChild #this makes more sense since 1 = yes, has siblings. 2 = no, I am an only child registry <- data$registry
registry <- data$Registry
boarding <- data$Boarding

PeerInfluence <- data$PeerInfluence
SelfMotivation <- data$SelfMotivation
PISAmath <- data$PISAmath
PISAread <- data$PISAread
PISAscience <- data$PISAscience

# Recode some variables
# Recode Gender: 1 (male) to 0 and 2 (female) to 1
gender <- ifelse(gender == 1, 0, 1)

# Recode OnlyChild: 1 (has siblings) to 0 and 2 (only child) to 1
sibling <- ifelse(sibling == 1, 0, 1)

# Recode Region: 1 (rural) to 0 and 2 (urban) to 1
registry <- ifelse(registry == 1, 0, 1)

# Recode Boarding: 1 (boarding) to 1 and 2 (non-boarding) to 0
boarding <- ifelse(boarding == 1, 1, 0)

#checking descriptives of the binary variables
freq(gender) #0 for male is 206 cases, 1 for female is 182 cases
freq(sibling) #0 for siblings is 320 cases, 1 for only child is 68 cases
freq(registry) #0 for rural is 251 cases, 1 for urban is 137 cases
freq(boarding) #0 for non boarding is 139 cases, 1 for boarding is 249 cases

# I want to recode the dataframe and have the 1s and 2s replaced with 0s and 1s
data$Gender <- ifelse(data$Gender == 1, 0, 1)  # Recoding Gender: 1 (male) to 0, 2 (female) to 1
data$OnlyChild <- ifelse(data$OnlyChild == 1, 0, 1)    # Recoding Only Child: 1 (has sibling) to 0, 2 (only child) to 1
data$Registry <- ifelse(data$Registry == 1, 0, 1)    # Recoding Registry: 1 (rural) to 0, 2 (urban) to 1
data$Boarding <- ifelse(data$Boarding == 2, 0, 1)    # Recoding Boarding: 2 (non-boarding) to 0, 1 (boarding) to 1

# Check if the data has been changed
head(data)

# Rename the columns 
names(data)[names(data) == "Gender"] <- "F"
names(data)[names(data) == "OnlyChild"] <- "OC"
names(data)[names(data) == "Registry"] <- "R"
names(data)[names(data) == "Boarding"] <- "BD"

#descriptive statistics for the numeric variables
summary(PeerInfluence)
summary(SelfMotivation)
summary(PISAmath)
summary(PISAread)
summary(PISAscience)

#trying to find the best way to turn these into binary by looking at the distribution
#boxplot(PeerInfluence, main="Boxplot", ylab="Peer Influence")
#boxplot(SelfMotivation, main="Boxplot", ylab="Self Motivation")
#boxplot(PISAmath, main="Boxplot", ylab="PISA Math Scores")
#boxplot(PISAread, main="Boxplot", ylab="PISA Reading Scores")
#boxplot(PISAscience, main="Boxplot", ylab="PISA Science Scores")

ggplot(data, aes(x = PeerInfluence)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(PeerInfluence)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(PeerInfluence)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_rug(sides = "b") + # Sides 'b' for bottom
  labs(title = "Density plot of Peer Influence", x = "Peer Influence")

ggplot(data, aes(x = SelfMotivation)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(SelfMotivation)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(SelfMotivation)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_rug(sides = "b") + # Sides 'b' for bottom
  labs(title = "Density plot of SelfMotivation", x = "SelfMotivation")

ggplot(data, aes(x = PISAmath)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(PISAmath)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(PISAmath)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_rug(sides = "b") + # Sides 'b' for bottom
  labs(title = "Density plot of PISAmath", x = "PISAmath")

ggplot(data, aes(x = PISAread)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(PISAread)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(PISAread)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_rug(sides = "b") + # Sides 'b' for bottom
  labs(title = "Density plot of PISAread", x = "PISAread")

ggplot(data, aes(x = PISAscience)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(PISAscience)), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(PISAscience)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_rug(sides = "b") + # Sides 'b' for bottom
  labs(title = "Density plot of PISAscience", x = "PISAscience")

#Using the method from demo
#Xplot(data$SelfMotivation, jitter= TRUE, cex =0.8)
#summary(data$SelfMotivation) 

# Next I want to dichotomize the numeric variables.
# also create new factors
# For PI (PeerInfluence), dichotomize by median
data$PeerInfluence <- ifelse(data$PeerInfluence >= median(data$PeerInfluence), 1, 0)

# For SM (SelfMotivation), dichotomize by median
data$SelfMotivation <- ifelse(data$SelfMotivation >= median(data$SelfMotivation), 1, 0)

# For PM (PISAmath), dichotomize by mean
data$PISAmath <- ifelse(data$PISAmath >= mean(data$PISAmath), 1, 0)

# For PR (PISAread), dichotomize by median
data$PISAread <- ifelse(data$PISAread >= median(data$PISAread), 1, 0)

# For PS (PISAscience), dichotomize by mean
data$PISAscience <- ifelse(data$PISAscience >= mean(data$PISAscience), 1, 0)

# Rename the rest of the columns 
names(data)[names(data) == "PeerInfluence"] <- "PINF"
names(data)[names(data) == "SelfMotivation"] <- "SMOT"
names(data)[names(data) == "PISAmath"] <- "MATH"
names(data)[names(data) == "PISAread"] <- "READ"
names(data)[names(data) == "PISAscience"] <- "SCIENCE"

#removing extra column and writing the clean dataset into a new one
data <- data[,-1]  # Removes the first column, which is 'ID'

# Save the dataset without the 'ID' column to a new CSV file
write.csv(data, "/Users/ariel/Desktop/Spring24/548_CNA/CNA_project/PISAdata_clean.csv", row.names = FALSE)

# Load the new CSV file into R as 'data_clean'
data_clean <- read.csv("/Users/ariel/Desktop/Spring24/548_CNA/CNA_project/PISAdata_clean.csv")


# Checking the distribution of the subjects
table(data_clean$F)
table(data_clean$OC) #note that there was much more non-only child subjects than only child
#In a separate analysis, I can examine just the only child subjects
table(data_clean$R)
table(data_clean$BD)

# checking the calibrated factors
skew.check(data = data_clean$PINF)
skew.check(data = data_clean$SMOT)
skew.check(data = data_clean$MATH)
skew.check(data = data_clean$READ)
skew.check(data = data_clean$SCIENCE)

# Generating a configuration table for data_clean
?configTable
cs_config <- configTable(data_clean, type = "cs")
cs_config

#checking skewness for whole dataset
SetMethods::skew.check(data_clean)

# Testing dependencies
# exclude the MATH, READ and SCIENCE columns and focus only on the factors.
# Exclude the last three columns of data_clean 
data_exogenous <- data_clean[, -((ncol(data_clean)-2):ncol(data_clean))]

# Conduct the dependency analysis using msc/cna, excluding the outcome variables
deptest <- msc(cna(data_exogenous, con = .8, cov = .8))

# Extract the results with a complexity of 1 to look at mutual dependencies
subset(deptest, complexity == 1) # yielded nothing
subset(deptest, complexity == 2)
subset(deptest, complexity == 3)
subset(deptest, complexity == 4) #none
subset(deptest, complexity == 5) #none
subset(deptest, complexity == 6) #none

#msc routine - factor selection/data reduction
#maxstep argument
#where 'outcome = ' sets the outcome

#where maxstep = 
#default: c(3, 4, 10). 
#c(max conjuncts, max disjuncts, max complexity/#of factors)
#i = max i conjuncts
#j = max j disjunts
#k = max complexity/# of factors
#where 'con.msc = ' sets the consistency level

# First, when high MATH score is the outcome.
cs_config_outcomeMATH <- cs_config[, !colnames(cs_config) %in% c("READ", "SCIENCE")]
cna_MATH_cs <- cscna(cs_config_outcomeMATH,outcome="MATH", maxstep = c(4, 4, 5),con.msc = 0.7)
msc(cna_MATH_cs)
CNA_MATH_output <- data.frame(msc(cna_MATH_cs))
write.csv(CNA_MATH_output,"CNA_MATH_output.csv")

#MATH
cna_MATH <- cscna(cs_config_outcomeMATH, outcome = "MATH", strict=F, con=0.7, cov = 0.7) 
cna_MATH

#putting the SCIENCE, READ back
cs_config <- configTable(data_clean, type = "cs")

# Examining when high reading score is the outcome
cs_config_READ <- cs_config[, !colnames(cs_config) %in% c("MATH", "SCIENCE")]
cna_READ_cs <- cscna(cs_config_READ,outcome="READ", maxstep = c(4, 4, 5),con.msc = 0.8)
msc(cna_READ_cs)

CNA_READ_output <- data.frame(msc(cna_READ_cs))
write.csv(CNA_READ_output,"CNA_READ_output.csv")


# Examining when high SCIENCE score is the outcome
cs_config_SCIENCE <- cs_config[, !colnames(cs_config) %in% c("MATH", "READ")]
cna_SCIENCE_cs <- cscna(cs_config_SCIENCE,outcome="SCIENCE", maxstep = c(4, 4, 5),con.msc = 0.8)
msc(cna_SCIENCE_cs)

CNA_SCIENCE_output <- data.frame(msc(cna_SCIENCE_cs))
write.csv(CNA_SCIENCE_output,"CNA_SCIENCE_output.csv")

