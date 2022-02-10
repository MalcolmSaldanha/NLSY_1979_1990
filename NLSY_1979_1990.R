library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
# Importing the data set
NLSY <- read.csv("NLSY1979_1990.csv")
View(NLSY)

# Data cleaning process

# Creating tables for certain columns
table1 <- data.table(NLSY[c("YEAR_OF_BIRTH","COUNTRY_OF_BIRTH","SAMPLE_RACE",
                              "SAMPLE_SEX","HAVING_HEALTHPLAN",
                                          "EVER_IN_POVERTY","JOBSNUM_")])
View(table1)

# Cleaning ( Remove Numerical and Character values)
table1$YEAR_OF_BIRTH <- gsub("[^0-9\\.]", "",table1$YEAR_OF_BIRTH)
table1$COUNTRY_OF_BIRTH <- gsub("\\d", "", table1$COUNTRY_OF_BIRTH)
table1$SAMPLE_RACE <- gsub("\\d", "", table1$SAMPLE_RACE)
table1$SAMPLE_SEX<- gsub("\\d", "", table1$SAMPLE_SEX)
table1$HAVING_HEALTHPLAN <- gsub("\\d", "", table1$HAVING_HEALTHPLAN)
table1$EVER_IN_POVERTY <- gsub("\\d", "", table1$EVER_IN_POVERTY)
table1$JOBSNUM_ <- gsub("[^0-9\\.]", "", table1$JOBSNUM_)

# Cleaning ( Remove Special Character values)
table1$YEAR_OF_BIRTH <- gsub("[[:punct:]]", "",table1$YEAR_OF_BIRTH)
table1$COUNTRY_OF_BIRTH <- gsub("[[:punct:]]", "", table1$COUNTRY_OF_BIRTH)
table1$SAMPLE_RACE <- gsub("[[:punct:]]", "", table1$SAMPLE_RACE)
table1$SAMPLE_SEX<- gsub("[[:punct:]]", "", table1$SAMPLE_SEX)
table1$HAVING_HEALTHPLAN <- gsub("[[:punct:]]", "", table1$HAVING_HEALTHPLAN)
table1$EVER_IN_POVERTY <- gsub("[[:punct:]]", "", table1$EVER_IN_POVERTY)
table1$JOBSNUM_ <- gsub("[[:punct:]]", "", table1$JOBSNUM_)

# Cleaning ( Replace empty character values with None and Numberical values with 0 )
table1$YEAR_OF_BIRTH[table1$YEAR_OF_BIRTH == ""] <- 0
table1$COUNTRY_OF_BIRTH[table1$COUNTRY_OF_BIRTH == ""] <- "NONE"
table1$SAMPLE_RACE[table1$SAMPLE_RACE == ""] <- "NONE"
table1$SAMPLE_SEX[table1$SAMPLE_SEX == ""] <- "FEMALE"
table1$HAVING_HEALTHPLAN[table1$HAVING_HEALTHPLAN == ""] <- "No"
table1$EVER_IN_POVERTY[table1$EVER_IN_POVERTY == ""] <- "Yes"
table1$JOBSNUM_[table1$JOBSNUM_ == ""] <- 0

table1$JOBSNUM_ <- as.numeric(table1$JOBSNUM_)
table1$YEAR_OF_BIRTH <- as.numeric(table1$YEAR_OF_BIRTH)

View(table1)

# Visualization
hist(table1$JOBSNUM_,
     breaks = 31,
     col = terrain.colors(24),
     ylim = c(0,1000),
     xlab = "Number of Jobs",
     main = "Histogram of Number of Jobs")

hist(table1$YEAR_OF_BIRTH,
     col = terrain.colors(7),
     breaks = 5,
     xlab = "Year of Birth",
     main = "Histogram of Birth Year")

boxplot(table1$YEAR_OF_BIRTH ~ table1$COUNTRY_OF_BIRTH,
        main="Box plot of Birth year with Country of Birth",
        col.main = "red",
        xlab="Country of Birth",
        ylab="Frequency",
        cex.main=1, 
        cex.lab=1, 
        col.lab = "blue",
        col = c("#DA7B7B", "#9D8FF3", "#3113F3", "#3BBD59"),
        las=1,
        frame.plot=FALSE,
        boxwex = 0.6)

boxplot(table1$YEAR_OF_BIRTH ~ table1$SAMPLE_RACE,
        main="Box plot of Birth year with Race of the person",
        col.main = "red",
        xlab="Race",
        ylab="Frequency",
        cex.main=1, 
        cex.lab=1, 
        col.lab = "green",
        col = c("#DA7B7B","#3113F3", "#3BBD59"),
        las=1,
        frame.plot=FALSE,
        boxwex = 0.6)

boxplot(table1$YEAR_OF_BIRTH ~ table1$SAMPLE_SEX,
        main="Box plot of Birth year with Sex of the person",
        col.main = "red",
        xlab="Sex of the person",
        ylab="Frequency",
        cex.main=1, 
        cex.lab=1, 
        col.lab = "lightblue",
        col = c("#DA7B7B","#3113F3", "#3BBD59"),
        las=1,
        frame.plot=FALSE,
        boxwex = 0.6)

boxplot(table1$YEAR_OF_BIRTH ~ table1$HAVING_HEALTHPLAN,
        main="Box plot of Birth year with No. of people with Health Plan",
        col.main = "red",
        xlab="Health Plan",
        ylab="Frequency",
        cex.main=1, 
        cex.lab=1, 
        col.lab = "black",
        col = c("#DA7B7B", "#3BBD59"),
        las=1,
        frame.plot=FALSE,
        boxwex = 0.6)

boxplot(table1$YEAR_OF_BIRTH ~ table1$EVER_IN_POVERTY,
        main="Box plot of Birth year with Poverty Count",
        col.main = "red",
        xlab="Poverty",
        ylab="Frequency",
        cex.main=1, 
        cex.lab=1, 
        col.lab = "cyan",
        col = c("#DA7B7B", "#3BBD59"),
        las=1,
        frame.plot=FALSE,
        boxwex = 0.6)

#------------------------------------------------------------------------------
a.tb <- table(table1$EVER_IN_POVERTY)
barplot(a.tb, 
        main="Bar plot of Poverty",
        xlab="Frequency",
        ylab = "Poverty",
        border = "red",
        las = 1,
        col = c("Green","Yellow"),
        horiz = TRUE)  

b.tb <- table(table1$COUNTRY_OF_BIRTH)
par(mar = c(9,10,6,3))
barplot(b.tb, 
        main="Bar plot of Country of Birth",
        xlab="Frequency",
        border = "red",
        las = 1,
        col = c("Green","Yellow"),
        horiz = TRUE)

c.tb <- table(table1$SAMPLE_RACE)
par(mar = c(9,14,6,3))
barplot(c.tb, 
        main="Bar plot of Race",
        xlab="Cylinders per car",
        border = "red",
        las = 1,
        col = c("Green","Yellow","LightBlue"),
        horiz = TRUE)

d.tb <- table(table1$SAMPLE_SEX)
par(mar = c(8,8,6,3))
barplot(d.tb, 
        main="Bar plot of Sex",
        xlab="Frequency",
        border = "red",
        las = 1,
        col = c("Green","Yellow"),
        horiz = TRUE)

e.tb <- table(table1$HAVING_HEALTHPLAN)
par(mar = c(8,8,6,3))
barplot(e.tb, 
        main="Bar plot of People with Health Plan",
        xlab="Frequency",
        ylab = "Health Plan",
        border = "red",
        las = 1,
        col = c("Green","Yellow"),
        horiz = TRUE)
