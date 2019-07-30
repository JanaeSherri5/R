################################
# Introduction to R            #           
# Final Project                #                    
# Authors:                     #
# Melissa Hudson,              # 
# Janae Logan, and             #
# Mikia Thomas                 #
# Date: 7/18/2019              #
################################

#Import Dataset
LungCancer <- read.csv("CANCERDATA.csv")

#Descriptive Statistics
dim(LungCancer)
str(LungCancer)
summary(LungCancer)

#Rename Rural_Suburban_Urban variable and values
names(LungCancer)[names(LungCancer)=="Rural_Suburban_Urban"] <- "Region"
#Rename
LungCancer$Region1[LungCancer$Region=="R"] <- 1
LungCancer$Region1[LungCancer$Region=="S"] <- 2
LungCancer$Region1[LungCancer$Region=="U"] <- 3

#Rename Stage Values
LungCancer$Stage1[LungCancer$Stage == "I"] <- 1
LungCancer$Stage1[LungCancer$Stage == "II"] <- 2
LungCancer$Stage1[LungCancer$Stage == "III"] <- 3
LungCancer$Stage1[LungCancer$Stage == "IV"] <- 4
LungCancer$Stage1[LungCancer$Stage == "OCCULT"] <- 5
LungCancer$Stage1[LungCancer$Stage == "UNK"] <- 6
LungCancer$Stage1[LungCancer$Stage == "NA"] <- 99

#Correlation Test
cor(LungCancer[,c(19,1,3,5,7,13,14,16,19,20)], method="pearson", use="complete")
pairs(~Stage1+Zip+Histo+Age+Race+Sex+TobaccoHistory+FamHX+Region1, data=LungCancer, main="Simple Scatterplot Matrix")

#Find if each quantitative variable is statistically significant to Stage
cor.test(LungCancer$Stage1, LungCancer$Zip, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Histo, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Age, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Race, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Sex, use="complete")
cor.test(LungCancer$Stage1, LungCancer$TobaccoHistory, use="complete")
cor.test(LungCancer$Stage1, LungCancer$FamHX, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Region1, use="complete")

#1. Does the different age groups affect the stage of cancer?
#Using ANOVA for since Age.Group variable has 8 levels
lc.aov <- aov(LungCancer$Stage1 ~ LungCancer$Age.Group)
anova(lc.aov)

TukeyHSD(lc.aov)
colors<-c("Yellow", "Blue", "Green", "Red", "Orange", "Grey", "Violet")
boxplot(LungCancer$Stage1~LungCancer$Age.Group, col=colors, main = "Figure 1: Plot of Lung Cancer Stage and Age Group", xlab = "Age Group", ylab="Lung Cancer Stage")

#2. How does the family history of an individual affect the stages of cancer?
#Using ANOVA for more than 2 levels
lc1.aov <- aov(LungCancer$Stage1~LungCancer$FamHx_Desc)
anova(lc1.aov)

TukeyHSD(lc1.aov)
colors<-c("Yellow", "Red", "Orange", "Blue", "Grey", "Violet", "Green")
boxplot(LungCancer$Stage1~LungCancer$FamHx_Desc, col=colors, main = "Figure 2: Plot of Lung Cancer Stage and Family History", xlab = "Family History", ylab="Lung Cancer Stage")

#3. How does tobacco history affect various lung cancer stages?
#Using ANOVA 
lc2.aov <- aov(LungCancer$Stage1~LungCancer$Tobacco_Desc)
anova(lc2.aov)

TukeyHSD(lc2.aov)
colors<-c("Yellow", "Red", "Orange", "Blue", "Grey", "Violet", "Green")
boxplot(LungCancer$Stage1~LungCancer$Tobacco_Desc, col=colors, main = "Figure 3: Plot of Lung Cancer Stage and Tobacco Description", xlab = "Tobacco Description", ylab="Lung Cancer Stage")