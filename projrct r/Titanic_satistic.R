install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("effectsize")
install.packages("lsr")
install.packages("nhstplot")


library(dplyr)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(effectsize)
library(lsr)
library(nhstplot)

Titanic <- read.csv("Titanic.csv", na.strings="")
#We will check if there are any NA in the Age variable
sum(is.na(Titanic$Age))
#We'll check the median age and replace the NA with it.
medianAge <- median(Titanic$Age,na.rm = TRUE)
Titanic[is.na(Titanic$Age),"Age"] <- medianAge

sum(is.na(Titanic$Age))
#We'll check if there any NA in the Embarked variable
sum(is.na(Titanic$Embarked))
#We'll replace it with 's' variable becouse it the most frequent one.
Titanic[is.na(Titanic$Embarked == TRUE),"Embarked"] <- "S"
sum(is.na(Titanic$Embarked))

#Question 1

# Filtering the data by the class level.

class <- Titanic %>% select(Age, Pclass)
class_1_age <- filter(class, Pclass=="1")
class_2_age <- filter(class, Pclass=="2")
class_3_age <- filter(class, Pclass=="3")

# we will calculate the mean of each variable

one <- mean(class_1_age$Age)
two <- mean(class_2_age$Age)
three <- mean(class_3_age$Age)

# checking assumptions

ggqqplot(class_1_age$Age)
ggqqplot(class_2_age$Age)
ggqqplot(class_3_age$Age)
shapiro.test(class_1_age$Age)
shapiro.test(class_2_age$Age)
shapiro.test(class_3_age$Age)
bartlett.test(Age~Pclass, Titanic)

# we will preform an anova test

f_test <- aov(Pclass ~ Age, data = class)
summary(f_test)

# plot

ggboxplot(data=Titanic, x = "Pclass", y = "Age", color = "Pclass", ylab = "Age", xlab = "Pclass", shape= 50)+
  stat_summary(geom = "point", fun = "mean", color = "black", size = 1.25)+
  ggtitle("Age means according to class")

#Cheking effect size
etaSquared(f_test)

#Question 2

#Creating a new varibale "Family size"

Titanic <- mutate(Titanic ,Familysize= (Titanic$SibSp+Titanic$Parch) )

#We'll check pearson corrlation between the familly size and the fare price.
cor(Titanic$Familysize, Titanic$Fare)

#Cheking normal distribution for each variable
ggqqplot(Titanic$Familysize)
ggqqplot(Titanic$Fare)
shapiro.test(Titanic$Fare)
shapiro.test(Titanic$Familysize)

#Runing the Pearson's Correlation

cor.test(Titanic$Familysize,Titanic$Fare, method = "pearson")


ggscatter(Titanic, "Fare", "Familysize", add = "reg.line",
          conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson",  fill = "red",
          shape = 21,size = 5,palette = c("black", "red"))

#Question 3
# see the embarked numbers
table(Titanic$Embarked)

# do the chi test
chisq.test(Titanic$Embarked, Titanic$Pclass)
chisq <- chisq.test(Titanic$Embarked, Titanic$Pclass)
#check if Expected N >/= 5
chisq$expected

#plot
plotchisqtest(chisq, theme = "blueandred")

ggplot(data = Titanic,aes(x= Pclass, fill= Embarked))+
  geom_bar(stat="count", position="dodge") +
  labs(x = 'Class') + ggtitle("class and Emabrked")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Question 4

# Filtering the data according to Sex and Fares
#Creating a new varibale "Agecut" 
Titanic <- mutate(Titanic ,Agecut= cut(Age, breaks = c(0,18,50,150), include.lowest = TRUE, labels = c("Child","Adult", "Senior")))
fe <- Titanic %>% select(Fare, Sex, Agecut)
fare_female <- filter(fe, Sex=="female"& Agecut=="Adult")
fare_male <- filter(fe, Sex=="male"& Agecut=="Adult")

# we will calculate the mean of each variable

f <- mean(fare_female$Fare)
m <- mean(fare_male$Fare)

# checking assumptions

ggqqplot(fare_female$Fare)
ggqqplot(fare_male$Fare)
shapiro.test(fare_female$Fare)
shapiro.test(fare_male$Fare)
var.test(fare_female$Fare, fare_male$Fare)

# we will preform a t.test

t.test(fare_female$Fare, fare_male$Fare, alternative = "two.sided", var.equal = FALSE)

# plot
          
 ggplot(data=fe, aes(x=Sex, y=Fare, fill= Sex))+geom_violin(width=1)+ geom_boxplot(width=0.1, color="black", alpha=0.2)+ ggtitle("Fare distribution by sex")+ coord_cartesian(ylim = c(0, 300))+
   stat_summary(geom = "point", fun = "mean", color = "purple", size = 1.25)
          
#Cheking effect size
 cohens_d(fare_female$Fare, fare_male$Fare, fe, pooled_sd = TRUE, paired = FALSE)
 
#Question 5
 #creating a new data frame
 Titanic_Age_Survived <- data.frame(Titanic$Age,Titanic$Survived)
 colnames(Titanic_Age_Survived) <-  c("Age","Survived")
 Titanic_Age_Survived$Survived[Titanic_Age_Survived$Survived==1] <- "Yes"
 Titanic_Age_Survived$Survived[Titanic_Age_Survived$Survived==0] <- "No"
 #Cheking the Age mean diffrence 
 mean_diff <- mean(Titanic_Age_Survived$Age[Titanic_Age_Survived$Survived=="No"])- mean(Titanic_Age_Survived$Age[Titanic_Age_Survived$Survived=="Yes"])
 
 Titanic_shuffled <- Titanic_Age_Survived
 Titanic_shuffled$Survived <- shuffel_survived
 shuffel_survived <- sample(Titanic_Age_Survived$Survived)
 permutation <- 10000
 perm.diff <- mean(Titanic_shuffled[Titanic_shuffled$Survived=="No"])- mean(Titanic_shuffled$Age[Titanic_shuffled$Survived=="Yes"])
 perm.diff <- rep(0, permutation)
 
 #Running the premutation
  for(i in  1:permutation) {
   shuffel_survived <- sample(Titanic_Age_Survived$Survived)
   Titanic_shuffled$Survived <- shuffel_survived
   perm.diff[i] <- mean(Titanic_shuffled$Age[Titanic_shuffled$Survived=="No"])- mean(Titanic_shuffled$Age[Titanic_shuffled$Survived=="Yes"])
 }
 
 p.value <- sum(abs(perm.diff)>abs(mean_diff))/permutation
 p.value
 
 #plot
 
 ggplot() + aes(perm.diff)+ geom_histogram(binwidth=0.3, colour="black", fill="blue")+ geom_vline(xintercept = mean_diff, color = "red", size=1.5)+ ggtitle("Permutation distribution age and survival")+geom_vline(xintercept = p.value, color = "yellow", size=1.5)
 
 