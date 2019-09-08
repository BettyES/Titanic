library(tidyverse)
library(data.table)
library(reshape2)
source("Titanic_Functions.R")

titanic_train = read_csv("../Data/raw/train.csv")
titanic_test = read_csv("../Data/raw/test.csv")

summary(titanic_train)

titanic_train = replace_NAs(titanic_train)

titanic_train = transform_columns(titanic_train)

titanic_test = replace_NAs(titanic_test)

titanic_test = transform_columns(titanic_test)
titanic_test = subset(titanic_test,select=c(Name, Age, Sex, Embarked, Pclass, Fare, SibSp, Parch))

write_csv(titanic_train, "../Data/interim/titanic.csv")
write_csv(titanic_test, "../Data/interim/titanic_test.csv")

#data exploration

ggplot(titanic_train)+geom_histogram(aes(Age,fill=Survived),color = "white",position="dodge")+facet_wrap(~Sex)
ggplot(titanic_train)+geom_histogram(aes(Pclass,fill=Survived),color = "white",stat="count",position="dodge")
ggplot(titanic_train)+geom_histogram(aes(Sex,fill=Survived),color = "white",stat="count",position="dodge")
ggplot(titanic_train)+geom_histogram(aes(Embarked,fill=Survived),color = "white",stat="count",position="dodge")
ggplot(titanic_train)+geom_boxplot(aes(as.factor(Sex),Fare,fill=Survived))
ggplot(titanic_train)+geom_density(aes(Fare,fill=Survived),alpha = 0.5)

#prediction

#logistic regression
model<-glm(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train, family = "binomial")
pred<-predict(model,titanic_test)
mP = mean(pred,na.rm = TRUE)
pred = ifelse(pred>=mP,1,0)

gendercl$pred = pred
gendercl = na.omit(gendercl)

cor(gendercl$pred,gendercl$Survived)

#regression tree
library(rpart)
rmodel = rpart(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train, method = 'class')
pred = predict(rmodel, titanic_test,method = 'class')

result = ifelse(pred[,1]>0.5,0,1)

gendercl<-read.csv("../Data/raw/gender_submission.csv")
gendercl$res = result

cor(gendercl$res,gendercl$Survived)

#conditional inference tree
library("party")
model<-ctree(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train)
pred<-predict(model,titanic_test)
plot(model)
print(model)

gendercl<-read.csv("../Data/raw/gender_submission.csv")
cor(as.numeric(pred)-1,gendercl$Survived)


min(titanic_test$Fare[titanic_test$Pclass==3])
summary(titanic_test$Fare[titanic_test$Pclass==3])
summary(titanic_test$Fare[titanic_test$Pclass==2])
summary(titanic_test$Fare[titanic_test$Pclass==1])
summary(titanic_test$SibSp[titanic_test$Age>9])
summary(titanic_test$Parch)

