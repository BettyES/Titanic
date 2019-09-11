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
model<-glm(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train, family = binomial())
pred<-predict(model,titanic_test)
mP = mean(pred,na.rm = TRUE)
pred = ifelse(pred>=mP,1,0)

gendercl<-read.csv("../Data/raw/gender_submission.csv")
gendercl$pred = as.numeric(pred)
gendercl = na.omit(gendercl)
cor(gendercl$pred,gendercl$Survived)

gendercl$Survived_2 <- as.numeric(gendercl$Survived)
titanic_test = na.omit(titanic_test)

# Confusion matrix:
library(caret)
confusionMatrix(factor(gendercl$pred, levels = c(1,0)), 
  reference = factor(na.omit(gendercl$Survived), levels = c(1,0)))


# Simplified model:
model_2 <- glm(Survived ~Fare + Sex + Pclass,data=titanic_train, family = "binomial")

data_test <- na.omit(titanic_test)
lm.pred <- predict(model,data_test, type = "resp")
newdata = expand.grid(Fare = seq(0,200, 1), Sex = c("female", "male"), Pclass = c("1", "2", "3"))
lm.pred2 <- predict(model_2,newdata , type = "resp")

pclass.labs <- c("1st Class", "2nd Class", "3rd Class")
names(pclass.labs) <- c("1", "2", "3")


pl <- ggplot(newdata, aes(x = Fare, y= lm.pred2, group = Sex, color = Sex)) + geom_point(size = 1) + 
  facet_wrap(~Pclass, labeller = labeller(Pclass = pclass.labs)) + 
  xlab("Fare")  + ylab("Probability of survival") + scale_shape_discrete(name  ="Gender") + 
  scale_color_discrete(name = "Gender") + scale_y_continuous(breaks=seq(0, 1, .20)) + theme_bw() + 
  theme(axis.text = element_text(size = 14), axis.title=element_text(size=16,face="bold"), 
    legend.text = element_text(size=14), legend.title=element_text(size=16,face="bold"),
    strip.text.x = element_text(size = 14)
  )
pl
ggsave(filename = "../Data/mdl_logRegplot.png", pl,  width = 10, height = 8, dpi = 150, units = "in", device='png')

#regression tree
library(rpart)
rmodel = rpart(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train, method = 'class')
pred_rpart = predict(rmodel, titanic_test,method = 'class')

result = ifelse(pred_rpart[,1]>0.5,0,1)

gendercl<-read.csv("../Data/raw/gender_submission.csv")
gendercl$res = result

cor(gendercl$res,gendercl$Survived)

confusionMatrix(factor(gendercl$res, levels = c(1,0)), 
  reference = factor(na.omit(gendercl$Survived), levels = c(1,0)))

#conditional inference tree
library("party")
model<-ctree(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train)
pred<-predict(model,titanic_test)
rpart.plot(model)
print(model)

# Using rpart:

titanic_train2 <- as.data.table(titanic_train)
titanic_train2 <- titanic_train2[Survived ==0, Survived2 :="Died"]
titanic_train2 <- titanic_train2[Survived ==1, Survived2 :="Survived"]
titanic_train2[, Survived2:= as.factor(Survived2)]
mdl.rpart<-rpart(Survived ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train2)
rpart.plot(mdl.rpart, box.palette="RdBu", shadow.col="gray")

mdl.rpart2<-rpart(Survived2 ~Pclass+Sex+Age+Embarked+Fare+SibSp+Parch,data=titanic_train2)
saveRDS(mdl.rpart2, file = "../Data/mdl.rpart.rda")
mdl_plot <- rpart.plot(mdl.rpart2, box.palette=c("RdYlGn"), shadow.col="gray", extra = 106)
#ggsave(filename = "../Data/mdl_plot.png", mdl_plot, width = 10, height = 8, dpi = 150, units = "in", device='png')


rgendercl<-read.csv("../Data/raw/gender_submission.csv")
cor(as.numeric(pred)-1,gendercl$Survived)


min(titanic_test$Fare[titanic_test$Pclass==3])
summary(titanic_test$Fare[titanic_test$Pclass==3])
summary(titanic_test$Fare[titanic_test$Pclass==2])
summary(titanic_test$Fare[titanic_test$Pclass==1])
summary(titanic_test$SibSp[titanic_test$Age>9])
summary(titanic_test$Parch)


### PLOTS: Data exploration ####

titanic_train

# Plot to get the legend:
# ggplot(titanic_train) +  geom_histogram(aes(x = Age, y = ..count.., fill=Survived),
#   color = "white", position="dodge", bins = 10) + facet_wrap(~Sex) +  theme_bw() + 
#   theme(axis.text = element_text(size = 14), axis.title=element_text(size=16,face="bold"), 
#     legend.text = element_text(size=14), legend.title=element_blank(),
#     strip.text.x = element_text(size = 14), legend.position = "bottom", legend.background = element_rect(fill="white",
#       size=1, linetype="solid", colour ="grey"), legend.spacing.x = unit(1, 'cm'))+
#   scale_fill_manual(values=c("red", "#009E73"), labels = c("Died", "Survived"))

pl_age<- ggplot(titanic_train) +  geom_histogram(aes(x = Age, y = ..count.., fill=Survived),
  color = "white", position="dodge", bins = 10) + facet_wrap(~Sex) +  theme_bw() + 
  theme(axis.text = element_text(size = 14), axis.title=element_text(size=16,face="bold"),
    legend.position = "none", strip.text.x = element_text(size = 14)) + scale_fill_manual(values=c("red", "#009E73"))

ggsave(filename = "../Data/Age_plot.png", pl_age,  
  width = 10, height = 7, dpi = 150, units = "in", device='png')

pl_class <- ggplot(titanic_train) +  geom_histogram(aes(x = Pclass, fill=Survived),
  color = "white", position="dodge", stat = "count") + facet_wrap(~Sex) +  theme_bw() +  
  theme(axis.text = element_text(size = 14), axis.title=element_text(size=16,face="bold"), 
    legend.position = "none", strip.text.x = element_text(size = 14)) + 
  scale_fill_manual(values=c("red", "#009E73")) + xlab("Class")

ggsave(filename = "../Data/Class_plot.png", pl_class,  
  width = 11, height = 8, dpi = 150, units = "in", device='png')

pl_gender <- ggplot(titanic_train) +  geom_histogram(aes(x = Sex, fill=Survived),
  color = "white", position="dodge", stat = "count") +  theme_bw() +  
  theme(axis.text = element_text(size = 20), axis.title=element_text(size=18,face="bold"), 
    legend.position = "none", strip.text.x = element_text(size = 14),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + 
  scale_fill_manual(values=c("red", "#009E73")) + xlab("Gender")

ggsave(filename = "../Data/gender_plot.png", pl_gender,  
  width = 8, height = 6, dpi = 150, units = "in", device='png')

pl_harbour <- ggplot(titanic_train) +  geom_histogram(aes(x = Embarked, fill=Survived),
  color = "white", position="dodge", stat = "count") +  theme_bw() +  
  theme(axis.text = element_text(size = 16), axis.title=element_text(size=16,face="bold"), 
    legend.position = "none", strip.text.x = element_text(size = 14),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +  
    #axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_fill_manual(values=c("red", "#009E73")) + xlab("Harbour") + 
  scale_x_discrete(breaks=c("Q","S","C"), labels=c("Queenstown", "Southhampton", "Cherbourg"))

ggsave(filename = "../Data/harbour_plot.png", pl_harbour,  
  width = 10, height = 7, dpi = 150, units = "in", device='png')

