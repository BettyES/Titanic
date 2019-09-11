
# Welcome to the RM Data Science team! 
# Please have a go and try to complete the words missing in the code below.

# Instructions: 
# To run a line you can: 
  # 1) Copy and paste the line into the console below and then hit enter. 
  # 2) In the Source code, place the cursor on the line you want to run, press "cmd" + enter (the results will appear in the console)

##################################
#### Code #####


# Loading libraries that you will need to run this code!
  # the 3 libraries below allow us to manipulate data
library(tidyverse)  
library(reshape2)   
library(data.table)
  # the below libraries are related to modelling approaches we will use to estimate the models
library(caret)
library(rpart)
  # the below library is to generate plots 
library(ggplot2)
  # the below command loads all functions that are in "Titanic_Function.R" script
source("Titanic_Functions.R")

# Loading the data that you will need to run the code: 
titanic_train = read_csv("../Data/raw/train.csv")
titanic_test = read_csv("../Data/raw/test.csv")

# Now, we want to quickly have a look at how our data looks like:
# (substitute '---' for the correct word that should go there). 
 
  # train data:
-------(titantic_train)
  # test data:
summary(-------)

# to see first few rows, we can type: 
head(titanic_train)

# is there any Na? if so we need to process and clean our data:
titanic_train = replace_NAs(titanic_train)
titanic_train = transform_columns(titanic_train)

titanic_test = replace_NAs(titanic_test)
titanic_test = transform_columns(titanic_test)

# let's create some visualisation: 
# count of people by gender (i.e. gender column is named 'Sex') 
# change '----' for column name we want to plot

ggplot(titanic_train) +  geom_histogram(aes(x = -----), stat = "count")

# Did women survived in general more than men? Let's plot it:
# (hint: column that tells you whether the person has survived or not is called "Survived")
# (hint2: fill inside of geom_histogram will colour the categories different)
ggplot(titanic_train) +  geom_histogram(aes(x = Sex, fill = Survived), stat = "count")
  
# let's make the plot pretty: 
ggplot(titanic_train) +  geom_histogram(aes(x = Sex, fill=Survived),
  color = "white", position="dodge", stat = "count") +  
  theme_bw() +  # this changes the backfround of the plot to a white background
  theme(axis.text = element_text(size = 20), # this changes the text size for the axis elements
    axis.title=element_text(size=18,face="bold"),   # this changes the text size for the axis title
    legend.position = "none",  # this removes the legend
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), # separates the y axis title from the plot
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) + # separates the x axis title from the plot
  scale_fill_manual(values=c("red", "#009E73")) + # Specifies the exact colours to use to distinguish whether they have survived or not
  xlab("Gender") # changes the x-axis title to Gender

# let's try few things: 
# familiarise witht the above ggplot command 
  # 1) Add the legend back again. 
  # 2) Insted of gender, plot Cabin (column name is "Cabin")
  # 3) Change formating of the graph as you like. 


# let's estimate a model:
  # Since this is classification problem (the response output is either Yes or No survived) we need to use models
  # that are made for this purposes. For example: 
    # a) logistic regression will give you a probability ( a number between 0 and 1) of surviving. 
    # b) decision trees will label the output based on the characteristic of the input.

# Decision tree model 

  # train model with titanic_train data
rmodel = rpart(Survived ~ Pclass + Sex + Age + Embarked + Fare + SibSp + Parch, data=titanic_train, method = 'class')
  # Survived ~ Pclass + Sex + Age + Embarked + Fare + SibSp + Parch is the formulation of the model 
  # data is the data base where we are taking all that information 
  # method = class becuase this is a classification problem

#The model is stored in rmodel; you can see a summary of it by typing summary(rmodel) in the console
# to preduict with new data -i.e. titanic_test run the below command

pred_rpart = predict(rmodel, titanic_test, method = 'class')
# can you check on the console the first few numbers of pred_rpart?

# titanic_test doesn't contained the actual survived data so we need to load it:
output <- read.csv("../Data/raw/gender_submission.csv")
head(output)

# let's add the column with the predicitons:
output$pred = pred_rpart 
head(output) 

# Now let's evalute the model: 

confusionMatrix(data = output$pred, reference = output$Survived)



