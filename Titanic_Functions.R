#Data Exploration



#Data cleaning

replace_NAs = function(x){
  x$Age[is.na(x$Age)] = mean(x$Age, na.rm=TRUE) 
  x$Age = as.integer(x$Age)
  x$Embarked[is.na(x$Embarked)] = "Q"
  return(x)
}

transform_columns = function(x){
  x$Pclass = as.factor(x$Pclass)
  if(any(names(x)=="Survived")==TRUE){
  x$Survived = as.factor(x$Survived)
  }
  x$Sex = as.factor(x$Sex)
  x$Embarked = as.factor(x$Embarked)
  x$SibSp = as.factor(x$SibSp)
  return(x)
}
