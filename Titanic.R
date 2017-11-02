train <- read.table("train.csv", h=TRUE, sep=",")
test <- read.table("test.csv", h=TRUE, sep=",")
full <-rbind(train[,-2],test)
#stats
str(full)



#кол-во частот
table(train$Survived)
prop.table(table(train$Survived))

classes <- table(train$Pclass)
classes
prop.table(classes)

#tabl
table(train$Sex, train$Survived)
crosstable <- table(train$Sex, train$Survived)
crosstable
mosaicplot(crosstable)

table(train$SibSp, train$Survived)
crosstable1 <- table(train$SibSp, train$Survived)
crosstable1
mosaicplot(crosstable1)

table(train$Age, train$Survived)
crosstable2 <- table(train$Age, train$Survived)
crosstable2
mosaicplot(crosstable2)

mean(train$Age)
mean(train$Age, na.rm = TRUE)
train[3,]

mean(train$Age[train$Survived==0],na.rm = TRUE)


colSums(is.na(full))
table(train$Embarked)
train$Embarked[train$Embarked==""]="S"
table(train$Embarked)
clear_train<- train[complete.cases(train),]
cor(clear_train$Fare, clear_train$Pclass)

#type

str(train)
apply(train,2,function(x) length(unique(x)))

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

install.packages("ggplot2")
library(ggplot2)
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar()
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar(position = "fill")
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar()+facet_wrap(~Sex)
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar(position = "fill") + facet_wrap(~Sex)
ggplot(train, aes(Embarked, fill=Survived)) + geom_bar()
ggplot(train, aes(Embarked, fill=Survived)) + geom_bar(position = "fill")

split <- runif(dim(train)[1])>0.2
train_set<-train[split,]
test_set<-train[!split,]

#tree
library(rpart)
fit_DT<-rpart(Survived~Sex+Pclass, data=train, method="class")
fit_DT
plot(fit_DT)
text(fit_DT)