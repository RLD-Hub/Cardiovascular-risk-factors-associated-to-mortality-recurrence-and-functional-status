#We install the packages necessaries to run our data visualizations
#install.packages("ggplot2",type = "binary",dependencies = TRUE,repos = "https://cloud.r-project.org")
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(treemap)
library(rpart)
library(rpart.plot)
getwd( ) 
#We keep the database of the studied pacients
Data<-read.table("C:/Users/Ricardo Lopez Dawn/Desktop/R studio scripts/Jon/Base EVC.txt", header = TRUE, sep = "", dec = ".")
Data_aux <- Data[,-19]
Data_aux <- Data_aux[,-1]
df <- dplyr::select_if(Data_aux, is.numeric)
r <- cor(df, use="complete.obs")
round(r,2)
#WE plot the correlation matrix of the variables
ggcorrplot(r)
#We print the name of the variables
names(Data_aux)
#We keep the variables to study
Data_aux.Mortalidad<-Data_aux[,21]
Data_aux.Recurrencia<-Data_aux[,22]
Data_aux.Rankin<-Data_aux[,23]
#We plot the regression tree of the Mortality
fit <- rpart(Data_aux.Mortalidad ~ ., data=Data_aux[,1:20], method = "anova")
print(fit)
rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#We plot the regression tree of the Recurrence
fit2 <- rpart(Data_aux.Recurrencia ~ ., data=Data_aux[,1:20], method = "anova")
print(fit2)
rpart.plot(fit2, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#We plot the regression tree of the Rankin based on classes
fit3 <- rpart(Data_aux.Rankin ~ ., data=Data_aux[,1:20], method = "class")
print(fit3)
rpart.plot(fit3, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#We run the logistic regression analysis on the Mortality
model1 <- glm(Data_aux.Mortalidad ~ ., data=Data_aux[,1:20], family = binomial)
summary(model1)$coef
sink(file = "Mortalidad.txt")
summary(model1)
#We run the logistic regression analysis on the Recurrence
model2 <- glm(Data_aux.Recurrencia ~ ., data=Data_aux[,1:20], family = binomial)
summary(model2)$coef
sink(file = "Recurrencia.txt")
summary(model2)
#We run the logistic regression analysis on the Rankin
model3 <- glm(Data_aux.Rankin ~ ., data=Data_aux[,1:20], family = poisson)
summary(model3)$coef
sink(file = "Rankin.txt")
summary(model3)