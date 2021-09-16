#*************************************************************
#### ASSIGNMENT 1  #####
##  DATA MINING TECHNIQUES
#*************************************************************
## GROUP 60 
## Cristina Buerbaum del Río (2704514)
## Paola Sastre Dordelly (2716547)
## Andrés Romero Herrera (2716340)


#### Our code was inspired on the Kaggle Kernels of: Megan L. Risdal, Landon Wall, BrianTremaine.

### 0. HOUSEKEEPING ####
#***********************

## 0.1 Clear session ####

rm(list = ls())                       # Clear previous environment
if(!is.null(dev.list())) dev.off()    # Clear previous plots

## 0.2 Set working directory ####

setwd("~/Desktop/titanic 2")

## 0.3 Download needed packages iff they are not installed yet  ####

pckg = c("readxl","dplyr","ggplot2","tidyverse", "forecast",
         "xts","aTSA", "urca", "tseries", "stats", "vars",
         "tsDyn", "dynlm", "cointReg", "ecm", "lubridate", 
         "mice", "randomForest", "aod", "caret", "caretEnsemble",
         "Amelia", "GGally", "rpart", "e1071", "klaR", "patchwork", 
         "caTools", "RATTLE") 

is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 
for(i in 1:length(pckg)) {
  if (!is.installed(pckg[i])){
    install.packages(pckg[i])
  }
}
lapply(pckg, require, character.only = TRUE)     # Load libraries 

## 0.4 Read excel file with required data ####

test = read.csv("test.csv", header = TRUE) 
train = read.csv("train.csv", header = TRUE)
data  = bind_rows(train, test) # test + train

# Quick view of the data
summary(data)
str(data)

#Quick view of the train
summary(train)
str(train)


## 1. Exploratory Analysis ####
## 1.1 Age ####
plot1 = ggplot() +
  geom_histogram(data=train, 
                 aes(Age, fill= factor(Survived))) +
  ggtitle("Age and Survival Rates")+
  theme_classic()
plot1

## 1.2 Sex ####
plot2= ggplot() + 
  geom_bar(data=train, 
                  aes(Sex, fill= factor(Survived))) +
  ggtitle("Gender and Survival")+
  theme_classic()
plot2

## 1.3 Sex and Age ####
plot3 = ggplot() +
  geom_histogram(data=train, 
                 aes(Age, fill= factor(Survived))) +
  facet_grid(.~Sex)+
  ggtitle("Age, Gender and Survival")+
  theme_classic()
plot3

## 1.4 Sex and Pclass#### 
plot4= ggplot() + 
  geom_bar(data=train, 
           aes(Sex, fill= factor(Survived))) +
  facet_grid(.~Pclass)+
  ggtitle("Gender, Passanger Class and Survival")+
  theme_classic()
plot4

## 1.5 Sex and Pclass (Percentage) #### 
plot5= ggplot() + 
  geom_bar(data=train, 
           aes(Sex, fill= factor(Survived)),
           position = "fill") +
  facet_grid(.~Pclass)+
  ggtitle("Gender, Passanger Class and (%)Survival")+
  theme_classic()
plot5

## 1.6 Age, Gender, Sex and Pclass (Percentage) #### 
plot6= ggplot(train, aes(x=Age, y=Sex)) + 
  geom_jitter(aes(color= factor(Survived)))+
  facet_wrap(.~Pclass)+
  ggtitle("Age, Gender, Plass and Survival")+
  theme_classic()
plot6

## 1.7 All plots together
(plot1 | plot2| plot3) /
  (plot5 | plot7| plot6)

## 2. Correlation between variables ###
aux = train %>%
  dplyr::select(Survived, Pclass, Sex, Fare, Age)

aux$Survived  = as.numeric(aux$Survived)
aux$Pclass    = as.numeric(aux$Pclass)
aux$Sex       = as.numeric(aux$Sex)
aux$Fare      = as.numeric(aux$Fare)
aux$Age       = as.numeric(aux$Age)

aux=na.omit(aux)
res = cor(aux)
round(res, 2)

### 3 Transformations and Imputations ####
## 3.0 Check how many missing values are in each category ####
missing = data %>% summarize_all(funs(sum(is.na(.))))
missing #there is: 1 missing value for fare, 2 missing values for embarked (doesnt appear like NA) and 263 missing values for age.

## 3.1 Fare
aux = data %>% filter(Pclass == 3) %>% filter(!is.na(Fare))
data[1044,10] = mean(aux$Fare)

## 3.2 Embarked 
## Since the majority of the people were in Port S, assign it to the passengers
data$Embarked[c(62, 830)] = 'S'

## Make Numeric for Modeling
data$Embarked = str_replace_all(as.character(data$Embarked), 
                                c("S" = "0",
                                  "Q" = "1",
                                  "C" = "2"))

data %>% count(Embarked) #double check for missing values

## Additional feature engenieering ####

## 3.3.1 Title ####
# New column with titles
data$Title = gsub('(.*, )|(\\..*)', '', data$Name)
table(data$Sex, data$Title)

# Lets make the titles a bit more homogeneous
data$Name = as.character(data$Name)
officers = c('Capt', 'Col', 'Dr', 'Major', 'Rev')
nobles = c('Lady', 'the Countess','Sir', 'Jonkheer')

data$Title[data$Title == 'Mlle'] = 'Miss' 
data$Title[data$Title == 'Ms'] ='Miss'
data$Title[data$Title == 'Mme'] ='Mrs'
data$Title[data$Title == 'Dona'] ='Mrs'
data$Title[data$Title == 'Don'] ='Mr'
data$Title[data$Title %in% nobles] = 'Noble'
data$Title[data$Title %in% officers] = 'Officer'

ll = strsplit(data$Name, ',')
data$lastname = sapply(ll, function(x) x[1])

# Graph Title
aux = data %>% na.omit(Title)
plot7= ggplot() + 
  geom_bar(data=aux, 
           aes(Title, fill= factor(Survived)),
           position = "fill") +
  ggtitle("Title and (%) Survival")
plot7

## 3.3.2 Family Size ####
# Family Size
data$Fsize = data$SibSp + data$Parch + 1

plot8 = ggplot(data[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position = "fill") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Family Size and Survival Rate")
plot8

### 3.4 Age ####
## We use MICE package (multiple imputation chained equations) to impute the missing values of age
vars = c('PassengerId','Pclass','Sex','Embarked','Title','Surname','Family','Fsize')
as.factor(vars)
set.seed(123)
aux = data %>% 
  dplyr::select(Pclass, Sex, Title, Fsize, Fare, Age)
mice = mice(aux, m=5,maxit=50, meth='cart', seed=500) #tried methods: bayesian, pmm, but cart seems to work best, see Help() for the methods
summary(mice)
mice_results = complete(mice)

# Plot age distributions to check the distribution of the imputed values
par(mfrow=c(1,2))
hist(data$Age, freq=F, main='Age: Original Data')
hist(mice_results$Age, freq=F, main='Age: Imputation')

# Replace mice results in NAs
data$Age = mice_results$Age

### 4. Modeling phase! ####
## Lets run different classification algorithms, compare and pick the best
## First, we divide the lastest data into test and train
train = data[1:891,]
test = data[892:1309,]
### Making varibles as factors ####
test$Survived = as.factor(test$Survived)
train$Sex = as.factor(train$Sex)
train$Embarked = as.factor(train$Embarked)
train$Title = as.factor(train$Title)
train$Embarked = as.factor(train$Embarked)
train$Title = as.factor(train$Title)

#### 4.1 Logistic Regression ####
fitControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)
logit_model = train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Fsize + Title + Embarked, 
                  data = train,
                  method = 'LogitBoost',
                  trControl = fitControl)
confusionMatrix(logit_model)
# Create object of importance of our variables 
logit_importance = varImp(logit_model) 
logitplot = ggplot(data = logit_importance, mapping = aes(x = logit_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "Boosted Logistic Model") + # Title
  theme_light() # Theme
logitplot



prediction_logit = predict(logit_model, test)
table(prediction_logit, test$Survived) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 
n_prediction_logit = as.numeric(as.character(prediction_logit))
sum(n_prediction_logit)

#### 4.2 Decision Trees ####
# Create model
dt_model = train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Fsize + Title + Embarked, 
                  data = train, #Data
                  method = 'rpart', # Specify Decision tree
                  trControl = fitControl) # Use cross validation

#Confusion Matrix
confusionMatrix(dt_model)

# Create object of importance of our variables 
dt_importance = varImp(dt_model)

# Create plot of importance of variables
dtplot = ggplot(data = dt_importance, mapping = aes(x = dt_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "Decision tree Model") + # Title
  theme_light() # Theme
dtplot

fancyRpartPlot(dt_model$finalModel, sub = '')

prediction_dt = predict(dt_model, test)
table(prediction_dt, test$Survived) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 
n_prediction_dt = as.numeric(as.character(prediction_dt))
sum(n_prediction_dt)

#### 4.3 Naives Bayes ####
fitControl = trainControl(method = "cv", number = 10, savePredictions = TRUE)
nb_model = train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Fsize + Title + Embarked, 
                  data = train,
                  method = "nb",
                  trControl = fitControl)
confusionMatrix(nb_model)
# Create object of importance of our variables 
nb_importance = varImp(nb_model) 
nbplot = ggplot(data = nb_importance, mapping = aes(x = nb_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "Naive Bayes Model") + # Title
  theme_light() # Theme
nbplot

prediction_nb = predict(nb_model, test)
table(prediction_nb, test$Survived) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 
n_prediction_nb = as.numeric(as.character(prediction_nb))
sum(n_prediction_nb)

#### 4.4 Support Vector Machines ####
svm_model = train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Fsize + Title + Embarked, 
                   data = train, # Data
                   method = 'svmLinear', # Specify SVM model
                   trControl = fitControl) # Use cross validation
confusionMatrix(svm_model)

# Create object of importance of our variables 
svm_importance = varImp(svm_model)

# Create box plot
svmplot = ggplot(data = svm_importance, mapping = aes(x = svm_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "SVM Model") + # Title
  theme_light() # Theme
svmplot

#prediction of values
prediction_svm <- predict(svm_model, test)
table(prediction_svm, test$Survived) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 
n_prediction_svm = as.numeric(as.character(prediction_svm))
sum(n_prediction_svm)


## 4.5 Random Forest (again)
rf_model = train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Fsize + Title + Embarked, 
                   data = train, # Data
                   method = 'rf', # Specify SVM model
                   trControl = fitControl) # Use cross validation

confusionMatrix(rf_model)

# Create object of importance of our variables 
rf_importance = varImp(rf_model) 

# Create box plot of importance of variables
rfplot = ggplot(data = rf_importance, mapping = aes(x = rf_importance[,1])) + # Data & mapping
  geom_boxplot() + # Create box plot
  labs(title = "Random forest Model") + # Title
  theme_light() # Theme
rfplot

#prediction of values
prediction_rf <- predict(rf_model, test)
table(prediction_rf, test$Survived) %>% # Create prediction table. 
  prop.table() %>% # Convert table values into proportions instead of counts. 
  round(2) # Round numbers to 2 significant values. 
n_prediction_rf = as.numeric(as.character(prediction_rf))
sum(n_prediction_rf)

#### 4.6 All plots together ####
(logitplot |svmplot| nbplot) /
  (dtplot | rfplot)


#### write solution
solution_group60 <- data.frame(PassengerID = test$PassengerId, Survived = prediction_rf)
write.csv(solution_group60, file = 'Titanic_Solution.csv', row.names = F)



