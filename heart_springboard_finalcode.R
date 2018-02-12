#load libraries
library("dplyr")
library("ggplot2")
library("data.table")
library("gridExtra")
library("caTools")
library("ROCR")
library("mice")
library("corrplot")
library("caret")
library("glmnet")
library("tidyr")
library("scales")
library("psych")

#############################  DATA WRANGLING ############################# 
# read in data
cleveland <- read.csv("cleveland.txt", na.strings = "?")
switzerland <- read.csv("switzerland.txt", na.strings = "?")
hungarian <- read.csv("hungarian.txt", na.strings = "?")
longbeach <- read.csv("longbeach.txt", na.strings = "?")

# inserting column names
colnames(cleveland) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
colnames(switzerland) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
colnames(hungarian) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
colnames(longbeach) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

# trestbps changing into numeric
cleveland$age <- as.numeric(cleveland$age)
switzerland$age <- as.numeric(switzerland$age)
hungarian$age <- as.numeric(hungarian$age)
longbeach$age <- as.numeric(longbeach$age)

# sex change into factor
cleveland$sex <-ifelse(cleveland$sex==1,"Male","Female")
cleveland$sex <-factor(cleveland$sex)
switzerland$sex <-ifelse(switzerland$sex==1,"Male","Female")
switzerland$sex<-factor(switzerland$sex)
hungarian$sex <-ifelse(hungarian$sex==1,"Male","Female")
hungarian$sex<-factor(hungarian$sex)
longbeach$sex <-ifelse(longbeach$sex==1,"Male","Female")
longbeach$sex<-factor(longbeach$sex)

# cp change into factor
cleveland$cp <- ifelse(cleveland$cp==1,"Typical Angina",
                       ifelse(cleveland$cp==2,"Atypical Angina",
                              ifelse(cleveland$cp==3,"Non-Anginal Pain","Asymptomatic")))
cleveland$cp  <-factor(cleveland$cp)

switzerland$cp <- ifelse(switzerland$cp==1,"Typical Angina",
                         ifelse(switzerland$cp==2,"Atypical Angina",
                                ifelse(switzerland$cp==3,"Non-Anginal Pain","Asymptomatic")))
switzerland$cp  <-factor(switzerland$cp)

hungarian$cp <- ifelse(hungarian$cp==1,"Typical Angina",
                       ifelse(hungarian$cp==2,"Atypical Angina",
                              ifelse(hungarian$cp==3,"Non-Anginal Pain","Asymptomatic")))
hungarian$cp  <-factor(hungarian$cp)

longbeach$cp <- ifelse(longbeach$cp==1,"Typical Angina",
                       ifelse(longbeach$cp==2,"Atypical Angina",
                              ifelse(longbeach$cp==3,"Non-Anginal Pain","Asymptomatic")))
longbeach$cp  <-factor(longbeach$cp)

# trestbps changing into numeric
cleveland$trestbps <- as.numeric(cleveland$trestbps)
switzerland$trestbps <- as.numeric(switzerland$trestbps)
hungarian$trestbps <- as.numeric(hungarian$trestbps)
longbeach$trestbps <- as.numeric(longbeach$trestbps)

# chol changing into numeric
cleveland$chol <- as.numeric(cleveland$chol)
switzerland$chol <- as.numeric(switzerland$chol)
hungarian$chol <- as.numeric(hungarian$chol)
longbeach$chol <- as.numeric(longbeach$chol)

# fbs changing into factor
cleveland$fbs <- ifelse(cleveland$fbs==1, "High", "Low")
cleveland$fbs<-factor(cleveland$fbs)
switzerland$fbs <- ifelse(switzerland$fbs==1, "High", "Low")
switzerland$fbs<-factor(switzerland$fbs)
hungarian$fbs <- ifelse(hungarian$fbs==1, "High", "Low")
hungarian$fbs<-factor(hungarian$fbs)
longbeach$fbs <- ifelse(longbeach$fbs==1, "High", "Low")
longbeach$fbs<-factor(longbeach$fbs)

# restecg into factor
cleveland$restecg <- ifelse(cleveland$restecg==0, "Normal",
                            ifelse(cleveland$restecg==1, "ST-T Wave Abnormality", "Left Ventricle Hypertrophy"))
switzerland$restecg <- ifelse(switzerland$restecg==0, "Normal",
                            ifelse(switzerland$restecg==1, "ST-T Wave Abnormality", "Left Ventricle Hypertrophy"))
hungarian$restecg <- ifelse(hungarian$restecg==0, "Normal",
                            ifelse(hungarian$restecg==1, "ST-T Wave Abnormality", "Left Ventricle Hypertrophy"))
longbeach$restecg <- ifelse(longbeach$restecg==0, "Normal",
                            ifelse(longbeach$restecg==1, "ST-T Wave Abnormality", "Left Ventricle Hypertrophy"))
cleveland$restecg<- factor(cleveland$restecg)
switzerland$restecg<- factor(switzerland$restecg)
hungarian$restecg<- factor(hungarian$restecg)
longbeach$restecg<- factor(longbeach$restecg)

# thalach to numeric
cleveland$thalach<- as.numeric(cleveland$thalach)
switzerland$thalach<- as.numeric(switzerland$thalach)
hungarian$thalach<- as.numeric(hungarian$thalach)
longbeach$thalach<- as.numeric(longbeach$thalach)

# exang to factor
cleveland$exang<- ifelse(cleveland$exang==1, "Induced Angina", "Non-induced Angina")
cleveland$exang<- factor(cleveland$exang)
switzerland$exang<- ifelse(switzerland$exang==1, "Induced Angina", "Non-induced Angina")
switzerland$exang<- factor(switzerland$exang)
hungarian$exang<- ifelse(hungarian$exang==1, "Induced Angina", "Non-induced Angina")
hungarian$exang<- factor(hungarian$exang)
longbeach$exang<- ifelse(longbeach$exang==1, "Induced Angina", "Non-induced Angina")
longbeach$exang<- factor(longbeach$exang)

# slope to factor
cleveland$slope<- ifelse(cleveland$slope==1, "Upsloping", 
                         ifelse(cleveland$slope==2,"Flat","Downsloping"))
cleveland$slope<- factor(cleveland$slope)
switzerland$slope<- ifelse(switzerland$slope==1, "Upsloping", 
                         ifelse(switzerland$slope==2,"Flat","Downsloping"))
switzerland$slope<- factor(switzerland$slope)
hungarian$slope<- ifelse(hungarian$slope==1, "Upsloping", 
                         ifelse(hungarian$slope==2,"Flat","Downsloping"))
hungarian$slope<- factor(hungarian$slope)
longbeach$slope<- ifelse(longbeach$slope==1, "Upsloping", 
                         ifelse(longbeach$slope==2,"Flat","Downsloping"))
longbeach$slope<- factor(longbeach$slope)

# ca to factor
cleveland$ca<- factor(cleveland$ca)
switzerland$ca<- factor(switzerland$ca)
hungarian$ca<- factor(hungarian$ca)
longbeach$ca<- factor(longbeach$ca)

# thal to factor
cleveland$thal<- ifelse(cleveland$thal==3, "Normal", 
                        ifelse(cleveland$thal==6,"Fixed Defect","Reversable Defect"))
cleveland$thal<- factor(cleveland$thal)
switzerland$thal<- ifelse(switzerland$thal==3, "Normal", 
                          ifelse(switzerland$thal==6,"Fixed Defect","Reversable Defect"))
switzerland$thal<- factor(switzerland$thal)
hungarian$thal<- ifelse(hungarian$thal==3, "Normal", 
                        ifelse(hungarian$thal==6,"Fixed Defect","Reversable Defect"))
hungarian$thal<- factor(hungarian$thal)
longbeach$thal<- ifelse(longbeach$thal==3, "Normal", 
                        ifelse(longbeach$thal==6,"Fixed Defect","Reversable Defect"))
longbeach$thal<- factor(longbeach$thal)

#num to binomial
cleveland$num<- ifelse(cleveland$num>0, "Presence", "Absence")
cleveland$num<- factor(cleveland$num)
switzerland$num<- ifelse(switzerland$num>0, "Presence", "Absence")
switzerland$num<- factor(switzerland$num)
hungarian$num<- ifelse(hungarian$num>0, "Presence", "Absence")
hungarian$num<- factor(hungarian$num)
longbeach$num<- ifelse(longbeach$num>0, "Presence", "Absence")
longbeach$num<- factor(longbeach$num)

identical(sapply(longbeach, class), sapply(hungarian, class))

#creating ID from which data set each observation originated from
cleveland <-cleveland %>% mutate(dsetid = "cleveland")
switzerland <-switzerland %>% mutate(dsetid = "switzerland")
hungarian <-hungarian %>% mutate(dsetid = "hungarian")
longbeach <-longbeach %>% mutate(dsetid = "longbeach")

#finalizing heart data set
heartdata <-bind_rows(cleveland,switzerland,hungarian,longbeach)
heartdata$ca <- factor(heartdata$ca)
heartdata$dsetid <- factor(heartdata$dsetid)
heartDT <- heartdata

############################# EXPLORATORY DATA ANALYSIS ############################# 

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

plot_Missing(heartdata[,colSums(is.na(heartdata)) > 0])


#separating categorical vs. numerical variables by name
setDT(heartDT)
cat_var <- names(heartDT)[which(sapply(heartDT, is.factor))]
num_var <- names(heartDT)[which(sapply(heartDT, is.numeric))]

#creating cat and num data sets of heart data
heart_cat <-heartDT[, .SD, .SDcol = cat_var]
heart_num <-heartDT[, .SD, .SDcol = num_var]

#summaries of categorical vs. numerical values
summary(heartDT[, .SD, .SDcols = num_var])
summary(heartDT[, .SD, .SDcols = cat_var])

sum(is.na(heartDT))/(nrow(heartDT)*ncol(heartDT))*100

percentage <- colSums(sapply(heartDT, is.na))
(percentage/nrow(heartDT))*100


colSums(sapply(heartDT[,.SD, .SDcols = cat_var], is.na))
colSums(sapply(heartDT[,.SD, .SDcols = num_var], is.na))

#missing data for each subset of data
colSums(is.na(cleveland))/nrow(heartDT)
colSums(is.na(switzerland))/nrow(heartDT)
colSums(is.na(hungarian))/nrow(heartDT)
colSums(is.na(longbeach))/nrow(heartDT)
colSums(is.na(heartDT))/nrow(heartDT)


plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =0))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

heart_cat <-heartDT[, .SD, .SDcol = cat_var]
heart_num <-heartDT[, .SD, .SDcol = num_var]

doPlots(heart_cat, fun=plotHist, ii=1:4, ncol = 2)
doPlots(heart_cat, fun=plotHist, ii=5:8, ncol = 2)
doPlots(heart_cat, fun=plotHist, ii=9:10, ncol = 2)



heartDT <-heartDT[, -c("ca","thal","slope", "dsetid")]
#Imputing missing values

#numeric impute
imputed_data_num <-mice(data = heart_num, method = "pmm", maxit = 50, seed=3)
summary(imputed_data_num)
imputed_data_num$imp
completeDatanum <- mice::complete(imputed_data_num,1)

densityplot(imputed_data_num)
stripplot(imputed_data_num, pch = 20, cex = 1.2)

heartDT$trestbps <- completeDatanum$trestbps
heartDT$chol <- completeDatanum$chol
heartDT$thalach <- completeDatanum$thalach
heartDT$oldpeak <- completeDatanum$oldpeak


#categorical impute
imputed_data_cat <-mice(data = heart_cat, method = "polyreg", maxit = 50, seed=3)
summary(imputed_data_cat)
completeDatacat <- mice::complete(imputed_data_cat,1)

histogram(imputed_data_cat)
stripplot(imputed_data_cat, pch = 20, cex = 1.2)

heartDT$fbs <-completeDatacat$fbs
heartDT$restecg <-completeDatacat$restecg
heartDT$exang <-completeDatacat$exang

## Correlation Plots
heart_cat <-heartDT[, .SD, .SDcol = cat_var]
heart_num <-heartDT[, .SD, .SDcol = num_var]
M <- cor(heart_num)
corrplot(M, method = "square")



#############################  MACHINE LEARNING/ANALYSIS ############################# 
#creating testing and training data sets
set.seed(3)
split <- sample.split(heartDT$num, SplitRatio = .75)
nrow(heartTrain)

heartTrain <- subset(heartDT, split == TRUE)
heartTest <- subset(heartDT, split == FALSE)
str(heartTest)

#forwards and backwards models
null=glm(num ~ 1, data=heartTrain, family="binomial")
full=glm(num ~ ., data=heartTrain, family="binomial")

#using step for feature selection
fwdstep<- step(null, scope=list(lower=null, upper=full), direction="forward")
bwdstep<- step(full, scope=list(lower=null, upper=full), direction="backward")

#used for confusionmatrix in caret package
heartfwd <- train(num ~ cp + exang + chol + oldpeak + sex + age + thalach + fbs, data =heartTrain, model= "glm", family= "binomial")
heartbwd <- train(num ~ age + sex + cp + chol + fbs + thalach + exang + oldpeak, data= heartTrain, model= "glm", family="binomial")
summary(heartfwd)
summary(heartbwd)
#creating predictions for confusionmatrix
fwdpred <- predict(heartfwd, heartTest, type ="raw")
bwdpred <- predict(heartbwd, heartTest, type ="raw")
confusionMatrix(fwdpred, heartTest$num)
confusionMatrix(bwdpred, heartTest$num)

#LASSO method, creating X and Y data sets for training and test data
X_train <- as.matrix(heartTrain[,-c("num")])
Y_train <- heartTrain$num

X_test <- as.matrix(heartTest[,-c("num")])
Y_test <- heartTest$num

f_test<- as.formula(heartTrain$num ~.)
X_train <-model.matrix(f, heartTrain)[,-1]

f_test<- as.formula(heartTest$num ~.)
X_test <-model.matrix(f_test, heartTest)[,-1]

#logistic lasso technique
fit = glmnet(x=X_train, y=Y_train, family = "binomial")
plot(fit,label = TRUE)
predict(fit, newx = X_test, type = "class", s = .01)


cvfit = cv.glmnet(x=X_train, y=Y_train, family = "binomial", type.measure = "class")
plot(cvfit)

#finding actual coefficients for lasso model 1se
fit1se<- glmnet(x=X_train, y=Y_train, family = "binomial", alpha=1, lambda=CV$lambda.min)
fit1se$beta[,1]

#finding actual coefficients for lasso model min
fitmin<-glmnet(x=X_train, y=Y_train, family = "binomial", alpha=1, lambda=CV$lambda.1se)
fitmin$beta[,1]

cvfit$lambda.min
cvfit$lambda.1se

#creating predict object for confusionmatrix
cv_pred_1se<-predict(cvfit, newx = X_test, type = "link", s = cvfit$lambda.1se)
cv_pred_min<-predict(cvfit, newx = X_test, type = "class", s = cvfit$lambda.min)


#caret confusion matrix for cvpred min and 1se
confusionMatrix(cv_pred_1se, heartTest$num)
confusionMatrix(cv_pred_min, heartTest$num)

