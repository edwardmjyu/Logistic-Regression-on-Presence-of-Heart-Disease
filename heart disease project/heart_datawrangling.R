#load libraries
library("Hmisc")
library("dplyr")
library("data.table")
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

# apply var.labels to each variable
#var.labels = c(age="Age in Years", sex="Sex (1= male; 0=female)", cp="chest pain type (1= typical angina; 2=atypical angina; 3=non-anginal pain; 4=asyomptomatic)", trestbps="resting blood pressure in mm Hg", chol="serum cholestoral in mg/dl", fbs="fasting blood sugar >120 mg/dl, 1=true; 0=false", restecg="resting electrocardiographic results (0=normal;1=having ST-T wave abnormality;2= showing probable or definite left ventricular hypertrophy by Estes' criteria", thalach="maximum heart rate achieved", exang="exercise induced angina (1=yes;2=no)",oldpeak="ST depression induced by exercise relative to rest", slope="slope of the peak exercise ST segment (1=upsloping;2=flat;3=downsloping)",ca="number of major vessels",thal="3= normal; 6= fixed defect; 7= reversable defect",num="diagnosis of heart disease angiographic diseases status (0= < 50% diameter narrowing; 1= >50% diameter narrowing")

#label(cleveland) <- lapply(names(var.labels), 
#                           function(x) label(cleveland[,x]) = var.labels[x])
#label(switzerland) <- lapply(names(var.labels), 
#                           function(x) label(switzerland[,x]) = var.labels[x])
#label(hungarian) <- lapply(names(var.labels), 
#                           function(x) label(hungarian[,x]) = var.labels[x])
#label(longbeach) <- lapply(names(var.labels), 
#                           function(x) label(longbeach[,x]) = var.labels[x])

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

heartdata <-bind_rows(cleveland,switzerland,hungarian,longbeach)

str(cleveland)
str(switzerland)
str(hungarian)
str(longbeach)
View(cleveland)
View(switzerland)
View(hungarian)
View(longbeach)
str(heartdata)

#ca and exang issuse

ggplot(cleveland, aes(age)) + geom_histogram(binwidth = 1)
ggplot(switzerland, aes(age)) + geom_histogram(binwidth = 1)
ggplot(hungarian, aes(age)) + geom_histogram(binwidth = 1)
ggplot(longbeach, aes(age)) + geom_histogram(binwidth = 1)


View(cleveland)
str(cleveland)
str(hungarian)
str(longbeach)
str(switzerland)

ggplot(cleveland, aes(x=age, y=trestbps)) + geom_point()



