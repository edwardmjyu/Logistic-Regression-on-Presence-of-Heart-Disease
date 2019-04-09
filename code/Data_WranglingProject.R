############
###### Springboard Capstone Project: Data Wrangling

###### Activate libraries
library(farff)
library(dplyr)
library(ggplot2)

###### Using farff to import datasets into data frames
asd_child <- readARFF("Autism-Child-Data.arff")
asd_adol <- readARFF("Autism-Adolescent-Data.arff")
asd_adult <- readARFF("Autism-Adult-Data.arff")

###### Viewing data options  
tbl_df(asd_child)
str(asd_child)
str(asd_adol)
str(asd_adult)
View(asd_child)
View(asd_adol)
View(asd_adult)

###### Renaming variables to more convenient names
asd_child <-rename(asd_child, "class" = "Class/ASD", "country_res" = "contry_of_res", "jaundice" = "jundice", "family_autism" = "austim")
asd_adol <-rename(asd_adol, "class" = "Class/ASD", "country_res" = "contry_of_res", "jaundice" = "jundice", "family_autism" = "austim")
asd_adult <-rename(asd_adult, "class" = "Class/ASD", "country_res" = "contry_of_res", "jaundice" = "jundice", "family_autism" = "austim")

###### Coercing variables: ethnicity, country_res, age_desc, and relation from factor -> char
asd_child$ethnicity <- as.character(asd_child$ethnicity)
asd_adol$ethnicity <- as.character(asd_adol$ethnicity)
asd_adult$ethnicity <- as.character(asd_adult$ethnicity)

asd_child$country_res <- as.character(asd_child$country_res)
asd_adol$country_res <- as.character(asd_adol$country_res)
asd_adult$country_res <- as.character(asd_adult$country_res)

asd_child$age_desc <- as.character(asd_child$age_desc)
asd_adol$age_desc <- as.character(asd_adol$age_desc)
asd_adult$age_desc <- as.character(asd_adult$age_desc)

asd_child$relation <- as.character(asd_child$relation)
asd_adol$relation <- as.character(asd_adol$relation)
asd_adult$relation <- as.character(asd_adult$relation)

identical(sapply(asd_adult, class), sapply(asd_adol, class))

###### Binding all datasets into master data set
asd_master <-bind_rows(asd_child, asd_adol, asd_adult)

###### View final cleaned master dataset
View(asd_master)
sapply(asd_master, class)
tbl_df(asd_master)
