## Load the states data
## ────────────────────────

getwd() # where am I?
setwd("C:/R/Springboard_Project/linear_regression")
list.files("dataSets") # files in the dataSets folder

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)
View(states.data)

## Exercise 1: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##1.
stat.energy.metro <- subset(states.data, select = c("metro", "energy"))
plot(stat.energy.metro)
summary(stat.energy.metro)

##2. 
modelEnMet <- lm(energy ~ metro, data= stat.energy.metro)
summary(modelEnMet)

#In predicting energy consumption per capita from the percentage of residents living in 
#metropolitan areas, if Y and X are denoted as Energy and Metro respectively, then the two 
#dependent and independent variables can be described in the following equation: 
#           Y = -2.2871X + 501.0292
#The R^2 value of .1154 tells us this metro is weakly correlated with energy consumption. As 
#the percentage of residents in metropolitan areas increased there is not a strong resulting
#increase in the energy consumption per capita. 

##3.
plot(modelEnMet)
#From the Normal Q-Q plots, we can see that there is a left skew and heavily tailed 
#distribution. The residual plot shows a few outliers 19, 2, and 51. If we were to exclude
#these points, we see the remaining residuals show mostly a random pattern with a slight 
#inversion, suggesting this non-linear fit may be slightly better. 

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

##4.
stat.energy.tg <- subset(states.data, select = c("toxic", "green","energy"))
plot(stat.energy.tg)

##5. 
modelEnTG <- lm(energy ~ toxic + green, data= stat.energy.tg)
summary(modelEnTG, which = c(1,2))

#After testing a few variables predicting energy, I found that toxic and greenhouse variables 
#to be most significant. If E, T, G represent energy, toxic, and green variables, then: 
#           E = 2.6455T + 4.6722G + 179.826
#Both toxic and green variables were shown to be significant under alpha level of .001 and 
#positively correlated with energy per capita consumption. 

##6. 
plot(modelEnTG, which =c(1,2))
#Residuals vs fitted plot show that most values are grouped near the left side and skewed by 
#outliers. THe normal QQ plot also shows that the distribution is left skewed with heavy tails.
#In the next step I remove outliers.

removedoutliersenergy <- stat.energy.tg %>% filter(toxic < 100) %>% filter(green < 100) %>% filter(energy < 500)
modeloutliers <- lm(energy ~ toxic + green, data = removedoutliersenergy)
plot(modeloutliers, which = c(1,2))

#Removing these outliers we can see the residual fit is still slightly grouped left, but the
#residuals show a much better randomized residual suggesting no change to the current linear 
#model. The normal QQ plot distribution is more normalized with outliers removed with a some
#degree of heavy tailing.



## Exercise 2: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

##1. 
modelinteraction <- lm(energy ~ toxic + green + toxic*green, data = stat.energy.tg)
summary(modelinteraction)

##2. 
#Using the default contr.treatment
state.energy.region <- subset(states.data, select = c("region", "energy"))
model.energy.region <- lm(energy ~ region, data = state.energy.region)
summary(model.energy.region)
coef(summary(model.energy.region))
anova(model.energy.region)
#There is no significant differences across the four regions in terms of energy use. 
plot(state.energy.region)
