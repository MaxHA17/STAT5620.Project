Final Project

# Clear Workspace
rm(list=ls())

# Load Data

seal_data = read.csv("Data_Q1.csv", header = T)
summary(seal_data)

seal_data



## Maternal mass change ~ dominant prey species + dietary energy density + diet diversity +
## maternal ID (random effect to account for repeated measures of some females) +
## year (to account for changes in prey availability/fluctuation)



###### Exploring Data and Distribution of Each Variale (Predictor and Response )


# Plot varibles to see distributions of predictor and response variables
require(flexplot)
# data = seal_data
# variables names( seal_data)
#[1] "MomID"                  "Year"                   "Dietary.energy.density" "Diet.diversity"
[5] "Dominant.prey.species"  "Mass.change"

a = flexplot (MomID~1, data= seal_data)
b = flexplot (Year~1, data= seal_data)
c = flexplot (Dietary.energy.density~1, data= seal_data)
d = flexplot (Diet.diversity~1, data= seal_data)
e = flexplot (Dominant.prey.species~1, data= seal_data)
f = flexplot (Mass.change~1, data= seal_data)


require (cowplot)
plot_grid(a,b,c,d,e,f)


##Explore the data with plots (One Predictor vs. Response )

# Mass Change + Dietary.energy.density
ggplot(seal_data) + geom_point(aes(seal_data$Dietary.energy.density, seal_data$Mass.change)) +
  labs(title = "Mass Change and Dietary Energy Density", x = "Dietary Energy Density (kJ/g of prey tissue)", y = "Mass Change (Kg)") +
  geom_smooth(aes(seal_data$Dietary.energy.density, seal_data$Mass.change), method="lm", color = "red", se=T) + theme_minimal() +theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.spacing = unit(2, "lines"))



# Mass Change + Diet Diveristy
ggplot(seal_data) + geom_point(aes(seal_data$Diet.diversity, seal_data$Mass.change)) +
  labs(title = "Mass Change and Diet Diversity", x = "Diet Diveristy (no specific units for Shannon index)", y = "Mass Change (Kg)") +
  geom_smooth(aes(seal_data$Diet.diversity, seal_data$Mass.change), method="lm", color = "red", se=T) + theme_minimal() +theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing = unit(2, "lines"))

# Mass Change + Dominant Prey Species
ggplot(seal_data) + geom_point(aes(seal_data$Dominant.prey.species, seal_data$Mass.change)) +
  labs(title = "Mass Change and Dominant Prey Species", x = "Dominant Prey Species", y = "Mass Change (Kg)")
geom_smooth(aes(seal_data$Dominant.prey.species, seal_data$Mass.change), method="lm", colour = "red", se=F) + theme_minimal() +theme(
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  panel.spacing = unit(2, "lines"))


# Mass Change + Dominant Prey Species
ggplot(seal_data) + geom_boxplot(aes(seal_data$Dominant.prey.species, seal_data$Mass.change), fill = "lightblue") +
  labs(title = "Mass Change and Dominant Prey Species", x = "Dominant Prey Species", y = "Mass Change (Kg)")
geom_smooth(aes(seal_data$Dominant.prey.species, seal_data$Mass.change), method="lm", colour = "red", se=F) + theme_minimal() +theme(
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  panel.spacing = unit(2, "lines"))






# Mass Change + Year
ggplot(seal_data) + geom_point(aes(seal_data$Year, seal_data$Mass.change, color = seal_data$MomID)) +
  labs(title = "Mass Change and Year", x = "Year", y = "Mass Change (Kg)") +
  geom_smooth(aes(seal_data$Year, seal_data$Mass.change), method="lm", se=F)

# Mass Change + MomID
ggplot(seal_data) + geom_point(aes(seal_data$MomID, seal_data$Mass.change)) +
  labs(title = "Mass Change and Mom ID", x = "Mom ID", y = "Mass Change (Kg)")
geom_smooth(aes(seal_data$MomID, seal_data$Mass.change), method="lm", se=F)





###############.    Fit Linear Model (all predictor variables)

## convert catagorical data to factors

seal_data$MomID = as.factor(seal_data$MomID)
seal_data$Year = as.factor(seal_data$Year)
seal_data$Dominant.prey.species = as.factor(seal_data$Dominant.prey.species)


### all three factor variables in model found infinite model so "mother ID" factor was removed

Q1 = lm(seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year, data = seal_data)

summary(Q1)

# p-value: 1.006e-06
# Adjusted R-squared - 0.5174
plot(Q1)


### all three factor variables in model found infinite model so "year" factor was removed

Q2 = lm(seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$MomID, data = seal_data)

summary (Q2)

# p-value: 0.2608
# Adjusted R-squared - 0.4349
plot(Q2)


### all three factor variavles in model found infinite model so "Dominant.prey.species" factor was removed

Q3 = lm(seal_data$Mass.change ~  seal_data$Diet.diversity + seal_data$Dietary.energy.density
             + seal_data$MomID + seal_data$Year, data = seal_data)

summary (Q3)

### model doesn't work
### Fouind Year and Mom ID can't be in same model
### Year creates a higher Adjusted R-square and much more significant p-value so Q1 is best linear model.




## Check for correlation of numeric data

# Graphically look at corrolation

pairs( ~Diet.diversity + Dietary.energy.density, data = seal_data)

# numerically inspect the data by plotting with paris to inpect
# for collinearity

cor.test( ~Diet.diversity + Dietary.energy.density, data = seal_data)


## 	Pearson's product-moment correlation

# data:  Diet.diversity and Dietary.energy.density
# t = -0.35989, df = 74, p-value = 0.72
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.2647605  0.1854027
# sample estimates:
#   cor
# -0.0418001


# Not significant p-value so not correlated variables




#Step Function

fwd.model = step (Q1, direction='forward')
backward.model = step(Q1, direction='backward')

### Backward found best model included (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity +
### seal_data$Year) to be the best model with AIC = 465.39

# We will now run this linear model with the data

Q1_Reduced = lm(seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Year,
        data = seal_data)

summary(Q1_Reduced)

# We find that a p-value =  4.625e-07 with a Adjusted R-square of 0.5241 (52 % of variance explained by model)


### Conclusions that the most significant variables are prey species Capelin and Pollock as well as the year 2005. This shows the
### importance of the mixed model (GLMM)

### show graphs MC vs. DED (single variables) and Dominant prey species

### This leads us to look into dominant prey species each year. I will now create this graph


# Mass Change + Dominant Prey Species

require(ggplot2)

ggplot(seal_data) + geom_point(aes(seal_data$Year, seal_data$Dominant.prey.species)) +
  labs(title = "Year and Dominant Prey Species", x = "Year", y = "Dominant Prey Species")

a1 = flexplot (Dominant.prey.species ~ Year, data=seal_data, method ="gaussian", se=T)
a1


### Again conclusions show that the most significant variables are prey species Capelin and Pollock as well as the year 2005. This shows the
### importance of the mixed model (GLMM)


###################  Fit Generalized Linear Model (all predictor variables)

Q1A = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
            + seal_data$Year + seal_data$MomID, data = seal_data, family = gaussian)


# model doesn't work (note year and MomID in same model doesn't work)


# remove Mom ID
Q1A_A = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
            + seal_data$Year , data = seal_data, family = gaussian)
summary (Q1A_A)
#AIC = 684.76

#remove Year

Q1A_B = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$MomID, data = seal_data, family = gaussian)

summary (Q1A_B)
#AIC = 614.52



### With this GLM the Mom ID is better to keep in and remove the Year



#Step Function

fwd.model = step(Q1A_B, direction='forward')
backward.model = step(Q1A_B, direction='backward')

### step backward found that the best model only included 3 predictor variables
# and has a AIC = 610.54 for the variables seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$MomID


# We now run this Gernalized Linear Model (GLM) with a Gaussian family

Q1A_C = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$MomID, family = gaussian)

summary (Q1A_C)

## AIC = 610.54



#########################.   Fit a GLMM

library(lme4)

glmm_1 = glmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                            (1 | Year) + (1 | MomID), data = seal_data, family = gaussian)


glmm_2 = glmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                            (1 | Year), data = seal_data, family = gaussian)


glmm_3 = glmer(Mass.change ~ (1 | Dominant.prey.species) + Diet.diversity +
                (1 | Year), data = seal_data, family = gaussian)


# Comparing GLMM's
model.comparison(glmm_1, glmm_2 )
# lmm_2 is better model AIC = 649.36


# Comparing GLMM's
model.comparison(glmm_2, glmm_3 )
# lmm_2 is a much better model AIC = 649.36




lmm_1 = lmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                (1 | Year) + (1 | MomID), data = seal_data


lmm_2 = lmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                (1 | Year), data = seal_data)


lmm_3 = lmer(Mass.change ~ (1 | Dominant.prey.species) + Diet.diversity +
                (1 | Year), data = seal_data)



# Comparing GLMM's
model.comparison(lmm_1, lmm_2 )
# lmm_2 is better model AIC = 649.36


# Comparing GLMM's
model.comparison(lmm_2, lmm_3 )
# lmm_2 is a much better model AIC = 649.36


step (lmm_1, direction = backwards)


library(lmerTest)





# Comparing best LM to best GLM (Gaussian)
model.comparison(Q1_Reduced, Q1A_C)
# Best Linear Model (Q1_Reducted) AIC = 683.1 and Best Generalized Linear Model (Q1A_C) AIC = 610.54
# Generalized Linear Model best with AIC = 610.54

# Comparing best GLMM and GLM
model.comparison(lmm_2, Q1A_C )

# Best Model is the Generalized Linear Model not the GLMM

Gaus_mixed_full_2 = glmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                            (1 | Year), data = seal_data, family = gaussian)







#### This is best model

Q1A_C = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$MomID, family = gaussian)
summary (Q1A_C)
## AIC = 610.54



glmm_4 = glmer(seal_data$Mass.change ~ seal_data$Dominant.prey.species + (1 | seal_data$MomID), family = gaussian)

model.comparison(glmm_4, Q1A_C )

## Q1A_C still the best model












# Testing Assumptions (Q1) full model

qqnorm(Q1$residuals)
# Therefore Figure 3G demonstrates that we can assume normality of the data.

#Histogram of the residuals
hist(Q1$residuals)
# see Figure 3E, the bell shaped curve of the histogram suggests normality.

#Shapiro-Wilk's Test
shapiro.test(Q1$residuals)
# data:  Q1$residuals
# W = 0.98071, p-value = 0.3009
# note the p-value is not significant so their is not a variation of the assumption of normality.

# Therefore from these 3 explorations we are now confident in normality of the data.

### CHECKING FOR HOMOGENEITY OF VARIANCE

# Breusch_Pagan Test (lmtest package)
bptest(Q1)
# BP = 5.9408, df = 9, p-value = 0.7458
# Thus the p-value is greater then 0.05 so we accept the null hypothesis and assume we have
# homoscedasticity.


### Checking for Independence of Predictor Varibles

# Dublin-Watson
data:  Q1
DW = 1.9464, p-value = 0.3671
alternative hypothesis: true autocorrelation is greater than 0 p-number

# Close to DW = 2 indicates little of no autocorrolation


#Figure 1G
pairs(Q1_2)

# numerically inspect the data by plotting with paris to inpect
# for collinearity

abs(cor(Q1[c(1:5)]))

## a rule of thumb is to avoid including variables with >0.5
# correlation in a single model
#.                         Mean.Length..cm.  Mean.Wieght...g.  Mean.Wingspan..cm.
# Mean.Length..cm.         1.00000000        0.7973477         0.81773499
# Mean.Wieght...g.         0.79734771        1.0000000         0.74707904
# Mean.Wingspan..cm.       0.81773499        0.7470790         1.00000000
# Autumn.value             0.03030111        0.2442615         0.02690382
# Spring.value             0.03881401        0.2462854         0.02228644
#
#                      Autumn.value Spring.value
# Mean.Length..cm.     0.03030111   0.03881401
# Mean.Wieght...g.     0.24426151   0.24628544
# Mean.Wingspan..cm.   0.02690382   0.02228644
# Autumn.value         1.00000000   0.97783319
# Spring.value         0.97783319   1.00000000







######################## Question Two



## Pup weaning mass ~ dominant prey species + dietary energy density + diet
## diversity + maternal ID (random effect to account for repeated measures of some females) + pup
## sex (include to account for sex difference in mass) + maternal age (include as quadratic effect to
## account for reproductive senescence)


###### Exploring Data and Distribution of Each Variale (Predictor and Response )


# Clear Workspace
rm(list=ls())

# Load Data
seal_data = read.csv("Data_Q2.csv", header = T)
summary(seal_data)

seal_data

# rows= n=56 observations


# Plot varibles to see distributions of predictor and response variables

require(flexplot)

# data = seal_data

names (seal_data)

[1] "MomID"                  "Year"                   "Dietary.energy.density" "Diet.diversity"
[5] "Dominant.prey.species"  "Mom.Age"                "Pup.sex"                "Pup.Wean.Mass"

a = flexplot (MomID~1, data= seal_data)
b = flexplot (Year~1, data= seal_data)
c = flexplot (Dietary.energy.density~1, data= seal_data)
d = flexplot (Diet.diversity~1, data= seal_data)
e = flexplot (Dominant.prey.species~1, data= seal_data)
f = flexplot (Mom.Age~1, data= seal_data)
g = flexplot (Pup.sex~1, data= seal_data)
h = flexplot (Pup.Wean.Mass~1, data= seal_data)

require (cowplot)
plot_grid(a,b,c,d,e,f,g,h)



##Explore the data with plots (One Predictor vs. Response )

require(ggplot2)

# Pup Wean Mass + Dietary.energy.density
ggplot(seal_data) + geom_point(aes(seal_data$Dietary.energy.density, seal_data$Pup.Wean.Mass, color = Dominant.prey.species)) +
  labs(title = "Pup Wean Mass and Dietary Energy Density", x = "Dietary Energy Density (kJ/g of prey tissue)", y = "Pup Wean Mass (Kg)") +
  geom_smooth(aes(seal_data$Dietary.energy.density, seal_data$Pup.Wean.Mass), method="lm", se=T)

# Pup Wean Mass + Diet Diveristy
ggplot(seal_data) + geom_point(aes(seal_data$Diet.diversity, seal_data$Pup.Wean.Mass, color = seal_data$MomID)) +
  labs(title = "Pup Wean Mass and Diet Diversity", x = "Diet Diveristy (no specific units for Shannon index)", y = "Pup Wean Mass (Kg)") +
  geom_smooth( method="lm", se=T)

# Pup Wean Mass + Year
ggplot(seal_data) + geom_point(aes(seal_data$Year, seal_data$Pup.Wean.Mass, color = seal_data$MomID)) +
  labs(title = "Pup Wean Mass and Year", x = "Year", y = "Pup Wean Mass (Kg)") +
  geom_smooth(aes(seal_data$Year, seal_data$Pup.Wean.Mass), method="lm", se=T)

# Pup Wean Mass + MomID
ggplot(seal_data) + geom_point(aes(seal_data$MomID, seal_data$Pup.Wean.Mass)) +
  labs(title = "Pup Wean Mass and Mom ID", x = "Mom ID", y = "Pup Wean Mass (Kg)") +
geom_smooth(aes(seal_data$MomID, seal_data$Pup.Wean.Mass), method="lm", se=T)

# Pup Wean Mass + Dominant Prey Species
ggplot(seal_data) + geom_point(aes(seal_data$Dominant.prey.species, seal_data$Pup.Wean.Mass)) +
  labs(title = "Pup Wean Mass and Dominant Prey Species", x = "Dominant Prey Species", y = "Pup Wean Mass (Kg)")

# Pup Wean Mass + Pup Sex
ggplot(seal_data) + geom_point(aes(seal_data$Pup.sex, seal_data$Pup.Wean.Mass)) +
  labs(title = "Pup Wean Mass and Pup Sex", x = "Pup Sex", y = "Pup Wean Mass (Kg)") +
  geom_smooth(aes(seal_data$Pup.sex, seal_data$Pup.Wean.Mass), method="lm", se=T)

# Pup Wean Mass + Maternal Age
ggplot(seal_data) + geom_point(aes(seal_data$Mom.Age, seal_data$Pup.Wean.Mass)) +
  labs(title = "Pup Wean Mass and Maternal Age", x = "Maternal Age", y = "Pup Wean Mass (Kg)") + geom_point() +
  geom_smooth(aes(seal_data$Mom.Age, seal_data$Pup.Wean.Mass), method="lm", se=T)





##### Exploring Pup Wean Mass based on individuals fish species


# Pup Wean Mass + Dietary.energy.density
ggplot(seal_data) + geom_point(aes(Dietary.energy.density, Pup.Wean.Mass, color = Dominant.prey.species)) + geom_point() +
geom_smooth(method = "lm" , se=F) + theme_minimal()





###############.    Fit Linear Model (all predictor variables)

## convert categorical data to factors

seal_data$MomID = as.factor(seal_data$MomID)
seal_data$Year = as.factor(seal_data$Year)
seal_data$Dominant.prey.species = as.factor(seal_data$Dominant.prey.species)
seal_data$Pup.sex = as.factor(seal_data$Pup.sex)
seal_data$Pup.Mom.Age = as.factor(seal_data$Mom.Age)

nlevels(seal_data$MomID)
# 51
nlevels(seal_data$Year)
# 13
nlevels(seal_data$Dominant.prey.species)
# 5
nlevels(seal_data$Pup.sex)
# 2
nlevels(seal_data$Pup.Mom.Age)
# 17

Q1 = lm(seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year + seal_data$Mom.Age + seal_data$Pup.sex + seal_data$MomID, data = seal_data)

summary(Q1)

### Does not work
#### meaning more columns than rows, meaning in turn more variables than observations, which directly leads to the NaN on 0 degrees of freedom.
### too many variales and not enough data
### need to remove some factors so try each one to see which model is better

### all four factor variables in model found infinite model so "mother ID" factor was removed below

Q2 = lm(seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year +  seal_data$Mom.Age + seal_data$Pup.sex , data = seal_data)

summary (Q2)

### Adjusted R-squared:  0.209
###  p-value: 0.08393


### remove "year" variable

Q3 = lm(seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        +  seal_data$Mom.Age + seal_data$Pup.sex + seal_data$MomID, data = seal_data)

summary (Q3)


### Does not work

### remove "Dominant.prey.species" variable

Q4 = lm(seal_data$Pup.Wean.Mass ~  seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year + seal_data$Mom.Age + seal_data$Pup.sex + seal_data$MomID, data = seal_data)

summary (Q4)

### Does not work

###  Best lm is all varialbes except MomID so Q2 model above



#Step Function

fwd.model = step (Q2, direction='forward')

### all varailbles with AIC = 216.76

backward.model = step(Q2, direction='backward')

### dominant.prey.species and diet.diveristy with AIC 202.62

### Backward found best model included (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity)
# We will now run this linear model with the data

Q1_Reduced = lm(seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity,
                data = seal_data)

summary(Q1_Reduced)


# We find that a p-value: 0.00121 with a Adjusted R-squared:  0.2715  (27 % of variance explained by model)


### Conclusions that the most significant variables are prey species Pollock and White Hake. This shows the
### importance of the mixed model (GLMM)



#### Interact Plot to see how the categories within the Dominant.prey.species variable act independently

library(jtools)

qplot (seal_data$Dominant.prey.species, Q1_Reduced)






###################  Fit Generalized Linear Model (all predictor variables)

Q2A = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Year + seal_data$Mom.Age + seal_data$Pup.sex + seal_data$MomID, data = seal_data, family = gaussian)


summary (Q2A)


### NOTE:: we have 7 predictor variables and 5 are catagorical

# model doesn't work and has an AIC = -3013.8
## Same as linear model were we have more variavles with all the factors than data observed.


# REMOVE MomID variable

Q2B = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Year + seal_data$Mom.Age + seal_data$Pup.sex, data = seal_data, family = gaussian)


summary (Q2B)
#AIC = 377.68

#REMOVE Year

Q2C = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Mom.Age + seal_data$Pup.sex + seal_data$MomID, data = seal_data, family = gaussian)

summary (Q2C)

# Doesn't Work with AIC = -3146.2


# REMOVE dominant prey species variable

Q2D = glm (seal_data$Pup.Wean.Mass ~ seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Year + seal_data$Mom.Age + seal_data$Pup.sex + seal_data$MomID, data = seal_data, family = gaussian)

summary (Q2D)

# Doesn't Work with AIC = -2919.9


# REMOVE mom age

Q2E = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Year + seal_data$Pup.sex, data = seal_data, family = gaussian)


summary (Q2E)
#AIC = 375.75
# only intercept is significant


# REMOVE pup sex

Q2F = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Year + seal_data$Mom.Age + seal_data$MomID, data = seal_data, family = gaussian)

summary (Q2F)

### Does not work with AIC = -2922.3



### In summary, removing Mom age and Mom ID give viable models because is reduces the
#### number of variables to a point that we have enough data observations

# Lets try removing both these variables in one model to see the AIC score


Q2G = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
           + seal_data$Year + seal_data$Pup.sex, data = seal_data, family = gaussian)


summary (Q2G)
## AIC = 375.75
## Same as removal of Mom Age alone, so should we keep Mom ID in the model?
### Best model to use for the step function is Q2E


## So use Q2E model to step



#Step Function

fwd.model = step(Q2E, direction='forward')
# all varialves except MomID and Mom Age
### AIC = 377.68

backward.model = step(Q2E, direction='backward')
# only Dominant prey species and Diet Diversity and AIC = 363.54

### step backward found that the best model only included 2 predictor variables
# and has a AIC = 363.54 for the variables (Seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity)


# We now run this Gernalized Linear Model (GLM) with a Gaussian family

Q2H = glm (seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity, family = gaussian)

summary (Q2H)

## AIC = 363.54

### Conclusion is Q2H is best model with AIC 363.54 for GLM and Pollock (p-value = 0.000949) and
## White Hake (p-value = 0.044627) are the most significant variables.

### This show importance of running a GLMM because both these variables are categorical data.




###### Fit Generalized Linear Mixed Model


require(lme4)

GLMM_A = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density + (1 | Dominant.prey.species)
           + (1 | Year) + (1 | Mom.Age) + (1 | Pup.sex) + (1 | MomID), data = seal_data, family = gaussian)


summary(GLMM_A)


### Compare Full Generalized Linear Mixed Model (GLMM_A) to Best Generalized Linear Model (Q2H)

model.comparison(GLMM_A, Q2H)

#        aic     bic            bayes.factor
# GLMM_A 362.559 380.787        0.593
# Q2H    363.539 379.742        1.686




### Romove Mom_ID

GLMM_=B = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density + (1 | Dominant.prey.species)
               + (1 | Year) + (1 | Mom.Age) + (1 | Pup.sex), data = seal_data, family = gaussian)


summary(GLMM_B)

model.comparison(GLMM_A, GLMM_B)

#       aic         bic             bayes.factor     p
# GLMM_A 362.559   380.787        0.218              0.195
# GLMM_B 361.534   377.737        4.596

## model better without Mom ID variable



### Remove Mom_Age

GLMM_C = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density + (1 | Dominant.prey.species)
                + (1 | Year) +  (1 | Pup.sex), data = seal_data, family = gaussian)


model.comparison(GLMM_B, GLMM_C)

#        aic      bic            bayes.factor p
# GLMM_B 361.534  377.737        0.134        1
# GLMM_C 359.534  373.712        7.482

# Model better fit without Mom ID and Mom Age



### Remove Year

GLMM_D = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density + (1 | Dominant.prey.species)
               + (1 | Pup.sex), data = seal_data, family = gaussian)

model.comparison(GLMM_C, GLMM_D)

#        aic     bic             bayes.factor p
# GLMM_C 359.534 373.712        0.134         1
# GLMM_D 357.534 369.686        7.483

# Model better fit without Mom ID and Mom Age + Year


### Remove Pup Sex

GLMM_E = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density + (1 | Dominant.prey.species) ,
               data = seal_data, family = gaussian)

model.comparison(GLMM_D, GLMM_E)

#       aic      bic           bayes.factor p
#GLMM_D 357.534 369.686        0.134        1
#GLMM_E 355.534 365.661        7.483


### Model Better fit without Pup Sex



### Remove Pup Dietary.engery.density

GLMM_F = glmer(Pup.Wean.Mass ~  Diet.diversity + (1 | Dominant.prey.species) ,
               data = seal_data, family = gaussian)

model.comparison(GLMM_E, GLMM_F)

#.      aic     bic            bayes.factor     p
#GLMM_E 355.534 365.661        4.283           0.131
#GLMM_F 360.469 368.570        0.233


# Model GLMM_E actually better so keep Dietaty. energy. density


### Remove Diet Diversity

GLMM_G = glmer(Pup.Wean.Mass ~  Dietary.energy.density + (1 | Dominant.prey.species) ,
               data = seal_data, family = gaussian)

model.comparison(GLMM_E, GLMM_G)

#          aic     bic         bayes.factor     p
# GLMM_E 355.534 365.661      126.498        0.005
# GLMM_G 367.240 375.342        0.008


# Model GLMM_E actually better so keep Diet Diversity




### Remove Dominant.prey.species

GLMM_H = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density,
               data = seal_data, family = gaussian)

model.comparison(GLMM_E, GLMM_H)

#       aic      bic           bayes.factor p
#GLMM_D 357.534 369.686        0.134        1
#GLMM_E 355.534 365.661        7.483


### Model doesn't work because Mixed Model without random effects




### Compare Best Mixed Model to Best Linear Model

model.comparison(GLMM_E,Q1_Reduced )

#             aic     bic         bayes.factor
# GLMM_E     355.534 365.661     1141.872
# Q1_Reduced 363.539 379.742        0.001

### Mixed Model better fit



### Compare Best Mixed Model to Best Generalized Linear Model

model.comparison(GLMM_E,Q2H)

#         aic     bic         bayes.factor
# GLMM_E 355.534 365.661     1141.872
# Q2H    363.539 379.742        0.001


##### Conclusion is the best mixed model is the best model overall and it is

GLMM_E = glmer(Pup.Wean.Mass ~  Diet.diversity + Dietary.energy.density + (1 | Dominant.prey.species) ,
               data = seal_data, family = gaussian)

model.comparison(GLMM_D, GLMM_E)

#       aic      bic           bayes.factor p
#GLMM_E 355.534 365.661        7.483






####.   MAX CODE

#' Plot Continuous and Categorical Predictors Against a Response Variable
#'
#' This function takes a dataframe and creates:
#' - Scatterplots for each selected continuous predictor against a response variable.
#' - Boxplots for each selected categorical predictor against a response variable.
#'
#' @param data A dataframe containing the variables to be plotted.
#' @param response A string specifying the response variable (must be numeric).
#' @param continuous_vars A character vector of continuous predictor variables (numeric columns).
#' @param categorical_vars A character vector of categorical predictor variables (factor/character columns).
#'
#' @return A list of ggplot objects, one for each predictor variable plotted against the response.
#' @import ggplot2
#' @export
#'
#' @examples
#' # Example using the built-in mtcars dataset
#' plot_explore(mtcars, response = "mpg",
#'                 continuous_vars = c("hp", "wt", "disp"),
#'                 categorical_vars = c("cyl", "gear"))
plot_explore <- function(data, response, continuous_vars = NULL, categorical_vars = NULL) {
  library(ggplot2)  # Load ggplot2 for visualization
  library(gridExtra)  # Load gridExtra for arranging plots

  plots <- list()  # Store plots

  # Ensure the response and selected variables exist in the dataframe
  all_vars <- c(response, continuous_vars, categorical_vars)
  valid_vars <- all_vars[all_vars %in% names(data)]  # Filter out non-existing variables

  # Remove rows with NAs in the valid response and predictor variables
  data_clean <- data[complete.cases(data[valid_vars]), ]

  # 1 Scatterplots for Continuous Predictors
  if (!is.null(continuous_vars)) {
    continuous_vars <- continuous_vars[continuous_vars %in% valid_vars]  # Ensure continuous_vars are valid
    for (var in continuous_vars) {
      p <- ggplot(data_clean, aes_string(x = var, y = response)) +
        geom_point(alpha = 0.6, color = "black") +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        labs(title = paste(response, "vs", var),
             x = var, y = response) +
        theme_minimal() +theme(
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.spacing = unit(2, "lines"))
      plots[[var]] <- p  # Add to list
    }
  }

  # 2 Boxplots for Categorical Predictors
  if (!is.null(categorical_vars)) {
    categorical_vars <- categorical_vars[categorical_vars %in% valid_vars]  # Ensure categorical_vars are valid
    for (var in categorical_vars) {
      # Check if the variable is factor/character, and if not, convert it to factor
      data_clean[[var]] <- as.factor(data_clean[[var]])  # Convert categorical variable to factor
      p <- ggplot(data_clean, aes_string(x = var, y = response)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(title = paste(response, "by", var),
             x = var, y = response) +
        theme_minimal() +theme(
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.spacing = unit(2, "lines"))
      plots[[var]] <- p  # Add to list
    }
  }

  # 3 Create a Composite Figure (Grid of All Plots)
  if (length(plots) > 0) {
    # Arrange plots in a grid layout
    grid.arrange(grobs = plots, ncol = 2)  # Change ncol to adjust layout
  } else {
    message("No plots were created.")
  }

  return(plots)  # Return all plots in a list
}

