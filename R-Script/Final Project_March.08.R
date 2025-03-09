Final Project

# Clear Workspace
rm(list=ls())

# Load Data

seal_data = read.csv("Data_Q1.csv", header = T)
summary(seal_data)

seal_data



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
ggplot(seal_data) + geom_point(aes(seal_data$Dietary.energy.density, seal_data$Mass.change, color = seal_data$MomID)) +
  labs(title = "Mass Change and Dietary Energy Density", x = "Dietary Energy Density (kJ/g of prey tissue)", y = "Mass Change (Kg)") +
  geom_smooth(aes(seal_data$Dietary.energy.density, seal_data$Mass.change), method="lm", se=F)

# Mass Change + Diet Diveristy
ggplot(seal_data) + geom_point(aes(seal_data$Diet.diversity, seal_data$Mass.change, color = seal_data$MomID)) +
  labs(title = "Mass Change and Diet Diversity", x = "Diet Diveristy (no specific units for Shannon index)", y = "Mass Change (Kg)") +
  geom_smooth(aes(seal_data$Diet.diversity, seal_data$Mass.change), method="lm", se=F)

# Mass Change + Year
ggplot(seal_data) + geom_point(aes(seal_data$Year, seal_data$Mass.change, color = seal_data$MomID)) +
  labs(title = "Mass Change and Year", x = "Year", y = "Mass Change (Kg)") +
  geom_smooth(aes(seal_data$Year, seal_data$Mass.change), method="lm", se=F)

# Mass Change + MomID
ggplot(seal_data) + geom_point(aes(seal_data$MomID, seal_data$Mass.change)) +
  labs(title = "Mass Change and Mom ID", x = "Mom ID", y = "Mass Change (Kg)")
geom_smooth(aes(seal_data$MomID, seal_data$Mass.change), method="lm", se=F)

# Mass Change + Dominant Prey Species
ggplot(seal_data) + geom_point(aes(seal_data$Dominant.prey.species, seal_data$Mass.change)) +
  labs(title = "Mass Change and Dominant Prey Species", x = "Dominant Prey Species", y = "Mass Change (Kg)")
geom_smooth(aes(seal_data$Dominant.prey.species, seal_data$Mass.change), method="lm", se=F)



###### Fit Linear Model (all predictor variables)

## convert catagorical data to factors

seal_data$MomID = as.factor(seal_data$MomID)
seal_data$Year = as.factor(seal_data$Year)
seal_data$Dominant.prey.species = as.factor(seal_data$Dominant.prey.species)


Q1 = lm(seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year + seal_data$MomID,
        data = seal_data)

summary(Q1)

# p-value: 1.006e-06
# Adjusted R-squared - 0.5174
plot(Q1)


###### Fit Generalized Linear Model (all predictor variables)

Gaus = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
            + seal_data$Year + seal_data$MomID, data = seal_data, family = gaussian)

summary (Gaus)
#AIC = 684.76

plot (Gaus)

## Before we step the models and reduce predictor variabvles lets compair the plots
## of the linear model and the GLM

compare.plot





#Step Function

fwd.model = step(Gaus, direction='forward')
backward.model = step(Gaus, direction='backward')

### step backward found that the best model only included 3 predictor variables
# and has a AIC = 683.07

## seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity +
## seal_data$Year

Q1_A = glm (seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity +
              + seal_data$Year, data = seal_data, family = gaussian)

summary (Q1_A)

## AIC = 683.07

plot (Q1_A)

### Fit a GLMM

library(lme4)

Gaus_mixed_full_1 = glmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                            (1 | Year) + (1 | MomID), data = seal_data, family = gaussian)

Gaus_mixed_full_2 = glmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                            (1 | Year), data = seal_data, family = gaussian)


summary (Gaus_mixed_full_2)

summary (Gaus_mixed_full_1)

# Comparing GLMM's
model.comparison(Gaus_mixed_full_1, Gaus_mixed_full_2 )
# Gaus_mixed_full_2 is better model AIC = 649.36

# Comparing best LM to best GLM (Gaussian)
model.comparison(Q1, Q1_A)
# Q1_A (GLM) is a better model AIC = 683.07

# Comparing best GGLM and GLM
model.comparison(Q1_A, Gaus_mixed_full_2 )
# Gaus_mixed_full_2 is better model AIC = 649.36

Gaus_mixed_full_2 = glmer(Mass.change ~ Dominant.prey.species + Diet.diversity +
                            (1 | Year), data = seal_data, family = gaussian)













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




Hllo


