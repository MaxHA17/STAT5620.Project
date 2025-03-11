Final Project

# Clear Workspace
rm(list=ls())

# Load Data

seal_data_1 = read.csv("Data_Q1.csv", header = T)
summary(seal_data_1)


## Make Variables Factors

seal_data_1$MomID = as.factor(seal_data_1$MomID)
seal_data_1$Dominant.prey.species = as.factor(seal_data_1$Dominant.prey.species)

# Fit Linear Model (all predictor variables)

Q1 = lm(seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year + seal_data$MomID,
        data = seal_data)


# Fit Linear Model (all predictor variables - dominant.prey.species)

Q1_2 = lm(seal_data$Mass.change ~ seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year + seal_data$MomID,
        data = seal_data)

summary (Q1_2)


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





## Question # 1 Explore the data with plots

#### Mass Change and Predictor Variables


# Mass Change + Dietary.energy.density
ggplot(seal_data_1) + geom_point(aes(seal_data_1$Mass.change, seal_data_1$Dietary.energy.density, color = seal_data_1$MomID)) +
labs(title = "Mass Change and Dietary Energy Density", x = "Mass Change", y = "Dietary Energy Density") +
geom_smooth(aes(seal_data_1$Mass.change, seal_data_1$Dietary.energy.density), method="lm", se=F)

# Mass Change + Diet Diveristy
ggplot(seal_data_1) + geom_point(aes(seal_data_1$Mass.change, seal_data_1$Diet.diversity, color = seal_data_1$MomID)) +
labs(title = "Mass Change and Diet Diversity", x = "Mass Change", y = "Diet Diversity ") +
geom_smooth(aes(seal_data_1$Mass.change, seal_data_1$Diet.diversity), method="lm", se=F)

# Mass Change + Year
ggplot(seal_data_1) + geom_point(aes(seal_data_1$Mass.change, seal_data_1$Year, color = seal_data_1$MomID)) +
labs(title = "Mass Change and Year", x = "Mass Change",
     y = "Year")

# Mass Change + Dominant Prey Species

ggplot(seal_data_1) + geom_point(aes(seal_data_1$Mass.change, seal_data_1$Dominant.prey.species, color = seal_data_1$MomID)) +
  labs(title = "Mass Change and Diet Diversity", x = "Mass Change", y = "Dominant Prey Species")


# Mass Change + MomID
ggplot(seal_data_1) + geom_point(aes(seal_data_1$Mass.change, seal_data_1$MomID)) +
  geom_smooth(aes(seal_data_1$Mass.change, seal_data_1$MomID), method="lm", se=F)





### Mom ID and Predictor Varialbes

# Mom ID and Diet Diveristy
ggplot(seal_data_1) + geom_point(aes(seal_data_1$MomID, seal_data_1$Diet.diversity, color = seal_data_1$MomID)) +
  labs(title = "Mother ID and Diet Diversity", x = "Mother ID",
       y = "Diet Diversity")

# Mom ID and Dietary Engery Density
ggplot(seal_data) + geom_point(aes(MomID,Dietary.energy.density, color = seal_data$MomID)) +
  labs(title = "Mother ID and Dietary Energy Density", x = "Mother ID",
    y = "Dietary Energy Density")



######.  Question # 2  #########


# Load Data Question_1

seal_data_1 = read.csv("Data_Q1.csv", header = T)
summary(seal_data_1)

seal_data_1

# Load Data Question_2

seal_data_2 = read.csv("Data_Q2.csv", header = T)
summary(seal_data_2)

seal_data_2


# Fit Linear Model Question # 2 (all predictor variables)

Q2 = lm(seal_data_2$Pup.Wean.Mass ~ seal_data_2$Dominant.prey.species + seal_data_2$Diet.diversity + seal_data_2$Dietary.energy.density
        + seal_data_2$MomID + seal_data_2$Pup.sex + seal_data_2$Mom.Age,
        data = seal_data_2)
summary(Q2)

#Residual standard error: 5.696 on 45 degrees of freedom
#Multiple R-squared:  0.417,	Adjusted R-squared:  0.2874
#F-statistic: 3.219 on 10 and 45 DF,  p-value: 0.003359


# Testing Assumptions (Q2) full model

qqnorm(Q2$residuals)
# Therefore Figure 3G demonstrates that we can assume normality of the data.

#Histogram of the residuals
hist(Q2$residuals)
# see Figure 3E, the bell shaped curve of the histogram suggests normality.

#Shapiro-Wilk's Test
shapiro.test(Q2$residuals)
#data:  Q2$residuals
#W = 0.98958, p-value = 0.9098
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


Q2 = lm(seal_data$Pup.Wean.Mass ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$MomID + seal_data$Pup.sex + seal_data$Mom.Age,
        data = seal_data)
summary(Q2)



##Explore the data with plots Question_2

# Pup Wean Mass + Mother Dietary.energy.density
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Dietary.energy.density)) +
  labs(title = "Pup Wean Mass and Mother Dietary Energy Density", x = "Pup Wean Mass", y = "Dietary Energy Density") +
  geom_smooth(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Dietary.energy.density), method="lm", se=F)


# Pup Wean Mass + Mother Diet Diversity
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Diet.diversity)) +
  labs(title = "Pup Wean Mass and Mother Diet Diversity", x = "Pup Wean Mass", y = "Diet Diveristy") +
  geom_smooth(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Diet.diversity), method="lm", se=F)


# Pup Wean Mass + Mother Age
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Mom.Age)) +
  labs(title = "Pup Wean Mass and Mother Age", x = "Pup Wean Mass", y = "Age") +
  geom_smooth(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Mom.Age), method="lm", se=F)


# Pup Wean Mass + Pup Sex
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Pup.sex)) +
  labs(title = "Pup Wean Mass and Pup Sex", x = "Pup Wean Mass", y = "Pup Sex") +
  geom_smooth(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Pup.sex), method="lm", se=F)




# Load Data

seal_data_2 = read.csv("Data_Q2.csv", header = T)
summary(seal_data_2)


Make Variables Factors

seal_data_2$Dominant.prey.species = as.factor(seal_data_2$Dominant.prey.species)
seal_data_2$Dominant.prey.species = as.factor(seal_data_2$Dominant.prey.species)

# Fit Linear Model (all predictor variables)

Q2 = lm(seal_data_2$Pup.Wean.Mass ~ seal_data_2$Dietary.energy.density + seal_data_2$Diet.diversity + seal_data_2$Dominant.prey.species
        + seal_data_2$Mom.Age + seal_data_2$Pup.sex + seal_data_2$Year,
        data = seal_data_2)



## Question # 2 Explore the data with plots

#### Mass Change and Predictor Variables


# Wean Mass + Dietary Energy Density
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Dietary.energy.density)) +
  labs(title = "Wean Mass and Mother Dietary Energy Density", x = "Wean Mass", y = "Dietary Energy Density") +
  geom_smooth(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Dietary.energy.density), method="lm", se=F)

# Wean Mass + Diet Diveristy
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Diet.diversity)) +
  labs(title = "Wean Mass and Mother Diet Diversity", x = "Wean Mass", y = "Diet Diversity ") +
  geom_smooth(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Diet.diversity), method="lm", se=F)

# Wean Mass + Dominant Prey Species
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Dominant.prey.species)) +
  labs(title = "Wean Mass and Mother Dominant Prey Species", x = "Wean Mass", y = "Dominant Prey Species")

# Wean Mass + Mom Age (Year)
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Mom.Age, color = seal_data_2$Year)) +
  labs(title = "Wean Mass and Mother Age", x = "Wean Mass", y = "Mother Age")

# Wean Mass + Year
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Year)) +
  labs(title = "Wean Mass and Year", x = "Wean Mass", y = "Year")

# Wean Mass + Year
ggplot(seal_data_2) + geom_point(aes(seal_data_2$Pup.Wean.Mass, seal_data_2$Year, color = seal_data_2$Mom.Age)) +
  labs(title = "Wean Mass and Year", x = "Wean Mass", y = "Year")

