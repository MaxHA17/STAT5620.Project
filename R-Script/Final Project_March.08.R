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
data = cystfibr

a = flexplot (age~1, data=cystfibr)
b = flexplot (sex~1, data=cystfibr)
c = flexplot (height~1, data=cystfibr)
d = flexplot (weight~1, data=cystfibr)
e = flexplot (bmp~1, data=cystfibr)
f = flexplot (fev1~1, data=cystfibr)
g = flexplot (rv~1, data=cystfibr)
h = flexplot (frc~1, data=cystfibr)
i = flexplot (tlc~1, data=cystfibr)
j = flexplot (pemax~1, data=cystfibr)

require (cowplot)
plot_grid(a,b,c,d,e,f,g,h,i,j)








###### Fit Linear Model (all predictor variables)

Q1 = lm(seal_data$Mass.change ~ seal_data$Dominant.prey.species + seal_data$Diet.diversity + seal_data$Dietary.energy.density
        + seal_data$Year + seal_data$MomID,
        data = seal_data)

summary(Q1)





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



##Explore the data with plots

# Mass Change + Dietary.energy.density
ggplot(seal_data) + geom_point(aes(seal_data$Mass.change, seal_data$Dietary.energy.density, color = seal_data$MomID)) +
labs(title = "Mass Change and Dietary Energy Density", x = "Mass Change", y = "Dietary Energy Density") +
geom_smooth(aes(seal_data$Mass.change, seal_data$Dietary.energy.density), method="lm", se=F)

# Mass Change + Diet Diveristy
ggplot(seal_data) + geom_point(aes(seal_data$Mass.change, seal_data$Diet.diversity, color = seal_data$MomID)) +
labs(title = "Mass Change and Diet Diversity", x = "Mass Change", y = "Diet Diversity ") +
geom_smooth(aes(seal_data$Mass.change, seal_data$Diet.diversity), method="lm", se=F)

# Mass Change + Year
ggplot(seal_data) + geom_point(aes(seal_data$Mass.change, seal_data$Year, color = seal_data$MomID)) +
labs(title = "Mass Change and Year", x = "Mass Change",
     y = "Year")

# Mass Change + MomID
ggplot(seal_data) + geom_point(aes(seal_data$Mass.change, seal_data$MomID)) +
  geom_smooth(aes(seal_data$Mass.change, seal_data$MomID), method="lm", se=F)



seal_data$MomID = as.factor(seal_data$MomID)

# Mom ID and Diet Diveristy
ggplot(seal_data) + geom_point(aes(seal_data$MomID, seal_data$Diet.diversity, color = seal_data$MomID)) +
  labs(title = "Mother ID and Diet Diversity", x = "Mother ID",
       y = "Diet Diversity")

# Mom ID and Dietary Engery Density
ggplot(seal_data) + geom_point(aes(MomID,Dietary.energy.density, color = seal_data$MomID)) +
  labs(title = "Mother ID and Dietary Energy Density", x = "Mother ID",
    y = "Dietary Energy Density")



###### Exploring Data and Distribution of Each Variale (Predictor and Response )


# Plot varibles to see distributions of predictor and response variables
require(flexplot)
data = cystfibr

a = flexplot (age~1, data=cystfibr)
b = flexplot (sex~1, data=cystfibr)
c = flexplot (height~1, data=cystfibr)
d = flexplot (weight~1, data=cystfibr)
e = flexplot (bmp~1, data=cystfibr)
f = flexplot (fev1~1, data=cystfibr)
g = flexplot (rv~1, data=cystfibr)
h = flexplot (frc~1, data=cystfibr)
i = flexplot (tlc~1, data=cystfibr)
j = flexplot (pemax~1, data=cystfibr)

require (cowplot)
plot_grid(a,b,c,d,e,f,g,h,i,j)




ffff




