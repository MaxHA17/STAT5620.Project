Final Project

# Clear Workspace
rm(list=ls())

# Load Data

seal_data = read.csv("Data_Q1.csv", header = T)
summary(seal_data)

seal_data

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













##### Testing Assumptions

qqnorm(M_total$residuals)
# Therefore Figure 3G demonstrates that we can assume normality of the data.

#Histogram of the residuals
hist(M_total$residuals)
# see Figure 3E, the bell shaped curve of the histogram suggests normality.

#Shapiro-Wilk's Test
shapiro.test(M_total$residuals)
# data:  Mtotal$residuals
# W = 0.98797, p-value = 0.9111
# note the p-value is not significant so their is not a variation of the assumption of normality.

# Therefore from these 3 explorations we are now confident in normality of the data.

### CHECKING FOR HOMOGENEITY OF VARIANCE

# Breusch_Pagan Test
bptest(M_total)
# BP = 3.3469, df = 5, p-value = 0.3412
# Thus the p-value is greater then 0.05 so we accept the null hypothesis and assume we have
# homoscedasticity.

### We assumed a Fixed X

### Checking for Independence of Predictor Varibles

# Dublin-Watson
dwtest(M_total)
# DW = 0.92502, p-value = 0.3412
# this p-value suggests no autocorrelation.



#Figure 1G
pairs(migraA[c(1:5)])

# numerically inspect the data by plotting with paris to inpect
# for collinearity

abs(cor(migraA[c(1:5)]))

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

### Autumn Values

# Length
M1 = lm(Autumn.value ~ Mean.Length..cm., data = log_migraD)
summary(M1)
# P-value = 0.4785
# R-squared = 0.01148

# Weight
M2 = lm(Autumn.value ~ Mean.Wieght...g., data = log_migraD)
summary (M2)
# P-value = 0.5453
# R-squared = 0.008375

# Wingspan
M3 = lm(Autumn.value ~ Mean.Wingspan..cm., data = log_migraD)
summary (M3)
# P-value = 0.4093
# R-squared = 0.01553

#Length and Wingspan
M4 = lm(Autumn.value ~ Mean.Length..cm. + Mean.Wingspan..cm., data = log_migraD)
summary (M4)
# P-value = 0.688
# Adjusted R-squared =  0.01724

#Length and Weight
M5 = lm(Autumn.value ~ Mean.Length..cm. + Mean.Wieght...g., data = log_migraD)
summary (M5)
# P-value = 0.738
# Adjusted R-squared = 0.01403

# Wingspan and Weight
M6 = lm(Autumn.value ~ Mean.Wingspan..cm. + Mean.Wieght...g., data = log_migraD)
summary (M6)
# P-value = 0.6102
# Adjusted R-squared = 0.02271


# Full Model
M_total = lm(Autumn.value ~ Mean.Wingspan..cm. + Mean.Wieght...g. + Mean.Length..cm.,
             data = log_migraD)
summary (M_total)
# P-value = 0.8022
# Adjusted R-squared = 0.02316


### Spring Values

# Length
M1 = lm(Spring.value ~ Mean.Length..cm., data = log_migraD)
summary(M1)
# P-value = 0.3769
# R-squared = 0.01779

# Weight
M2 = lm(Spring.value ~ Mean.Wieght...g., data = log_migraD)
summary (M2)
# P-value = 0.3277
# R-squared = 0.02178

# Wingspan
M3 = lm(Spring.value ~ Mean.Wingspan..cm., data = log_migraD)
summary (M3)
# P-value = 0.3642
# R-squared =  0.01875

#Length and Wingspan
M4 = lm(Spring.value ~ Mean.Length..cm. + Mean.Wingspan..cm., data = log_migraD)
summary (M4)
# P-value = 0.6647
# Adjusted R-squared =  0.01882

#Length and Weight
M5 = lm(Spring.value ~ Mean.Length..cm. + Mean.Wieght...g., data = log_migraD)
summary (M5)
# P-value = 0.6016
# Adjusted R-squared = 0.02336

# Wingspan and Weight
M6 = lm(Spring.value ~ Mean.Wingspan..cm. + Mean.Wieght...g., data = log_migraD)
summary (M6)
# P-value = 0.6215
# Adjusted R-squared = 0.02188


# Full Model
M_total = lm(Spring.value ~ Mean.Wingspan..cm. + Mean.Wieght...g. + Mean.Length..cm.,
             data = log_migraD)
summary (M_total)
# P-value = 0.7991
# Adjusted R-squared = 0.02346



###Polynomial model for wingspan

M7 = lm(Autumn.value ~ Mean.Wingspan..cm. + I(Mean.Wingspan..cm.^2), data = log_migraE)
summary (M7)
# P-value = 0.04007 **
# R-squared = 0.3488 (34.9%)

abline (M7, lwd=3, col ="red")
plot (Autumn.value, Mean.Wingspan..cm., data=M7 )
lines(smooth.spline(Mean.Wingspan..cm.,predict(M7), col="blue", lwd=3))

ggplot(log_migraE) + geom_point(aes(log_migraE$Mean.Wingspan..cm., migraE$Autumn.value)) +
  geom_smooth(aes(migraE$Mean.Wingspan..cm., migraE$Autumn.value), method="lm", se=F)


## Didn't Figure Polynomial out

#### Npw looking at >70cm wingspan


migraF = read.csv("TD(days)>70_wingspan.csv", header = T)
summary(migraF)
migraF

log_migraF = log(migraF)

##Explore the data with plots

# Autumn Mean Weight and Total Migration Speed (km/day)

ggplot(log_migraF) + geom_point(aes(log_migraF$Mean.Wieght...g., log_migraF$Autumn.value)) +
  geom_smooth(aes(log_migraF$Mean.Wieght...g., log_migraF$Autumn.value), method="lm", se=F)

#Figure 3I
# Spring Mean Weight and Total Migration Speed (km/day)
ggplot(migraF) + geom_point(aes(migraF$Mean.Wieght...g., migraF$Spring.value)) +
  geom_smooth(aes(migraF$Mean.Wieght...g., migraF$Spring.value), method="lm", se=F)

ggplot(log_migraF) + geom_point(aes(log_migraF$Mean.Wieght...g., log_migraF$Spring.value)) +
  geom_smooth(aes(log_migraF$Mean.Wieght...g., log_migraF$Spring.value), method="lm", se=F)

#Figure 3J
# Autumn Mean Wingspan and Total Migration Speed (km/day)
ggplot(migraF) + geom_point(aes(migraF$Mean.Wingspan..cm., migraF$Autumn.value)) +
  geom_smooth(aes(migraF$Mean.Wingspan..cm., migraF$Autumn.value), method="lm", se=F)

ggplot(log_migraF) + geom_point(aes(log_migraF$Mean.Wingspan..cm., log_migraF$Autumn.value)) +
  geom_smooth(aes(log_migraF$Mean.Wingspan..cm., log_migraF$Autumn.value), method="lm", se=F)

#Figure 3K
# Spring Mean Wingspan and Total Migration Speed (km/day)
ggplot(migraE) + geom_point(aes(migraE$Mean.Wingspan..cm., migraE$Spring.value)) +
  geom_smooth(aes(migraE$Mean.Wingspan..cm., migraE$Spring.value), method="lm", se=F)

ggplot(log_migraF) + geom_point(aes(log_migraF$Mean.Wingspan..cm., log_migraF$Spring.value)) +
  geom_smooth(aes(log_migraF$Mean.Wingspan..cm., log_migraF$Spring.value), method="lm", se=F)


#Figure 3L
# Autumn Mean Length and Total Migration Speed (km/day)
ggplot(migraF) + geom_point(aes(migraF$Mean.Length..cm., migraF$Autumn.value)) +
  geom_smooth(aes(migraE$Mean.Length..cm., migraE$Autumn.value), method="lm", se=F)

ggplot(log_migraF) + geom_point(aes(log_migraF$Mean.Length..cm., log_migraF$Autumn.value)) +
  geom_smooth(aes(log_migraF$Mean.Length..cm., log_migraF$Autumn.value), method="lm", se=F)


#Figure 3M
# Spring Mean Length and Total Migration Speed (km/day)
ggplot(migraF) + geom_point(aes(migraF$Mean.Length..cm., migraF$Spring.value)) +
  geom_smooth(aes(migraF$Mean.Length..cm., migraF$Spring.value), method="lm", se=F)

ggplot(log_migraF) + geom_point(aes(log_migraF$Mean.Length..cm.,log_migraF$Spring.value)) +
  geom_smooth(aes(log_migraF$Mean.Length..cm., log_migraF$Spring.value), method="lm", se=F)


###Test For Assumptions non-logged model below and find no need to log this


M_total = lm(Autumn.value ~ Mean.Wingspan..cm. + Mean.Wieght...g. + Mean.Length..cm.,
             data = migraF)


qqnorm(M_total$residuals)
# Therefore Figure 3G demonstrates that we can assume normality of the data.

#Histogram of the residuals
hist(M_total$residuals)
# see Figure 3E, the bell shaped curve of the histogram suggests normality.

#Shapiro-Wilk's Test
shapiro.test(M_total$residuals)
# data:  Mtotal$residuals
# W = 0.93883, p-value = 0.05689
# note the p-value is not significant so their is not a variation of the assumption of normality.

# Therefore from these 3 expolorations we are now confident in normality of the data.

### CHECKING FOR HOMOGENEITY OF VARIANCE

# Breusch_Pagan Test
bptest(M_total)
# BP = 1.2021, df = 5, p-value = 0.7525
# Thus the p-value is greater then 0.05 so we accept the null hypothesis and assume we have
# homoscedasticity.

### We assumed a Fixed X

### Checking for Independence of Predictor Varibles

# Dublin-Watson
dwtest(M_total)
# DW = 1.8472, p-value = 0.3223
# this p-value suggests some autocorrelation. The 1.709 DW value indicataes
# a slight positive autocorrelation but it lands within
# an acceptible range (DW between 1.5 and 2.5)


####.  Check log_migraF for Assumptions

M_total = lm(Autumn.value ~ Mean.Wingspan..cm. + Mean.Wieght...g. + Mean.Length..cm.,
             data = log_migraF)


qqnorm(M_total$residuals)
# Therefore Figure 3G demonstrates that we can assume normality of the data.

#Histogram of the residuals
hist(M_total$residuals)
# see Figure 3E, the bell shaped curve of the histogram suggests normality.

#Shapiro-Wilk's Test
shapiro.test(M_total$residuals)
# data:  Mtotal$residuals
# W = 0.95683, p-value = 0.1963
# note the p-value is not significant so their is not a variation of the assumption of normality.

# Therefore from these 3 expolorations we are now confident in normality of the data.

### CHECKING FOR HOMOGENEITY OF VARIANCE

# Breusch_Pagan Test
bptest(M_total)
# BP = 0.32141, df = 5, p-value = 0.956
# Thus the p-value is greater then 0.05 so we accept the null hypothesis and assume we have
# homoscedasticity.

### We assumed a Fixed X

### Checking for Independence of Predictor Varibles

# Dublin-Watson
dwtest(M_total)
# DW = 1.9955, p-value = 0.5253
# this p-value suggests some autocorrelation. The 1.709 DW value indicataes
# a slight positive autocorrelation but it lands within
# an acceptible range (DW between 1.5 and 2.5)


### Autumn Values

# Length
M1 = lm(Autumn.value ~ Mean.Length..cm., data = log_migraF)
summary(M1)
# P-value = 0.7223
# R-squared = 0.004 (0.4%)

# Weight
M2 = lm(Autumn.value ~ Mean.Wieght...g., data = log_migraF)
summary (M2)
# P-value = 0.4795
# R-squared = 0.01575 (1.6%)

# Wingspan
M3 = lm(Autumn.value ~ Mean.Wingspan..cm., data = log_migraF)
summary (M3)
# P-value = 0.7465
# R-squared = 0.003312 (0.3%)

#Length and Wingspan
M4 = lm(Autumn.value ~ Mean.Length..cm. + Mean.Wingspan..cm., data = log_migraF)
summary (M4)
# P-value = 0.9387
# Adjusted R-squared =  0.004071 (0.41%)

#Length and Weight
M5 = lm(Autumn.value ~ Mean.Length..cm. + Mean.Wieght...g., data = log_migraF)
summary (M5)
# P-value = 0.5883
# Adjusted R-squared = 0.03365 (3.4%)

# Wingspan and Weight
M6 = lm(Autumn.value ~ Mean.Wingspan..cm. + Mean.Wieght...g., data = log_migraF)
summary (M6)
# P-value = 0.6747
# Adjusted R-squared = 0.02507  (2.5%)


# Full Model
M_total = lm(Autumn.value ~ Mean.Wingspan..cm. + Mean.Wieght...g. + Mean.Length..cm.,
             data = log_migraF)
summary (M_total)
# P-value = 0.7576
# Adjusted R-squared = 0.03798   (3.8%)


### Spring Values

# Length
M1 = lm(Spring.value ~ Mean.Length..cm., data = log_migraF)
summary(M1)
# P-value = 0.9797
# R-squared =  2.055e-05 (0%)

# Weight
M2 = lm(Spring.value ~ Mean.Wieght...g., data = log_migraF)
summary (M2)
# P-value = 0.7533
# R-squared = 0.00313 (0.3%)

# Wingspan
M3 = lm(Spring.value ~ Mean.Wingspan..cm., data = log_migraF)
summary (M3)
# P-value = 0.885
# R-squared = 0.0006641 (0%)

#Length and Wingspan
M4 = lm(Spring.value ~ Mean.Length..cm. + Mean.Wingspan..cm., data = log_migraF)
summary (M4)
# P-value = 0.9749
# Adjusted R-squared = -0.06277    (-6.2%)

#Length and Weight
M5 = lm(Spring.value ~ Mean.Length..cm. + Mean.Wieght...g., data = log_migraF)
summary (M5)
# P-value = 0.6895
# Adjusted R-squared = -0.03928  (-3.9%)

# Wingspan and Weight
M6 = lm(Spring.value ~ Mean.Wingspan..cm. + Mean.Wieght...g., data = log_migraF)
summary (M6)
# P-value = 0.69
# Adjusted R-squared = -0.03954   (-4.0%)


# Full Model
M_total = lm(Spring.value ~ Mean.Wingspan..cm. + Mean.Wieght...g. + Mean.Length..cm.,
             data = log_migraF)
summary (M_total)
# P-value = 0.7727
# Adjusted R-squared = -0.0604  (-6.0%)


#### Summary --- small birds (under 70cm wingspan duration of migration decrease as wingspan
### increases up to 70cm then the trend is not longer significant) This is similar
# to trend in part 1 that showed the same trend for weight under 50g and daily travel speed.

## Travel speed and total migration duration are inversely relational bird size up to 50g weight
# and 70cm wingspan. Birds larger than these dimension show no corrolation between body size and
# migratory speed or duration.


fhdlsahkfldhlasd
