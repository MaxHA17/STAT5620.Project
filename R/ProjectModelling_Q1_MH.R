##Data modelling question 1
#Research question:	Does variation in diet influence maternal mass change over the foraging period leading up to parturition in Northwest Atlantic grey seals?

#Load the data
library(readr)
Data_Q1 <- read_csv("Processed_Data/Data_Q1.csv")
View(Data_Q1)

#Ensure the data types are correctly assigned
Data_Q1$MomID <- as.factor(Data_Q1$MomID)
#Data_Q1$Year <- as.factor(Data_Q1$Year)
Data_Q1$`Dominant prey species`<- as.character(Data_Q1$`Dominant prey species`)
Data_Q1$`Diet diversity` <- as.numeric(Data_Q1$`Diet diversity`)
Data_Q1$`Dietary energy density` <- as.numeric(Data_Q1$`Dietary energy density`)

summary(Data_Q1)

#rename columns
library(dplyr)
Data_Q1 <- Data_Q1 %>%
  rename(
    DietEngDen = `Dietary energy density`,
    DietDiv = `Diet diversity`,
    DomSpp = `Dominant prey species`,
    MassChange = `Mass change`)

#Check number of observations for each dominant prey species
table(Data_Q1$DomSpp)

#remove white hake since there is only 1 observation
Data_Q1 <- Data_Q1 %>%
  filter(DomSpp != "WhiteHake")

#With only 3 observations of capelin and 1 observation of white hake, these individuals may have to be removed

##plot and explore the data
library(STAT5620.Project)
see <- plot_explore(Data_Q1, response = "MassChange",
             continuous_vars = c("DietEngDen", "DietDiv"),
             categorical_vars = c("Year", "DomSpp"))
see

##Initially, it appears that both diet diversity and energy density are negatively related to mass change.
##It also appears that animals with cod, redfish or sandlance as the dominant prey species gain more mass during foraging.

##Prepare a glm
#Plot the distribution of mass change
hist(Data_Q1$MassChange)

library(lme4)
mom_mod <- glm(data=Data_Q1, family = gaussian(link="identity"), formula = MassChange ~ DietDiv + DietEngDen + Year + DomSpp)

summary(mom_mod)

#The initial model suggests that individuals with capelin or pollock as the dominant prey species gain less mass during foraging. Note that these were the species with few observations.

#Check residuals
par(mfrow = c(2, 2))
plot(mom_mod)

#The q-q plot suggests model fit is not great. The values cannot be log transformed because many are negative. Try adding a constant value (the largest negative number) to all mass values
Data_1 <- Data_Q1
Data_1$MassChange <- Data_Q1$MassChange + 50

#Repated the analysis with a log link function
mom_mod <- glm(data=Data_1, family = gaussian(link="log"), formula = MassChange ~ DietDiv + DietEngDen + Year + DomSpp)

summary(mom_mod)

par(mfrow = c(2, 2))
plot(mom_mod)

#This greatly improved the residual plots

#Now use step selection function to find the best model
step(mom_mod, direction = "both")

#The lowest AIC model removes energy density. Rerun the glm with the lowest AIC model

mom_mod2 <- glm(formula = MassChange ~ DietDiv + Year + DomSpp, family = gaussian(link = "log"),
    data = Data_1)

summary(mom_mod2)


coef_estimates <- coef(summary(mom_mod2))[, "Estimate"]
response_scale_coefs <- exp(coef_estimates)
print(response_scale_coefs)

plot(mom_mod2)

##Create a fitted vs predicted values plots
# Get the fitted values (predicted values from the model)
fitted_values1 <- fitted(mom_mod2)

# Get the observed response variable (actual values)
observed_values1 <- Data_1$MassChange

# Plot the fitted vs. observed values
ggplot(data = Data_1, aes(x = fitted_values1, y = observed_values1)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add 1:1 line
  labs(
    title = "Fitted vs. Observed Vlues with 1:1 line",
    x = "Fitted Values",
    y = "Observed Values"
  ) +
  theme_minimal()



##Make a boxplot to demonstrate the effect. add astricks to the plot?
library(ggsignif)
ggplot(Data_Q1, aes(x = DomSpp, y = MassChange, fill = DomSpp)) +
  geom_boxplot() +
  stat_summary(
    fun = "mean", geom = "errorbar",
    aes(ymax = ..y.., ymin = ..y..),  # Make it a horizontal line
    width = 0.75, color = "red", size = 1.2  # Control line width and color
  )  + geom_text(x = "Capelin", y = -5, label = "*",  # Asterisk at "Capelin" on x-axis and y = -5
    aes(x = x, y = y, label = label),  # Position asterisk
    color = "black", size = 6, fontface = "bold"
  ) + geom_text(x = "Pollock", y = 20, label = "*",  # Asterisk at "Pollock" on x-axis and y = 20
                aes(x = x, y = y, label = label),  # Position asterisk
                color = "black", size = 6, fontface = "bold"
  ) +
  labs(
    title = "Median (black line) mean (red line) mass change by dominant prey species",
    x = "Prey species",
    y = "Mass change (kg)", fill = "Dominant prey species"
  ) +
  theme_minimal() + theme(
    axis.line = element_line(color = "black", size = 1),  # Add axis lines
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(Data_Q1$MassChange) + 55, by = 25) - 50
  )
