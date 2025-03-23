##Data modelling - question 2
#Research question:	Does female diet predict pup weaning mass in Northwest Atlantic grey seals?

#Load the data
library(readr)
Data_Q2 <- read_csv("Processed_Data/Data_Q2.csv")
View(Data_Q2)

#Ensure the data types are correctly assigned
Data_Q2$MomID <- as.factor(Data_Q2$MomID)
Data_Q2$Year <- as.factor(Data_Q2$Year)
Data_Q2$`Mom Age` <- as.factor(Data_Q2$`Mom Age`)
Data_Q2$`Pup sex` <- as.factor(Data_Q2$`Pup sex`)
Data_Q2$`Dominant prey species`<- as.character(Data_Q2$`Dominant prey species`)
Data_Q2$`Diet diversity` <- as.numeric(Data_Q2$`Diet diversity`)
Data_Q2$`Dietary energy density` <- as.numeric(Data_Q2$`Dietary energy density`)
Data_Q2$`Pup Wean Mass` <- as.numeric(Data_Q2$`Pup Wean Mass`)

summary(Data_Q2)

#rename columns
library(dplyr)
Data_Q2 <- Data_Q2 %>%
  rename(
    DietEngDen = `Dietary energy density`,
    DietDiv = `Diet diversity`,
    DomSpp = `Dominant prey species`,
    MomAge = `Mom Age`,
    WeanMass = 'Pup Wean Mass',
    PupSex = 'Pup sex')

#Check number of observations for each dominant prey species
table(Data_Q2$DomSpp)

#remove white hake since there is only 1 observation
Data_Q2 <- Data_Q2 %>%
  filter(DomSpp != "WhiteHake")
#With only 3 observations of capelin and 1 observation of white hake, these individuals may have to be removed

##plot and explore the data
library(STAT5620.Project)
see <- plot_explore(Data_Q2, response = "WeanMass",
                    continuous_vars = c("DietEngDen", "DietDiv", "MomAge"),
                    categorical_vars = c("Year", "DomSpp", "PupSex"))
see

#Initially, it looks like weaning mass is negatively correlated with diet diversity and having pollock as the dominant prey species.

##Prepare a glm
#Plot the distribution of mass change
hist(Data_Q2$WeanMass)

library(lme4)
pup_mod <- glm(data=Data_Q2, family = Gamma(link="log"), formula = WeanMass ~ DietDiv + DietEngDen +  Year + I(MomAge^2) + PupSex + DomSpp)

summary(pup_mod)

#A gamma distribution slightly improves residuals over a gaussian family link function.

#The initial model suggests that individuals with higher diet diversity and pollock as the dominant prey species wean lighter pups. Note that these were the species with few observations.

#Check residuals
plot(pup_mod)

#Residuals look good outside of the tails at very high fitted values. However, the data has already been processed and implausible values removed so high leverage values are not too concerning.

#Now use step selection function to find the best model
step(pup_mod, direction = "both")

#The lowest AIC model retains only diet diversity and dominant prey species. Rerun the glm with the lowest AIC model

pup_mod2 <- glm(formula = WeanMass ~ DietDiv + DomSpp, family = Gamma(link = "log"),
                data = Data_Q2)

summary(pup_mod2)


##Create a fitted vs predicted values plots
# Get the fitted values (predicted values from the model)
fitted_values <- fitted(pup_mod2)

# Get the observed response variable (actual values)
observed_values <- Data_Q2$WeanMass

# Plot the fitted vs. observed values
ggplot(data = Data_Q2, aes(x = fitted_values, y = observed_values)) +
  geom_point(color = "blue") +  # Plot the points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Add 1:1 line
  labs(
    title = "Fitted vs. Observed Weaning Mass Values",
    x = "Fitted Values",
    y = "Observed Values"
  ) +
  theme_minimal()


##Make a boxplot to demonstrate the effect.
ggplot(Data_Q2, aes(x = DomSpp, y = WeanMass, fill = DomSpp)) +
  geom_boxplot() +  # Create the boxplot
  stat_summary(
    fun = "mean", geom = "point", shape = 18, size = 3, color = "red"  # Add mean as a red point
  ) + geom_text(x = "Pollock", y = 45, label = "*",  # Asterisk at "Capelin" on x-axis and y = 45
                aes(x = x, y = y, label = label),  # Position asterisk
                color = "black", size = 6, fontface = "bold"
  ) +
  labs(
    title = "Median (black line) mean (red dot) pup weaning mass by dominant prey species",
    x = "Prey species",
    y = "Pup weaning mass (kg)", fill = "Dominant prey species"
  ) +
  theme_minimal() + theme(
    axis.line = element_line(color = "black", size = 1),  # Add axis lines
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(Data_Q2$WeanMass) + 55, by = 10)
  )
