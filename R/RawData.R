#' Grey seal diet and reproductive success data
#'
#' The RawData file contains all of the raw diet and reproductive success data to be used in this project:
#'
#' - **MomID**: Unique identifier for the mother.
#' - **Year**: Deployment year.
#' - **Dietary.energy.density**: Energy density (kJ/g) of the female diet calculated from the average energy content per gram of prey body tissue.
#' - **Diet.diversity**: Standardized Shannon-Weaver diversity index to quantify individual diet diversity.
#' - **Dominant.prey.species**: The prey species making up the greatest proportion of the mothers diet.
#' - **Mom.Age**: Age of the mother in years.
#' - **Deploy.Mass**: The mothers mass (in kg) at initial deployment before the foraging period.
#' - **Recover.Mass**: The mass of the mother (in kg) when she returns to the breeding colony to give birth to her pup.
#' - **Mass.change**: The mothers recovery mass minus her deployment mass.
#' - **Pup.age.at.Mom.Mass.Measurement**: If known, the age of the pup (in days) when the mother was found and samples with her pup.
#' - **PupID**: Unique identifier for the individual pup.
#' - **Pup.sex**: A binary character representing the sex of male (1) or female (2) pups.
#' - **Pup.Birth.date**: If known, the date the pup was born.
#' - **Pup.birth.mass**: The mass (in kg) of the pup at birth.
#' - **Pup.Age.at.Weaning**: The age (in days) of the pup when the mother terminated lactation and the pup was weaned.
#' - **Pup.Wean.Mass.date**: The date the pup was weaned.
#' - **Pup.Wean.Mass**: The mass (in kg) of the pup at weaning.
#' - **Lactation.duration**: The number of days between the pups birth date and weaning date.
#'
#' @format A data frame with 80 rows and 18 variables:
#'
#' @source Data from the Department of Fisheries and Oceans Canada grey seal program
#'
#' @examples
#' data(raw_data)
#' summary(raw_data)

#Load the data and add the data to the r package
raw_data <- STAT5620_Data <- read.csv("C:/Users/Maxhe/OneDrive - Dalhousie University/Desktop/STAT5620/STAT5620_Data.csv")
