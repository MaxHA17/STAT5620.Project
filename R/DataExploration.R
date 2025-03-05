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

  # 1️⃣ Scatterplots for Continuous Predictors
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

  # 2️⃣ Boxplots for Categorical Predictors
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

  # 3️⃣ Create a Composite Figure (Grid of All Plots)
  if (length(plots) > 0) {
    # Arrange plots in a grid layout
    grid.arrange(grobs = plots, ncol = 2)  # Change ncol to adjust layout
  } else {
    message("No plots were created.")
  }

  return(plots)  # Return all plots in a list
}
