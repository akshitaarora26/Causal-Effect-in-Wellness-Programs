library(broom)

install.packages("broom")
# Load the claims dataset from a Stata file
claims <- haven::read_dta("https://raw.githubusercontent.com/aamish29/bigdata/main/claims.dta")

# View the first few rows of the dataset
head(claims)

# Helper function to perform linear regression and extract relevant statistics
analyze_variable <- function(variable) {
  formula <- as.formula(paste(variable, "~ treat"))
  model <- lm(formula, data = claims)
  tidy_model <- tidy(model)
  
  control_mean <- coef(model)[1]  # Intercept (Control group mean)
  treatment_mean <- coef(model)[1] + coef(model)[2]  # Intercept + Coefficient (Treatment group mean)
  p_value <- tidy_model$p.value[2]  # p-value for the treatment effect
  
  # Return the results as a data frame row
  data.frame(
    Variable = variable,
    Control_Mean = control_mean,
    Treatment_Mean = treatment_mean,
    P_Value = p_value
  )
}

# List of pre-randomization variables
pre_randomization_vars <- c("covg_0715_0716", "diabetes_1015_0716", "hyperlipidemia_1015_0716", 
                            "hypertension_1015_0716", "pcp_any_office_1015_0716", "spendOff_0715_0716")

# Apply the analysis function to each variable and combine results into a table
results <- do.call(rbind, lapply(pre_randomization_vars, analyze_variable))

# Display the results table
print(results)


# List of outcome variables (first year following randomization)
outcome_vars <- c("covg_0816_0717", "diabetes_0816_0717", "hyperlipidemia_0816_0717", "hypertension_0816_0717",
                  "pcp_any_office_0816_0717", "spendOff_0816_0717")

# Function to run regression and extract results
analyze_outcome <- function(variable) {
  # Regression without demographic controls
  formula1 <- as.formula(paste(variable, "~ treat"))
  model1 <- lm(formula1, data = claims)
  summary1 <- tidy(model1)
  est_diff1 <- summary1$estimate[2]
  se1 <- summary1$std.error[2]
  
  # Regression with demographic controls
  formula2 <- as.formula(paste(variable, "~ treat + male + white + age37_49 + age50"))
  model2 <- lm(formula2, data = claims)
  summary2 <- tidy(model2)
  est_diff2 <- summary2$estimate[2]
  se2 <- summary2$std.error[2]
  
  # Return a data frame row with the results
  data.frame(
    Variable = variable,
    No_Controls = paste0(round(est_diff1, 4), " (", round(se1, 4), ")"),
    With_Controls = paste0(round(est_diff2, 4), " (", round(se2, 4), ")")
  )
}

# Apply the analysis function to each outcome variable
results <- do.call(rbind, lapply(outcome_vars, analyze_outcome))

# Display the results table
print(results)

# Example of outcomes you want to analyze
outcome_variables <- c("completed_screening_nomiss_2016")

# Function to run the regressions and capture results
calculate_regression <- function(data, outcome_var) {
  # No controls regression
  model_no_controls <- lm(data[[outcome_var]] ~ treat, data = data)
  no_controls_estimate <- coef(model_no_controls)[2]  # difference between participants & non-participants
  no_controls_se <- summary(model_no_controls)$coefficients[2, 2]  # standard error
  
  # With controls regression
  model_with_controls <- lm(data[[outcome_var]] ~ treat + male + white + age37_49 + age50, data = data)
  with_controls_estimate <- coef(model_with_controls)[2]  # difference between participants & non-participants
  with_controls_se <- summary(model_with_controls)$coefficients[2, 2]  # standard error
  
  return(data.frame(
    Variable = outcome_var,
    No_Controls = paste0(round(no_controls_estimate, 4), " (", round(no_controls_se, 4), ")"),
    With_Controls = paste0(round(with_controls_estimate, 4), " (", round(with_controls_se, 4), ")")
  ))
}

# Assuming 'claims_data' is the dataset containing the claims data
results <- do.call(rbind, lapply(outcome_variables, function(outcome) calculate_regression(claims, outcome)))

# Display the results
print(results)

