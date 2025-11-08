library(dplyr)
library(ggplot2)
library(broom)
library(sjPlot)
library(forcats)

# run_logistic_model
## fits a logistic regression model for primary success (win/loss)
## given user-specified predictors.

run_logistic_model <- function(data, formula) {
  model <- glm(formula, data = data, family = "binomial")
  return(model)
}

# tidy_model_output
## produce clean summary of the model coefficients & odds ratios.

tidy_model_output <- function(model) {
  results <- broom::tidy(model) %>%
    mutate(
      odds_ratio = exp(estimate),
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE            ~ ""
      )
    ) %>%
    select(term, estimate, odds_ratio, p.value, significance)
  
  return(results)
}

# plot_model_coefficients
## visualizes logistic regression coefficients using sjPlot.

plot_model_coefficients <- function(model, title = "Logistic Regression Coefficients") {
  sjPlot::plot_model(model, show.values = TRUE, value.offset = .3, title = title)
}

# plot_win_rates
plot_win_rates <- function(data, group_var, fill_var = "won_primary") {
  ggplot(data, aes_string(x = group_var, fill = fill_var)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("Win Rates by", group_var),
      y = "Proportion of Wins",
      x = group_var
    ) +
    theme_minimal(base_size = 13)
}

# party_comparison_models
## Fits separate logistic models for Democrats and Republicans
## and returns a list of results for easy comparison.

party_comparison_models <- function(data, formula) {
  dem_data <- data %>% filter(party == "Democrat")
  gop_data <- data %>% filter(party == "Republican")
  
  dem_model <- run_logistic_model(dem_data, formula)
  gop_model <- run_logistic_model(gop_data, formula)
  
  results <- list(
    Democrat = tidy_model_output(dem_model),
    Republican = tidy_model_output(gop_model)
  )
  return(results)
}

# visualize_predicted_probabilities

visualize_predicted_probabilities <- function(model, variable, data) {
  new_data <- data %>%
    group_by(!!sym(variable)) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  new_data$predicted_prob <- predict(model, newdata = new_data, type = "response")
  
  ggplot(new_data, aes_string(x = variable, y = "predicted_prob", fill = variable)) +
    geom_col() +
    labs(
      title = paste("Predicted Probability of Winning by", variable),
      y = "Predicted Probability",
      x = variable
    ) +
    theme_minimal(base_size = 13)
}
