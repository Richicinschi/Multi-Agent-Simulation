# Load required libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)
library(rstatix)
library(effectsize)
library(gridExtra)
library(scales)
library(zoo)
library(car)
library(corrplot)
library(mgcv)
library(gratia)

analyze_climate_comprehensive <- function() {
  # Create output directories
  dir.create("results/climate", recursive = TRUE, showWarnings = FALSE)
  dir.create("results/climate/plots", recursive = TRUE, showWarnings = FALSE)

  # Read data files from resultsSQ1 directory
  detailed_files <- list.files("resultsSQ1", pattern = "data.*\\.csv$", full.names = TRUE)

  # Read and process data with corrected resistance categories
  detailed_data <- map_df(detailed_files, function(file) {
    filename <- basename(file)
    params <- str_match(filename, "hr(\\d+\\.\\d+)_cr(\\d+\\.\\d+)")

    suppressMessages(read_csv(file, show_col_types = FALSE)) %>%
      mutate(
        heat_resistance = as.numeric(params[2]),
        cold_resistance = as.numeric(params[3]),
        run_id = cur_group_id(),

        # Calculate core metrics
        total_regional = hot_region_infected + cold_region_infected + moderate_region_infected,
        hot_proportion = hot_region_infected / total_regional,
        cold_proportion = cold_region_infected / total_regional,
        moderate_proportion = moderate_region_infected / total_regional,

        # Calculate spread rates
        spread_rate = (total_infected - lag(total_infected)) / pmax(1, lag(total_infected)),

        # Calculate transmission efficiency
        hot_transmission_efficiency = hot_region_transmission_rate / pmax(0.01, hot_proportion),
        cold_transmission_efficiency = cold_region_transmission_rate / pmax(0.01, cold_proportion),

        # Dominant region identification
        dominant_region = case_when(
          hot_proportion > cold_proportion & hot_proportion > moderate_proportion ~ "Hot",
          cold_proportion > hot_proportion & cold_proportion > moderate_proportion ~ "Cold",
          TRUE ~ "Moderate"
        ),

        # Wave identification
        wave = case_when(
          tick <= 150 ~ "First Wave",
          TRUE ~ "Second Wave"
        ),

        # Resistance categories with correct ordering
        heat_resistance_cat = factor(case_when(
          heat_resistance <= 0.3 ~ "Low",
          heat_resistance <= 0.6 ~ "Medium",
          TRUE ~ "High"
        ), levels = c("Low", "Medium", "High")),

        cold_resistance_cat = factor(case_when(
          cold_resistance <= 0.3 ~ "Low",
          cold_resistance <= 0.6 ~ "Medium",
          TRUE ~ "High"
        ), levels = c("Low", "Medium", "High"))
      )
  })

  # 1. Disease Progression Plot
  p1 <- ggplot(detailed_data,
               aes(x = tick, y = total_infected,
                   color = interaction(heat_resistance, cold_resistance))) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
    scale_color_viridis_d(
      name = "Resistance\n(Heat, Cold)",
      labels = function(x) gsub("\\.", ", ", x)
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right",
      legend.text = element_text(size = 8)
    ) +
    labs(
      title = "Disease Progression by Temperature Resistance",
      subtitle = "Smoothed trends with confidence intervals",
      x = "Time (Ticks)",
      y = "Total Infected Population"
    )

  # 2. Regional Distribution Plot
  p2 <- detailed_data %>%
    group_by(heat_resistance, cold_resistance, tick) %>%
    summarize(
      across(
        c(hot_proportion, cold_proportion, moderate_proportion),
        list(mean = mean, sd = sd),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    ggplot() +
    geom_ribbon(aes(x = tick,
                    ymin = hot_proportion_mean - hot_proportion_sd,
                    ymax = hot_proportion_mean + hot_proportion_sd,
                    fill = "Hot"), alpha = 0.2) +
    geom_line(aes(x = tick, y = hot_proportion_mean, color = "Hot")) +
    geom_ribbon(aes(x = tick,
                    ymin = cold_proportion_mean - cold_proportion_sd,
                    ymax = cold_proportion_mean + cold_proportion_sd,
                    fill = "Cold"), alpha = 0.2) +
    geom_line(aes(x = tick, y = cold_proportion_mean, color = "Cold")) +
    geom_ribbon(aes(x = tick,
                    ymin = moderate_proportion_mean - moderate_proportion_sd,
                    ymax = moderate_proportion_mean + moderate_proportion_sd,
                    fill = "Moderate"), alpha = 0.2) +
    geom_line(aes(x = tick, y = moderate_proportion_mean, color = "Moderate")) +
    facet_wrap(~ heat_resistance + cold_resistance,
               labeller = labeller(.cols = label_both),
               ncol = 4) +
    scale_color_manual(values = c("Hot" = "#FF4E4E", "Cold" = "#4E9BFF", "Moderate" = "#4EFF4E")) +
    scale_fill_manual(values = c("Hot" = "#FF4E4E", "Cold" = "#4E9BFF", "Moderate" = "#4EFF4E")) +
    theme_minimal() +
    labs(title = "Regional Distribution of Infections",
         subtitle = "Proportion of total infections by region over time",
         x = "Time (Ticks)",
         y = "Proportion of Total Infections",
         color = "Region Type",
         fill = "Region Type")

  # 3. Peak Analysis
  peak_data <- detailed_data %>%
    group_by(heat_resistance_cat, cold_resistance_cat, run_id) %>%
    summarize(
      peak_infected = max(total_infected),
      time_to_peak = tick[which.max(total_infected)],
      .groups = "drop"
    )

  # Peak Infection Plot with correct ordering
  p3 <- ggplot(peak_data %>%
               group_by(heat_resistance_cat, cold_resistance_cat) %>%
               summarize(avg_peak = mean(peak_infected), .groups = "drop"),
         aes(x = heat_resistance_cat, y = cold_resistance_cat)) +
    geom_tile(aes(fill = avg_peak)) +
    scale_fill_viridis_c(name = "Average Peak\nInfected") +
    theme_minimal() +
    labs(title = "Peak Infection by Resistance Values",
         x = "Heat Resistance",
         y = "Cold Resistance")

  # 4. Wave Analysis Plot
  p4 <- detailed_data %>%
    group_by(heat_resistance_cat, cold_resistance_cat, wave) %>%
    summarize(
      peak_infected = max(total_infected),
      mean_spread_rate = mean(spread_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = heat_resistance_cat, y = cold_resistance_cat)) +
    geom_tile(aes(fill = peak_infected)) +
    facet_wrap(~wave) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "Peak Infections by Wave",
         x = "Heat Resistance",
         y = "Cold Resistance")

  # Statistical Analysis

  # 1. Two-way ANOVA
  anova_model <- aov(total_infected ~ heat_resistance_cat * cold_resistance_cat,
                     data = filter(detailed_data, tick == max(tick)))

  # 2. GAM Analysis
  gam_model <- gam(total_infected ~ s(heat_resistance, k = 4, bs = "cs") +
                                  s(cold_resistance, k = 4, bs = "cs") +
                                  ti(heat_resistance, cold_resistance, k = 3),
                   data = filter(detailed_data, tick == max(tick)))

  # 3. Regional Transitions
  regional_transitions <- detailed_data %>%
    group_by(heat_resistance_cat, cold_resistance_cat) %>%
    summarize(
      first_hot_dominance = first(tick[hot_proportion > 0.5]),
      first_cold_dominance = first(tick[cold_proportion > 0.5]),
      transition_count = sum(dominant_region != lag(dominant_region), na.rm = TRUE),
      final_dominant = last(dominant_region),
      .groups = "drop"
    )

  # Generate comprehensive report
  sink("results/climate/climate_comprehensive_analysis.txt")

  cat("COMPREHENSIVE CLIMATE RESISTANCE ANALYSIS\n")
  cat("========================================\n\n")

  cat("1. ANOVA RESULTS\n")
  cat("---------------\n")
  print(summary(anova_model))
  print(effectsize::eta_squared(anova_model))

  cat("\n2. GAM ANALYSIS\n")
  cat("---------------\n")
  print(summary(gam_model))

  cat("\n3. REGIONAL TRANSITIONS\n")
  cat("--------------------\n")
  print(regional_transitions)

  # Correlation analysis
  cat("\n4. CORRELATION ANALYSIS\n")
  cat("--------------------\n")
  correlation_matrix <- detailed_data %>%
    select(total_infected, hot_proportion, cold_proportion,
           hot_region_transmission_rate, cold_region_transmission_rate) %>%
    cor(use = "complete.obs")
  print(correlation_matrix)

  sink()

  # Save all plots
  ggsave("results/climate/plots/disease_progression.png", p1, width = 12, height = 8)
  ggsave("results/climate/plots/regional_distribution.png", p2, width = 15, height = 12)
  ggsave("results/climate/plots/peak_infections.png", p3, width = 10, height = 8)
  ggsave("results/climate/plots/wave_analysis.png", p4, width = 12, height = 8)
  ggsave("results/climate/plots/gam_analysis.png", draw(gam_model), width = 10, height = 8)

  # Return complete results
  return(list(
    plots = list(
      progression = p1,
      regional = p2,
      peak = p3,
      waves = p4,
      gam = draw(gam_model)
    ),
    models = list(
      anova = anova_model,
      gam = gam_model
    ),
    statistics = list(
      regional_transitions = regional_transitions,
      correlations = correlation_matrix
    ),
    data = detailed_data
  ))
}

# Run the analysis
results <- analyze_climate_comprehensive()

# Display plots
print(results$plots$progression)
print(results$plots$regional)
print(results$plots$peak)
print(results$plots$waves)
print(results$plots$gam)