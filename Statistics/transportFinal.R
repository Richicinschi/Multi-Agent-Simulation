# Load required libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)
library(rstatix)
library(effectsize)
library(gridExtra)
library(scales)
library(zoo) # For rolling averages
library(corrplot) # For correlation analysis
library(car) # For statistical testing

analyze_transport_complete <- function() {
  # Create output directories
  dir.create("results/transport", recursive = TRUE, showWarnings = FALSE)
  dir.create("results/transport/plots", recursive = TRUE, showWarnings = FALSE)

  # Read all data files
  detailed_files <- list.files("results", pattern = "data.*\\.csv$", full.names = TRUE)

  # Read and process data
  detailed_data <- map_df(detailed_files, function(file) {
    filename <- basename(file)
    # Extract parameters from filename
    params <- str_match(filename, "hr(.+?)_cr(.+?)_tp(.+?)_is(.+?)_ap(.+?)_run(\\d+)")

    suppressMessages(read_csv(file, show_col_types = FALSE)) %>%
      mutate(
        heat_resistance = as.numeric(params[2]),
        cold_resistance = as.numeric(params[3]),
        transport_enabled = as.logical(params[4]),
        isolation_enabled = as.logical(params[5]),
        num_airports = as.numeric(params[6]),
        run_id = as.numeric(params[7]),

        # Calculate additional metrics
        transport_usage = airport_usage + port_usage,
        cross_border_ratio = cross_border_infections / (total_infected + 1), # Add 1 to avoid division by zero
        spread_rate = (total_infected - lag(total_infected)) / pmax(1, lag(total_infected)),

        # Calculate efficiency metrics
        transport_efficiency = cross_border_infections / pmax(1, transport_usage),
        regional_spread_index = num_affected_countries / pmax(1, tick),

        # Categorize transport levels
        transport_level = case_when(
          !transport_enabled ~ "No Transport",
          num_airports <= 5 ~ "Limited Transport",
          num_airports <= 15 ~ "Moderate Transport",
          TRUE ~ "Extensive Transport"
        )
      )
  })

  # 1. Disease Progression Analysis
  p1 <- ggplot(detailed_data,
               aes(x = tick, y = total_infected,
                   color = transport_level)) +
    # Replace geom_smooth(method = "loess") with:
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
    scale_color_viridis_d() +
    theme_minimal() +
    labs(title = "Disease Progression by Transport Infrastructure",
         x = "Time (Ticks)",
         y = "Total Infected Population",
         color = "Transport Level") +
    theme(legend.position = "bottom")

  # 2. Cross-Border Transmission Analysis
  p2 <- ggplot(detailed_data,
               aes(x = tick, y = cross_border_ratio,
                   color = transport_level)) +
    # Replace geom_smooth(method = "loess") with:
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
    scale_color_viridis_d() +
    scale_y_continuous(labels = percent_format()) +
    theme_minimal() +
    labs(title = "Cross-Border Transmission Patterns",
         x = "Time (Ticks)",
         y = "Proportion of Cross-Border Infections",
         color = "Transport Level")

  # Add data reduction for efficiency plot
  efficiency_plot <- detailed_data %>%
    # Sample or bin the data to reduce size
    group_by(transport_level, tick) %>%
    summarize(
      transport_usage = mean(transport_usage),
      cross_border_infections = mean(cross_border_infections),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = transport_usage, y = cross_border_infections,
               color = transport_level)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme_minimal() +
    labs(title = "Transport System Efficiency",
         x = "Transport Usage",
         y = "Cross-Border Infections",
         color = "Transport Level")

  # Add data reduction for wave analysis
  wave_analysis <- detailed_data %>%
    group_by(transport_level, tick) %>%
    summarize(
      total_infected = mean(total_infected),
      .groups = "drop"
    ) %>%
    group_by(transport_level) %>%
    mutate(
      rolling_avg = rollmean(total_infected, k = 7, fill = NA),
      wave = case_when(
        tick <= 150 ~ "First Wave",
        TRUE ~ "Second Wave"
      )
    ) %>%
    ggplot(aes(x = tick, y = rolling_avg, color = transport_level)) +
    geom_line() +
    facet_wrap(~wave) +
    theme_minimal() +
    labs(title = "Disease Waves by Transport Level",
         x = "Time (Ticks)",
         y = "7-Day Rolling Average of Infections",
         color = "Transport Level")

  # 3. Transport Usage Heatmap
  transport_heatmap <- detailed_data %>%
    group_by(num_airports, tick) %>%
    summarize(
      avg_transport_usage = mean(transport_usage),
      avg_infections = mean(total_infected),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = tick, y = factor(num_airports))) +
    geom_tile(aes(fill = avg_transport_usage)) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "Transport Usage Intensity",
         x = "Time (Ticks)",
         y = "Number of Airports",
         fill = "Average Usage")

  # 4. Regional Impact Analysis
  regional_impact <- detailed_data %>%
    group_by(transport_level, tick) %>%
    summarize(
      avg_affected_countries = mean(num_affected_countries),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = tick, y = avg_affected_countries, color = transport_level)) +
    geom_line() +
    scale_color_viridis_d() +
    theme_minimal() +
    labs(title = "Regional Disease Spread",
         x = "Time (Ticks)",
         y = "Average Number of Affected Countries",
         color = "Transport Level")

  # 5. Statistical Analysis

  # Peak Analysis
  peak_data <- detailed_data %>%
    group_by(transport_level, run_id) %>%
    summarize(
      peak_infected = max(total_infected),
      time_to_peak = tick[which.max(total_infected)],
      max_cross_border = max(cross_border_ratio),
      total_transport_usage = max(transport_usage),
      final_affected_countries = last(num_affected_countries),
      .groups = "drop"
    )

  # ANOVA and effect size calculations
  peak_anova <- aov(peak_infected ~ transport_level, data = peak_data)
  peak_effect_sizes <- eta_squared(peak_anova)

  # Time series analysis
  time_series_stats <- detailed_data %>%
    group_by(transport_level) %>%
    summarize(
      avg_spread_rate = mean(spread_rate, na.rm = TRUE),
      max_spread_rate = max(spread_rate, na.rm = TRUE),
      avg_transport_efficiency = mean(transport_efficiency, na.rm = TRUE),
      avg_regional_spread = mean(regional_spread_index, na.rm = TRUE),
      .groups = "drop"
    )

  # Correlation analysis
  correlation_data <- detailed_data %>%
    select(transport_usage, total_infected, cross_border_infections,
           num_affected_countries, spread_rate) %>%
    cor(use = "complete.obs")

  # Generate comprehensive report
  sink("results/transport/transport_analysis_report.txt")

  cat("TRANSPORTATION INFRASTRUCTURE IMPACT ANALYSIS\n")
  cat("===========================================\n\n")

  cat("1. PEAK INFECTION ANALYSIS\n")
  cat("-------------------------\n")
  print(summary(peak_anova))
  cat("\nEffect Sizes:\n")
  print(peak_effect_sizes)

  cat("\n2. TIME SERIES STATISTICS\n")
  cat("------------------------\n")
  print(time_series_stats)

  cat("\n3. CORRELATION ANALYSIS\n")
  cat("----------------------\n")
  print(correlation_data)

  cat("\n4. SUMMARY STATISTICS BY TRANSPORT LEVEL\n")
  cat("-------------------------------------\n")
  print(peak_data %>%
        group_by(transport_level) %>%
        summarize(
          avg_peak = mean(peak_infected),
          avg_time_to_peak = mean(time_to_peak),
          avg_max_cross_border = mean(max_cross_border),
          avg_transport_usage = mean(total_transport_usage),
          avg_affected_countries = mean(final_affected_countries),
          .groups = "drop"
        ))

  sink()

  # Additional Specialized Analysis

  # 6. Transport Efficiency Analysis
  efficiency_plot <- ggplot(detailed_data,
                           aes(x = transport_usage, y = cross_border_infections,
                               color = transport_level)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Transport System Efficiency",
         x = "Transport Usage",
         y = "Cross-Border Infections",
         color = "Transport Level")

  # 7. Comparative Wave Analysis
  wave_analysis <- detailed_data %>%
    group_by(transport_level) %>%
    mutate(
      rolling_avg = rollmean(total_infected, k = 7, fill = NA),
      wave = case_when(
        tick <= 150 ~ "First Wave",
        TRUE ~ "Second Wave"
      )
    ) %>%
    ggplot(aes(x = tick, y = rolling_avg, color = transport_level)) +
    geom_line() +
    facet_wrap(~wave) +
    theme_minimal() +
    labs(title = "Disease Waves by Transport Level",
         x = "Time (Ticks)",
         y = "7-Day Rolling Average of Infections",
         color = "Transport Level")

  # Save all plots
  ggsave("results/transport/plots/disease_progression.png", p1, width = 12, height = 8)
  ggsave("results/transport/plots/cross_border_transmission.png", p2, width = 12, height = 8)
  ggsave("results/transport/plots/transport_heatmap.png", transport_heatmap, width = 10, height = 8)
  ggsave("results/transport/plots/regional_impact.png", regional_impact, width = 12, height = 8)
  ggsave("results/transport/plots/efficiency_analysis.png", efficiency_plot, width = 10, height = 8)
  ggsave("results/transport/plots/wave_analysis.png", wave_analysis, width = 15, height = 8)

  # Return complete results
  return(list(
    plots = list(
      progression = p1,
      cross_border = p2,
      heatmap = transport_heatmap,
      regional_impact = regional_impact,
      efficiency = efficiency_plot,
      waves = wave_analysis
    ),
    statistics = list(
      peak_anova = peak_anova,
      effect_sizes = peak_effect_sizes,
      time_series = time_series_stats,
      correlations = correlation_data,
      peak_data = peak_data
    ),
    data = detailed_data
  ))
}

# Run the analysis
results <- analyze_transport_complete()

# Display plots in R Studio
print(results$plots$progression)
print(results$plots$cross_border)
print(results$plots$heatmap)
print(results$plots$regional_impact)
print(results$plots$efficiency)
print(results$plots$waves)