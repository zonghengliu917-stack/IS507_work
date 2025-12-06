# ============================================================================
# Research Question 4: Moderating Effects of Cabin Class × Flight Distance
# on Service Attributes and Passenger Satisfaction
# ============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(viridis)
library(RColorBrewer)
library(corrplot)
library(broom)

# ============================================================================
# 1. Data Loading and Preparation
# ============================================================================

# Load the dataset
train_data <- read.csv("train.csv", stringsAsFactors = FALSE)

# Display basic information about the dataset
cat("Dataset dimensions:", dim(train_data), "\n")
cat("Column names:", colnames(train_data), "\n")
str(train_data)

# ============================================================================
# 2. Data Cleaning and Variable Preparation
# ============================================================================

# Check for missing values
cat("\nMissing values per column:\n")
print(colSums(is.na(train_data)))

# Create a working dataset
df <- train_data

# Convert satisfaction to binary (if not already)
if("satisfaction" %in% colnames(df)) {
  df$satisfaction_binary <- ifelse(df$satisfaction == "satisfied", 1, 0)
} else if("Satisfaction" %in% colnames(df)) {
  df$satisfaction_binary <- ifelse(df$Satisfaction == "satisfied", 1, 0)
}

# Create flight distance categories (short, medium, long-haul)
# Typical definitions: short < 1000km, medium 1000-3000km, long > 3000km
# Adjust based on your data distribution
if("Flight.Distance" %in% colnames(df)) {
  df$distance_category <- cut(df$Flight.Distance,
                              breaks = c(0, 1000, 3000, Inf),
                              labels = c("Short-haul (<1000km)", 
                                         "Medium-haul (1000-3000km)", 
                                         "Long-haul (>3000km)"),
                              include.lowest = TRUE)
} else if("Flight Distance" %in% colnames(df)) {
  df$distance_category <- cut(df$`Flight Distance`,
                              breaks = c(0, 1000, 3000, Inf),
                              labels = c("Short-haul (<1000km)", 
                                         "Medium-haul (1000-3000km)", 
                                         "Long-haul (>3000km)"),
                              include.lowest = TRUE)
}

# Get cabin class variable name
cabin_col <- if("Class" %in% colnames(df)) "Class" else 
  if("Cabin.Class" %in% colnames(df)) "Cabin.Class" else
    grep("class|Class", colnames(df), ignore.case = TRUE, value = TRUE)[1]

# Create interaction term: Cabin Class × Flight Distance
df$cabin_distance <- interaction(df[[cabin_col]], df$distance_category, sep = " × ")

# Service attributes (adjust column names based on your dataset)
service_attributes <- c(
  "Seat.comfort", "Leg.room.service", "Cleanliness", "Food.and.drink",
  "Inflight.entertainment", "Inflight.wifi.service", "Inflight.service",
  "Baggage.handling", "Checkin.service", "Online.boarding",
  "Ease.of.Online.booking", "Gate.location", "On.board.service",
  "Departure.Arrival.time.convenient"
)

# Alternative names (common variations)
service_attributes_alt <- c(
  "Seat comfort", "Leg room service", "Cleanliness", "Food and drink",
  "Inflight entertainment", "Inflight wifi service", "Inflight service",
  "Baggage handling", "Checkin service", "Online boarding",
  "Ease of Online booking", "Gate location", "On-board service",
  "Departure/Arrival time convenient"
)

# Find actual column names
service_cols <- c()
for(attr in c(service_attributes, service_attributes_alt)) {
  matches <- grep(attr, colnames(df), ignore.case = TRUE, value = TRUE)
  if(length(matches) > 0) {
    service_cols <- c(service_cols, matches[1])
  }
}

# Clean service column names for display
service_names <- gsub("\\.", " ", service_cols)
service_names <- gsub("  ", " ", service_names)

cat("\nService attributes found:", length(service_cols), "\n")
print(service_cols)

# ============================================================================
# 3.1 Heatmap of Average Ratings by Cabin Class × Flight Distance
# ============================================================================

# Calculate average ratings for each service by cabin_distance group
heatmap_data <- df %>%
  select(all_of(service_cols), cabin_distance) %>%
  pivot_longer(cols = all_of(service_cols), 
               names_to = "Service", 
               values_to = "Rating") %>%
  group_by(cabin_distance, Service) %>%
  summarise(Avg_Rating = mean(Rating, na.rm = TRUE), .groups = "drop") %>%
  mutate(Service = gsub("\\.", " ", Service))

# Create heatmap
p1 <- ggplot(heatmap_data, aes(x = Service, y = cabin_distance, fill = Avg_Rating)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(name = "Avg\nRating", option = "plasma", 
                       limits = c(1, 5), na.value = "grey90") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "right"
  ) +
  labs(
    title = "3.4.1 Heatmap of Average Ratings",
    subtitle = "Average service scores by Cabin Class × Flight Distance",
    x = "Service",
    y = "Cabin Class × Flight Distance",
    caption = "Figure 5: Average service ratings by Cabin Class × Flight Distance"
  )

print(p1)
ggsave("Q4_heatmap.png", p1, width = 14, height = 8, dpi = 300)

# ============================================================================
# 3.2 Top-5 Services by Cabin Class × Flight Distance Group
# ============================================================================

# Calculate top 5 services for each cabin_distance group
top5_data <- heatmap_data %>%
  group_by(cabin_distance) %>%
  arrange(desc(Avg_Rating)) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  mutate(Service = factor(Service, levels = unique(Service[order(Avg_Rating, decreasing = TRUE)])))

# Create faceted bar charts
p2 <- ggplot(top5_data, aes(x = reorder(Service, Avg_Rating), y = Avg_Rating, fill = Avg_Rating)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ cabin_distance, scales = "free_y", ncol = 3) +
  scale_fill_viridis_c(name = "Avg\nRating", option = "plasma", 
                       limits = c(1, 5), guide = "none") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 8, face = "bold")
  ) +
  labs(
    title = "3.4.2 Top-5 Services by Cabin Class × Flight Distance",
    x = "Service",
    y = "Average Rating",
    caption = "Figure 6: Top-5 service dimensions by Cabin Class × Flight Distance"
  )

print(p2)
ggsave("Q4_top5_services.png", p2, width = 16, height = 10, dpi = 300)

# ============================================================================
# 3.3 Statistical Tests: Comparing Distributions
# ============================================================================

# Perform K-S tests comparing different cabin_distance groups
# Compare extreme groups (e.g., Business Long-haul vs Economy Short-haul)
cabin_distance_levels <- unique(df$cabin_distance)
cat("\nCabin × Distance groups:\n")
print(cabin_distance_levels)

# Select two extreme groups for comparison (adjust based on your data)
if(length(cabin_distance_levels) >= 2) {
  group1 <- cabin_distance_levels[1]
  group2 <- cabin_distance_levels[length(cabin_distance_levels)]
  
  ks_results <- data.frame(
    Service = character(),
    KS_statistic = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(service_cols)) {
    service <- service_cols[i]
    data1 <- df[df$cabin_distance == group1, service]
    data2 <- df[df$cabin_distance == group2, service]
    
    if(length(data1) > 0 && length(data2) > 0) {
      ks_test <- ks.test(data1, data2)
      ks_results <- rbind(ks_results, data.frame(
        Service = service_names[i],
        KS_statistic = ks_test$statistic,
        p_value = ks_test$p.value
      ))
    }
  }
  
  ks_results$neg_log10_p <- -log10(ks_results$p_value)
  ks_results <- ks_results[order(ks_results$neg_log10_p, decreasing = TRUE), ]
  
  # Create K-S test visualization
  p3 <- ggplot(ks_results, aes(x = neg_log10_p, y = reorder(Service, neg_log10_p))) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "red", linewidth = 1) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste0("3.4.3 K-S Test Results: ", group1, " vs ", group2),
      x = "-log10(p-value)",
      y = "Service",
      caption = "Figure 7: Two-Sample Kolmogorov-Smirnov test results\nRed dashed line indicates p = 0.05"
    )
  
  print(p3)
  ggsave("Q4_ks_test.png", p3, width = 10, height = 8, dpi = 300)
  
  cat("\nK-S Test Results:\n")
  print(ks_results)
}

# ============================================================================
# 3.4 ANOVA: Testing Interaction Effects
# ============================================================================

# Perform ANOVA for each service attribute to test interaction effects
anova_results <- data.frame(
  Service = character(),
  Cabin_Effect = numeric(),
  Distance_Effect = numeric(),
  Interaction_Effect = numeric(),
  Cabin_p = numeric(),
  Distance_p = numeric(),
  Interaction_p = numeric(),
  stringsAsFactors = FALSE
)

for(i in seq_along(service_cols)) {
  service <- service_cols[i]
  cabin_var <- df[[cabin_col]]
  distance_var <- df$distance_category
  
  # Fit ANOVA model with interaction
  model <- aov(df[[service]] ~ cabin_var * distance_var, data = df)
  anova_summary <- summary(model)
  
  # Extract p-values
  p_values <- anova_summary[[1]]$`Pr(>F)`
  
  anova_results <- rbind(anova_results, data.frame(
    Service = service_names[i],
    Cabin_Effect = anova_summary[[1]]$`F value`[1],
    Distance_Effect = anova_summary[[1]]$`F value`[2],
    Interaction_Effect = anova_summary[[1]]$`F value`[3],
    Cabin_p = p_values[1],
    Distance_p = p_values[2],
    Interaction_p = p_values[3]
  ))
}

# Sort by interaction p-value
anova_results <- anova_results[order(anova_results$Interaction_p), ]

cat("\nANOVA Results (Interaction Effects):\n")
print(anova_results)

# Visualize interaction effects
anova_results_long <- anova_results %>%
  select(Service, Interaction_p) %>%
  mutate(neg_log10_p = -log10(Interaction_p))

p4 <- ggplot(anova_results_long, aes(x = neg_log10_p, y = reorder(Service, neg_log10_p))) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = "3.4.4 ANOVA: Cabin Class × Flight Distance Interaction Effects",
    x = "-log10(p-value) for Interaction",
    y = "Service",
    caption = "Figure 8: Significance of interaction effects\nRed dashed line indicates p = 0.05"
  )

print(p4)
ggsave("Q4_anova_interaction.png", p4, width = 10, height = 8, dpi = 300)


# ============================================================================
# Detailed Analysis: Service Ratings by Group
# ============================================================================

# Create detailed comparison tables
comparison_table <- df %>%
  select(all_of(service_cols), cabin_distance, satisfaction_binary) %>%
  pivot_longer(cols = all_of(service_cols), 
               names_to = "Service", 
               values_to = "Rating") %>%
  group_by(cabin_distance, Service) %>%
  summarise(
    Avg_Rating = mean(Rating, na.rm = TRUE),
    Satisfaction_Rate = mean(satisfaction_binary, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(Service = gsub("\\.", " ", Service))

cat("\nDetailed Comparison Table (sample):\n")
print(head(comparison_table, 20))

# Save results
write.csv(comparison_table, "Q4_comparison_table.csv", row.names = FALSE)
write.csv(anova_results, "Q4_anova_results.csv", row.names = FALSE)
write.csv(top_terms, "Q4_logistic_results.csv", row.names = FALSE)

# ============================================================================
# 4. Summary and Interpretation
# ============================================================================

cat("\n" , rep("=", 80), "\n")
cat("SUMMARY OF FINDINGS\n")
cat(rep("=", 80), "\n\n")

cat("1. HEATMAP ANALYSIS:\n")
cat("   - Services with highest average ratings across cabin × distance groups:\n")
top_services <- comparison_table %>%
  group_by(Service) %>%
  summarise(Overall_Avg = mean(Avg_Rating), .groups = "drop") %>%
  arrange(desc(Overall_Avg)) %>%
  head(5)
print(top_services)

cat("\n2. INTERACTION EFFECTS (ANOVA):\n")
cat("   - Services with significant cabin × distance interactions (p < 0.05):\n")
sig_interactions <- anova_results %>%
  filter(Interaction_p < 0.05) %>%
  select(Service, Interaction_p)
print(sig_interactions)

cat("\n3. LOGISTIC REGRESSION:\n")
cat("   - Key predictors of satisfaction:\n")
key_predictors <- top_terms %>%
  filter(p.value < 0.05) %>%
  select(term_clean, estimate, p.value) %>%
  head(10)
print(key_predictors)

cat("\n4. RECOMMENDATIONS:\n")
cat("   - Focus service improvements on attributes showing significant interactions\n")
cat("   - Consider cabin class and flight distance when allocating service resources\n")
cat("   - Prioritize services that strongly predict satisfaction in specific contexts\n")

cat("\n", rep("=", 80), "\n")
cat("Analysis complete! Check generated PNG files and CSV files for detailed results.\n")
cat(rep("=", 80), "\n")