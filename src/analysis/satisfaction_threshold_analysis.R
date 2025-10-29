# First install required packages if they are not installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2", repos = "https://cran.rstudio.com/")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr", repos = "https://cran.rstudio.com/")
}

# Load required libraries
library(ggplot2)
library(dplyr)

# Set working directory to project root
setwd("/Users/jackleo/R_project/IS507_work")

# Source visualization functions
source("src/visualization/satisfaction_viz.R")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
    dir.create("output")
}

# Read the data
train_data <- read.csv("dataset/train.csv")
test_data <- read.csv("dataset/test.csv")

# 1. Data Preprocessing
preprocess_data <- function(data) {
  # Handle missing values if any
  data <- na.omit(data)
  
  # Create customer segments
  data$customer_segment <- paste(data$Customer.Type, 
                               data$Class, 
                               data$Type.of.Travel, 
                               sep="_")
  
  return(data)
}

# 2. Calculate comprehensive experience score
calculate_exp_score <- function(data) {
  # Select numerical satisfaction indicators
  satisfaction_cols <- c("Inflight.wifi.service", "Departure.Arrival.time.convenient",
                        "Ease.of.Online.booking", "Gate.location", "Food.and.drink",
                        "Online.boarding", "Seat.comfort", "Inflight.entertainment",
                        "On.board.service", "Leg.room.service", "Baggage.handling",
                        "Checkin.service", "Inflight.service", "Cleanliness")
  
  # Calculate weighted score (you may adjust weights based on importance)
  data$exp_score <- rowMeans(data[satisfaction_cols], na.rm = TRUE)
  
  return(data)
}

# 3. Find satisfaction threshold
find_threshold <- function(data) {
  # Group by customer segment
  segment_analysis <- data %>%
    group_by(customer_segment) %>%
    summarise(
      mean_score = mean(exp_score),
      satisfaction_rate = mean(satisfaction == "satisfied"),
      threshold = quantile(exp_score[satisfaction == "satisfied"], 0.1)
    )
  
  return(segment_analysis)
}

# 4. Identify important factors
analyze_factors <- function(data) {
  # Correlation analysis with satisfaction
  satisfaction_cols <- c("Inflight.wifi.service", "Departure.Arrival.time.convenient",
                        "Ease.of.Online.booking", "Gate.location", "Food.and.drink",
                        "Online.boarding", "Seat.comfort", "Inflight.entertainment",
                        "On.board.service", "Leg.room.service", "Baggage.handling",
                        "Checkin.service", "Inflight.service", "Cleanliness")
  
  importance_analysis <- data %>%
    group_by(customer_segment) %>%
    summarise(across(all_of(satisfaction_cols), 
                    ~cor(., exp_score, method = "spearman")))
  
  return(importance_analysis)
}

# Main analysis
main_analysis <- function() {
  # Load and preprocess data
  data <- preprocess_data(train_data)
  
  # Calculate experience score
  data <- calculate_exp_score(data)
  
  # Find thresholds
  thresholds <- find_threshold(data)
  
  # Analyze important factors
  factor_importance <- analyze_factors(data)
  
  # Save results
  write.csv(thresholds, "output/satisfaction_thresholds.csv")
  write.csv(factor_importance, "output/factor_importance.csv")
  
  # Create visualizations
  create_visualizations(data, thresholds)
}

# Run analysis
main_analysis()