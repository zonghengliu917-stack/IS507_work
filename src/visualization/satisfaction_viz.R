# Visualization functions for satisfaction threshold analysis
if (!requireNamespace("tidyr", quietly = TRUE)) {
    install.packages("tidyr", repos = "https://cran.rstudio.com/")
}

library(ggplot2)
library(dplyr)
library(tidyr)

create_visualizations <- function(data, thresholds) {
  # 1. Distribution of experience scores by segment
  p1 <- ggplot(data, aes(x = exp_score, fill = customer_segment)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = thresholds, aes(xintercept = threshold, color = customer_segment)) +
    theme_minimal() +
    labs(title = "体验分数分布（按客户群组）",
         x = "体验分数",
         y = "密度") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save plot
  ggsave("output/exp_score_distribution.png", p1, width = 12, height = 8)
  
  # 2. Satisfaction rate vs experience score
  p2 <- ggplot(data, aes(x = exp_score, y = as.numeric(satisfaction == "satisfied"), 
                         color = customer_segment)) +
    geom_smooth(method = "loess") +
    theme_minimal() +
    labs(title = "满意度与体验分数的关系",
         x = "体验分数",
         y = "满意度比率") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save plot
  ggsave("output/satisfaction_vs_score.png", p2, width = 12, height = 8)
  
  # 3. Factor importance heatmap
  factor_importance <- analyze_factors(data)
  factor_importance_long <- factor_importance %>%
    pivot_longer(-customer_segment, 
                names_to = "factor", 
                values_to = "correlation")
  
  p3 <- ggplot(factor_importance_long, 
               aes(x = factor, y = customer_segment, fill = correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "各因素重要性（按客户群组）",
         x = "服务因素",
         y = "客户群组",
         fill = "相关性")
  
  # Save plot
  ggsave("output/factor_importance_heatmap.png", p3, width = 15, height = 10)
}
