## ============================================================================
## RQ3:
## How do business and personal travelers trade off schedule convenience
## versus onboard comfort in shaping their satisfaction?
## ============================================================================

suppressPackageStartupMessages({
  pkgs <- c("dplyr", "ggplot2", "readr", "broom", "tidyr", "scales", "gridExtra", "grid")
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cran.rstudio.com/")
    }
  }
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(broom)
  library(tidyr)
  library(scales)
  library(gridExtra)
  library(grid)
})

root_dir <- getwd()  

out_dir <- "output/jiaqig11/figures"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## 1) 数据预处理 --------------------------------------------------------------
prep_rq3_data <- function(path = "dataset/train.csv") {
  message(">>> Loading data for RQ3 ...")
  if (!file.exists(path)) stop("Cannot find dataset/train.csv")
  
  df <- read.csv(path, check.names = TRUE)
  
  # 核心变量名
  needed <- c(
    "satisfaction",
    "Type.of.Travel",
    "Departure.Arrival.time.convenient",
    "Seat.comfort",
    "Inflight.entertainment",
    "Leg.room.service",
    "On.board.service",
    "Food.and.drink"
  )
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop(paste("Missing columns:", paste(miss, collapse = ", ")))
  }
  
  df <- df %>%
    dplyr::select(all_of(needed)) %>%
    tidyr::drop_na()
  
  # satisfaction 因子化
  df$satisfaction <- ifelse(
    df$satisfaction == "neutral or dissatisfied",
    "neutral_or_dissatisfied",
    "satisfied"
  )
  df$satisfaction <- factor(
    df$satisfaction,
    levels = c("neutral_or_dissatisfied", "satisfied")
  )
  
  # Type.of.Travel 因子化
  df$Type.of.Travel <- factor(df$Type.of.Travel)
  
  # 二元 0/1 + time_score + comfort_score
  df <- df %>%
    mutate(
      satisfaction_bin = ifelse(satisfaction == "satisfied", 1L, 0L),
      BusinessTravel   = ifelse(Type.of.Travel == "Business travel", 1L, 0L),
      time_score       = Departure.Arrival.time.convenient,
      comfort_score    = rowMeans(across(c(
        Seat.comfort,
        Leg.room.service,
        Inflight.entertainment,
        On.board.service,
        Food.and.drink
      )), na.rm = TRUE)
    )
  
  message(sprintf("Data for RQ3: %d rows after cleaning.", nrow(df)))
  message("Satisfaction by travel type:")
  print(table(df$Type.of.Travel, df$satisfaction))
  
  return(df)
}

## 2) Logistic 回归 + 交互 ----------------------------------------------------
fit_rq3_model <- function(df) {
  message(">>> Fitting logistic regression with interactions ...")
  
  m <- glm(
    satisfaction_bin ~ time_score * BusinessTravel +
      comfort_score * BusinessTravel,
    data   = df,
    family = binomial(link = "logit")
  )
  
  tidy_m <- tidy(m, conf.int = TRUE)
  print(tidy_m)
  
  # 按组计算边际效应 系数or
  coefs <- coef(m)
  beta_time_P <- coefs[["time_score"]]
  beta_conf_P <- coefs[["comfort_score"]]
  beta_time_B <- beta_time_P + coefs[["time_score:BusinessTravel"]]
  beta_conf_B <- beta_conf_P + coefs[["BusinessTravel:comfort_score"]]
  
  ORs <- c(
    OR_time_P  = exp(beta_time_P),
    OR_conf_P  = exp(beta_conf_P),
    OR_time_B  = exp(beta_time_B),
    OR_conf_B  = exp(beta_conf_B)
  )
  message("\nOdds ratios per +1 score:")
  print(round(ORs, 3))
  
  list(model = m, tidy = tidy_m, ORs = ORs)
}

## 回归结果表 --------------------------------------------------------

generate_regression_table <- function(results, out_dir = "output/jiaqig11/figures") {
  message(">>> Generating regression results table ...")
  
  m <- results$model
  tidy_m <- results$tidy
  
  V <- vcov(m)
  coefs <- coef(m)
  
  # Personal travelers
  beta_time_P <- coefs[["time_score"]]
  se_time_P <- sqrt(V["time_score", "time_score"])
  beta_conf_P <- coefs[["comfort_score"]]
  se_conf_P <- sqrt(V["comfort_score", "comfort_score"])
  
  # Business travelers = main effect + interaction
  beta_time_B <- beta_time_P + coefs[["time_score:BusinessTravel"]]
  se_time_B <- sqrt(
    V["time_score", "time_score"] +
      V["time_score:BusinessTravel", "time_score:BusinessTravel"] +
      2 * V["time_score", "time_score:BusinessTravel"]
  )
  
  beta_conf_B <- beta_conf_P + coefs[["BusinessTravel:comfort_score"]]
  se_conf_B <- sqrt(
    V["comfort_score", "comfort_score"] +
      V["BusinessTravel:comfort_score", "BusinessTravel:comfort_score"] +
      2 * V["comfort_score", "BusinessTravel:comfort_score"]
  )
  
  z_time_P <- beta_time_P / se_time_P
  z_conf_P <- beta_conf_P / se_conf_P
  z_time_B <- beta_time_B / se_time_B
  z_conf_B <- beta_conf_B / se_conf_B
  
  p_time_P <- 2 * (1 - pnorm(abs(z_time_P)))
  p_conf_P <- 2 * (1 - pnorm(abs(z_conf_P)))
  p_time_B <- 2 * (1 - pnorm(abs(z_time_B)))
  p_conf_B <- 2 * (1 - pnorm(abs(z_conf_B)))
  
  add_stars <- function(p_val) {
    if (is.na(p_val) || p_val >= 0.1) return("")
    if (p_val < 0.001) return("***")
    if (p_val < 0.01) return("**")
    if (p_val < 0.05) return("*")
    return("")
  }
  

  model_table <- tidy_m %>%
    mutate(
      Term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "time_score" ~ "Schedule convenience (Personal)",
        term == "comfort_score" ~ "Onboard comfort (Personal)",
        term == "BusinessTravel" ~ "Business travel (main effect)",
        term == "time_score:BusinessTravel" ~ "Schedule convenience × Business",
        term == "BusinessTravel:comfort_score" ~ "Onboard comfort × Business",
        TRUE ~ term
      ),
      `Coefficient` = sprintf("%.4f%s", round(estimate, 4), sapply(p.value, add_stars)),
      `Std. Error` = round(std.error, 4),
      `OR` = sprintf("%.3f%s", round(exp(estimate), 3), sapply(p.value, add_stars)),
      `OR 95% CI` = sprintf("[%.3f, %.3f]", round(exp(conf.low), 3), round(exp(conf.high), 3)),
      `p-value` = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(Term, Coefficient, `Std. Error`, OR, `OR 95% CI`, `p-value`)
  
  p_vals_combined <- c(p_time_P, p_conf_P, p_time_B, p_conf_B)
  combined_table <- tibble::tibble(
    `Travel Type` = c("Personal", "Personal", "Business", "Business"),
    `Attribute` = c("Schedule convenience", "Onboard comfort", "Schedule convenience", "Onboard comfort"),
    `Coefficient` = sprintf("%.4f%s", 
                           c(round(beta_time_P, 4), round(beta_conf_P, 4), 
                             round(beta_time_B, 4), round(beta_conf_B, 4)),
                           sapply(p_vals_combined, add_stars)),
    `Std. Error` = c(round(se_time_P, 4), round(se_conf_P, 4),
                     round(se_time_B, 4), round(se_conf_B, 4)),
    `OR` = sprintf("%.3f%s",
                   c(round(exp(beta_time_P), 3), round(exp(beta_conf_P), 3),
                     round(exp(beta_time_B), 3), round(exp(beta_conf_B), 3)),
                   sapply(p_vals_combined, add_stars)),
    `OR 95% CI` = c(
      sprintf("[%.3f, %.3f]", round(exp(beta_time_P - 1.96 * se_time_P), 3),
              round(exp(beta_time_P + 1.96 * se_time_P), 3)),
      sprintf("[%.3f, %.3f]", round(exp(beta_conf_P - 1.96 * se_conf_P), 3),
              round(exp(beta_conf_P + 1.96 * se_conf_P), 3)),
      sprintf("[%.3f, %.3f]", round(exp(beta_time_B - 1.96 * se_time_B), 3),
              round(exp(beta_time_B + 1.96 * se_time_B), 3)),
      sprintf("[%.3f, %.3f]", round(exp(beta_conf_B - 1.96 * se_conf_B), 3),
              round(exp(beta_conf_B + 1.96 * se_conf_B), 3))
    ),
    `p-value` = c(ifelse(p_time_P < 0.001, "<0.001", sprintf("%.3f", p_time_P)),
                  ifelse(p_conf_P < 0.001, "<0.001", sprintf("%.3f", p_conf_P)),
                  ifelse(p_time_B < 0.001, "<0.001", sprintf("%.3f", p_time_B)),
                  ifelse(p_conf_B < 0.001, "<0.001", sprintf("%.3f", p_conf_B)))
  )
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  readr::write_csv(combined_table, file.path(out_dir, "rq3_combined_effects.csv"))
  
  create_table_grob <- function(df, title) {
   
    df_display <- as.data.frame(lapply(df, as.character))

    tg <- gridExtra::tableGrob(
      df_display,
      rows = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = 10,
        padding = unit(c(4, 4), "mm"),
        core = list(
          fg_params = list(hjust = 0, x = 0.05),
          bg_params = list(fill = c("white", "grey95"))
        ),
        colhead = list(
          fg_params = list(fontface = "bold"),
          bg_params = list(fill = "grey80")
        )
      )
    )
    
    title_grob <- grid::textGrob(
      title,
      gp = grid::gpar(fontsize = 14, fontface = "bold"),
      x = unit(0.5, "npc"),
      y = unit(0.95, "npc")
    )
    
    note_text <- "Significance levels: *** p<0.001, ** p<0.01, * p<0.05"
    note_grob <- grid::textGrob(
      note_text,
      gp = grid::gpar(fontsize = 9, col = "grey40"),
      x = unit(0.5, "npc"),
      y = unit(0.02, "npc")
    )
    
    gridExtra::grid.arrange(
      title_grob,
      tg,
      note_grob,
      ncol = 1,
      heights = c(0.1, 0.85, 0.05)
    )
  }

  
  png(file.path(out_dir, "rq3_combined_effects.png"),
      width = 10, height = 5, units = "in", res = 300)
  create_table_grob(combined_table, "Combined Effects by Travel Type")
  dev.off()
  message("Combined effects table saved: rq3_combined_effects.png")

  message("\n=== Combined Effects by Travel Type ===")
  print(combined_table)
  
  invisible(list(model_coefficients = model_table, combined_effects = combined_table))
}

## 3) 描述性图表 --------------------------------------------------------------

plot_descriptive <- function(df) {
  message(">>> Creating descriptive plots ...")
  
  # 图1：满意率
  sat_rate <- df %>%
    group_by(Type.of.Travel) %>%
    summarise(
      n         = n(),
      sat_rate  = mean(satisfaction_bin),
      .groups   = "drop"
    )
  
  p1 <- ggplot(sat_rate,
               aes(x = Type.of.Travel, y = sat_rate, fill = Type.of.Travel)) +
    geom_col() +
    geom_text(aes(label = percent(sat_rate, accuracy = 0.1)),
              vjust = -0.4, size = 4, color = "black") +
    scale_fill_manual(values = c("Business travel" = "#FFB6C1", "Personal Travel" = "black")) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Satisfaction rate by travel type",
      x     = "Type of travel",
      y     = "Share of satisfied passengers"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
  
  ggsave(file.path(out_dir, "rq3_fig1_satisfaction_by_travel.png"),
         p1, width = 6, height = 4, dpi = 300)
  
  # 图2：时间分布
  p2 <- ggplot(df,
               aes(x = time_score, fill = Type.of.Travel)) +
    geom_histogram(position = "dodge", bins = 20, color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("Business travel" = "#FFB6C1", "Personal Travel" = "black")) +
    labs(
      title = "Distribution of schedule convenience score",
      x     = "Departure/Arrival time convenient (1–5)",
      y     = "Count",
      fill  = "Type of travel"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.border = element_blank())
  
  ggsave(file.path(out_dir, "rq3_fig2_time_score_distribution.png"),
         p2, width = 7, height = 4.5, dpi = 300)
  
  # 图3：舒适度分布
  p3 <- ggplot(df,
               aes(x = comfort_score, fill = Type.of.Travel)) +
    geom_histogram(position = "dodge", bins = 20, color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("Business travel" = "#FFB6C1", "Personal Travel" = "black")) +
    labs(
      title = "Distribution of onboard comfort score",
      x     = "Comfort score (mean of seat, leg room, entertainment, service, food)",
      y     = "Count",
      fill  = "Type of travel"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.border = element_blank())
  
  ggsave(file.path(out_dir, "rq3_fig3_comfort_score_distribution.png"),
         p3, width = 7, height = 4.5, dpi = 300)
  
  invisible(list(p1 = p1, p2 = p2, p3 = p3))
}

  
## 4) 交互效应-----------------------------------------------------

plot_effects <- function(m, df) {
  message(">>> Creating effect plots (time x travel, comfort x travel) ...")
  
  mean_time    <- mean(df$time_score)
  mean_comfort <- mean(df$comfort_score)
  
  ## 图4：时间 × 旅行类型 ----------
  grid_time <- expand_grid(
    time_score     = seq(1, 5, by = 0.1),
    BusinessTravel = c(0L, 1L)
  ) %>%
    mutate(
      comfort_score  = mean_comfort,
      Type.of.Travel = factor(
        ifelse(BusinessTravel == 1L, "Business travel", "Personal Travel"),
        levels = c("Business travel", "Personal Travel")
      )
    )
  
  grid_time$pred_prob <- predict(m, newdata = grid_time, type = "response")
  
  p4 <- ggplot(grid_time,
               aes(x = time_score, y = pred_prob, color = Type.of.Travel)) +
    geom_line(size = 1.1) +
    scale_color_manual(values = c("Business travel" = "#FFB6C1", "Personal Travel" = "black")) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(
      title    = "Effect of schedule convenience on satisfaction",
      subtitle = "Comfort fixed at sample mean",
      x        = "Schedule convenience score (1–5)",
      y        = "Predicted probability of satisfaction",
      color    = "Type of travel"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(file.path(out_dir, "rq3_fig5_time_effect_by_travel.png"),
         p4, width = 7, height = 4.5, dpi = 300)
  
  ## 图5：舒适 × 旅行类型 ----------
  grid_comfort <- expand_grid(
    comfort_score   = seq(1, 5, by = 0.1),
    BusinessTravel  = c(0L, 1L)
  ) %>%
    mutate(
      time_score      = mean_time,
      Type.of.Travel  = factor(
        ifelse(BusinessTravel == 1L, "Business travel", "Personal Travel"),
        levels = c("Business travel", "Personal Travel")
      )
    )
  
  grid_comfort$pred_prob <- predict(m, newdata = grid_comfort, type = "response")
  
  p5 <- ggplot(grid_comfort,
               aes(x = comfort_score, y = pred_prob, color = Type.of.Travel)) +
    geom_line(size = 1.1) +
    scale_color_manual(values = c("Business travel" = "#FFB6C1", "Personal Travel" = "black")) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(
      title    = "Effect of onboard comfort on satisfaction",
      subtitle = "Schedule convenience fixed at sample mean",
      x        = "Comfort score (1–5)",
      y        = "Predicted probability of satisfaction",
      color    = "Type of travel"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(file.path(out_dir, "rq3_fig6_comfort_effect_by_travel.png"),
         p5, width = 7, height = 4.5, dpi = 300)
  
  invisible(list(p4 = p4, p5 = p5))
}

## 5) 系数图--------------------------------------------------------------

make_rq3_forest <- function(results, out_dir = "output/jiaqig11/figures") {
  message(">>> Creating grouped forest plot for odds ratios ...")
  
  m    <- results$model
  ORs  <- results$ORs
  
  V <- vcov(m)
  coefs <- coef(m)
  
  # 个人旅客
  # time_score
  beta_time_P <- coefs[["time_score"]]
  se_time_P   <- sqrt(V["time_score", "time_score"])
  
  # comfort_score  
  beta_conf_P <- coefs[["comfort_score"]]
  se_conf_P   <- sqrt(V["comfort_score", "comfort_score"])
  
  # 商务旅客
  # time_score + time_score:BusinessTravel
  beta_time_B <- beta_time_P + coefs[["time_score:BusinessTravel"]]
  se_time_B   <- sqrt(
    V["time_score", "time_score"] +
      V["time_score:BusinessTravel", "time_score:BusinessTravel"] +
      2 * V["time_score", "time_score:BusinessTravel"]
  )
  
  # comfort_score + BusinessTravel:comfort_score
  beta_conf_B <- beta_conf_P + coefs[["BusinessTravel:comfort_score"]]
  se_conf_B   <- sqrt(
    V["comfort_score", "comfort_score"] +
      V["BusinessTravel:comfort_score", "BusinessTravel:comfort_score"] +
      2 * V["comfort_score", "BusinessTravel:comfort_score"]
  )
  
  # Calculate p-values for each effect
  z_score_time_P <- beta_time_P / se_time_P
  z_score_conf_P <- beta_conf_P / se_conf_P
  z_score_time_B <- beta_time_B / se_time_B
  z_score_conf_B <- beta_conf_B / se_conf_B
  
  p_time_P <- 2 * (1 - pnorm(abs(z_score_time_P)))
  p_conf_P <- 2 * (1 - pnorm(abs(z_score_conf_P)))
  p_time_B <- 2 * (1 - pnorm(abs(z_score_time_B)))
  p_conf_B <- 2 * (1 - pnorm(abs(z_score_conf_B)))
  
  # Construct OR data frame
  OR_df <- tibble::tibble(
    effect = c("Comfort.Personal", "Time.Personal",
               "Comfort.Business", "Time.Business"),
    OR = c(exp(beta_conf_P), exp(beta_time_P),
           exp(beta_conf_B), exp(beta_time_B)),
    p_value = c(p_conf_P, p_time_P, p_conf_B, p_time_B),
    group = c("Personal", "Personal", "Business", "Business")
  )
  
  OR_df <- OR_df %>%
    mutate(
      sig = ifelse(p_value < 0.05, "Significant", "Not significant"),
      color = ifelse(p_value < 0.05,
                     ifelse(group == "Business", "#FFB6C1", "black"),
                     "grey70"),
      effect = factor(effect, levels = c("Comfort.Personal", "Time.Personal",
                                         "Comfort.Business", "Time.Business"))
    )
  
  message("Odds ratios by group:")
  print(OR_df)
  
  # Plot
  p_forest <- ggplot(OR_df, aes(x = OR, y = effect)) +
    geom_point(aes(color = color), size = 6) +
    scale_color_identity() +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10() +
    labs(
      title = "RQ3: Effects of Time and Comfort on Satisfaction",
      subtitle = "Odds ratios by travel type (Business vs Personal)",
      x = "Odds Ratio (log scale)",
      y = NULL
    ) +
    theme_minimal(base_size = 14)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggsave(
    file.path(out_dir, "rq3_fig4_coefficients.png"),
    p_forest, width = 7, height = 4.5, dpi = 300
  )
  
  message("Grouped forest plot saved: rq3_fig4_coefficients.png")
  
  invisible(p_forest)
}


## 6) run-------------------------------------------------------------------

run_rq3 <- function() {
  message("\n==========================================")
  message("RQ3: Business vs Personal – Time vs Comfort")
  message("==========================================\n")
  
  df   <- prep_rq3_data()
  fit  <- fit_rq3_model(df)
  
  # 描述性图
  desc_plots <- plot_descriptive(df)
  # 回归结果表
  reg_tables <- generate_regression_table(fit)
  # 系数
  coef_plot  <- make_rq3_forest(fit)
  # 交互
  eff_plots  <- plot_effects(fit$model, df)
  
  message("RQ3 finished. Figures saved in: ", out_dir)
  invisible(list(
    data   = df,
    model  = fit$model,
    tidy   = fit$tidy,
    ORs    = fit$ORs,
    plots  = c(desc_plots, eff_plots)
  ))
}

results_rq3 <- run_rq3()
