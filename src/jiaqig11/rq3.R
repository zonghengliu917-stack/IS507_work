## ============================================================================
## RQ3:
## How do business and personal travelers trade off schedule convenience
## versus onboard comfort in shaping their satisfaction?
##
## File: src/jiaqig11/rq3.R
## ============================================================================

suppressPackageStartupMessages({
  pkgs <- c("dplyr", "ggplot2", "readr", "broom", "tidyr", "scales")
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
  
  # 按组计算边际效应（logit 系数 → OR）
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

## 3) 描述性图表 --------------------------------------------------------------

plot_descriptive <- function(df) {
  message(">>> Creating descriptive plots ...")
  
  # 图1：不同旅行类型的满意率
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
              vjust = -0.4, size = 4) +
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
  
  # 图2：时间便利性评分分布
  p2 <- ggplot(df,
               aes(x = time_score, fill = Type.of.Travel)) +
    geom_histogram(position = "dodge", bins = 20) +
    labs(
      title = "Distribution of schedule convenience score",
      x     = "Departure/Arrival time convenient (1–5)",
      y     = "Count",
      fill  = "Type of travel"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(file.path(out_dir, "rq3_fig2_time_score_distribution.png"),
         p2, width = 7, height = 4.5, dpi = 300)
  
  # 图3：舒适度评分分布
  p3 <- ggplot(df,
               aes(x = comfort_score, fill = Type.of.Travel)) +
    geom_histogram(position = "dodge", bins = 20) +
    labs(
      title = "Distribution of onboard comfort score",
      x     = "Comfort score (mean of seat, leg room, entertainment, service, food)",
      y     = "Count",
      fill  = "Type of travel"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(file.path(out_dir, "rq3_fig3_comfort_score_distribution.png"),
         p3, width = 7, height = 4.5, dpi = 300)
  
  invisible(list(p1 = p1, p2 = p2, p3 = p3))
}

  
## 4) 交互效应故事图 ----------------------------------------------------------

plot_effects <- function(m, df) {
  message(">>> Creating effect plots (time x travel, comfort x travel) ...")
  
  mean_time    <- mean(df$time_score)
  mean_comfort <- mean(df$comfort_score)
  
  ## ---------- 图4：时间便利性 × 旅行类型 ----------
  # 先构造 grid，再单独调用 predict()
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
  
  ggsave(file.path(out_dir, "rq3_fig4_time_effect_by_travel.png"),
         p4, width = 7, height = 4.5, dpi = 300)
  
  ## ---------- 图5：舒适度 × 旅行类型 ----------
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
  
  ggsave(file.path(out_dir, "rq3_fig5_comfort_effect_by_travel.png"),
         p5, width = 7, height = 4.5, dpi = 300)
  
  invisible(list(p4 = p4, p5 = p5))
}


## 5) 主函数 -------------------------------------------------------------------

run_rq3 <- function() {
  message("\n==========================================")
  message("RQ3: Business vs Personal – Time vs Comfort")
  message("==========================================\n")
  
  df   <- prep_rq3_data()
  fit  <- fit_rq3_model(df)
  
  # 描述性图
  desc_plots <- plot_descriptive(df)
  # 交互效应图
  eff_plots  <- plot_effects(fit$model, df)
  
  message("\n✅ RQ3 finished. Figures saved in: ", out_dir)
  invisible(list(
    data   = df,
    model  = fit$model,
    tidy   = fit$tidy,
    ORs    = fit$ORs,
    plots  = c(desc_plots, eff_plots)
  ))
}

## 6) 执行分析
results_rq3 <- run_rq3()
