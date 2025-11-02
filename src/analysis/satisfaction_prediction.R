## =========================================================
## Airline Satisfaction - End-to-End (Single File Version)
## Author: You
## Run: source(".../satisfaction_prediction.R", encoding = "UTF-8")
## Output: output/models/*.csv, output/figures/*.png
## =========================================================

## ---------- 0) Setup ----------
suppressPackageStartupMessages({
  if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret", repos = "https://cran.rstudio.com/")
  if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest", repos = "https://cran.rstudio.com/")
  if (!requireNamespace("xgboost", quietly = TRUE)) install.packages("xgboost", repos = "https://cran.rstudio.com/")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", repos = "https://cran.rstudio.com/")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2", repos = "https://cran.rstudio.com/")
  if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC", repos = "https://cran.rstudio.com/")
  if (!requireNamespace("pdp", quietly = TRUE)) install.packages("pdp", repos = "https://cran.rstudio.com/")

  library(caret)
  library(randomForest)
  library(xgboost)
  library(dplyr)
  library(ggplot2)
  library(pROC)
  library(pdp)
})

## 可选：根据你的机器情况开启/关闭
root_dir <- "/Users/jackleo/R_project/IS507_work"
if (dir.exists(root_dir)) setwd(root_dir)

## 输出目录
if (!dir.exists("output/models")) dir.create("output/models", recursive = TRUE)
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

## ---------- 1) Data Preprocess ----------
preprocess_data <- function(data) {
  # 统一正类/负类顺序，保证 "satisfied" 为正类（排在后面）
  if (!("satisfaction" %in% names(data))) {
    stop("Column 'satisfaction' not found in data.")
  }
  data$satisfaction <- factor(
    data$satisfaction,
    levels = c("neutral or dissatisfied", "satisfied")
  )

  # 关键分类变量 -> 因子
  for (col in c("Customer.Type", "Type.of.Travel", "Class")) {
    if (col %in% names(data)) data[[col]] <- factor(data[[col]])
  }

  # 缺失值处理：简单方式（可改用 medianImpute）
  data <- na.omit(data)
  data
}

## ---------- 2) Split & Feature Engineering ----------
split_and_engineer <- function(processed_data, seed = 42) {
  set.seed(seed)

  service_cols <- c(
    "Inflight.wifi.service", "Departure.Arrival.time.convenient",
    "Ease.of.Online.booking", "Gate.location", "Food.and.drink",
    "Online.boarding", "Seat.comfort", "Inflight.entertainment",
    "On.board.service", "Leg.room.service", "Baggage.handling",
    "Checkin.service", "Inflight.service", "Cleanliness"
  )

  # 校验列是否都在
  missing_cols <- setdiff(service_cols, names(processed_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data:", paste(missing_cols, collapse = ", ")))
  }

  # 拆分
  idx <- caret::createDataPartition(processed_data$satisfaction, p = 0.8, list = FALSE)
  train_df <- processed_data[idx, ]
  test_df  <- processed_data[-idx, ]

  # 综合服务分数
  train_df$service_score <- rowMeans(train_df[service_cols])
  test_df$service_score  <- rowMeans(test_df[service_cols])

  # 仅在训练集上拟合 dummyVars，避免泄漏
  dv <- caret::dummyVars(~ Customer.Type + Type.of.Travel + Class, data = train_df)
  train_dv <- predict(dv, train_df)
  test_dv  <- predict(dv, test_df)

  X_train <- cbind(train_dv, train_df[, c(service_cols, "service_score")])
  X_test  <- cbind(test_dv,  test_df[,  c(service_cols, "service_score")])
  y_train <- train_df$satisfaction
  y_test  <- test_df$satisfaction

  list(
    X_train = X_train, X_test = X_test,
    y_train = y_train, y_test = y_test,
    service_cols = service_cols,
    positive_class = "satisfied"
  )
}

## ---------- 3) Train Models (RF + XGB) ----------
train_models <- function(X_train, y_train) {
  # Random Forest（可拿到概率）
  rf_model <- randomForest(
    x = X_train,
    y = y_train,
    importance = TRUE
  )

  # XGBoost（显式二分类标签：satisfied=1）
  y_train_num <- as.integer(y_train == "satisfied")
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_num)

  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,
    verbose = 0
  )

  list(rf = rf_model, xgb = xgb_model)
}

## ---------- 4) Evaluate + Visualize ----------
evaluate_and_visualize <- function(models, X_test, y_test, positive_class,
                                   out_dir_tab = "output/models",
                                   out_dir_fig = "output/figures") {
  if (!dir.exists(out_dir_tab)) dir.create(out_dir_tab, recursive = TRUE)
  if (!dir.exists(out_dir_fig)) dir.create(out_dir_fig, recursive = TRUE)

  # 预测
  # RF：类别 & 概率（用于 AUC/ROC）
  rf_pred <- predict(models$rf, X_test)
  rf_prob <- predict(models$rf, X_test, type = "prob")[, positive_class]

  # XGB：概率 & 类别
  xgb_prob <- predict(models$xgb, as.matrix(X_test))
  xgb_pred <- factor(ifelse(xgb_prob > 0.5, positive_class, "neutral or dissatisfied"),
                     levels = levels(y_test))

  # 评估（加上 AUC / F1）
  cm_rf  <- caret::confusionMatrix(rf_pred,  y_test, positive = positive_class)
  cm_xgb <- caret::confusionMatrix(xgb_pred, y_test, positive = positive_class)

  y_test_num <- as.integer(y_test == positive_class)
  auc_rf  <- pROC::auc(y_test_num, rf_prob)
  auc_xgb <- pROC::auc(y_test_num, xgb_prob)

  f1 <- function(cm) {
    pr <- cm$byClass["Precision"]; rc <- cm$byClass["Recall"]
    if (is.na(pr) || is.na(rc) || (pr + rc) == 0) return(NA_real_)
    2 * pr * rc / (pr + rc)
  }

  results <- data.frame(
    Model       = c("Random Forest", "XGBoost"),
    Accuracy    = c(cm_rf$overall["Accuracy"], cm_xgb$overall["Accuracy"]),
    Sensitivity = c(cm_rf$byClass["Sensitivity"], cm_xgb$byClass["Sensitivity"]),
    Specificity = c(cm_rf$byClass["Specificity"], cm_xgb$byClass["Specificity"]),
    AUC         = c(as.numeric(auc_rf), as.numeric(auc_xgb)),
    F1          = c(f1(cm_rf), f1(cm_xgb))
  )

  print(results)
  write.csv(results, file.path(out_dir_tab, "model_evaluation.csv"), row.names = FALSE)

  ## --- Feature Importance ---
  # RF
  rf_imp <- randomForest::importance(models$rf)
  rf_imp_df <- data.frame(Feature = rownames(rf_imp), Importance = rf_imp[, "MeanDecreaseGini"])
  rf_imp_df <- rf_imp_df[order(-rf_imp_df$Importance), ]
  write.csv(rf_imp_df, file.path(out_dir_tab, "rf_feature_importance.csv"), row.names = FALSE)

  p_rf <- ggplot(head(rf_imp_df, 15),
                 aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col() + coord_flip() +
    labs(title = "Random Forest Feature Importance (Top 15)", x = NULL, y = "MeanDecreaseGini")
  ggsave(file.path(out_dir_fig, "rf_feature_importance_top15.png"), p_rf, width = 7, height = 5, dpi = 150)

  # XGB
  xgb_imp <- xgboost::xgb.importance(
    feature_names = colnames(X_test),
    model = models$xgb
  )
  write.csv(xgb_imp, file.path(out_dir_tab, "xgb_feature_importance.csv"), row.names = FALSE)

  p_xgb <- ggplot(head(xgb_imp, 15),
                  aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col() + coord_flip() +
    labs(title = "XGBoost Feature Importance (Gain Top 15)", x = NULL, y = "Gain")
  ggsave(file.path(out_dir_fig, "xgb_feature_importance_top15.png"), p_xgb, width = 7, height = 5, dpi = 150)

  ## --- ROC Curves ---
  roc_rf  <- pROC::roc(y_test_num, rf_prob)
  roc_xgb <- pROC::roc(y_test_num, xgb_prob)

  png(file.path(out_dir_fig, "roc_rf_vs_xgb.png"), width = 900, height = 650)
  plot(roc_xgb, col = "black",
       main = sprintf("ROC Curves (AUC: RF=%.3f, XGB=%.3f)", as.numeric(auc_rf), as.numeric(auc_xgb)))
  lines(roc_rf, col = "gray40")
  legend("bottomright",
         legend = c(sprintf("XGB (AUC=%.3f)", as.numeric(auc_xgb)),
                    sprintf("RF  (AUC=%.3f)", as.numeric(auc_rf))),
         col = c("black", "gray40"), lwd = 2)
  dev.off()

  ## --- PDP（阈值/拐点可视化，正好对应你的 RQ5） ---
  if (nrow(xgb_imp) > 0) {
    top_feat <- xgb_imp$Feature[1]
    pd <- pdp::partial(models$xgb, pred.var = top_feat,
                       train = as.matrix(X_test), which.class = 1, prob = TRUE)
    p_pdp <- autoplot(pd) + ggtitle(sprintf("Partial Dependence of %s (XGB)", top_feat))
    ggsave(file.path(out_dir_fig, sprintf("pdp_%s_xgb.png",
                                          gsub("[^A-Za-z0-9_]", "_", top_feat))),
           p_pdp, width = 7, height = 5, dpi = 150)
  }

  invisible(list(
    metrics = results,
    rf_importance = rf_imp_df,
    xgb_importance = xgb_imp
  ))
}

## ---------- 5) Main Runner ----------
run_all <- function() {
  message(">>> Checking dataset path ...")
  stopifnot(file.exists("dataset/train.csv"))

  message(">>> Loading data ...")
  train_data <- read.csv("dataset/train.csv")

  message(">>> Preprocessing ...")
  processed <- preprocess_data(train_data)

  message(">>> Split + Feature engineering ...")
  se <- split_and_engineer(processed)

  message(">>> Training models ...")
  models <- train_models(se$X_train, se$y_train)

  message(">>> Evaluating + Visualizing ...")
  artifacts <- evaluate_and_visualize(
    models,
    X_test = se$X_test,
    y_test = se$y_test,
    positive_class = se$positive_class,
    out_dir_tab = "output/models",
    out_dir_fig = "output/figures"
  )

  message("✅ Done. CSV in output/models, figures in output/figures.")
  list(models = models, artifacts = artifacts)
}

## ---------- 6) Execute ----------
artifacts <- run_all()
