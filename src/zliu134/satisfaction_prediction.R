## ============================================================================
## 研究问题 (Research Question): 
## 我们能否准确预测乘客满意度？哪些因素是最重要的预测因子？
## Can we accurately predict passenger satisfaction, and which factors are 
## the most important predictors?
## ============================================================================

## ============================================================================
## 0) 环境设置与包加载 (Setup and Package Loading)
## ============================================================================
## 此部分用于安装和加载所需的R包，设置工作目录，创建输出文件夹
## This section installs and loads required R packages, sets working directory,
## and creates output folders

suppressPackageStartupMessages({
  # 定义需要使用的所有R包列表
  # Define list of all required R packages
  packages <- c("caret",        # 分类和回归训练 - Classification and Regression Training
                "randomForest", # 随机森林模型 - Random Forest model
                "xgboost",      # XGBoost梯度提升模型 - XGBoost gradient boosting
                "dplyr",        # 数据操作和转换 - Data manipulation
                "ggplot2",      # 数据可视化 - Data visualization
                "pROC",         # ROC曲线分析 - ROC curve analysis
                "pdp",          # 偏依赖图 - Partial dependence plots
                "gridExtra",    # 图形布局 - Grid layout for plots
                "RColorBrewer", # 颜色方案 - Color palettes
                "corrplot",     # 相关性图 - Correlation plots
                "tidyr")        # 数据整理 - Data tidying
  
  # 循环检查每个包是否已安装，如果没有则自动安装
  # Loop through packages and install if not already installed
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.rstudio.com/")
    }
  }
  
  # 加载所有必需的库
  # Load all required libraries
  library(caret)         # 提供模型训练和评估工具
  library(randomForest)  # 随机森林算法
  library(xgboost)       # XGBoost算法
  library(dplyr)         # 数据操作管道函数
  library(ggplot2)       # 图形绘制
  library(pROC)          # ROC和AUC计算
  library(pdp)           # 偏依赖图绘制
  library(gridExtra)      # 多图组合
  library(RColorBrewer)   # 颜色主题
  library(corrplot)      # 相关性可视化
  library(tidyr)         # 数据长宽格式转换
})

## 设置工作目录 - 根据实际路径调整
## Set working directory - adjust path as needed
root_dir <- "/Users/jackleo/R_project/IS507_work"
if (dir.exists(root_dir)) setwd(root_dir)

## 创建输出目录（如果不存在）
## Create output directories if they don't exist
## output/zliu134/models: 存储模型评估结果和特征重要性CSV文件
## output/zliu134/figures: 存储所有可视化图表
if (!dir.exists("output/zliu134/models")) dir.create("output/zliu134/models", recursive = TRUE)
if (!dir.exists("output/zliu134/figures")) dir.create("output/zliu134/figures", recursive = TRUE)

## ============================================================================
## 1) 数据预处理函数 (Data Preprocessing Function)
## ============================================================================
## 功能：清洗和准备数据，包括因子转换、缺失值处理等
## Purpose: Clean and prepare data, including factor conversion, missing value handling

preprocess_data <- function(data) {
  message("Preprocessing data...")
  
  ## 检查目标变量是否存在
  ## Check if target variable exists
  if (!("satisfaction" %in% names(data))) {
    stop("Column 'satisfaction' not found in data.")
  }
  
  ## 将满意度转换为有效的R变量名（R不支持包含空格的因子水平）
  ## Convert satisfaction to valid R variable names (R doesn't support spaces in factor levels)
  ## 原始值："neutral or dissatisfied" -> "neutral_or_dissatisfied"
  ## 原始值："satisfied" -> "satisfied"
  data$satisfaction <- ifelse(data$satisfaction == "neutral or dissatisfied",
                              "neutral_or_dissatisfied", "satisfied")
  
  ## 将满意度转换为因子类型，并指定水平顺序
  ## Convert satisfaction to factor type with specified levels
  ## 注意：第一个水平是负类，第二个水平是正类（用于模型评估）
  data$satisfaction <- factor(
    data$satisfaction,
    levels = c("neutral_or_dissatisfied", "satisfied")
  )
  
  ## 将关键分类变量转换为因子类型
  ## Convert key categorical variables to factor type
  ## 这些变量在后续分析中需要作为分类变量处理
  categorical_vars <- c("Gender",           # 性别
                        "Customer.Type",    # 客户类型（忠诚/不忠诚）
                        "Type.of.Travel",   # 旅行类型（商务/个人）
                        "Class")            # 舱位等级（经济/商务等）
  
  for (col in categorical_vars) {
    if (col %in% names(data)) {
      data[[col]] <- factor(data[[col]])
    }
  }
  
  ## 处理缺失值：简单删除包含缺失值的行
  ## Handle missing values: simple deletion of rows with missing values
  ## 注意：生产环境可能需要更复杂的缺失值处理策略（如插补）
  data <- na.omit(data)
  
  ## 输出预处理后的数据信息
  ## Output preprocessed data information
  message(sprintf("Preprocessed data: %d rows, %d columns", nrow(data), ncol(data)))
  message(sprintf("Satisfaction distribution:\n"))
  print(table(data$satisfaction))
  
  return(data)
}

## ============================================================================
## 2) 特征工程与数据划分函数 (Feature Engineering & Data Split Function)
## ============================================================================
## 功能：创建新特征、划分训练/测试集、进行独热编码
## Purpose: Create new features, split train/test sets, perform one-hot encoding

split_and_engineer <- function(processed_data, seed = 42) {
  ## 设置随机种子以确保结果可重现
  ## Set random seed for reproducibility
  set.seed(seed)
  message("Splitting data and engineering features...")
  
  ## 定义所有服务评分列（14个服务维度）
  ## Define all service rating columns (14 service dimensions)
  ## 这些是乘客对各种服务的评分（通常为1-5分）
  service_cols <- c(
    "Inflight.wifi.service",              # 机上WiFi服务
    "Departure.Arrival.time.convenient",  # 出发/到达时间便利性
    "Ease.of.Online.booking",             # 在线预订便利性
    "Gate.location",                      # 登机口位置
    "Food.and.drink",                     # 餐饮服务
    "Online.boarding",                    # 在线登机服务
    "Seat.comfort",                       # 座位舒适度
    "Inflight.entertainment",             # 机上娱乐
    "On.board.service",                   # 机上服务
    "Leg.room.service",                   # 腿部空间服务
    "Baggage.handling",                   # 行李处理
    "Checkin.service",                    # 值机服务
    "Inflight.service",                   # 飞行中服务
    "Cleanliness"                         # 清洁度
  )
  
  ## 检查所有必需列是否存在于数据中
  ## Check if all required columns exist in the data
  missing_cols <- setdiff(service_cols, names(processed_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  ## 使用分层抽样划分训练集和测试集（80/20比例）
  ## Split data into training and test sets using stratified sampling (80/20 ratio)
  ## createDataPartition确保训练集和测试集中各类别的比例与原始数据一致
  idx <- caret::createDataPartition(processed_data$satisfaction, p = 0.8, list = FALSE)
  train_df <- processed_data[idx, ]   # 训练集：80%
  test_df  <- processed_data[-idx, ]  # 测试集：20%
  
  message(sprintf("Train set: %d rows, Test set: %d rows", nrow(train_df), nrow(test_df)))
  
  ## 创建综合服务分数（特征工程）
  ## Create composite service score (feature engineering)
  ## 计算所有服务评分的平均值，作为整体服务质量的综合指标
  train_df$service_score <- rowMeans(train_df[, service_cols], na.rm = TRUE)
  test_df$service_score  <- rowMeans(test_df[, service_cols], na.rm = TRUE)
  
  ## 对分类变量进行独热编码（One-Hot Encoding）
  ## Perform one-hot encoding for categorical variables
  ## 重要：只在训练集上拟合编码器，避免数据泄漏（data leakage）
  ## 如果使用测试集信息来拟合编码器，会导致模型性能评估过于乐观
  categorical_vars <- c("Gender", "Customer.Type", "Type.of.Travel", "Class")
  dv <- caret::dummyVars(
    ~ Gender + Customer.Type + Type.of.Travel + Class, 
    data = train_df  # 只在训练集上拟合
  )
  train_dv <- predict(dv, train_df)  # 对训练集编码
  test_dv  <- predict(dv, test_df)    # 对测试集编码（使用训练集拟合的编码器）
  
  ## 组合所有特征：独热编码的分类变量 + 服务评分 + 综合分数 + 数值特征
  ## Combine all features: one-hot encoded categorical + service ratings + composite score + numeric features
  X_train <- cbind(train_dv, train_df[, c(service_cols, "service_score", 
                                          "Age",                        # 年龄
                                          "Flight.Distance",            # 飞行距离
                                          "Departure.Delay.in.Minutes", # 出发延误（分钟）
                                          "Arrival.Delay.in.Minutes")]) # 到达延误（分钟）
  X_test  <- cbind(test_dv, test_df[, c(service_cols, "service_score",
                                        "Age", "Flight.Distance",
                                        "Departure.Delay.in.Minutes",
                                        "Arrival.Delay.in.Minutes")])
  
  ## 提取目标变量
  ## Extract target variables
  y_train <- train_df$satisfaction
  y_test  <- test_df$satisfaction
  
  ## 返回所有需要的数据和元信息
  ## Return all required data and metadata
  return(list(
    X_train = X_train,      # 训练集特征矩阵
    X_test = X_test,        # 测试集特征矩阵
    y_train = y_train,      # 训练集标签
    y_test = y_test,        # 测试集标签
    train_df = train_df,    # 原始训练数据框（用于后续分析）
    test_df = test_df,      # 原始测试数据框
    service_cols = service_cols,  # 服务列名列表（用于特征重要性分析）
    positive_class = "satisfied"  # 正类标签（用于模型评估）
  ))
}

## ============================================================================
## 3) 模型训练函数（含交叉验证）(Model Training with Cross-Validation)
## ============================================================================
## 功能：训练Random Forest和XGBoost模型，并进行交叉验证评估
## Purpose: Train RF and XGBoost models, perform cross-validation evaluation

train_models <- function(X_train, y_train, cv_folds = 5) {
  message("Training models with cross-validation...")
  
  ## ========== Random Forest 模型训练 ==========
  ## ========== Random Forest Model Training ==========
  message("Training Random Forest...")
  rf_model <- randomForest(
    x = X_train,                    # 特征矩阵
    y = y_train,                    # 目标变量
    ntree = 500,                    # 树的数量（更多树通常更好，但计算成本更高）
    mtry = sqrt(ncol(X_train)),     # 每次分裂时考虑的变量数（sqrt是常用选择）
    importance = TRUE,              # 计算特征重要性
    do.trace = FALSE                # 不显示训练过程
  )
  
  ## ========== XGBoost 模型训练 ==========
  ## ========== XGBoost Model Training ==========
  message("Training XGBoost...")
  ## 将因子标签转换为数值（XGBoost需要数值标签）
  ## Convert factor labels to numeric (XGBoost requires numeric labels)
  y_train_num <- as.integer(y_train == "satisfied")  # satisfied=1, neutral_or_dissatisfied=0
  
  ## 创建XGBoost数据矩阵
  ## Create XGBoost data matrix
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_num)
  
  ## 设置XGBoost超参数
  ## Set XGBoost hyperparameters
  params <- list(
    objective = "binary:logistic",  # 二分类逻辑回归目标函数
    eval_metric = "auc",            # 评估指标：AUC（ROC曲线下面积）
    max_depth = 6,                  # 树的最大深度（控制模型复杂度）
    eta = 0.1,                      # 学习率（步长，较小的值需要更多轮次但更稳定）
    subsample = 0.8,                # 每棵树使用的样本比例（防止过拟合）
    colsample_bytree = 0.8,         # 每棵树使用的特征比例（防止过拟合）
    min_child_weight = 1            # 叶子节点最小权重（控制过拟合）
  )
  
  ## 训练XGBoost模型
  ## Train XGBoost model
  xgb_model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 200,    # 迭代轮数（boosting轮数）
    verbose = 0       # 不显示训练过程
  )
  
  ## ========== 交叉验证（用于稳健的准确率评估）==========
  ## ========== Cross-Validation (for robust accuracy assessment) ==========
  message("Performing cross-validation for robust accuracy assessment...")
  
  ## 设置交叉验证控制参数
  ## Set cross-validation control parameters
  cv_control <- trainControl(
    method = "cv",                    # 交叉验证方法：k折交叉验证
    number = cv_folds,                # 折数（默认5折）
    summaryFunction = twoClassSummary, # 二分类汇总函数
    classProbs = TRUE,                # 返回类别概率
    verboseIter = FALSE               # 不显示每次迭代的详细信息
  )
  
  ## Random Forest 交叉验证
  ## Random Forest Cross-Validation
  rf_cv <- train(
    x = X_train,
    y = y_train,
    method = "rf",                    # 随机森林方法
    trControl = cv_control,
    metric = "ROC",                   # 优化指标：ROC（AUC）
    tuneGrid = data.frame(mtry = sqrt(ncol(X_train))),  # 固定mtry参数
    ntree = 500
  )
  
  ## XGBoost 交叉验证
  ## XGBoost Cross-Validation
  xgb_cv <- train(
    x = X_train,
    y = y_train,
    method = "xgbTree",              # XGBoost树方法
    trControl = cv_control,
    metric = "ROC",
    tuneGrid = expand.grid(           # 超参数网格（这里使用固定值）
      nrounds = 200,
      max_depth = 6,
      eta = 0.1,
      gamma = 0,                      # 最小损失减少量
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
  )
  
  ## 返回所有模型和交叉验证结果
  ## Return all models and cross-validation results
  return(list(
    rf = rf_model,      # 训练好的随机森林模型
    xgb = xgb_model,    # 训练好的XGBoost模型
    rf_cv = rf_cv,      # RF交叉验证结果
    xgb_cv = xgb_cv     # XGBoost交叉验证结果
  ))
}

## ============================================================================
## 4) 综合模型评估函数 (Comprehensive Model Evaluation Function)
## ============================================================================
## 功能：评估模型性能，计算各种指标（准确率、AUC、F1等）
## Purpose: Evaluate model performance, calculate various metrics (accuracy, AUC, F1, etc.)

evaluate_models <- function(models, X_test, y_test, positive_class) {
  message("Evaluating models...")
  
  ## ========== 模型预测 ==========
  ## ========== Model Predictions ==========
  
  ## Random Forest 预测
  ## Random Forest predictions
  rf_pred <- predict(models$rf, X_test)  # 预测类别
  rf_prob <- predict(models$rf, X_test, type = "prob")[, positive_class]  # 预测概率（正类）
  
  ## XGBoost 预测
  ## XGBoost predictions
  xgb_prob <- predict(models$xgb, as.matrix(X_test))  # 预测概率（0-1之间）
  ## 将概率转换为类别（阈值0.5）
  ## Convert probabilities to classes (threshold 0.5)
  xgb_pred <- factor(
    ifelse(xgb_prob > 0.5, positive_class, "neutral_or_dissatisfied"),
    levels = levels(y_test)
  )
  
  ## ========== 混淆矩阵 ==========
  ## ========== Confusion Matrices ==========
  ## 混淆矩阵显示预测结果与真实标签的对应关系
  ## Confusion matrix shows correspondence between predictions and true labels
  cm_rf  <- confusionMatrix(rf_pred, y_test, positive = positive_class)
  cm_xgb <- confusionMatrix(xgb_pred, y_test, positive = positive_class)
  
  ## ========== AUC（ROC曲线下面积）计算 ==========
  ## ========== AUC (Area Under ROC Curve) Calculation ==========
  ## AUC衡量模型区分正负类的能力，值越接近1越好
  ## AUC measures model's ability to distinguish positive and negative classes
  y_test_num <- as.integer(y_test == positive_class)  # 转换为数值（0/1）
  auc_rf  <- as.numeric(pROC::auc(y_test_num, rf_prob))
  auc_xgb <- as.numeric(pROC::auc(y_test_num, xgb_prob))
  
  ## ========== F1分数计算 ==========
  ## ========== F1 Score Calculation ==========
  ## F1 = 2 * (Precision * Recall) / (Precision + Recall)
  ## F1是精确率和召回率的调和平均数，平衡两者
  f1 <- function(cm) {
    pr <- cm$byClass["Precision"]  # 精确率：预测为正类中真正为正类的比例
    rc <- cm$byClass["Recall"]     # 召回率：真正为正类中被正确预测的比例
    if (is.na(pr) || is.na(rc) || (pr + rc) == 0) return(NA_real_)
    2 * pr * rc / (pr + rc)
  }
  
  ## ========== 编译评估结果 ==========
  ## ========== Compile Evaluation Results ==========
  results <- data.frame(
    Model = c("Random Forest", "XGBoost"),
    Accuracy = c(cm_rf$overall["Accuracy"], cm_xgb$overall["Accuracy"]),  # 准确率
    Sensitivity = c(cm_rf$byClass["Sensitivity"], cm_xgb$byClass["Sensitivity"]),  # 敏感度（召回率）
    Specificity = c(cm_rf$byClass["Specificity"], cm_xgb$byClass["Specificity"]),  # 特异度
    Precision = c(cm_rf$byClass["Precision"], cm_xgb$byClass["Precision"]),  # 精确率
    AUC = c(auc_rf, auc_xgb),  # ROC曲线下面积
    F1 = c(f1(cm_rf), f1(cm_xgb))  # F1分数
  )
  
  ## ========== 交叉验证结果汇总 ==========
  ## ========== Cross-Validation Results Summary ==========
  ## 从交叉验证结果中提取准确率和ROC的均值和标准差
  ## Extract mean and SD of accuracy and ROC from CV results
  rf_acc <- if("Accuracy" %in% names(models$rf_cv$resample)) {
    mean(models$rf_cv$resample$Accuracy, na.rm = TRUE)
  } else NA
  xgb_acc <- if("Accuracy" %in% names(models$xgb_cv$resample)) {
    mean(models$xgb_cv$resample$Accuracy, na.rm = TRUE)
  } else NA
  
  cv_results <- data.frame(
    Model = c("Random Forest (CV)", "XGBoost (CV)"),
    Mean_Accuracy = c(rf_acc, xgb_acc),  # 平均准确率
    SD_Accuracy = c(if("Accuracy" %in% names(models$rf_cv$resample)) 
                      sd(models$rf_cv$resample$Accuracy, na.rm = TRUE) else NA,
                    if("Accuracy" %in% names(models$xgb_cv$resample))
                      sd(models$xgb_cv$resample$Accuracy, na.rm = TRUE) else NA),  # 准确率标准差
    Mean_ROC = c(mean(models$rf_cv$resample$ROC, na.rm = TRUE),
                 mean(models$xgb_cv$resample$ROC, na.rm = TRUE)),  # 平均ROC
    SD_ROC = c(sd(models$rf_cv$resample$ROC, na.rm = TRUE),
               sd(models$xgb_cv$resample$ROC, na.rm = TRUE))  # ROC标准差
  )
  
  ## 返回所有评估结果
  ## Return all evaluation results
  return(list(
    results = results,        # 测试集评估结果
    cv_results = cv_results,  # 交叉验证结果
    cm_rf = cm_rf,           # RF混淆矩阵
    cm_xgb = cm_xgb,         # XGBoost混淆矩阵
    rf_pred = rf_pred,       # RF预测类别
    rf_prob = rf_prob,       # RF预测概率
    xgb_pred = xgb_pred,     # XGBoost预测类别
    xgb_prob = xgb_prob,      # XGBoost预测概率
    auc_rf = auc_rf,         # RF的AUC值
    auc_xgb = auc_xgb,       # XGBoost的AUC值
    y_test_num = y_test_num  # 数值化的测试标签
  ))
}

## ============================================================================
## 5) 特征重要性分析函数 (Feature Importance Analysis Function)
## ============================================================================
## 功能：分析并比较两个模型的特征重要性，识别最重要的预测因子
## Purpose: Analyze and compare feature importance from both models, identify top predictors

analyze_feature_importance <- function(models, X_test, service_cols) {
  message("Analyzing feature importance...")
  
  ## ========== Random Forest 特征重要性 ==========
  ## ========== Random Forest Feature Importance ==========
  ## RF使用MeanDecreaseGini衡量特征重要性
  ## RF uses MeanDecreaseGini to measure feature importance
  rf_imp <- importance(models$rf)
  rf_imp_df <- data.frame(
    Feature = rownames(rf_imp),
    Importance = rf_imp[, "MeanDecreaseGini"],  # Gini不纯度减少量
    Model = "Random Forest"
  )
  rf_imp_df <- rf_imp_df[order(-rf_imp_df$Importance), ]  # 按重要性降序排列
  
  ## ========== XGBoost 特征重要性 ==========
  ## ========== XGBoost Feature Importance ==========
  ## XGBoost使用Gain（增益）衡量特征重要性
  ## XGBoost uses Gain to measure feature importance
  xgb_imp <- xgb.importance(
    feature_names = colnames(X_test),
    model = models$xgb
  )
  xgb_imp_df <- data.frame(
    Feature = xgb_imp$Feature,
    Importance = xgb_imp$Gain,  # 特征带来的增益
    Model = "XGBoost"
  )
  xgb_imp_df <- xgb_imp_df[order(-xgb_imp_df$Importance), ]  # 按重要性降序排列
  
  ## ========== 归一化重要性分数（0-100尺度）用于比较 ==========
  ## ========== Normalize Importance Scores (0-100 scale) for Comparison ==========
  ## 由于两个模型使用不同的重要性度量，需要归一化以便比较
  ## Since two models use different importance metrics, normalize for comparison
  rf_imp_df$Importance_Normalized <- (rf_imp_df$Importance / max(rf_imp_df$Importance)) * 100
  xgb_imp_df$Importance_Normalized <- (xgb_imp_df$Importance / max(xgb_imp_df$Importance)) * 100
  
  ## ========== 合并两个模型的重要性结果 ==========
  ## ========== Combine Importance from Both Models ==========
  ## 合并RF和XGBoost的重要性分数，计算平均值
  ## Merge RF and XGBoost importance scores, calculate average
  combined_imp <- merge(
    rf_imp_df[, c("Feature", "Importance_Normalized")],
    xgb_imp_df[, c("Feature", "Importance_Normalized")],
    by = "Feature",
    suffixes = c("_RF", "_XGB"),
    all = TRUE  # 保留所有特征（即使某个模型中缺失）
  )
  combined_imp[is.na(combined_imp)] <- 0  # 缺失值填充为0
  ## 计算平均重要性（两个模型的平均值）
  ## Calculate average importance (mean of both models)
  combined_imp$Average_Importance <- (combined_imp$Importance_Normalized_RF + 
                                       combined_imp$Importance_Normalized_XGB) / 2
  combined_imp <- combined_imp[order(-combined_imp$Average_Importance), ]  # 按平均重要性排序
  
  ## ========== 识别特征类型 ==========
  ## ========== Identify Feature Types ==========
  ## 将特征分类为：服务评分、客户人口统计、其他
  ## Categorize features: Service Rating, Customer Demographics, Other
  combined_imp$Feature_Type <- ifelse(
    combined_imp$Feature %in% service_cols | 
    grepl("service", combined_imp$Feature, ignore.case = TRUE),
    "Service Rating",  # 服务评分类特征
    ifelse(
      grepl("Type|Class|Gender", combined_imp$Feature),
      "Customer Demographics",  # 客户人口统计特征
      "Other"  # 其他特征
    )
  )
  
  ## 返回所有重要性分析结果
  ## Return all importance analysis results
  return(list(
    rf_imp_df = rf_imp_df,        # RF特征重要性数据框
    xgb_imp_df = xgb_imp_df,      # XGBoost特征重要性数据框
    combined_imp = combined_imp   # 合并后的特征重要性（用于可视化）
  ))
}

## ============================================================================
## 6) 可视化函数 (Visualization Functions)
## ============================================================================
## 功能：创建各种可视化图表来展示模型性能和特征重要性
## Purpose: Create various visualizations to showcase model performance and feature importance

create_visualizations <- function(eval_results, importance_results, models, 
                                 X_test, y_test_num, positive_class,
                                 out_dir = "output/zliu134/figures") {
  message("Creating visualizations...")
  
  ## ========== 图表1：模型准确率对比 ==========
  ## ========== Plot 1: Model Accuracy Comparison ==========
  ## 直接回答研究问题的第一部分：能否准确预测？
  ## Directly answers first part of research question: Can we accurately predict?
  p1 <- ggplot(eval_results$results, aes(x = Model, y = Accuracy, fill = Model)) +
    geom_col(alpha = 0.8) +  # 柱状图
    geom_text(aes(label = sprintf("%.3f", Accuracy)), vjust = -0.5, size = 4) +  # 添加数值标签
    scale_fill_brewer(palette = "Set2") +  # 使用颜色方案
    labs(title = "Model Accuracy Comparison",
         subtitle = "Can we accurately predict passenger satisfaction?",
         y = "Accuracy", x = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11))
  
  ## ========== 图表2：综合性能指标对比 ==========
  ## ========== Plot 2: Comprehensive Performance Metrics Comparison ==========
  ## 展示多个评估指标，全面了解模型性能
  ## Show multiple evaluation metrics for comprehensive model understanding
  metrics_long <- eval_results$results %>%
    select(Model, Accuracy, Sensitivity, Specificity, Precision, F1, AUC) %>%
    tidyr::pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
  
  p2 <- ggplot(metrics_long, aes(x = Metric, y = Value, fill = Model)) +
    geom_col(position = "dodge", alpha = 0.8) +  # 并排柱状图
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Comprehensive Model Performance Metrics",
         y = "Score", x = "Metric") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  # x轴标签倾斜
          plot.title = element_text(size = 14, face = "bold"))
  
  ## ========== 图表3：ROC曲线 ==========
  ## ========== Plot 3: ROC Curves ==========
  ## ROC曲线展示模型在不同阈值下的真阳性率和假阳性率
  ## ROC curve shows true positive rate vs false positive rate at different thresholds
  roc_rf  <- roc(y_test_num, eval_results$rf_prob)
  roc_xgb <- roc(y_test_num, eval_results$xgb_prob)
  
  ## 准备ROC数据用于绘图
  ## Prepare ROC data for plotting
  roc_data <- data.frame(
    FPR = c(1 - roc_rf$specificities, 1 - roc_xgb$specificities),  # 假阳性率
    TPR = c(roc_rf$sensitivities, roc_xgb$sensitivities),          # 真阳性率
    Model = c(rep("Random Forest", length(roc_rf$sensitivities)),
              rep("XGBoost", length(roc_xgb$sensitivities)))
  )
  
  p3 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
    geom_line(linewidth = 1.2) +  # ROC曲线
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # 对角线（随机分类器）
    scale_color_brewer(palette = "Set2") +
    labs(title = sprintf("ROC Curves (AUC: RF=%.3f, XGB=%.3f)",
                         eval_results$auc_rf, eval_results$auc_xgb),
         x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ## ========== 图表4：最重要的15个预测因子（综合视图）==========
  ## ========== Plot 4: Top 15 Most Important Predictors (Combined View) ==========
  ## 直接回答研究问题的第二部分：哪些因素最重要？
  ## Directly answers second part of research question: Which factors are most important?
  top_features <- head(importance_results$combined_imp, 15)  # 取前15个
  top_features$Feature <- factor(top_features$Feature, 
                                 levels = rev(top_features$Feature))  # 反转顺序用于水平条形图
  
  p4 <- ggplot(top_features, aes(x = Feature, y = Average_Importance, 
                                 fill = Feature_Type)) +
    geom_col(alpha = 0.8) +
    coord_flip() +  # 水平条形图
    scale_fill_brewer(palette = "Set1") +
    labs(title = "Top 15 Most Important Predictors (Average of Both Models)",
         subtitle = "Which factors are the most important predictors?",
         x = NULL, y = "Average Normalized Importance",
         fill = "Feature Type") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11))
  
  ## ========== 图表5：特征重要性对比（RF vs XGBoost）==========
  ## ========== Plot 5: Feature Importance Comparison (RF vs XGBoost) ==========
  ## 展示两个模型对同一特征的重要性评估是否一致
  ## Show if both models agree on feature importance
  top_15_rf <- head(importance_results$rf_imp_df, 15)
  top_15_xgb <- head(importance_results$xgb_imp_df, 15)
  
  ## 找出两个模型共同认为重要的特征
  ## Find features that both models consider important
  common_features <- intersect(top_15_rf$Feature, top_15_xgb$Feature)
  comparison_data <- data.frame(
    Feature = common_features,
    RF_Importance = top_15_rf$Importance_Normalized[match(common_features, top_15_rf$Feature)],
    XGB_Importance = top_15_xgb$Importance_Normalized[match(common_features, top_15_xgb$Feature)]
  )
  comparison_data <- comparison_data[order(-(comparison_data$RF_Importance + 
                                              comparison_data$XGB_Importance)), ]
  comparison_data$Feature <- factor(comparison_data$Feature, 
                                    levels = rev(comparison_data$Feature))
  
  ## 转换为长格式用于绘图
  ## Convert to long format for plotting
  comparison_long <- comparison_data %>%
    tidyr::pivot_longer(cols = c(RF_Importance, XGB_Importance),
                       names_to = "Model", values_to = "Importance")
  comparison_long$Model <- gsub("_Importance", "", comparison_long$Model)
  
  p5 <- ggplot(comparison_long, aes(x = Feature, y = Importance, fill = Model)) +
    geom_col(position = "dodge", alpha = 0.8) +  # 并排柱状图
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Feature Importance: RF vs XGBoost Comparison",
         subtitle = "Top features identified by both models",
         x = NULL, y = "Normalized Importance") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ## ========== 图表6：混淆矩阵 ==========
  ## ========== Plot 6: Confusion Matrices ==========
  ## 混淆矩阵显示模型的分类错误模式
  ## Confusion matrix shows model's classification error patterns
  cm_rf_data <- as.data.frame(eval_results$cm_rf$table)
  cm_xgb_data <- as.data.frame(eval_results$cm_xgb$table)
  
  ## Random Forest混淆矩阵
  p6a <- ggplot(cm_rf_data, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +  # 热力图
    geom_text(aes(label = Freq), color = "black", size = 5) +  # 添加数值
    scale_fill_gradient(low = "white", high = "steelblue") +  # 颜色渐变
    labs(title = "Random Forest Confusion Matrix",
         x = "Actual", y = "Predicted") +
    theme_minimal()
  
  ## XGBoost混淆矩阵
  p6b <- ggplot(cm_xgb_data, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "XGBoost Confusion Matrix",
         x = "Actual", y = "Predicted") +
    theme_minimal()
  
  ## ========== 保存所有图表 ==========
  ## ========== Save All Plots ==========
  ggsave(file.path(out_dir, "01_accuracy_comparison.png"), p1, 
         width = 8, height = 6, dpi = 300)
  ggsave(file.path(out_dir, "02_comprehensive_metrics.png"), p2, 
         width = 10, height = 6, dpi = 300)
  ggsave(file.path(out_dir, "03_roc_curves.png"), p3, 
         width = 8, height = 6, dpi = 300)
  ggsave(file.path(out_dir, "04_top_features_combined.png"), p4, 
         width = 10, height = 7, dpi = 300)
  ggsave(file.path(out_dir, "05_feature_importance_comparison.png"), p5, 
         width = 10, height = 7, dpi = 300)
  ggsave(file.path(out_dir, "06a_confusion_matrix_rf.png"), p6a, 
         width = 6, height = 5, dpi = 300)
  ggsave(file.path(out_dir, "06b_confusion_matrix_xgb.png"), p6b, 
         width = 6, height = 5, dpi = 300)
  
  ## ========== 图表7：偏依赖图（PDP）==========
  ## ========== Plot 7: Partial Dependence Plot (PDP) ==========
  ## 展示最重要特征如何影响满意度预测概率
  ## Show how the most important feature affects satisfaction prediction probability
  top_feature <- importance_results$combined_imp$Feature[1]  # 最重要的特征
  if (top_feature %in% colnames(X_test)) {
    tryCatch({
      ## 计算偏依赖：固定其他特征，只改变目标特征，观察预测概率的变化
      ## Calculate partial dependence: fix other features, vary target feature, observe prediction change
      pd <- partial(models$xgb, pred.var = top_feature,
                   train = as.matrix(X_test), which.class = 1, prob = TRUE)
      p7 <- autoplot(pd) + 
        ggtitle(sprintf("Partial Dependence: %s", top_feature),
                subtitle = "How this top predictor affects satisfaction probability") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"))
      ggsave(file.path(out_dir, sprintf("07_pdp_%s.png", 
                                        gsub("[^A-Za-z0-9_]", "_", top_feature))),
             p7, width = 8, height = 6, dpi = 300)
    }, error = function(e) {
      message(sprintf("Could not create PDP for %s: %s", top_feature, e$message))
    })
  }
  
  message("All visualizations saved to ", out_dir)
  
  ## 返回所有图表对象（可选，用于后续组合或修改）
  ## Return all plot objects (optional, for later combination or modification)
  return(list(
    p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6a = p6a, p6b = p6b
  ))
}

## ============================================================================
## 7) 主执行函数 (Main Execution Function)
## ============================================================================
## 功能：整合所有步骤，执行完整的分析流程
## Purpose: Integrate all steps, execute complete analysis pipeline

run_all <- function() {
  ## 打印分析开始信息
  ## Print analysis start information
  message("\n==========================================")
  message("Passenger Satisfaction Prediction Analysis")
  message("RQ: Can we accurately predict passenger satisfaction,")
  message("     and which factors are the most important predictors?")
  message("==========================================\n")
  
  ## ========== 步骤1：加载数据 ==========
  ## ========== Step 1: Load Data ==========
  message(">>> Step 1: Loading data...")
  if (!file.exists("dataset/train.csv")) {
    stop("dataset/train.csv not found!")
  }
  train_data <- read.csv("dataset/train.csv")
  message(sprintf("Loaded %d rows, %d columns", nrow(train_data), ncol(train_data)))
  
  ## ========== 步骤2：数据预处理 ==========
  ## ========== Step 2: Data Preprocessing ==========
  message("\n>>> Step 2: Preprocessing data...")
  processed <- preprocess_data(train_data)
  
  ## ========== 步骤3：特征工程和数据划分 ==========
  ## ========== Step 3: Feature Engineering and Data Split ==========
  message("\n>>> Step 3: Splitting data and engineering features...")
  se <- split_and_engineer(processed)
  
  ## ========== 步骤4：模型训练 ==========
  ## ========== Step 4: Model Training ==========
  message("\n>>> Step 4: Training models...")
  models <- train_models(se$X_train, se$y_train)
  
  ## ========== 步骤5：模型评估 ==========
  ## ========== Step 5: Model Evaluation ==========
  message("\n>>> Step 5: Evaluating models...")
  eval_results <- evaluate_models(
    models, se$X_test, se$y_test, se$positive_class
  )
  
  ## 打印评估结果
  ## Print evaluation results
  message("\n>>> Model Performance Results:")
  print(eval_results$results)
  message("\n>>> Cross-Validation Results:")
  print(eval_results$cv_results)
  
  ## ========== 步骤6：特征重要性分析 ==========
  ## ========== Step 6: Feature Importance Analysis ==========
  message("\n>>> Step 6: Analyzing feature importance...")
  importance_results <- analyze_feature_importance(
    models, se$X_test, se$service_cols
  )
  
  ## 打印最重要的10个特征
  ## Print top 10 most important features
  message("\n>>> Top 10 Most Important Features:")
  print(head(importance_results$combined_imp[, c("Feature", "Average_Importance", 
                                                   "Feature_Type")], 10))
  
  ## ========== 步骤7：创建可视化 ==========
  ## ========== Step 7: Create Visualizations ==========
  message("\n>>> Step 7: Creating visualizations...")
  plots <- create_visualizations(
    eval_results, importance_results, models,
    se$X_test, eval_results$y_test_num, se$positive_class
  )
  
  ## ========== 步骤8：保存结果 ==========
  ## ========== Step 8: Save Results ==========
  message("\n>>> Step 8: Saving results...")
  ## 保存模型评估结果
  ## Save model evaluation results
  write.csv(eval_results$results, 
            "output/zliu134/models/model_evaluation.csv", row.names = FALSE)
  ## 保存交叉验证结果
  ## Save cross-validation results
  write.csv(eval_results$cv_results, 
            "output/zliu134/models/cv_results.csv", row.names = FALSE)
  ## 保存特征重要性结果
  ## Save feature importance results
  write.csv(importance_results$rf_imp_df, 
            "output/zliu134/models/rf_feature_importance.csv", row.names = FALSE)
  write.csv(importance_results$xgb_imp_df, 
            "output/zliu134/models/xgb_feature_importance.csv", row.names = FALSE)
  write.csv(importance_results$combined_imp, 
            "output/zliu134/models/combined_feature_importance.csv", row.names = FALSE)
  
  ## ========== 分析总结 ==========
  ## ========== Analysis Summary ==========
  message("\n==========================================")
  message("✅ Analysis Complete!")
  message("==========================================")
  message("\n📊 Key Findings:")
  ## 最佳模型准确率
  ## Best model accuracy
  message(sprintf("   • Best Model Accuracy: %.3f (%s)",
                  max(eval_results$results$Accuracy),
                  eval_results$results$Model[which.max(eval_results$results$Accuracy)]))
  ## 最佳模型AUC
  ## Best model AUC
  message(sprintf("   • Best Model AUC: %.3f (%s)",
                  max(eval_results$results$AUC),
                  eval_results$results$Model[which.max(eval_results$results$AUC)]))
  ## 最重要的预测因子
  ## Top predictor
  message(sprintf("   • Top Predictor: %s (Importance: %.2f)",
                  importance_results$combined_imp$Feature[1],
                  importance_results$combined_imp$Average_Importance[1]))
  message("\n📁 Output Files:")
  message("   • CSV files: output/zliu134/models/")
  message("   • Figures: output/zliu134/figures/")
  message("==========================================\n")
  
  ## 返回所有结果对象（可选，用于后续分析）
  ## Return all result objects (optional, for further analysis)
  return(list(
    models = models,                    # 训练好的模型
    eval_results = eval_results,        # 评估结果
    importance_results = importance_results,  # 特征重要性结果
    plots = plots                       # 图表对象
  ))
}

## ============================================================================
## 8) 执行分析 (Execute Analysis)
## ============================================================================
## 如果脚本不是以交互模式运行（即直接运行脚本），则执行完整分析
## If script is not run in interactive mode (i.e., run directly), execute full analysis
if (!interactive()) {
  results <- run_all()
}
