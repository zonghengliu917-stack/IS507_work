## ============================================================================
## 研究问题 (Research Question): 
## 乘客是否对更长的航班在"娱乐"和"餐饮"方面给予更高的评分或重要性？
## Do passengers place higher ratings or importance on "entertainment" and 
## "catering" for longer flights?
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
  packages <- c("dplyr",        # 数据操作和转换 - Data manipulation
                "ggplot2",       # 数据可视化 - Data visualization
                "gridExtra",    # 图形布局 - Grid layout for plots
                "RColorBrewer", # 颜色方案 - Color palettes
                "tidyr",        # 数据整理 - Data tidying
                "broom",        # 模型结果整理 - Model result tidying
                "lmtest",       # 线性模型检验 - Linear model tests
                "sandwich")     # 稳健标准误 - Robust standard errors
  
  # 循环检查每个包是否已安装，如果没有则自动安装
  # Loop through packages and install if not already installed
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.rstudio.com/")
    }
  }
  
  # 加载所有必需的库
  # Load all required libraries
  library(dplyr)         # 数据操作管道函数
  library(ggplot2)       # 图形绘制
  library(gridExtra)     # 多图组合
  library(RColorBrewer)  # 颜色主题
  library(tidyr)         # 数据长宽格式转换
  library(broom)          # 模型结果整理
  library(lmtest)       # 稳健标准误计算
  library(sandwich)     # 稳健标准误
})

## 设置工作目录 - 根据实际路径调整
## Set working directory - adjust path as needed
## 尝试找到项目根目录（包含dataset文件夹的目录）
## Try to find project root directory (directory containing dataset folder)
if (dir.exists("dataset")) {
  root_dir <- getwd()
} else if (dir.exists("../dataset")) {
  setwd("..")
  root_dir <- getwd()
} else if (dir.exists("../../dataset")) {
  setwd("../..")
  root_dir <- getwd()
} else {
  root_dir <- getwd()
  message("Warning: Could not find dataset folder. Using current directory: ", root_dir)
}

## 创建输出目录（如果不存在）
## Create output directories if they don't exist
## output/yiming44/models: 存储模型评估结果和回归系数CSV文件
## output/yiming44/figures: 存储所有可视化图表
if (!dir.exists("output/yiming44/models")) dir.create("output/yiming44/models", recursive = TRUE)
if (!dir.exists("output/yiming44/figures")) dir.create("output/yiming44/figures", recursive = TRUE)

## ============================================================================
## 1) 数据预处理函数 (Data Preprocessing Function)
## ============================================================================
## 功能：清洗和准备数据，包括因子转换、缺失值处理等
## Purpose: Clean and prepare data, including factor conversion, missing value handling

preprocess_data <- function(data) {
  message("Preprocessing data...")
  
  ## 标准化列名（将空格和斜杠替换为点）
  ## Standardize column names (replace spaces and slashes with dots)
  names(data) <- gsub(" ", ".", names(data))
  names(data) <- gsub("/", ".", names(data))
  
  ## 检查关键变量是否存在（使用标准化后的列名）
  ## Check if key variables exist (using standardized column names)
  required_vars <- c("Inflight.entertainment", "Food.and.drink", "Flight.Distance")
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
  }
  
  ## 将关键分类变量转换为因子类型
  ## Convert key categorical variables to factor type
  categorical_vars <- c("Gender",           # 性别
                        "Customer.Type",    # 客户类型（忠诚/不忠诚）
                        "Type.of.Travel",   # 旅行类型（商务/个人）
                        "Class")            # 舱位等级（经济/商务等）
  
  for (col in categorical_vars) {
    if (col %in% names(data)) {
      data[[col]] <- factor(data[[col]])
    }
  }
  
  ## 处理缺失值：删除包含缺失值的行
  ## Handle missing values: delete rows with missing values
  data <- na.omit(data)
  
  ## 创建飞行距离类别（用于描述性分析）
  ## Create flight distance categories (for descriptive analysis)
  data <- data %>%
    mutate(
      Distance.Category = case_when(
        Flight.Distance < 1000 ~ "Short (<1000 km)",
        Flight.Distance >= 1000 & Flight.Distance <= 3000 ~ "Medium (1000-3000 km)",
        Flight.Distance > 3000 ~ "Long (>3000 km)"
      ),
      Distance.Category = factor(Distance.Category, 
                                 levels = c("Short (<1000 km)", 
                                           "Medium (1000-3000 km)", 
                                           "Long (>3000 km)"))
    )
  
  ## 输出预处理后的数据信息
  ## Output preprocessed data information
  message(sprintf("Preprocessed data: %d rows, %d columns", nrow(data), ncol(data)))
  message(sprintf("Flight distance range: %.2f - %.2f km", 
                  min(data$Flight.Distance, na.rm = TRUE),
                  max(data$Flight.Distance, na.rm = TRUE)))
  
  return(data)
}

## ============================================================================
## 2) 描述性统计分析函数 (Descriptive Statistics Function)
## ============================================================================
## 功能：计算并保存描述性统计
## Purpose: Calculate and save descriptive statistics

calculate_descriptive_stats <- function(data) {
  message("Calculating descriptive statistics...")
  
  ## 计算按距离类别的平均评分
  ## Calculate average ratings by distance category
  desc_stats <- data %>%
    group_by(Distance.Category) %>%
    summarise(
      Entertainment_Mean = mean(`Inflight.entertainment`, na.rm = TRUE),
      Entertainment_SD = sd(`Inflight.entertainment`, na.rm = TRUE),
      Catering_Mean = mean(`Food.and.drink`, na.rm = TRUE),
      Catering_SD = sd(`Food.and.drink`, na.rm = TRUE),
      N = n(),
      .groups = "drop"
    )
  
  ## 保存描述性统计
  ## Save descriptive statistics
  write.csv(desc_stats, 
            "output/yiming44/models/descriptive_statistics.csv", 
            row.names = FALSE)
  
  message("Descriptive statistics saved to output/yiming44/models/descriptive_statistics.csv")
  
  return(desc_stats)
}

## ============================================================================
## 3) 回归模型拟合函数 (Regression Model Fitting Function)
## ============================================================================
## 功能：拟合多个线性回归模型（基础模型、完整模型、交互模型）
## Purpose: Fit multiple linear regression models (base, full, interaction)

fit_regression_models <- function(data) {
  message("Fitting regression models...")
  
  ## 定义控制变量
  ## Define control variables
  control_vars <- c("Age", "Gender", "Customer.Type", "Type.of.Travel", "Class",
                    "Seat.comfort", "Cleanliness", "On.board.service")
  
  ## 检查控制变量是否存在
  ## Check if control variables exist
  available_controls <- intersect(control_vars, names(data))
  missing_controls <- setdiff(control_vars, names(data))
  if (length(missing_controls) > 0) {
    message(sprintf("Warning: Missing control variables: %s", 
                    paste(missing_controls, collapse = ", ")))
  }
  
  ## 构建公式字符串
  ## Build formula strings
  base_formula_ent <- "`Inflight.entertainment` ~ Flight.Distance"
  base_formula_cat <- "`Food.and.drink` ~ Flight.Distance"
  
  full_formula_ent <- paste("`Inflight.entertainment` ~ Flight.Distance +",
                            paste(available_controls, collapse = " + "))
  full_formula_cat <- paste("`Food.and.drink` ~ Flight.Distance +",
                            paste(available_controls, collapse = " + "))
  
  interaction_formula_ent <- paste("`Inflight.entertainment` ~ Flight.Distance * Class +",
                                   paste(setdiff(available_controls, "Class"), collapse = " + "))
  interaction_formula_cat <- paste("`Food.and.drink` ~ Flight.Distance * Class +",
                                   paste(setdiff(available_controls, "Class"), collapse = " + "))
  
  ## 拟合模型
  ## Fit models
  models <- list()
  
  # 娱乐评分模型
  # Entertainment rating models
  models$ent_base <- lm(as.formula(base_formula_ent), data = data)
  models$ent_full <- lm(as.formula(full_formula_ent), data = data)
  models$ent_interaction <- lm(as.formula(interaction_formula_ent), data = data)
  
  # 餐饮评分模型
  # Catering rating models
  models$cat_base <- lm(as.formula(base_formula_cat), data = data)
  models$cat_full <- lm(as.formula(full_formula_cat), data = data)
  models$cat_interaction <- lm(as.formula(interaction_formula_cat), data = data)
  
  message("All models fitted successfully")
  
  return(models)
}

## ============================================================================
## 4) 模型结果提取函数 (Model Results Extraction Function)
## ============================================================================
## 功能：提取模型系数、p值、R²等统计量
## Purpose: Extract model coefficients, p-values, R², etc.

extract_model_results <- function(models) {
  message("Extracting model results...")
  
  ## 提取关键结果
  ## Extract key results
  results_list <- list()
  
  model_names <- c("ent_base", "ent_full", "ent_interaction",
                   "cat_base", "cat_full", "cat_interaction")
  model_labels <- c("Entertainment_Base", "Entertainment_Full", "Entertainment_Interaction",
                    "Catering_Base", "Catering_Full", "Catering_Interaction")
  
  for (i in seq_along(model_names)) {
    model <- models[[model_names[i]]]
    model_label <- model_labels[i]
    
    ## 使用稳健标准误计算
    ## Calculate robust standard errors
    robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
    
    ## 提取Flight.Distance的系数
    ## Extract coefficient for Flight.Distance
    coef_summary <- summary(model)$coefficients
    if ("Flight.Distance" %in% rownames(coef_summary)) {
      coef_idx <- which(rownames(coef_summary) == "Flight.Distance")
      coef_value <- coef_summary[coef_idx, "Estimate"]
      
      ## 计算稳健标准误的t统计量和p值
      ## Calculate t-statistic and p-value with robust standard errors
      t_stat <- coef_value / robust_se[coef_idx]
      p_value <- 2 * (1 - pnorm(abs(t_stat)))
      
      results_list[[model_label]] <- data.frame(
        Model = model_label,
        Coefficient = coef_value,
        Robust_SE = robust_se[coef_idx],
        T_statistic = t_stat,
        P_value = p_value,
        R_squared = summary(model)$r.squared,
        Adj_R_squared = summary(model)$adj.r.squared,
        stringsAsFactors = FALSE
      )
    }
  }
  
  ## 合并所有结果
  ## Combine all results
  key_results <- do.call(rbind, results_list)
  
  ## 保存关键结果
  ## Save key results
  write.csv(key_results, 
            "output/yiming44/models/key_results_flight_distance.csv", 
            row.names = FALSE)
  
  ## 提取所有模型的拟合统计量
  ## Extract fit statistics for all models
  fit_stats <- data.frame(
    Model = model_labels,
    R_squared = sapply(model_names, function(x) summary(models[[x]])$r.squared),
    Adj_R_squared = sapply(model_names, function(x) summary(models[[x]])$adj.r.squared),
    AIC = sapply(model_names, function(x) AIC(models[[x]])),
    BIC = sapply(model_names, function(x) BIC(models[[x]])),
    stringsAsFactors = FALSE
  )
  
  write.csv(fit_stats, 
            "output/yiming44/models/model_fit_statistics.csv", 
            row.names = FALSE)
  
  message("Model results saved to output/yiming44/models/")
  
  return(list(key_results = key_results, fit_stats = fit_stats))
}

## ============================================================================
## 5) 可视化函数 (Visualization Function)
## ============================================================================
## 功能：创建所有可视化图表
## Purpose: Create all visualization plots

create_visualizations <- function(data, models, results) {
  message("Creating visualizations...")
  
  out_dir <- "output/yiming44/figures"
  
  ## 1. 按距离类别的箱线图
  ## 1. Boxplots by distance category
  p1_data <- data %>%
    select(Distance.Category, `Inflight.entertainment`, `Food.and.drink`) %>%
    pivot_longer(cols = c(`Inflight.entertainment`, `Food.and.drink`),
                 names_to = "Service", values_to = "Rating") %>%
    mutate(Service = case_when(
      Service == "Inflight.entertainment" ~ "Entertainment",
      Service == "Food.and.drink" ~ "Catering"
    ))
  
  p1 <- ggplot(p1_data, aes(x = Distance.Category, y = Rating, fill = Service)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    facet_wrap(~ Service, ncol = 2) +
    labs(title = "Entertainment and Catering Ratings by Flight Distance Category",
         x = "Flight Distance Category",
         y = "Rating (0-5 scale)",
         fill = "Service") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  ggsave(file.path(out_dir, "01_ratings_by_distance_category.png"),
         p1, width = 10, height = 6, dpi = 300)
  
  ## 2. 娱乐评分 vs 飞行距离散点图
  ## 2. Entertainment rating vs flight distance scatter plot
  p2 <- ggplot(data, aes(x = Flight.Distance, y = `Inflight.entertainment`)) +
    geom_point(alpha = 0.3, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(title = "Entertainment Rating vs Flight Distance",
         x = "Flight Distance (km)",
         y = "Entertainment Rating (0-5 scale)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave(file.path(out_dir, "02_entertainment_vs_distance.png"),
         p2, width = 8, height = 6, dpi = 300)
  
  ## 3. 餐饮评分 vs 飞行距离散点图
  ## 3. Catering rating vs flight distance scatter plot
  p3 <- ggplot(data, aes(x = Flight.Distance, y = `Food.and.drink`)) +
    geom_point(alpha = 0.3, color = "darkgreen") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(title = "Catering Rating vs Flight Distance",
         x = "Flight Distance (km)",
         y = "Catering Rating (0-5 scale)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ggsave(file.path(out_dir, "03_catering_vs_distance.png"),
         p3, width = 8, height = 6, dpi = 300)
  
  ## 4. 系数比较图（带置信区间）
  ## 4. Coefficient comparison plot (with confidence intervals)
  key_results <- results$key_results
  full_results <- key_results %>%
    filter(Model %in% c("Entertainment_Full", "Catering_Full")) %>%
    mutate(
      Lower_CI = Coefficient - 1.96 * Robust_SE,
      Upper_CI = Coefficient + 1.96 * Robust_SE,
      Service = case_when(
        Model == "Entertainment_Full" ~ "Entertainment",
        Model == "Catering_Full" ~ "Catering"
      )
    )
  
  p4 <- ggplot(full_results, aes(x = Service, y = Coefficient, fill = Service)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                  width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Effect of Flight Distance on Service Ratings",
         subtitle = "Coefficient estimates with 95% confidence intervals (Full models)",
         x = "Service Type",
         y = "Coefficient (Flight Distance)",
         fill = "Service") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10),
          legend.position = "none")
  
  ggsave(file.path(out_dir, "04_coefficient_comparison.png"),
         p4, width = 8, height = 6, dpi = 300)
  
  message("All visualizations saved to ", out_dir)
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}

## ============================================================================
## 6) 主执行函数 (Main Execution Function)
## ============================================================================
## 功能：整合所有步骤，执行完整的分析流程
## Purpose: Integrate all steps, execute complete analysis pipeline

run_all <- function() {
  ## 打印分析开始信息
  ## Print analysis start information
  message("\n==========================================")
  message("Entertainment and Catering Analysis")
  message("RQ: Do passengers place higher ratings or importance on")
  message("     'entertainment' and 'catering' for longer flights?")
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
  
  ## ========== 步骤3：描述性统计分析 ==========
  ## ========== Step 3: Descriptive Statistics ==========
  message("\n>>> Step 3: Calculating descriptive statistics...")
  desc_stats <- calculate_descriptive_stats(processed)
  print(desc_stats)
  
  ## ========== 步骤4：拟合回归模型 ==========
  ## ========== Step 4: Fit Regression Models ==========
  message("\n>>> Step 4: Fitting regression models...")
  models <- fit_regression_models(processed)
  
  ## ========== 步骤5：提取模型结果 ==========
  ## ========== Step 5: Extract Model Results ==========
  message("\n>>> Step 5: Extracting model results...")
  results <- extract_model_results(models)
  
  ## 打印关键结果
  ## Print key results
  message("\n>>> Key Results (Full Models):")
  print(results$key_results %>% 
        filter(Model %in% c("Entertainment_Full", "Catering_Full")))
  
  ## ========== 步骤6：创建可视化 ==========
  ## ========== Step 6: Create Visualizations ==========
  message("\n>>> Step 6: Creating visualizations...")
  plots <- create_visualizations(processed, models, results)
  
  message("\n==========================================")
  message("Analysis completed successfully!")
  message("==========================================\n")
  
  return(list(data = processed, models = models, results = results, plots = plots))
}

## ============================================================================
## 7) 执行分析 (Execute Analysis)
## ============================================================================
## 如果直接运行此脚本，执行完整分析
## If running this script directly, execute full analysis

## 检查是否作为主脚本运行（通过检查命令行参数或环境）
## Check if running as main script
if (length(commandArgs(trailingOnly = TRUE)) > 0 || 
    !interactive() || 
    exists(".run_analysis", envir = .GlobalEnv)) {
  results <- run_all()
}

