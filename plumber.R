# plumber.R
library(plumber)
library(randomForest)
library(xgboost)
library(glmnet)
library(dplyr)

# ===================== 全局配置：解决跨域 =====================
# 允许所有域名访问
cors_filter <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# 注册跨域过滤器
register_plumber_filter("cors", cors_filter)

# ===================== 加载训练好的 .rds 模型 =====================
# 模型路径
model_dir <- "models"

# 加载3个水质模型
model_cod <- readRDS(file.path(model_dir, "COD_预测模型.rds"))
model_nh4 <- readRDS(file.path(model_dir, "氨氮_预测模型.rds"))
model_chl <- readRDS(file.path(model_dir, "叶绿素_预测模型.rds"))

cat("✅ 所有模型加载成功！\n")

# ===================== 通用预测函数 =====================
# 输入：特征列表 → 输出：集成预测结果
predict_ensemble <- function(model, input_features) {
  # 1. 转换为矩阵
  X <- as.matrix(input_features[model$feature_cols])
  
  # 2. 标准化
  X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
  
  # 3. 三个模型预测
  pred_rf <- predict(model$rf_model, newdata = X_scaled)
  pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
  pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
  
  # 4. 集成预测
  final <- (pred_rf + pred_xgb + pred_ridge) / 3
  return(round(final, 4))
}

# ===================== API 接口 =====================
#* @apiTitle 水质预测API
#* @apiDescription Render托管 + R Plumber + 集成学习模型

# 1. COD预测接口
#* @post /predict/cod
#* @filter cors
function(req, res) {
  features <- req$body
  tryCatch({
    result <- predict_ensemble(model_cod, features)
    list(success = TRUE, prediction = result, indicator = "COD")
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

# 2. 氨氮预测接口
#* @post /predict/nh4n
#* @filter cors
function(req, res) {
  features <- req$body
  tryCatch({
    result <- predict_ensemble(model_nh4, features)
    list(success = TRUE, prediction = result, indicator = "氨氮")
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

# 3. 叶绿素预测接口
#* @post /predict/chl
#* @filter cors
function(req, res) {
  features <- req$body
  tryCatch({
    result <- predict_ensemble(model_chl, features)
    list(success = TRUE, prediction = result, indicator = "叶绿素")
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

# 健康检查接口
#* @get /health
function() {
  list(status = "ok", message = "水质预测API运行正常")
}