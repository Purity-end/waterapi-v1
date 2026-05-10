# plumber.R
library(plumber)
library(randomForest)
library(xgboost)
library(glmnet)
library(dplyr)

# ===================== CORS跨域过滤器 =====================
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

# ===================== 加载训练好的 .rds 模型 =====================
model_dir <- "models"
model_cod <- readRDS(file.path(model_dir, "COD_预测模型.rds"))
model_nh4 <- readRDS(file.path(model_dir, "氨氮_预测模型.rds"))
model_chl <- readRDS(file.path(model_dir, "叶绿素_预测模型.rds"))

cat("✅ 所有模型加载成功！\n")

# ===================== 通用预测函数 =====================
predict_ensemble <- function(model, input_features) {
  X <- as.matrix(input_features[model$feature_cols])
  X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
  pred_rf <- predict(model$rf_model, newdata = X_scaled)
  pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
  pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
  final <- (pred_rf + pred_xgb + pred_ridge) / 3
  return(round(final, 4))
}

# ===================== API 接口 =====================
#* @apiTitle 水质预测API
#* @apiDescription 基于R Plumber的水质遥感预测服务

# 健康检查
#* @get /health
#* @filter cors
function() {
  list(status = "ok", message = "水质预测API运行正常")
}

# COD预测
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

# 氨氮预测
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

# 叶绿素预测
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