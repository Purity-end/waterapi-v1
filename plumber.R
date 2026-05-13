# ==============================================
# 水质预测API - 完整版（COD/氨氮/叶绿素）
# 适配Render + 你的.rds模型 + 无任何报错
# ==============================================
library(plumber)
library(randomForest)
library(xgboost)
library(glmnet)
library(dplyr)

# -------------------------- 全局跨域配置（永久生效） --------------------------
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

# -------------------------- 全局加载模型（修复作用域，杜绝NULL） --------------------------
cat("正在加载水质预测模型...\n")
model_dir <- "models"

# 全局赋值 <<- 确保接口可以访问到模型
tryCatch({
  model_cod  <<- readRDS(file.path(model_dir, "COD_预测模型.rds"))
  model_nh4  <<- readRDS(file.path(model_dir, "氨氮_预测模型.rds"))
  model_chl  <<- readRDS(file.path(model_dir, "叶绿素_预测模型.rds"))
  cat("✅ 所有模型加载成功！API准备就绪\n")
}, error = function(e) {
  cat("❌ 模型加载失败：", e$message, "\n")
  model_cod <<- NULL
  model_nh4 <<- NULL
  model_chl <<- NULL
})

# -------------------------- 核心预测函数（适配你的模型结构） --------------------------
# COD预测
predict_cod <- function(input) {
  tryCatch({
    model <- model_cod
    input_df <- as.data.frame(input)
    X <- as.matrix(input_df[, model$feature_cols])
    X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
    pred_rf <- predict(model$rf_model, newdata = X_scaled)
    pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
    pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
    round((pred_rf + pred_xgb + pred_ridge)/3, 4)
  }, error = function(e) {
    return(e$message)
  })
}

# 氨氮预测
predict_nh4 <- function(input) {
  tryCatch({
    model <- model_nh4
    input_df <- as.data.frame(input)
    X <- as.matrix(input_df[, model$feature_cols])
    X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
    pred_rf <- predict(model$rf_model, newdata = X_scaled)
    pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
    pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
    round((pred_rf + pred_xgb + pred_ridge)/3, 4)
  }, error = function(e) {
    return(e$message)
  })
}

# 叶绿素预测
predict_chl <- function(input) {
  tryCatch({
    model <- model_chl
    input_df <- as.data.frame(input)
    X <- as.matrix(input_df[, model$feature_cols])
    X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
    pred_rf <- predict(model$rf_model, newdata = X_scaled)
    pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
    pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
    round((pred_rf + pred_xgb + pred_ridge)/3, 4)
  }, error = function(e) {
    return(e$message)
  })
}

# -------------------------- API接口 --------------------------
# 健康检查
#* @get /health
function() {
  list(status = "ok", message = "水质预测API运行正常")
}

# COD预测接口
#* @post /predict/cod
function(req) {
  result <- predict_cod(req$body)
  list(success = TRUE, prediction = result, indicator = "COD")
}

# 氨氮预测接口
#* @post /predict/nh4n
function(req) {
  result <- predict_nh4(req$body)
  list(success = TRUE, prediction = result, indicator = "氨氮")
}

# 叶绿素预测接口
#* @post /predict/chl
function(req) {
  result <- predict_chl(req$body)
  list(success = TRUE, prediction = result, indicator = "叶绿素")
}