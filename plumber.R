# 水质预测API 最终完整版
library(plumber)
library(randomForest)
library(xgboost)
library(glmnet)
library(dplyr)

# 全局跨域过滤器
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

# ===================== 加载你的 .rds 模型 =====================
# 【重要】确保 models 文件夹存在，且文件名完全一致
cat("正在加载水质预测模型...\n")
model_dir <- "models"

# 加载模型（容错处理，防止崩溃）
tryCatch({
  model_cod  <- readRDS(file.path(model_dir, "COD_预测模型.rds"))
  model_nh4  <- readRDS(file.path(model_dir, "氨氮_预测模型.rds"))
  model_chl  <- readRDS(file.path(model_dir, "叶绿素_预测模型.rds"))
  cat("✅ 所有模型加载成功！API准备就绪\n")
}, error = function(e) {
  cat("❌ 模型加载失败：", e$message, "\n")
  # 加载失败不崩溃，保留健康接口
  model_cod <- NULL
  model_nh4 <- NULL
  model_chl <- NULL
})

# ===================== 预测函数 =====================
predict_ensemble <- function(model, input) {
  if(is.null(model)) return("模型未加载")
  X <- as.matrix(input[model$feature_cols])
  X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
  pred_rf <- predict(model$rf_model, newdata = X_scaled)
  pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
  pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
  round( (pred_rf + pred_xgb + pred_ridge)/3, 4 )
}

# ===================== API 接口 =====================
#* @get /health
function() {
  list(status = "ok", message = "水质预测API运行正常")
}

#* @post /predict/cod
function(req) {
  list(success = TRUE, prediction = predict_ensemble(model_cod, req$body), indicator = "COD")
}

#* @post /predict/nh4n
function(req) {
  list(success = TRUE, prediction = predict_ensemble(model_nh4, req$body), indicator = "氨氮")
}

#* @post /predict/chl
function(req) {
  list(success = TRUE, prediction = predict_ensemble(model_chl, req$body), indicator = "叶绿素")
}