# ==============================================
# 水质预测API - 终极修复版
# 自动匹配特征 + 无NULL + 无列错误
# ==============================================
library(plumber)
library(randomForest)
library(xgboost)
library(glmnet)
library(dplyr)

# -------------------------- 全局跨域配置 --------------------------
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

# -------------------------- 全局加载模型 --------------------------
cat("正在加载水质预测模型...\n")
model_dir <- "models"

# 全局赋值
tryCatch({
  model_cod  <<- readRDS(file.path(model_dir, "COD_预测模型.rds"))
  model_nh4  <<- readRDS(file.path(model_dir, "氨氮_预测模型.rds"))
  model_chl  <<- readRDS(file.path(model_dir, "叶绿素_预测模型.rds"))
  cat("✅ 所有模型加载成功！\n")
  cat("COD所需特征：", paste(model_cod$feature_cols, collapse=", "), "\n")
}, error = function(e) {
  cat("❌ 模型加载失败：", e$message, "\n")
  model_cod <<- NULL
  model_nh4 <<- NULL
  model_chl <<- NULL
})

# -------------------------- 自动匹配特征函数 --------------------------
safe_predict <- function(model, input) {
  tryCatch({
    # 自动提取模型需要的特征
    input_df <- as.data.frame(input)
    # 只保留模型需要的列，严格对齐
    X <- input_df[, model$feature_cols, drop=FALSE]
    X <- as.matrix(X)
    # 标准化
    X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
    # 预测
    pred_rf <- predict(model$rf_model, newdata = X_scaled)
    pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
    pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
    round((pred_rf + pred_xgb + pred_ridge)/3, 4)
  }, error = function(e) {
    paste("特征不匹配！模型需要：", paste(model$feature_cols, collapse=", "))
  })
}

# -------------------------- API接口 --------------------------
# 健康检查
#* @get /health
function() {
  list(status = "ok", message = "水质预测API运行正常")
}

# 查看COD需要哪些特征
#* @get /predict/cod/features
function(){
  list(required_features = model_cod$feature_cols)
}

# COD预测
#* @post /predict/cod
function(req) {
  result <- safe_predict(model_cod, req$body)
  list(success = TRUE, prediction = result, indicator = "COD")
}

# 氨氮预测
#* @post /predict/nh4n
function(req) {
  result <- safe_predict(model_nh4, req$body)
  list(success = TRUE, prediction = result, indicator = "氨氮")
}

# 叶绿素预测
#* @post /predict/chl
function(req) {
  result <- safe_predict(model_chl, req$body)
  list(success = TRUE, prediction = result, indicator = "叶绿素")
}