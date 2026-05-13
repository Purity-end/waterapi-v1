# 水质预测API 最终适配版
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

# ===================== 加载模型 =====================
cat("正在加载水质预测模型...\n")
model_dir <- "models"

tryCatch({
  # 🔥 关键修复：使用 <<- 全局赋值，让接口可以访问到模型
  model_cod <<- readRDS(file.path(model_dir, "COD_预测模型.rds"))
  model_nh4 <<- readRDS(file.path(model_dir, "氨氮_预测模型.rds"))
  model_chl <<- readRDS(file.path(model_dir, "叶绿素_预测模型.rds"))
  cat("✅ 所有模型加载成功！API准备就绪\n")
}, error = function(e) {
  cat("❌ 模型加载失败：", e$message, "\n")
  model_cod <<- NULL
  model_nh4 <<- NULL
  model_chl <<- NULL
})

# ===================== 【核心】适配模型预测函数 =====================
predict_ensemble <- function(model, input) {
  if(is.null(model)) return("模型未加载")
  
  # 1. 提取输入数据，严格匹配模型特征
  input_df <- as.data.frame(input)
  # 2. 对齐模型要求的特征
  X <- as.matrix(input_df[, model$feature_cols])
  # 3. 标准化（使用模型训练时的均值/标准差）
  X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
  # 4. 三个模型预测
  pred_rf <- predict(model$rf_model, newdata = X_scaled)
  pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
  pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
  # 5. 集成预测
  round( (pred_rf + pred_xgb + pred_ridge)/3, 4 )
}

# ===================== API接口 =====================
#* @get /health
function() {
  list(status = "ok", message = "水质预测API运行正常")
}

#* @post /predict/cod
function(req) {
  tryCatch({
    list(success = TRUE, prediction = predict_ensemble(model_cod, req$body), indicator = "COD")
  }, error = function(e) {
    list(success = FALSE, error = e$message, required_features = model_cod$feature_cols)
  })
}

#* @post /predict/nh4n
function(req) {
  tryCatch({
    list(success = TRUE, prediction = predict_ensemble(model_nh4, req$body), indicator = "氨氮")
  }, error = function(e) {
    list(success = FALSE, error = e$message, required_features = model_nh4$feature_cols)
  })
}

#* @post /predict/chl
function(req) {
  tryCatch({
    list(success = TRUE, prediction = predict_ensemble(model_chl, req$body), indicator = "叶绿素")
  }, error = function(e) {
    list(success = FALSE, error = e$message, required_features = model_chl$feature_cols)
  })
}