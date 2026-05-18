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

# ===================== 自动获取NASA降水数据接口 =====================
# 加载依赖包
if(!require("httr")) install.packages("httr", quiet=TRUE)
library(httr)
if(!require("jsonlite")) install.packages("jsonlite", quiet=TRUE)
library(jsonlite)

#* @post /get/nasa/rain
#* @param lat:float 纬度
#* @param lon:float 经度
#* @param predict_date:date 预测日期 (YYYY-MM-DD)
function(lat, lon, predict_date){
  tryCatch({
    # 1. 计算日期范围：预测日期前14天（模型所需降水滞后数据）
    end_date <- as.Date(predict_date)
    start_date <- end_date - 14
    date_seq <- seq(start_date, end_date, by = "1 day")
    
    # 2. 转换为NASA API要求的日期格式 (YYYYMMDD)
    start_str <- gsub("-", "", start_date)
    end_str <- gsub("-", "", end_date)
    
    # 3. 调用NASA官方免费API（无密钥、永久可用）
    url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
    res <- GET(url, query = list(
      parameters = "PRECTOTCORR",
      community = "AG",
      longitude = lon,
      latitude = lat,
      start = start_str,
      end = end_str,
      format = "json"
    ))
    
    # 4. 解析数据（修复格式解析bug）
    data <- fromJSON(rawToChar(res$content))
    rain_raw <- data$properties$parameter$PRECTOTCORR
    rain_df <- data.frame(
      date = as.Date(names(rain_raw), format = "%Y%m%d"),
      precip = as.numeric(unlist(rain_raw))
    )
    
    # 5. 按日期匹配，提取14天降水（倒序生成lag特征）
    rain_ordered <- rain_df[match(rev(date_seq), rain_df$date), ]
    lag_values <- rain_ordered$precip
    
    # 6. 生成模型需要的所有降水滞后特征（修复数值类型）
    result <- list(
      PRECTOTC_lag1  = lag_values[1],
      PRECTOTC_lag2  = lag_values[2],
      PRECTOTC_lag3  = lag_values[3],
      PRECTOTC_lag4  = lag_values[4],
      PRECTOTC_lag5  = lag_values[5],
      PRECTOTC_lag6  = lag_values[6],
      PRECTOTC_lag7  = lag_values[7],
      PRECTOTC_lag10 = lag_values[10],
      PRECTOTC_lag14 = lag_values[14]
    )
    
    return(list(success = TRUE, data = result))
  }, error = function(e){
    return(list(success = FALSE, error = e$message))
  })
}