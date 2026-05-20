# ==============================================
# 水质预测API - 完整版
# 功能：预测 + 降水获取 + 哨兵2光谱数值(扁平化输出)
# 适配Render部署 + 完全匹配模型输入格式
# ==============================================
library(plumber)
library(randomForest)
library(xgboost)
library(glmnet)
library(dplyr)
library(httr)
library(jsonlite)
library(rstac)
library(terra)
library(sf)

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
}, error = function(e) {
  cat("❌ 模型加载失败：", e$message, "\n")
  model_cod <<- NULL
  model_nh4 <<- NULL
  model_chl <<- NULL
})

# -------------------------- 自动匹配特征预测函数 --------------------------
safe_predict <- function(model, input) {
  tryCatch({
    input_df <- as.data.frame(input)
    X <- input_df[, model$feature_cols, drop=FALSE]
    X <- as.matrix(X)
    X_scaled <- scale(X, center = model$train_mean, scale = model$train_sd)
    
    pred_rf <- predict(model$rf_model, newdata = X_scaled)
    pred_xgb <- predict(model$xgb_model, newdata = X_scaled)
    pred_ridge <- as.numeric(predict(model$ridge_model, newx = X_scaled, s = "lambda.min"))
    round((pred_rf + pred_xgb + pred_ridge)/3, 4)
  }, error = function(e) {
    paste("特征不匹配！模型需要：", paste(model$feature_cols, collapse=", "))
  })
}

# -------------------------- 基础API接口 --------------------------
# 健康检查
#* @get /health
function() {
  list(status = "ok", message = "水质预测API运行正常")
}

# 查看COD所需特征
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

# -------------------------- NASA降水数据API --------------------------
#* @post /get/nasa/rain
#* @param lat:float 纬度
#* @param lon:float 经度
#* @param predict_date:date 预测日期 (YYYY-MM-DD)
function(lat, lon, predict_date){
  tryCatch({
    end_date <- as.Date(predict_date)
    start_date <- end_date - 14
    date_seq <- seq(start_date, end_date, by = "1 day")
    
    start_str <- gsub("-", "", start_date)
    end_str <- gsub("-", "", end_date)
    
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
    
    data <- fromJSON(rawToChar(res$content))
    rain_raw <- data$properties$parameter$PRECTOTCORR
    rain_df <- data.frame(
      date = as.Date(names(rain_raw), format = "%Y%m%d"),
      precip = as.numeric(unlist(rain_raw))
    )
    
    rain_ordered <- rain_df[match(rev(date_seq), rain_df$date), ]
    lag_values <- rain_ordered$precip
    
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

# -------------------------- 哨兵2光谱数值API（扁平化输出·匹配模型） --------------------------
# 配置：水质模型必需波段 + 滞后维度
s2_bands <- c("B2", "B3", "B4", "B5", "B8", "B11", "B12")
lag_days <- c(1,2,3,4,5,6,7,10,14)

#* @post /get/sentinel2
#* @param lat:float 纬度
#* @param lon:float 经度
#* @param target_date:date 目标日期 (YYYY-MM-DD)
function(lat, lon, target_date){
  tryCatch({
    # 基础参数
    target_date <- as.Date(target_date)
    lon_num <- as.numeric(lon)
    lat_num <- as.numeric(lat)
    point <- st_sfc(st_point(c(lon_num, lat_num)), crs = 4326)
    max_cloud <- 20
    use_last_year <- FALSE
    search_date <- target_date
    
    # 连接AWS免费STAC接口
    s <- stac("https://earth-search.aws.element84.com/v1")
    
    # 搜索当日数据
    res <- s %>% stac_search(
      collections = "sentinel-2-l2a",
      bbox = c(lon_num-0.01, lat_num-0.01, lon_num+0.01, lat_num+0.01),
      datetime = paste(search_date, search_date),
      limit = 1
    ) %>% get_request()
    
    # 当日无数据 → 自动使用前一年同期补全
    if(length(res$features) == 0 || res$features[[1]]$properties$`eo:cloud_cover` > max_cloud){
      search_date <- target_date - 365
      use_last_year <- TRUE
      res <- s %>% stac_search(
        collections = "sentinel-2-l2a",
        bbox = c(lon_num-0.01, lat_num-0.01, lon_num+0.01, lat_num+0.01),
        datetime = paste(search_date, search_date),
        limit = 1
      ) %>% get_request()
    }
    
    if(length(res$features) == 0){
      return(list(success = FALSE, error = "无可用光谱数据"))
    }
    
    # 提取单点光谱反射率数值
    item <- res$features[[1]]
    spectral_values <- list()
    for(band in s2_bands){
      tif_url <- item$assets[[paste0("B", substr(band,2,3))]]$href
      raster <- rast(tif_url)
      value <- round(as.numeric(extract(raster, point)[1,2]) / 10000, 2)
      spectral_values[[band]] <- value
    }
    
    # 构建扁平化返回结果（完全匹配模型输入）
    result <- list(
      success = TRUE,
      data_source = ifelse(use_last_year, "前一年同期插值补全", "当日有效数据"),
      cloud_cover = round(item$properties$`eo:cloud_cover`, 1)
    )
    
    # 生成所有 BX_lagX 顶层字段
    for(band in s2_bands){
      val <- spectral_values[[band]]
      for(lag in lag_days){
        key <- paste0(band, "_lag", lag)
        result[[key]] <- val
      }
    }
    
    return(result)
    
  }, error = function(e){
    return(list(success = FALSE, error = e$message))
  })
}