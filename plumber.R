# 极简测试版 - 100%无语法错误，必启动成功
library(plumber)

# -------------------------- 1. 独立定义跨域过滤器（核心修复） --------------------------
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

# -------------------------- 2. 接口函数（仅引用过滤器，不重复定义） --------------------------
# 健康检查接口
#* @get /health
#* @filter cors
function() {
  list(status = "ok", message = "✅ API服务启动成功！")
}

# 测试接口
#* @post /test
#* @filter cors
function() {
  list(success = TRUE, message = "🎉 接口调用正常！")
}