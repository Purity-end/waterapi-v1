# 最终正确版 - 彻底解决语法错误
library(plumber)

# 1. 全局跨域过滤器（单独定义，自动应用所有接口）
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

# 2. 健康检查接口（不加任何filter！）
#* @get /health
function() {
  list(status = "ok", message = "API启动成功！")
}

# 3. 测试接口（不加任何filter！）
#* @post /test
function() {
  list(success = TRUE, message = "接口调用正常")
}