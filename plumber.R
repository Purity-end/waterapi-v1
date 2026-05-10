# 极简测试版
library(plumber)

# 跨域过滤器
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

# 健康检查接口
#* @get /health
#* @filter cors
function() {
  list(status = "ok", message = "服务启动成功！等待加载模型")
}

# 测试接口
#* @post /test
#* @filter cors
function() {
  list(success = TRUE, message = "API运行正常！")
}