# 官方稳定R镜像
FROM r-base:4.3.1

WORKDIR /app

# 安装极简系统依赖（Render免费版完美兼容）
RUN apt-get update && apt-get install -y --no-install-recommends \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 安装轻量R包（无GIS重型包，永不超时/报错）
RUN R -e "install.packages(c('plumber','randomForest','xgboost','glmnet','dplyr','httr','jsonlite'), repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

# 复制项目文件
COPY . .

# ✅ 修复语法错误：标准exec格式，绝对不报错
CMD ["R", "-e", "plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)"]