# 官方稳定R镜像
FROM r-base:4.3.1

WORKDIR /app

# 补齐所有必需的系统依赖
RUN apt-get update && apt-get install -y --no-install-recommends \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 安装R包（清华源，极速稳定）
RUN R -e "install.packages(c('plumber','randomForest','xgboost','glmnet','dplyr'), repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

# 复制项目代码
COPY . .

# 启动API
CMD ["R", "-e", "plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)"]