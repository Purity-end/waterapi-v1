# 官方稳定R镜像
FROM r-base:4.3.1

WORKDIR /app

# 仅安装基础系统依赖（无GDAL，永不超时）
RUN apt-get update && apt-get install -y --no-install-recommends \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 安装轻量R包（无任何GIS包，Render秒装）
RUN R -e "install.packages(c(\
    'plumber','randomForest','xgboost','glmnet','dplyr',\
    'httr','jsonlite'\
), repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

# 复制项目
COPY . .

# 启动API
CMD ["R", "-e", \"plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)\""]