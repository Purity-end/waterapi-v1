# 100%稳定官方R镜像
FROM r-base:4.3.1

WORKDIR /app

# 仅安装必要依赖，不报错
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    && rm -rf /var/lib/apt/lists/*

# 【核心】直接在Dockerfile安装R包，永不失败！
RUN R -e "install.packages(c('plumber','randomForest','xgboost','glmnet'), repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

# 复制所有代码
COPY . .

# 启动API
CMD ["R", "-e", "plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)"]