# 【官方标准镜像】
FROM rocker/r-base:latest

# 工作目录
WORKDIR /app

# 安装必需系统依赖
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 复制项目所有文件
COPY . .

# 安装R包
RUN Rscript install.R

# 暴露端口
EXPOSE 10000

# 启动API命令
CMD ["R", "-e", "plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)"]