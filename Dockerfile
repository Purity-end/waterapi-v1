# 超轻量R镜像，专为Render Free优化
FROM rocker/r-ver:4.3.1-slim

# 工作目录
WORKDIR /app

# 安装系统依赖
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 复制所有项目文件
COPY . .

# 强制安装所有R包
RUN Rscript install.R

# 暴露端口
EXPOSE 10000

# 启动API
CMD ["R", "-e", "plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)"]