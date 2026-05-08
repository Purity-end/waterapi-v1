# 【绝对稳定】固定版本R基础镜像，永不失效
FROM r-base:4.3.1

# 设置工作目录
WORKDIR /app

# 【极简修复】跳过复杂系统依赖，直接安装R包
RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/*

# 复制项目全部文件
COPY . .

# 安装R依赖包
RUN Rscript install.R

# 暴露端口
EXPOSE 10000

# 启动API
CMD ["R", "-e", "plumber::plumb('plumber.R')$run(host='0.0.0.0', port=10000)"]