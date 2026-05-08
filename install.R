# install.R
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
install.packages("plumber", quiet = TRUE)
install.packages("randomForest", quiet = TRUE)
install.packages("xgboost", quiet = TRUE)
install.packages("glmnet", quiet = TRUE)