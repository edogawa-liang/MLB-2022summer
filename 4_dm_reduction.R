getwd()
#install.packages("sparsegl")
library(sparsegl)

data<- read.csv("3_MLBbatting_scale.csv")
View(data)

titledata <- data[, 1:6]
View(usedata)
usedata<- data[, -1:-6]
y = usedata[, which(colnames(usedata) == "R.PA")]
X = usedata[, -which(colnames(usedata) == "R.PA")]
X = as.matrix(X)
dim(X)
#View(X)
groups = c(1, 2, 2, 3, 4, 5, 6, 6, 6, 6, 7, 7, 8, 5, 5,	9, 9, 9, 6, 10, 10,	11, 12,	12,	12,	12,	12,	12,	11,	13,	13,	13,	13,	13,	13,	14,	14,	14,	15,	15,	15,	15,	16,	16)
length(groups)
newgp = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 16, 17,
          18, 19, 20, 21, 22, 22, 22, 22, 22, 22, 23, 24, 24, 25, 25, 26,
          27, 28, 28, 28, 29, 29, 29, 29, 30, 31)
length(newgp)

# group lasso
#install.packages("gglasso")
library(gglasso)
gr <- gglasso(X, y, group =  groups, loss="ls",
              intercept = F)

gr$lambda 
coef(gr, s = c(0, 0.001, 0.002, 0.005, 0.01))
plot(gr, y_axis = "coef", x_axis = "penalty", add_legend = FALSE)
plot(gr, y_axis = "group", x_axis = "lambda", add_legend = FALSE)
## group lasso 跟 sparse lasso 的結果也不一樣


# sparse group lasso
fit <- sparsegl(X, y, group = newgp)
fit
fit$b0
fit$beta
fit$beta[, c(29, 30)]
fit$df
fit$lambda
fit$group


plot(fit, y_axis = "coef", x_axis = "penalty", add_legend = TRUE)
plot(fit, y_axis = "group", x_axis = "lambda", add_legend = TRUE)
coef(fit, s = c(0.002, 0.003, 0.004, 0.005, 0.01))
round(coef(fit, s = c(0.003)), 3)

# lasso
library(glmnet)
m1 = glmnet(X, y, alpha = 1)
m1
coef(m1, s = c(0, 0.001, 0.002, 0.005, 0.01))
## 有沒有 + group 結果真的不一樣ㄟ


# backward
## 跟 sas 結果不一樣 ()
full = lm(y ~ ., data = as.data.frame(X))  
full
View(X)
backward.lm = step(full,  direction="backward")
summary(backward.lm)

write.csv(usedata, "usee.csv")



# 匯出檔案
export<- function(data, lambda, name){
  col = which(colnames(usedata) %in% names(which(coef(data, s = c(lambda))[, 1] != 0)[-1]))
  exportdata = cbind(titledata, usedata[, col])
  write.csv(exportdata, name, , row.names = FALSE)
}

export(fit, 0.001, "4_sgl_lamb0.01.csv")
export(fit, 0.002, "4_sgl_lamb0.02.csv")
export(fit, 0.005, "4_sgl_lamb0.05.csv")


View(usedata)
cor(usedata)

