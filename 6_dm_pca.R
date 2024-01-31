# 4_reg_p0.2

data<- read.csv("4_reg_p0.2.csv")
data<- read.csv("4_reg_p0.05.csv")
data<- read.csv("4_sgl_lamb0.01.csv")
data<- read.csv("4_sgl_lamb0.02.csv")


usedata = data[, -1:-6]
dim(usedata)
pp = prcomp(usedata, center = TRUE, scale=TRUE)
eigenvalue = round(pp$sdev^2/sum(pp$sdev^2), 2)
eigenvalue
x11()
plot(x = c(1:length(eigenvalue)), y = eigenvalue, col="blue", pch=20,
     xlab="Component Number", ylab="Eigenvalue")

cumsum(round(pp$sdev^2/sum(pp$sdev^2), 2))
round(pp$rotation[, 1:10], 2)

# 取n個主成分
n = 5
x = pp$x
round(pp$rotation[, 1:n], 2)
write.csv(round(pp$rotation[, 1:n], 2), "6_reg_p0.2_pcavar.csv")

x[, 1:n]

pcadata = cbind(data[, 1:6], x[, 1:n])
write.csv(pcadata, "6_sgl_lamb0.02_pca.csv",  row.names = FALSE)
