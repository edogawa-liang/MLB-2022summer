#install.packages("installr")
#installr::updateR()

install.packages("ComplexHeatmap")
library(ComplexHeatmap)

data<- read.csv("4_reg_p0.2.csv")

col30 = c("#FFDEDE", "#FF8C8C",  "#B20000", "#800000",   
          "#FFE8BF", "#FFD382",  "#E09200", "#AD7100", 
          "#FFFF26", "#DBDB00",  "#808000",
          "#ABFFAB", "#00FF00",  "#008000", 
          "#BFFFFF", "#1CFFFF",  "#00ADAD", "#008080", 
          "#D6D6FF", "#9999FF",  "#0000BD",
          "#E0B5FF", "#BD63FF",  '#4A0080',
          "#FFB5FF", "#FF6EFF",  "#800080",
          "#EDEDED", "#BFBFBF", "#3B3B3B")
length(col30)

rann = rowAnnotation(
    Team = data$team,
    col = list("Team" = c("HOU"= col30[1],
                          "TB"= col30[2],
                          "TOR"= col30[3],
                          "LAD"= col30[4],
                          "BOS"= col30[5],
                          "SF"= col30[6],
                          "CWS"= col30[7],
                          "ATL"= col30[8],
                          "CIN"= col30[9],
                          "OAK"= col30[10],
                          "COL"= col30[11],
                          "MIL"= col30[12],
                          "PHI"= col30[13],
                          "MIN"= col30[14],
                          "SD"= col30[15],
                          "WSH"= col30[16],
                          "LAA"= col30[17],
                          "CLE"= col30[18],
                          "NYY"= col30[19],
                          "STL"= col30[20],
                          "CHC"= col30[21],
                          "DET"= col30[22],
                          "SEA"= col30[23],
                          "KC"= col30[24],
                          "ARI"= col30[25],
                          "BAL"= col30[26],
                          "NYM"= col30[27],
                          "TEX"= col30[28],
                          "MIA"= col30[29],
                          "PIT"= col30[30],
    ))
  )


# 普通的 hc
library(cluster)
library(dendextend)
library(usmap)
library(ggplot2)
library(gplots)

# 4_reg_p0.2
data<- read.csv("6_reg_p0.2_pca.csv")
usedata = data[, -1:-6]
#View(new)
dis <- dist(usedata, method="euclidean")
hc <- hclust(dis, method="complete")
x11()
aacol = color_branches(as.dendrogram(hc), h=11, groupLabels = TRUE)
plot(aacol)

cluster = cutree(hc, h =11, order_clusters_as_data = T)
cluster
table(cluster)


new = cbind(data, cluster = as.data.frame(cluster))
write.csv(new , "5_reg_p0.2_pca_cluster.csv",  row.names = FALSE)



# 4_reg_p0.05
data<- read.csv("4_reg_p0.05.csv")
usedata = data[, -1:-6]
#View(usedata)

dis <- dist(usedata, method="euclidean")
hc <- hclust(dis, method="complete")
#x11()
aacol = color_branches(as.dendrogram(hc), h=10, groupLabels = TRUE)
plot(aacol)


# 4_sgl_lamb0.01
data<- read.csv("6_sgl_lamb0.01_pca.csv")
usedata = data[, -1:-6]
#(usedata)

dis <- dist(usedata, method="euclidean")
hc <- hclust(dis, method="complete")
x11()
aacol = color_branches(as.dendrogram(hc), h=13, groupLabels = TRUE)
plot(aacol)



# 4_sgl_lamb0.02
data<- read.csv("6_sgl_lamb0.02_pca.csv")
usedata = data[, -1:-6]
#View(usedata)

dis <- dist(usedata, method="euclidean")
hc <- hclust(dis, method="complete")
x11()
aacol = color_branches(as.dendrogram(hc), h=10, groupLabels = TRUE)
plot(aacol)


# 4_sgl_lamb0.03_new
data<- read.csv("4_sgl_lamb0.03_new.csv")
usedata = data[, -1:-6]

dis <- dist(usedata, method="euclidean")
hc <- hclust(dis, method="complete")
x11()
aacol = color_branches(as.dendrogram(hc), h=9, groupLabels = TRUE)
plot(aacol)

cluster = cutree(hc, h =9, order_clusters_as_data = TRUE)
cluster
table(cluster)

v = c()
for (i in 1:length(cluster)){
  if(cluster[i]==2){
   v[i] = 5
  }else if(cluster[i]==1){
    v[i] = 1
  }else if(cluster[i]==3){
    v[i] = 4
  }else if(cluster[i]==4){
    v[i] = 2
  }else if(cluster[i]==5){
    v[i] = 3}
}
table(v)

new = cbind(data, cluster = as.data.frame(v))
write.csv(new , "5_sgl_lamb0.03new_cluster.csv",  row.names = FALSE)



row.names(usedata)<- data[, 3]
hc = function(x) hclust(x, method = "complete")
dd = function(x) dist(x, method = "euclidean")
x11()
heatmap.2(
  as.matrix(usedata),
  Rowv = aacol, 
  Colv = NULL,
  distfun = dd,
  hclustfun = hc,
  dendrogram = "row",
  col = bluered(100),
  trace= "none",
  density= "none",
  na.color = "gray",
  keysize = 1.2,
  cexCol=1,
  cexRow=0.7,
)





