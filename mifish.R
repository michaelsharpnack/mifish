library(data.table)
library(umap)
library(RColorBrewer)
library(class)

#load data
data <- fread('Documents/mifish/94b_reg1_july_reg001.csv')
data_clean <- data[,c(15,16,17,20,21,24,25,28,29,32,33,36,37,40,41,44,45,48,49)]

#perform umap transformation
data.umap <- umap(data_clean)

#visualize umap
plot.umap <- data.frame(x=data.umap$layout[,1],
                        y=data.umap$layout[,2])
plot(plot.umap,pch=19,cex=0.5)

#cluster based on umap
#kmeans
for(cluster.centers in 5:9){
  umap.kmeans = kmeans(plot.umap,cluster.centers)
  umap.kmeans.plot = data.frame(x=data.umap$layout[,1],
                                y=data.umap$layout[,2],
                                z=as.factor(umap.kmeans$cluster))
  attach(umap.kmeans.plot); par(mfrow=c(1,2))
  
  plot(x,y,col=brewer.pal(10,"Set3")[z],xlab = "UMAP 1",ylab = "UMAP 2",main = "UMAP",pch=19,cex=0.6)
  plot(data$x,data$y,col=brewer.pal(10,"Set3")[z],xlab = "X position",ylab = "Y position",
       main = paste("UMAP+Kmeans n =",cluster.centers),pch=19,cex=0.6)
  detach(umap.kmeans.plot)
  
}



