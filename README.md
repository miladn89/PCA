# PCA
PCA for Twitter featurs of COVID-19 experts


https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff
##PCA and clustering

```{r}
library(tidyverse)

library(readr)
library(factoextra)

Final_features2 <- read_csv("C:/Users/mn852/Dropbox/SNA/network analysis and systems thinking project/Final_features3.csv")

mydata<- Final_features2
#glimpse(mydata)
#summary(mydata)
#header(mydata)
#features<-mydata[c(7:111)]
mydata$Org_VS_Ind
#names(mydata)<- c("Org_VS_Ind","id")
#names(mydata) <- c("id", "organization_or_individual", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))



####1. total 130 features
SNA.total <- prcomp(mydata[c(7:135)], center = TRUE, scale = TRUE)
summary(SNA.total)


screeplot(SNA.total, type = "l", npcs = 55, main = "Screeplot of the first 55 PCs for all existing features except follower analysis features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.total$sdev^2 / sum(SNA.total$sdev^2))
plot(cumpro[0:55], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 24 PCs")
abline(v = 24, col="blue", lty=5)
abline(h = 0.93218, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC24"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.total$x[,1],SNA.total$x[,2], xlab="PC20.1%)", ylab = "PC2 (12.2%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.total, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 130 features dataset") +
  theme(plot.title = element_text(hjust = 0.5))


####2. 13 account features
SNA.account <- prcomp(mydata[c(7:19)], center = TRUE, scale = TRUE)
summary(SNA.account)



screeplot(SNA.account, type = "l", npcs = 13, main = "Screeplot of the first 13 PCs for twitter account features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.account$sdev^2 / sum(SNA.account$sdev^2))
plot(cumpro[0:13], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 5 PCs")
abline(v = 5, col="blue", lty=5)
abline(h = 0.87954, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.account$x[,1],SNA.account$x[,2], xlab="PC1 (28.5%)", ylab = "PC2 (22.8%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.account, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 13 account features dataset") +
  theme(plot.title = element_text(hjust = 0.5))


####3. 30 all tweets features
SNA.alltweets <- prcomp(mydata[c(20:59)], center = TRUE, scale = TRUE)
summary(SNA.alltweets)


screeplot(SNA.alltweets, type = "l", npcs = 30, main = "Screeplot of the first 30 PCs for all tweets features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.alltweets$sdev^2 / sum(SNA.alltweets$sdev^2))
plot(cumpro[0:30], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 11 PCs")
abline(v = 11, col="blue", lty=5)
abline(h = 0.86025, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC11"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.alltweets$x[,1],SNA.alltweets$x[,2], xlab="PC1 (19.5%)", ylab = "PC2 (13.8%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.alltweets, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 30 all tweets feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))



####4. 25 organic tweets features
SNA.organic <- prcomp(mydata[c(60:84)], center = TRUE, scale = TRUE)
summary(SNA.organic)


screeplot(SNA.organic, type = "l", npcs = 25, main = "Screeplot of the first 25 PCs for organic tweets features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.organic$sdev^2 / sum(SNA.organic$sdev^2))
plot(cumpro[0:25], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 7 PCs")
abline(v = 7, col="blue", lty=5)
abline(h = 0.85518, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC7"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.organic$x[,1],SNA.organic$x[,2], xlab="PC1 (27.9%)", ylab = "PC2 (17.2%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.organic, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 25 organic tweets features dataset") +
  theme(plot.title = element_text(hjust = 0.5))


####5. source of tweets
SNA.source <- prcomp(mydata[c(85:90)], center = TRUE, scale = TRUE)
summary(SNA.source)


screeplot(SNA.source, type = "l", npcs = 6, main = "Screeplot of the first 6 PCs for source features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.source$sdev^2 / sum(SNA.source$sdev^2))
plot(cumpro[0:6], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 4 PCs")
abline(v = 4, col="blue", lty=5)
abline(h = 0.8943, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC4"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.source$x[,1],SNA.source$x[,2], xlab="PC1 (33.5%)", ylab = "PC2 (21.4%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.source, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 6 source features dataset") +
  theme(plot.title = element_text(hjust = 0.5))


#####6. word frequencies features
SNA.word <- prcomp(mydata[c(91:101)], center = TRUE, scale = TRUE)
summary(SNA.word)

screeplot(SNA.word, type = "l", npcs = 11, main = "Screeplot of the first 11 PCs for word frequency")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.word$sdev^2 / sum(SNA.word$sdev^2))
plot(cumpro[0:11], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 1 PC")
abline(v = 1, col="blue", lty=5)
abline(h = 0.8741, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC1"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.word$x[,1],SNA.word$x[,2], xlab="PC1 (87.4%)", ylab = "PC2 (7.3%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.word, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 11 word frequency features dataset") +
  theme(plot.title = element_text(hjust = 0.5))


#####7. sentimental features
SNA.sentimental <- prcomp(mydata[c(102:111)], center = TRUE, scale = TRUE)
summary(SNA.sentimental)

screeplot(SNA.sentimental, type = "l", npcs = 10, main = "Screeplot of the first 10 PCs for sentimental features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.sentimental$sdev^2 / sum(SNA.sentimental$sdev^2))
plot(cumpro[0:10], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 1 PC")
abline(v = 1, col="blue", lty=5)
abline(h = 0.9506, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC1"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.sentimental$x[,1],SNA.sentimental$x[,2], xlab="PC1 (95.1%)", ylab = "PC2 (2.9%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.sentimental, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 10 sentimental features dataset") +
  theme(plot.title = element_text(hjust = 0.5))


####8. Follower analysis
SNA.total <- prcomp(mydata[c(112:135)], center = TRUE, scale = TRUE)
summary(SNA.total)


screeplot(SNA.total, type = "l", npcs = 24, main = "Screeplot of the first 24 PCs for all existing features except follower analysis features")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(SNA.total$sdev^2 / sum(SNA.total$sdev^2))
plot(cumpro[0:24], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot for 5 PCs")
abline(v = 5, col="blue", lty=5)
abline(h = 0.83720, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.6)

plot(SNA.total$x[,1],SNA.total$x[,2], xlab="PC1 (52.2%)", ylab = "PC2 (13.7%)", main = "PC1 / PC2 - plot")

help(fviz_pca_ind)
fviz_pca_ind(SNA.total, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = mydata$Org_VS_Ind, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "organization vs. individual") +
  ggtitle("2D PCA-plot from 24 follower analysis features dataset") +
  theme(plot.title = element_text(hjust = 0.5))
```





```{r}
library("ggmap")
library(maptools)
library(maps)


visited <- c("SFO", "Chennai", "London", "Melbourne", "Johannesbury, SA")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat

```
