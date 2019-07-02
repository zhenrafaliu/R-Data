library(ggplot2)
library(factoextra)
library(purrr)
library(cluster)

table <- read.csv("USAedu.csv", header = T, sep = ",")


ggplot(table,aes(x=bachelor,y=gdp))+
  geom_point(aes(color=state))
set.seed(123)
km <- kmeans(table[,3:4],3,nstart=20)

table(km$cluster,table$state)
fviz_cluster(km,data=table[,3:4])


fviz_nbclust(table[,3:4], kmeans, method = "wss")


gap_stat <- clusGap(table[,3:4], FUN = kmeans, nstart=200,K.max=10,B = 50)
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)