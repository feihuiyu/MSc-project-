library(ggplot2)
library(factoextra)
library(NbClust)
library(dplyr)
library(cluster)
library(truncnorm)


setwd("/Users/yufeihui/Desktop")
rm(list = ls())

measure <- read.csv("measurement.csv")

#calculate SI
measure <- measure %>% 
  mutate(si = width.px/length.px*100)

#number of cooperia
sum(measure$id == "coop")

#histogram to see distribution
hist(measure$si, breaks = 30)

#find SI optimal cluster
sicluster <- measure %>%
  select(si)
fviz_nbclust(sicluster, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(sicluster, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")
#opt cluster is 2

#clustering
cluster <- kmeans(measure[,6], centers = 2)
measure$cluster<- cluster$cluster
cluster
plot(measure$length.px, 
     measure$width.px, 
     col=factor(measure$cluster))

#descriptive stats
summary(measure)
sd(measure$si)
measure %>% count(cluster)
descriptive <- measure %>% 
  group_by(cluster) %>% 
  summarise(mean = mean(si),
            max = max(si),
            min = min(si),
            sd = sd(si))
descriptive 

#comparing the 2 clusters
t.test(measure$si~measure$cluster)

#density plot of SI
ggplot(measure, aes(x = si)) + 
  geom_density(bw = 1)

#summarise each cluster to compare to literature
measure %>% 
  group_by(cluster) %>% 
    summarise(mean = mean(si),
              min = min(si),
              max = max(si),)

#compare cluster1 with coop in Shorb,1939
cluster1 <- measure %>% 
  select(si,cluster) %>% 
    filter(cluster == 1)

#repeat 1000 times
set.seed(123)
result <- data.frame(n = numeric(), t = numeric(), p = numeric(), co = numeric(), c_cluster=numeric())
for(i in 1:1000){
  coo <- data.frame(si = rtruncnorm(n=100, a=37, b=52, mean=45.9, sd=2.7), 
                  cluster="cooperia")
  data2 <- rbind(coo,cluster1)
  test<-t.test(data2$si~data2$cluster)
  temp<-data.frame(n=i, t=test$statistic[[1]] ,p=test$p.value[[1]], co= test$estimate[[1]], c_cluster=test$estimate[[2]])
  result<- rbind(result, temp)
}
mean(result$t)

#see which cluster(s) my identified cooperia fall into
my_coop <- measure %>% 
  filter(id == "coop")

#make a plot (figure 4)
clusterplot <- measure %>% 
  ggplot() +
  geom_density(aes(x = si,
                   group = factor(cluster),
                   fill = factor(cluster)),
               alpha = 0.5) +
  geom_histogram(aes(x = si,
                     y = after_stat(density)), color = "black", fill = "grey81", alpha = 0.7) +
  ylab("Density") +
  xlab("SI") +
  theme_bw() +
  scale_fill_manual(values = c("1" = "lightgoldenrodyellow", "2" = "thistle1"), # or burlywood1 or honeydew1
                    labels = c("A", "B")) +
  labs(fill = "Cluster") +
  ylim(0, 0.3) +
  geom_segment(aes(x = 45.9, y = 0, xend = 45.9, yend = 0.209), linetype = "dashed") +
  geom_segment(aes(x = 51, y = 0, xend = 51, yend = 0.209), linetype = "dashed") +
  geom_segment(aes(x = 53.7, y = 0, xend = 53.7, yend = 0.209), linetype = "dashed") +
  geom_segment(aes(x = 57.6, y = 0, xend = 57.6, yend = 0.209), linetype = "dashed") +
  annotate(
    geom = "text",
    label = "C. oncophora", 
    x = 45.9,
    y = 0.30,
    angle = 90,
    hjust = 1
  ) +
  annotate(
    geom = "text",
    label = "O. ostertagi", 
    x = 51,
    y = 0.290,
    angle = 90,
    hjust = 1
  ) +
  annotate(
    geom = "text",
    label = "H. contortus", 
    x = 53.7,
    y = 0.2945,
    angle = 90,
    hjust = 1
  ) + 
  annotate(
    geom = "text",
    label = "O. radiatum", 
    x = 57.6,
    y = 0.290,
    angle = 90,
    hjust = 1
  )

clusterplot





