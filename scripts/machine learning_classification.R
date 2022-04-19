rm(list = ls())
graphics.off()

pacman::p_load(caret,GGally,magrittr,pacman,parallel,randomForest,rattle,rio,tictoc,tidyverse,ggfortify)

#setwd("data/seminario R/")
data = readxl::read_excel("data/osl_parnaiba_table.xlsx", sheet = "by_aliquot") %>%
  print()

data$BOSLF = as.numeric(data$BOSLF)
data$BOSLM = as.numeric(data$BOSLM)
data$BOSLS = as.numeric(data$BOSLS)

colnames(data)[9] = "IRSL"

data$BOSLF[is.na(data$BOSLF)] = 33.333
data$BOSLM[is.na(data$BOSLM)] = 33.333
data$BOSLS[is.na(data$BOSLS)] = 33.333

osl.c = 1:length(data$BOSL1s)
for (i in 1:length(data$BOSL1s)) {
  if (data$BOSL1s[i]>9) {
    osl.c[i] = "High"
  } else if (data$BOSL1s[i]<4) {
    osl.c[i] = "Low"
  } else {
    osl.c[i] = "Medium"}
}

data %<>%
  #mutate(osl.t = ifelse(osl>=9,"High","Medium"), osl.t = as_factor(osl.t)) %>%
  mutate(osl.c = as.factor(osl.c)) %>%
  print()

#pca
pca <- prcomp(data[6:ncol(data)-1], scale. = TRUE)
autoplot(pca, data = data, colour = 'Group')

# Hierarchical clustering #####
library(factoextra); library(cluster); library(magrittr)
hc = data %>%
  select(BOSL1s:TL110oC, Unit) %>% 
  dist %>% hclust

hc %>% plot(labels = data$Unit, hang = -1, cex = 0.6)

#optimal number of clusters
data %>% 
  fviz_nbclust(FUN = hcut, method = "wss")+
  geom_vline(xintercept = 3, color = "red", linetype = "dotted")

data %>% 
  fviz_nbclust(FUN = hcut, method = "silhouette")

data %>% 
  na.omit(data) %>%
  select(-Unit, -Group, -sample, -osl.c, -...1) %>%
  clusGap(FUN = hcut, nstart = 50, K.max = 10, B = 100) %>%
  fviz_gap_stat()

hc %>% rect.hclust(k = 3, border = 2:5)

y.hc = cutree(hc, 3)
fviz_cluster(list(data = data %>% 
                    select(BOSL1s, BOSLF, BOSLM, BOSLS, IRSL, TL110oC),
                  cluster = y.hc))

# K-means clustering #####
data.s = data %>% 
  select(-...1, -sample, -Group, -Unit, -osl.c) %>%
  scale() %>% 
  print()

km = data.s %>% kmeans(3) %>% print()
data.s %>% clusplot(km$cluster, color = T, lines = 0, labels = 2)

#add clusters to the dataframe
data %<>%
  mutate(cluster = km$cluster) %>% 
  select(-...1) %>%
  arrange(cluster) %>% 
  print()
