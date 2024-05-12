### Generate clusters for the analysis 

setwd("C:/Users/wojci/OneDrive/WSZ/WEC2024")
rm(list = ls())
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(sf) # simple features -> do map
library(dlookr) # do przyjemnej EDA
library(ggplot2)
library(ggthemes)
library(stringr) # do operacji na tekście
library(corrplot)
library(weights)


datamaster_final_full <- read_excel("datamaster_final_full.xlsx")
gminy <- read_sf("WEC_original_data/shapefile/map_municipalities.shp")
colnames(gminy)[1] <- colnames(datamaster_final_full)[1]
colnames(datamaster_final_full)[1]
gminy$municipality_code <- as.numeric(gminy$municipality_code)
gminy <-  left_join(gminy, datamaster_final_full, by = join_by(municipality_code))

gminy <- st_transform(gminy, 4326)


data_municipalities <- datamaster_final_full
data_pca <- data_municipalities %>% 
  dplyr::select(municipality_code, 
                installations_watersupply,
                appartment_area_per_person,
                unemployment_rate,
                population_per_pharmacy,
                revenues_per_capita, 
                net_scholarization) 



data_PrePCA <- preProcess(data_pca[, c(-1)], method=c("center", "scale"))
data_PrePCA.s <- predict(data_PrePCA, data_pca[, c(-1)])
summary(data_PrePCA.s)


pre.cov <-cor(data_PrePCA.s)
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(100)
ord <- order(pre.cov[1, ])
data_ord <- pre.cov[ord, ord]

plotcorr(pre.cov, col=my_colors[data_ord*50+50] , mar=c(1,1,1,1))
KMO(data_PrePCA.s)
cortest.bartlett(data_PrePCA.s)

pca2_schools <- princomp(data_PrePCA.s) 
pca1_schools<- prcomp(data_PrePCA.s, center=FALSE, scale.=FALSE)


fviz_eig(pca2_schools, choice = "eigenvalue", barfill = "#B6163A", barcolor = "#999999", linecolor = "#000000",  addlabels = TRUE,   main = "Eigenvalues") +
  theme_minimal() 

values_pca <- get_eigenvalue(pca2_schools)
values_pca 

fviz_pca_var(pca2_schools, col.var = "black", parse = T) +
  theme_minimal() 

pca_var <- get_pca_var(pca1_schools)
pca_var$contrib

sort(abs(pca1_schools$rotation[,1]), decreasing = T)


first_component <- pca2_schools$scores[,1]
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
index_ineq <- range01(first_component)


data_municipalities$index_ineq <-  index_ineq
data_index_ineq <- data_municipalities[,c(1, 101)]
write.csv(data_index_ineq, "data_index_ineq.csv")

gminy <- read_sf("C:/Users/wojci/OneDrive/WSZ/WEC2024/WEC_original_data/shapefile/map_municipalities.shp")
colnames(gminy)[1] <- colnames(data_municipalities)[1]
colnames(data_municipalities)[1]

gminy_car <-  left_join(gminy, data_municipalities, by = "municipality_code"  )

gminy <- st_transform(gminy_car, 4326)

ggplot(gminy) +
  geom_sf(aes(fill = index_ineq, col = index_ineq)) +
  geom_sf(data = gminy, fill = NA, col = "red")+
  scale_fill_viridis_c()+
  guides(fill = guide_legend(title = "Development \nIndex"),
         color="none")+
  labs(title = "Development index",
       x= "\n",
       y= "\n")

vaccines <- ggplot(gminy) +
  geom_sf(aes(fill = percent_vaccinated,col = percent_vaccinated)) +
  geom_sf(data = gminy, fill = NA, col = "white")+
  scale_fill_viridis_c()+
  guides(fill = guide_legend(title = "Vaccination \nrate"),
         color="none")+
  labs(title = "Vaccination rate in Poland",
       x= "\n",
       y= "\n")+
  theme_minimal()

exposures <- ggplot(gminy) +
  geom_sf(aes(fill = high_exposure, col = high_exposure)) +
  geom_sf(data = gminy, fill = NA, col = "white")+
  scale_fill_viridis_c()+
  guides(fill = guide_legend(title = ""),
         color="none")+
  labs(title = "Excess deaths per 1000 inhabitants in 2020",
       x= "\n",
       y= "\n")+
  theme_bw()

index_ineq <- ggplot(gminy) +
  geom_sf(aes(fill = index_ineq, col = index_ineq)) +
  geom_sf(data = gminy, fill = NA, col = "white")+
  scale_fill_viridis_c()+
  guides(fill = guide_legend(title = ""),
         color="none")+
  labs(title = "Public Goods Provision index",
       x= "\n",
       y= "\n")+
  theme_bw()

ggsave("index_ineq.png",
       plot = index_ineq, 
       device = "png",
       width = 16,
       height = 11.62, 
       units = "cm")


corrplot(cor(data_municipalities[c("index_ineq", "revenues_per_capita", "installations_water")]), method = 'color', order = 'alphabet')

data_cluster <- datamaster_final_full %>% 
  dplyr::select(percent_vaccinated, population_total) 


# Evaluate the number of clusters
p1_kmeans <- fviz_nbclust(as.data.frame(scale(data_cluster[,1])), FUN = stats::kmeans, method ="wss",k.max = 22) + ggtitle("(A) Elbow method")
p1_kmeans
p2_kmeans
p2_kmeans <- fviz_nbclust(as.data.frame(scale(data_cluster[,1])), FUN = stats::kmeans, method = "silhouette", k.max = 22) +
  ggtitle("(B) Silhouette method")
kmeans <- eclust(as.data.frame(scale(data_cluster[,1])), FUNcluster = "kmeans" , k = 2)

datamaster_final_full$cluster_kmeans <- kmeans$cluster
gminy$cluster_kmeans <- kmeans$cluster

clustering <- ggplot(gminy) +
  geom_sf(aes(fill = as.factor(cluster_kmeans), col = as.factor(cluster_kmeans))) +
  geom_sf(data = gminy, fill = NA, col = "white")+
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title = ""),
         color="none")+
  labs(title = "Vaccination clustering",
       x= "\n",
       y= "\n")+
  theme_bw()

ggsave("clustering1.png",
       plot = clustering, 
       device = "png",
       width = 16,
       height = 11.62, 
       units = "cm")

# Weighted Comparison 
tab <- gminy %>% 
  group_by(cluster_kmeans) %>% 
  summarise(mean_vaccines = weighted.mean(x = percent_vaccinated, w = population_total),
            mean_KO = weighted.mean(x = glosy_KO, w = population_total),
            mean_PIS = weighted.mean(x = glosy_PIS, w = population_total),
            mean_sld = weighted.mean(x = glosy_SLD, w = population_total),
            mean_konf = weighted.mean(x = glosy_KONF, w = population_total),
            index_ineq = weighted.mean(x = index_ineq, w = population_total),
            exp = weighted.mean(x = high_exposure, w = population_total),
            type1 = weighted.mean(x = type_0_20k, w = population_total),
            type2 = weighted.mean(x = type_20_50k, w = population_total),
            type3 = weighted.mean(x = type_50_100k, w = population_total),
            type4 = weighted.mean(x = type_100_500k, w = population_total),
            type5 = weighted.mean(x = `type_500k+`, w = population_total)
            )
wtd.t.test(x = gminy$percent_vaccinated[gminy$cluster_kmeans == 1],
           y = gminy$percent_vaccinated[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$glosy_KO[gminy$cluster_kmeans == 1],
           y = gminy$glosy_KO[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$glosy_PIS[gminy$cluster_kmeans == 1],
           y = gminy$glosy_PIS[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$glosy_KONF[gminy$cluster_kmeans == 1],
           y = gminy$glosy_KONF[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$type_0_20k[gminy$cluster_kmeans == 1],
           y = gminy$type_0_20k[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$type_50_100k[gminy$cluster_kmeans == 1],
           y = gminy$type_50_100k[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$type_100_500k[gminy$cluster_kmeans == 1],
           y = gminy$type_100_500k[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)


wtd.t.test(x = gminy$`type_500k+`[gminy$cluster_kmeans == 1],
           y = gminy$`type_500k+`[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$high_exposure[gminy$cluster_kmeans == 1],
           y = gminy$high_exposure[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$index_ineq[gminy$cluster_kmeans == 1],
           y = gminy$index_ineq[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$pop_t_0_19_perc[gminy$cluster_kmeans == 1],
           y = gminy$pop_t_0_19_perc[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

wtd.t.test(x = gminy$pop_t_0_19_perc[gminy$cluster_kmeans == 1],
           y = gminy$pop_t_0_19_perc[gminy$cluster_kmeans == 2],
           weight = gminy$population_total[gminy$cluster_kmeans == 1],
           weighty = gminy$population_total[gminy$cluster_kmeans == 2],
           samedata = FALSE)

gminy <- gminy %>% 
  group_by(county_code) %>%
  mutate(pop_tot = sum(population_total),
         index_ineq_mean = weighted.mean(x = index_ineq, w = population_total), 
         pop_share = population_total/pop_tot, 
         index_iv  = index_ineq_mean - index_ineq*pop_share,
         sld_mean = weighted.mean(x = glosy_SLD, w = population_total),
         sld_iv  = sld_mean - glosy_SLD*pop_share)

cor(gminy$index_ineq, gminy$index_iv)
gminy_database <- st_drop_geometry(gminy)
write.csv(gminy_database , "gminy.csv")
