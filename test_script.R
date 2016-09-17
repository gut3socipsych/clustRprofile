library(reshape2)
library(dplyr)
#test dataset to produce a melted dataframe as arg in plot function 
head(mtcars) #dataset
cars_dist<-dist(mtcars, method = "euclidean") #distance measure  
cars_clust<-hclust(cars_dist) #cluster matrix 
plot(cars_clust) #denrogram to visualize number of clusters 
clust_assgn<-cutree(cars_clust, k = 3) #assign clusters based on number of clusters

cars_z<-mtcars %>% scale(., center = TRUE, scale = TRUE) #create standardized scores (z-scores)
cars_z<-tbl_df(as.data.frame(cars_z)) #reformat as local df

cars_z<-bind_cols(cars_z, as.data.frame(clust_assgn)) #create standardized df with cluster assignment df
cars_z<-tbl_df(cars_z) #reformat as local df 

cars_z_means<-cars_z %>% group_by(clust_assgn) %>% summarise_each(funs(mean(.,na.rm=TRUE))) #summarize standarized scores df
cars_melt<-melt(cars_z_means, id.vars = "clust_assgn") #reshape standardized scores df 

clustRprofile(data_melt = cars_melt)
