#WORKING NOTES....
library(reshape2)
source("clustRprofile_function_script.R")

test <- mtcars

test_dist <- dist(test,method = "e")
test_clust <- hclust(test_dist)
plot(test_clust)
clust_assgn <- cutree(test_clust, k = 3)
test_z <- as.data.frame(scale(x = test, center = T, scale = T))
test_z <- cbind(test_z, as.data.frame(clust_assgn)) #attach cluster assignment 
test_z <- aggregate(.~clust_assgn, data = test_z, FUN = mean) #summarize standarized scores df
test_z<-melt(test_z, id.vars = "clust_assgn") #reshape standardized scores df 

clustRprofile(data_melt = test_z)
test$clust_assign <- clust_assgn



a <- test_fun(test, 4)
a
clustRprofile(test, 2)
#necessary steps to format original data 
#standardized the values of each variable, 
#so that they can be plotted uniformlly 

#attach cluster assignment to the standarized data

clustRprofile_v2<-function(data_melt){
  require(ggplot2)
  c_theme_default<-theme_gray() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.25))
  profile_plot<-ggplot(data_melt, aes(x = variable, y = value, group = as.factor(clust_assgn), color = as.factor(clust_assgn))) +
    geom_path(alpha = 0.9) + geom_point() + 
    labs(x = "Variables", y = "Scaled Averages", title = "Cluster Profiles", color = "Clusters") + 
    c_theme_default
  print(profile_plot)
}

test_fun <- function(){
  
}

