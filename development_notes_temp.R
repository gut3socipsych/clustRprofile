#WORKING NOTES....

#develop into class object with multiple outputs 

clustRprofile <- function(dataset, cluster_count){
  setClass(Class = "clustRprofile",
           slots = c(cluster_tree_plot = "ANY", profile_plot = "ANY"))
  require(ggplot2);require(reshape2)
  #calculate cluster groups
  df_dist <- dist(dataset, method = "e")
  df_clust <- hclust(df_dist)
  cluster_tree_plot <- plot(df_clust)
  clust_assgn <- cutree(df_clust, k = cluster_count)
  df_z <- as.data.frame(scale(x = dataset, center = T, scale = T))
  df_z <- cbind(df_z, as.data.frame(clust_assgn))
  df_z <- aggregate(.~clust_assgn, data = df_z, FUN = mean)
  df_z <- melt(df_z, id.vars = "clust_assgn")
  #build cluster plot
  profile_plot <- ggplot(data = df_z, aes(x = variable, 
                                          y = value, 
                                          group = as.factor(clust_assgn), 
                                          color = as.factor(clust_assgn))) +
    geom_path(alpha = 0.9) + geom_point() + 
    labs(x = "Variables", y = "Scaled Averages", title = "Cluster Profiles", color = "Clusters") +
    theme_gray() + theme(axis.text.x = element_text(angle = 90, vjust = 0.25))
  return(new("clustRprofile",cluster_tree_plot=cluster_tree_plot ,profile_plot=profile_plot))
}

test <- clustRprofile(dataset = mtcars, cluster_count = 3)


test@profile_plot
test@cluster_tree_plot ###fix this ... 

