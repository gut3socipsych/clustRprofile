clustRprofile <- function(dataset, cluster_count){
  setClass(Class = "clustRprofile",
           slots = c(profile_plot = "ANY",
                     cluster_assigned_df = "data.frame",
                     cluster_values = "ANY"))
  require(ggplot2);require(reshape2)
  #calculate cluster groups
  df_dist <- dist(dataset, method = "e")
  df_clust <- hclust(df_dist)
  clust_assgn <- cutree(df_clust, k = cluster_count)
  df_z <- as.data.frame(scale(x = dataset, center = T, scale = T))
  df_z <- cbind(df_z, as.data.frame(clust_assgn))
  df_z <- aggregate(.~clust_assgn, data = df_z, FUN = mean)
  df_z <- melt(df_z, id.vars = "clust_assgn")
  #build cluster assigned data frame 
  cluster_assigned_df <- cbind(dataset, clust_assgn)
  #build cluster plot
  profile_plot <- ggplot(data = df_z, aes(x = variable, 
                                          y = value, 
                                          group = as.factor(clust_assgn), 
                                          color = as.factor(clust_assgn))) +
    geom_path(alpha = 0.9) + geom_point() + 
    labs(x = "Variables", y = "Scaled Averages", title = "Cluster Profiles", color = "Clusters") +
    theme_gray() + theme(axis.text.x = element_text(angle = 90, vjust = 0.25))
  return(new("clustRprofile",
             profile_plot=profile_plot,
             cluster_assigned_df=cluster_assigned_df,
             cluster_values=df_clust))
}


