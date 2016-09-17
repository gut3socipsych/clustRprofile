clustRprofile<-function(data_melt){
  require(ggplot2)
  c_theme_default<-theme_gray() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.25))
  profile_plot<-ggplot(data_melt, aes(x = variable, y = value, group = as.factor(clust_assgn), color = as.factor(clust_assgn))) +
    geom_path(alpha = 0.9) + geom_point() + 
    labs(x = "Variables", y = "Scaled Averages", title = "Cluster Profiles", color = "Clusters") + 
    c_theme_default
  print(profile_plot)
}


