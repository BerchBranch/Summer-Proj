library(ggplot2)
library(lubridate)
library(tidyverse)
stats <- data.frame(read.csv("stats.csv"))
bip_stats <- data.frame(read.csv("bip_stats.csv"))


# Unip null Exact
null_stds_exact <- get_null_stats(p = "Null sample/Exact degree", sd)
null_means_exact <- get_null_stats("Null sample/Exact degree", mean)

# Unip null Expected
null_stds_expected <- get_null_stats(p = "Null sample/Expected degree", sd)
null_means_expected <- get_null_stats("Null sample/Expected degree", mean)

# Unip null Non
null_stds_non <- get_null_stats(p = "Null sample/Expected degree", sd)
null_means_non <- get_null_stats("Null sample/Non degree", mean)


# Bip Null
bip_null_stds <- get_null_stats(p = "Bip null samples", sd)
bip_null_means <- get_null_stats("Bip null samples", mean)


join_mean_sd <- function(m, sd){
  merge(x=m, y=sd, by="Week")
}

joined_exact <- join_mean_sd(null_means_exact, null_stds_exact)

joined_expected <- join_mean_sd(null_means_expected, null_stds_expected)

joined_non <- join_mean_sd(null_means_non, null_stds_non)

combofy <- function(joined, s=stats){
  combo <- merge(x=s, y=joined, by.x = "Week", by.y = "Week")
  return(combo)
}

combo_exact <- combofy(joined_exact)
combo_expected <- combofy(joined_expected)
combo_non <- combofy(joined_non)

plot_density_zscore <- function(combo){
  combo %>%
    ggplot(aes(x = Week)) +
    geom_line(aes(y = (Density-Density.x)/Density.y), color="red1") +
    geom_hline(yintercept=3) +
    geom_hline(yintercept = -3) +
    #geom_smooth() +
    theme_minimal()
}


plot_density_zscore(combo_exact)
plot_density_zscore(combo_expected)
plot_density_zscore(combo_non)


plot_global_clustering_zscore <- function(combo){
  combo %>%
    ggplot(aes(x = Week)) +
    geom_line(aes(y = (Global_clustering-Global_clustering.x)/Global_clustering.y), color="red1") +
    geom_hline(yintercept=3) +
    geom_hline(yintercept = -3) +
    #geom_smooth() +
    theme_minimal()
}

plot_global_clustering_zscore(combo_exact)
plot_global_clustering_zscore(combo_expected)
plot_global_clustering_zscore(combo_non)



plot_avg_distance_zscore <- function(combo){
  combo %>%
    ggplot(aes(x = Week)) +
    geom_line(aes(y = (Avg_distance-Avg_distance.x)/Avg_distance.y), color="red1") +
    geom_hline(yintercept=3) +
    geom_hline(yintercept = -3) +
    #geom_smooth() +
    theme_minimal()
}

plot_avg_distance_zscore(combo_exact)
plot_avg_distance_zscore(combo_expected)
plot_avg_distance_zscore(combo_non)


comparison <- data.frame(unip = stats$Density, bip = bip_stats$Density, Week = stats$Week)

comparison %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = unip), color="red1") +
  geom_line(aes(y = bip*70), color="blue1") +
  #geom_smooth() +
  theme_minimal()






combo <- merge(x=stats, y=null_means_expected, by.x = "Week", by.y = "Week")





combo_exact %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = (Global_clustering-null_global_cluster)/null_stds$Global_clustering), color="red1") +
  geom_hline(yintercept=3) +
  geom_hline(yintercept = -3) +
  #geom_smooth() +
  theme_minimal()



combo <- merge(x=,stats, y=null_means_expected, by="Week")



# Density plots
combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = (Density.x-Density.y)/null_stds_expected$Density), color="red1") +
  geom_hline(yintercept=3) +
  geom_hline(yintercept = -3) +
  #geom_smooth() +
  theme_minimal()

combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Density.x), color="red1") +
  geom_line(aes(y = Density.y), color="blue1") +
  #geom_smooth() +
  theme_minimal()



# diameter plots
combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = (Diameter.x-Diameter.y)/null_stds_expected$Diameter), color="red1") +
  geom_hline(yintercept=3) +
  geom_hline(yintercept = -3) +
  #geom_smooth() +
  theme_minimal()

combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Diameter.x), color="red1") +
  geom_line(aes(y = Diameter.y), color="blue1") +
  #geom_smooth() +
  theme_minimal()



# Avg dist plots
combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = (Avg_distance.x-Avg_distance.y)/null_stds$Avg_distance), color="red1") +
  geom_hline(yintercept=3) +
  geom_hline(yintercept = -3) +
  #geom_smooth() +
  theme_minimal()

combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Avg_distance.x), color="red1") +
  geom_line(aes(y = Avg_distance.y), color="blue1") +
  #geom_smooth() +
  theme_minimal()



# Avg dist plots
combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = (Avg_distance.x-Avg_distance.y)/null_stds_expected$Avg_distance), color="red1") +
  geom_hline(yintercept=3) +
  geom_hline(yintercept = -3) +
  #geom_smooth() +
  theme_minimal()

combo %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Avg_distance.x), color="red1") +
  geom_line(aes(y = Avg_distance.y), color="blue1") +
  #geom_smooth() +
  theme_minimal()



# Bip vs Unip

both <- data.frame(Unip.density = stats$Density, Bip.Density =)