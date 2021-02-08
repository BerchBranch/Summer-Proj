randomized_edges <- function(data){
  random_order_convos <- sample(data$conversation_id)
  return(data.frame(username = data$username, conversation_id = random_order_convos))
}



run_for_loop <- function(num_randomizations){
  # Input the number of randomly generated null models (int)
  # Iterates over each week and generates the stats for the null model (average of the random models)
  # Writes csv for each week containing the stats of each generated graph
  # Returns the null model stats
  cur_start <- start_date
  cur_end <- start_date + 7
  
  
  stats <- tibble(Week_start = character(),
                  Week_end = character(),
                  Week = numeric(),
                  Net_size = numeric(),
                  Density = numeric(),
                  Diameter = numeric(),
                  Avg_distance = numeric())
  
  
  null_stats <- tibble(Week_start = character(),
                       Week_end = character(),
                       Week = numeric(),
                       Net_size = numeric(),
                       Density = numeric(),
                       Diameter = numeric(),
                       Avg_distance = numeric(),
                       Global_clustering = numeric(),
                       Average_clustering = numeric())
  
  
  set.seed(1234)
  for (i in 1:17) {
    print(i)
    graph_data <- dates_ord %>%
      filter(date >= cur_start & date < cur_end) %>%
      select(username, conversation_id) %>%
      na.omit() %>%
      distinct() %>%
      remove_singles()
    
    sample_tibble <- tibble(Density = numeric(),
                            Diameter = numeric(),
                            Avg_distance = numeric(),
                            Week = numeric())
    
    
    for (j in 1:num_randomizations){
      print(j)
      null <- randomized_edges(graph_data)
      null_G <- graph.data.frame(null)
      
      
      sample_tibble <- sample_tibble %>% add_row(Density = edge_density(null_G),
                                                 Diameter = diameter(null_G, weights = NA, directed = F),
                                                 Avg_distance = mean_distance(null_G, directed = F),
                                                 Week = i)
      
    }
    
    write_csv(sample_tibble, sprintf("Bip %s.csv", i))
    
    null_stats <- null_stats %>% add_row(Week_start = as.character(cur_start),
                                         Week_end = as.character(cur_end),
                                         Week = i,
                                         Net_size = gsize(null_G),
                                         Density = mean(sample_tibble$Density),
                                         Diameter = mean(sample_tibble$Diameter),
                                         Avg_distance = mean(sample_tibble$Avg_distance))
    
    
    #observed Stats
    G <- graph.data.frame(graph_data)
    
    
    
    stats <- stats %>% add_row(Week_start = as.character(cur_start),
                              Week_end = as.character(cur_end),
                        Week = i,
                        Net_size = gsize(G),
                        Density = edge_density(G),
                        Diameter = diameter(G, weights = NA, directed = F),
                        Avg_distance = mean_distance(G, directed = F))
    
    
    
    cur_start <- cur_start + 7
    cur_end <- cur_end + 7
  }
  write.csv(stats, "bip_stats.csv")
  return(null_stats)
}

null_bip_stats <- run_for_loop(10)









get_null_std <- function(){
  return_std <- function(file){
    df_new <- data.frame(read_csv(file))
    week <- df_new$Week[1]
    print(week)
    stds <- df_new %>% summarise_if(is.numeric, sd)
    stds <- mutate(stds, Week = week)
    
    return(stds)
    
  }
  
  print(list.files(path="Null samples", full.names = TRUE))
  
  std <- list.files(path="Null samples", full.names = TRUE) %>%
    lapply(return_std)
  
  std <- as.data.frame(do.call(rbind, std))
  std <- arrange(std, by_group=Week)
  return(std)
}








null_stats <- function(){
  
  null_stats <- tibble(Density = numeric(),
                       Diameter = numeric(),
                       Avg_distance = numeric(),
                       Global_clustering = numeric(),
                       Average_clustering = numeric())
  
  for (file in list.files(path="Null samples", full.names = TRUE)){
    sample_tibble <- tibble(read.csv(file))
    null_stats <- null_stats %>% add_row(Density = mean(sample_tibble$Density),
                                         Diameter = mean(sample_tibble$Diameter),
                                         Avg_distance = mean(sample_tibble$Avg_distance),
                                         Global_clustering = mean(sample_tibble$Global_clustering),
                                         Average_clustering = mean(sample_tibble$Average_clustering))
  }
  null_stats["Week"] <- 1:nrow(null_stats)
  return(null_stats)
}








