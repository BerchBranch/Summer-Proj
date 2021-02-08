get_null_stats <- function(p, stat){
  return_mean <- function(file){
    df_new <- data.frame(read_csv(file))
    week <- df_new$Week[1]
    m <- df_new %>% summarise_if(is.numeric, stat)
    m <- mutate(m, Week = week)
    
    return(m)
  }
  mean <- list.files(path=p, full.names = TRUE) %>%
    lapply(return_mean)
  mean <- as.data.frame(do.call(rbind, mean))
  mean <- arrange(mean, by_group=Week)
  return(mean)
}



add_week_col <- function(){
  for(i in 1:36){
    c <- data.frame(read.csv(sprintf("Null sample/Exact degree/Null sample week %s.csv",i)))
    c <- mutate(c, "Week"=i)
    write.csv(c, sprintf("Null sample/Exact degree/%s.csv",i),row.names = F)
  }
}

