predictCluster <- function(df_test, models, index) {
  model_s <- models[[index]]$model_s
  
  scores <- rxPredict(model_s, df_test)
}

predictLabel <- function(df_test, df_scores, models) {
  
  # get the highest score to determine the predictive model to use.
  
  id <- apply(df_scores, 1, function(x) which(x == min(x)))
  
  df_test <- 
    cbind(df_test, id) %>%
    mutate(key = row_number())
  
  model_index <- unique(id)
  
  pred <- NULL
  
  for (j in model_index) {
    
    df <- filter(df_test, id == j)
    
    model_c <- models[[j]]$model_c
    
    if (is.numeric(model_c)) {
      # since there is only one class within the cluster.
      
      pred <- rbind(pred, data.frame(Class_prob=rep(model_c, nrow(df)), key=df$key))
    } else {
      result <- rxPredict(model_c, df)
      
      pred <- rbind(pred, mutate(result, key=df$key))
    }
  }
  
  pred <- 
    mutate(pred, Class=ifelse(Class_prob > 0.5, 1, 0)) %>%
    arrange(key)
  
  pred
}