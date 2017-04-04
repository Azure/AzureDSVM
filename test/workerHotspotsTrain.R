# functions for hotspot analysis.

# -------------------------------------------------------------------------
# cluster analysis using kmeans algorithm.
# -------------------------------------------------------------------------

clusterAnalysis <- function(data, centers) {
  
  # to scale data.
  
  df <- rxImport(data)
  df <- subset(df, select=c(-Time, -Class))
  df <- as.data.frame(scale(df))
  
  for (i in 1:10) {
    clusters <- kmeans(df, 
                       centers=centers)
    # algorithm="Lloyd")
    
    # calculate the explained variance for a reference of determining the optimal number of clusters.
    
    model <- clusters
    exp_var <- clusters$betweenss / clusters$totss
    
    if (i == 1) {
      exp_var_opt <- exp_var
    } else {
      if (exp_var > exp_var_opt) {
        exp_var_opt <- exp_var
        model_opt <- model
      }
    }
  }
  
  list(model=model, metric=exp_var_opt)
}

# -------------------------------------------------------------------------
# find "elbow" in a 1-d curve using point-to-line distance. 
# -------------------------------------------------------------------------
# Input y is the y value of the points, and x is the index of the y variables. The function firstly defines a line passing through (x1, y1) and (xn, yn) where n is the length of the 1-d curve. Then distances between each point in the curve to that line is calculated. The maximum distance corresponds to the elbow.

distPointLine <- function(y) {
  if (length(y) < 3) 
    error("Please sweep initial number of clusters in a larger range (> 3).")
  
  x <- 1:length(y)
  
  x_1 <- x[1]
  x_2 <- x[length(x)]
  
  y_1 <- y[1]
  y_2 <- y[length(y)]
  
  dist <- NULL
  
  # calculate the distance
  
  for (i in 2:(length(y) - 1)) {
    dist[length(dist) + 1] <- abs((y_2 - y_1) * x[i] - (x_2 - x_1) * y[i] + x_2 * y_1 - y_2 * x_1) / (sqrt((y_2 - y_1) ^ 2 + (x_2 - x_1) ^ 2))
  }
  
  elbow <- which(dist == max(dist))
  
  elbow
}

# -------------------------------------------------------------------------
# create tree models to differentiate data segments and classify data within a cluster.
# -------------------------------------------------------------------------

modelCreation <- function(data, cluster, cl, lib) {
  library(dplyr)
  library(DMwR, lib.loc=lib)
  
  # TODO: do this natively on XDF with dplyrXdf.
  
  data <- rxImport(data)
  data <- subset(data, select=-Time)
  
  df_clust <- filter(data, row_number() %in% which(cluster$cluster == cl))
  df_not_clust <- filter(data, !(row_number() %in% which(cluster$cluster == cl)))
  
  df_one <- 
    select(df_clust, -Class) %>%
    mutate(label=factor(1, levels=c("0", "1"))) 
  
  df_another <- 
    select(df_not_clust, -Class) %>%
    mutate(label=factor(0, levels=c("0", "1")))
  
  df_train_s <- rbind(df_one, df_another)
  
  if (nrow(df_one) < round(.1 * nrow(df_another))) {
    df_train_s <- as.data.frame(df_train_s)
    df_train_s <- SMOTE(label ~ ., 
                        data=df_train_s,
                        perc.over=300, 
                        perc.under=150)
  }
  
  # train a tree model to differentiate one segment from the rest.
  
  names <- rxGetVarNames(df_train_s)
  vars <- names[-length(names)]
  form <- formula(paste0("label ~ ", paste(vars, collapse="+")))
  
  model_s <- rxBTrees(formula=form, data=df_train_s)
  
  # then train another model to do classification within the cluster.
  
  df_train_c <- 
    df_clust %>%
    mutate(Class=factor(Class))
  
  names <- rxGetVarNames(df_train_c)
  vars <- names[-length(names)]
  form <- formula(paste0("Class ~ ", paste(vars, collapse="+")))
  
  # the segment may contain merely one class of data - in this case, the model will not be created, and a NULL is returned.
  
  if (length(unique(df_train_c$Class)) == 1) {
    model_c <- as.numeric(df_train_c$Class[1]) # if there is no model being created, return the class label as a numerical number.
  } else {
    model_c <- rxBTrees(formula=form, data=df_train_c)
  }
  
  # return the two models.
  
  models <- list(model_s=model_s, model_c=model_c)
  
  models
}