# whole process of Hotspots analysis (including model creation and evaluation)

hotSpotsProcess <- function(data, 
                            number.of.clust=2:20,
                            train_ratio=0.7,
                            lib=.libPaths()) {
  
  # check compute context, and set it to local parallel if upper level is cluster parallel.
  
  if (rxGetComputeContext()@description == "dopar") {
    rxSetComputeContext(RxLocalParallel())
  }
  
  # source scripts where functions reside.
  
  source("workerHotspotsSetup.R")
  source("workerHotspotsFuncs.R")
  source("workerHotspotsTrain.R")
  source("workerHotspotsTest.R")
  
  # ------------------------------------------------------------------------
  # data preparation
  # ------------------------------------------------------------------------

  xdf <- RxXdfData(data)
  
  # split into training and testing.
  
  data_split <- dataSplit(xdf, train_ratio)
  data_train <- RxXdfData(data_split[[1]], varsToDrop="splitVar")
  data_test  <- RxXdfData(data_split[[2]], varsToDrop="splitVar")

  # ------------------------------------------------------------------------
  # model training
  # ------------------------------------------------------------------------
  
  # clustering.
  
  # segments <- rxExec(FUN=clusterAnalysis,
  #                    data=data_train,
  #                    centers=rxElemArg(number.of.clust))
  
  segments <- lapply(number.of.clust,
                     FUN=clusterAnalysis,
                     data=data_train)
  
  # determine the optimal k for kmeans with elbow.
  
  elbow <- distPointLine(unlist(lapply(segments, `[[`, "metric")))
  
  # train models for differentiating clusters and classifying data within a cluster.
  
  cluster_model <- segments[[elbow]]$model
  cl <- 1:max(cluster_model$cluster)
  
  models <- rxExec(FUN=modelCreation,
                   data=data_train,
                   cluster=cluster_model,
                   cl=rxElemArg(cl),
                   lib=lib)
  
  # models <- lapply(cl,
  #                  FUN=modelCreation,
  #                  data=data_train,
  #                  cluster=cluster_model,
  #                  lib=lib)
  
  # ------------------------------------------------------------------------
  # model testing
  # ------------------------------------------------------------------------
  
  # determine the cluster testing data belongs to and then use the classifier within that cluster to predict label.
  
  df_test     <- rxImport(data_test) # TODO: this can be optimized for xdf. 
  df_scores   <- NULL
  
  model_index <- 1:length(models)
  
  # predict cluster.
  
  df_scores <- rxExec(FUN=predictCluster,
                      df_test=df_test,
                      models=models,
                      index=rxElemArg(model_index))
  
  # df_scores <- lapply(model_index,
  #                     FUN=predictCluster,
  #                     df_test=df_test,
  #                     models=models)
  
  df_scores <- data.frame(do.call(cbind, df_scores))
  
  # predict label.
  
  pred <- predictLabel(df_test, df_scores, models)
  
  eval <- table(True=df_test$Class, Pred=pred$Class)
  
  eval
}