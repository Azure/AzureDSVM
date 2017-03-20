
# In this script a learning process that search for an optimal model for solving a classification problem is presented. To illustrate the convenience of using cloud for parallelizing such a learning process. AzureDSR is used.

# algorithms for use.

model_config <- list(name=c("rxLogit", "rxBTrees", "rxDForest"),
                     para=list(list(list(maxIterations=10,
                                         coeffTolerance=1e-6),
                                    list(maxIterations=15,
                                         coeffTolerance=2e-6),
                                    list(maxIterations=20,
                                         coeffTolerance=3e-6)),
                               list(list(nTree=10,
                                         learningRate=0.05),
                                    list(nTree=15,
                                         learningRate=0.1),
                                    list(nTree=20,
                                         learningRate=0.15)),
                               list(list(cp=0.01,
                                         nTree=10,
                                         mTry=3),
                                    list(cp=0.01,
                                         nTree=15,
                                         mTry=3),
                                    list(cp=0.01,
                                         nTree=20,
                                         mTry=3))))

# define a function for binary classification problem.

mlProcess <- function(formula, data, modelName, modelPara) {

  xdf <- RxXdfData(file=data)

  # split data into training set (70%) and testing set (30%).

  data_part <- c(train=0.7, test=0.3)

  data_split <-
    rxSplit(xdf,
            outFilesBase=tempfile(),
            splitByFactor="splitVar",
            transforms=list(splitVar=
                              sample(data_factor,
                                     size=.rxNumRows,
                                     replace=TRUE,
                                     prob=data_part)),
            transformObjects=
              list(data_part=data_part,
                   data_factor=factor(names(data_part), levels=names(data_part))))

  data_train <- data_split[[1]]
  data_test  <- data_split[[2]]

  # train model.

  if(missing(modelPara) ||
     is.null(modelPara) ||
     length(modelPara) == 0) {
    model <- do.call(modelName, list(data=data_train, formula=formula))
  } else {
    model <- do.call(modelName, c(list(data=data_train,
                                       formula=formula),
                                  modelPara))
  }

  # validate model

  scores <- rxPredict(model,
                      data_test,
                      extraVarsToWrite=names(data_test),
                      predVarNames="Pred",
                      outData=tempfile(fileext=".xdf"),
                      overwrite=TRUE)

  label <- as.character(formula[[2]])

  roc <- rxRoc(actualVarName=label,
               predVarNames=c("Pred"),
               data=scores)

  auc <- rxAuc(roc)

  # clean up.

  file.remove(c(data_train@file, data_test@file))

  return(list(model=model, metric=auc))
}

# -----------------------------------------------------------------------
# Step 0 - let's do some test. Set up the experiment.
# -----------------------------------------------------------------------

# read data.

CI_DATA <- "https://zhledata.blob.core.windows.net/mldata/creditcard.xdf"

download.file(CI_DATA,
              destfile="./data.xdf",
              mode="wb")

# download data to all nodes if it is cluster parallel.

if (rxGetComputeContext()@description == "dopar") {
  clusterCall(cl,
              download.file,
              url=CI_DATA,
              destfile="./data.xdf",
              mode="wb")
}

label <- data_config$label[data_index]
label <- as.character(label)

# create a formula.

names <- rxGetVarNames(data="./data.xdf")
names <- names[names != label]
formula <- as.formula(paste(label, "~", paste(names, collapse="+")))

# -----------------------------------------------------------------------
# Step1 - algorithm selection.
# -----------------------------------------------------------------------

# sweep candidate algorithms to select the best one - performance metric such as Area-Under-Curve can be used.

results1 <- rxExec(mlProcess,
                   formula=formula,
                   data="data.xdf",
                   modelName=rxElemArg(model_config$name))

metric1 <- lapply(results1, `[[`, "metric")

algo    <- model_config$name[which(metric1 == max(unlist(metric1)))]
para    <- model_config$para[[which(model_config$name == algo)]]

# -----------------------------------------------------------------------
# Step2 - parameter tuning.
# -----------------------------------------------------------------------
# after an algo is selected based on some criterion (let's say AUC, which is a balanced metric that considers both sensitivity and specificity.), another parallel execution on different sets of parameters are run - parameter tuning.

# sweep parameters of the selected algorithm to find the optimal model.

results2 <- rxExec(mlProcess,
                   formula=formula,
                   data="data.xdf",
                   modelName=algo,
                   modelPara=rxElemArg(para))

# select the optimal model with best performance.

metric2    <- lapply(results1, `[[`, "metric")

model_opt  <- results2[[which(metric2 == max(unlist(metric2)))]][["model"]]
metric_opt <- results2[[which(metric2 == max(unlist(metric2)))]][["metric"]]

# save results for reference.

results <- list(model_opt, metric_opt)
save(results, file="./results.RData")
