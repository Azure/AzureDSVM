# do preparation.

# -------------------------------------------------------------------------
# Split data into training and testing sets.
# -------------------------------------------------------------------------

dataSplit <- function(data, ratio) {
  
  data_part <- c(train=ratio, test= 1 - ratio)
  
  data_split <-
    rxSplit(data, 
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
  
  data_split
  
}