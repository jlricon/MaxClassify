#' Train a classifier
#' Given a matrix with word counts and a vector of labels, train a MaxEnt classifier
#' @param train_matrix A matrix containing word counts
#' @param train_labels A matrix containing the labels for the training matrix
#' @return A maxent model
#' @export
trainMC = function(feature_matrix,code_vector, l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE ){
UseMethod("trainMC",feature_matrix)
}

trainMC.matrix = function(feature_matrix,code_vector, l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE ){
  model   = maxent::maxent(feature_matrix,code_vector,
                           l1_regularizer = l1_regularizer,
                           l2_regularizer = l2_regularizer,
                           use_sgd        = use_sgd,
                           set_heldout    = set_holdout,
                           verbose        = verbose )
  returns = list()
  returns$model = model@model
  returns$weights = model@weights
  attr(returns, "class") = "MaxEntModel"
  return(returns)
}

trainMC.dfm = function(feature_matrix, l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE ){
  model   = maxent::maxent(feature_matrix, feature_matrix@docvars$labels,
                           l1_regularizer = l1_regularizer,
                           l2_regularizer = l2_regularizer,
                           use_sgd        = use_sgd,
                           set_heldout    = set_holdout,
                           verbose        = verbose )
  returns = list()
  returns$model = model@model
  returns$weights = model@weights
  attr(returns, "class") = "MaxEntModel"
  return(returns)
}
#' Predict using a previously trained classifier, given a matrix with labels and probabilities
#' @param model A MaxEnt model
#' @param feature_matrix The basis for prediction
#' @param probabilities {-1, 0, 1} {Return probabilities for all classes / only labels / only the top class}
#' @param batches In how many batches to split the dataset. This uses multithreading.
#' @return A data frame with predictions
#' @export
predictMC = function(model, feature_matrix, probabilities = -1, batches = 1 ){

if (class(model) != "MaxEntModel") stop("Model is not a MaxEntModel class!")
modelS4 = new("maxent",model = model$model,weights = model$weights)

if (batches > 1) {
  breakpoints = chunk2(seq(1:nrow(feature_matrix)),batches)
  chunks  = lapply(X = breakpoints, function(x) as.matrix(feature_matrix[x,]))
  pred = do.call(rbind,parallel::mclapply(FUN = function(x) data.table::data.table(maxent::predict.maxent(modelS4, x)), X = chunks,mc.cores = batches))
}
else{
  pred = data.table::data.table(maxent::predict.maxent(modelS4, feature_matrix))
}


if (probabilities == -1) {
  return(data.table::data.table(pred))
}
else if (probabilities == 1) {
  maxent_pred = pred[,1]
  maxent_prob = apply(pred[,-1], FUN = function(x) x[which.max(x)], MARGIN = 1)
  return(data.table::data.table(maxent_pred,probability = as.vector(maxent_prob,mode = "numeric")))
}
else if (probabilities == 0) {
  return(data.table::data.table(labels = pred[,labels]))
}
else{
  stop("The model does not have that many categories!")
}

}
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
