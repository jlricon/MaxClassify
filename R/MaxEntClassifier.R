#' Train a classifier
#' Given a matrix with word counts and a vector of labels, train a MaxEnt classifier
#' @param train_matrix A matrix containing word counts
#' @param train_labels A matrix containing the labels for the training matrix
#' @return A maxent model
#' @export
trainMC = function(feature_matrix,code_vector, l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE ){
UseMethod("trainMC",feature_matrix)
}
#' @export
trainMC.matrix = function(feature_matrix,code_vector, l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE ){
  model   = maxent::maxent(feature_matrix,code_vector,
                           l1_regularizer = l1_regularizer,
                           l2_regularizer = l2_regularizer,
                           use_sgd        = use_sgd,
                           set_heldout    = set_heldout,
                           verbose        = verbose )
  returns = list()
  returns$model = model@model
  returns$weights = model@weights
  attr(returns, "class") = "MaxEntModel"
  return(returns)
}
#' @export
trainMC.dfm = function(feature_matrix, l1_regularizer = 0.0, l2_regularizer = 0.0, use_sgd = FALSE, set_heldout = 0, verbose = FALSE ){
  model   = maxent::maxent(as(Matrix::sparseMatrix(i = feature_matrix@i, p = feature_matrix@p, x = feature_matrix@x,index1 = FALSE), "matrix.csr"),
                           feature_matrix@docvars$labels,
                           l1_regularizer = l1_regularizer,
                           l2_regularizer = l2_regularizer,
                           use_sgd        = use_sgd,
                           set_heldout    = set_heldout,
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
  #First and foremost, check if the model is empty. If it is empty, it is because there is only one label. Return that
  if (!(probabilities %in% c(-1,0,1))) stop("Probabilities must be in [-1,0,1]")
  if (model$model == "") {

    class = as.character(model$weights$Label[1])  %>% trimws()
    label = rep(class,feature_matrix@Dim[1])
    probability = rep(1,feature_matrix@Dim[1])
    if (probabilities %in% c(-1,1)) {
      #Return probability of class N
      return(data.table(labels = label,prob = probability) %>%  setnames(c("prob"),c(class)))
    }
    else{
      return(data.table(labels = label))
    }
  }
  if (batches == "auto") {
    if (feature_matrix@Dim[1] > 2000) {
      batches = parallel::detectCores()
    }
    else if (feature_matrix@Dim[1] > 1000) {
      batches = 2
    }
    else batches = 1

  }
  if ( batches > 1) cl = parallel::makeCluster(batches)
feature_matrix = as(sparseMatrix(i = feature_matrix@i, p = feature_matrix@p, x = feature_matrix@x,index1 = FALSE), "matrix.csr")
if (class(model) != "MaxEntModel") stop("Model is not a MaxEntModel class!")
modelS4 = new("maxent",model = model$model,weights = model$weights)

if (batches > 1) {
  breakpoints = chunk2(seq(1:nrow(feature_matrix)),batches)
  chunks  = lapply(X = breakpoints, function(x) feature_matrix[x,])
  pred = do.call(rbind,parallel::parLapply(function(x) data.table::data.table(maxent::predict.maxent(modelS4, x)), X = chunks,cl = cl))
}
else{

  pred = data.table::data.table(maxent::predict.maxent(modelS4, feature_matrix))
}

if (batches > 1) parallel::stopCluster(cl)
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
#Splits vector into n cuts
chunk2 = function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))

#' Scores the performance of a model
#' @param predicted Labels that were predicted
#' @param real Actual labels
#' @return Accuracy
#' @export
score = function(predicted, real){
  if(length(predicted)!=length(real)) stop("Label vectors are not the same length")
  sum(predicted==real)/length(real)
}
