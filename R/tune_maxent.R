#' Tunes a MaxEnt classifier)
#' @param feature_matrix A matrix with features
#' @param feature_labels The labels for the matrix
#' @param nfold Number of folds for k-fold crossvalidation (k = 5 for basic run, k = 10 is accurate)
#' @param showall Show all of the configurations
#' @param verbose Output extra information
#' @param cores How many cores to use
#' @return A matrix with the best parameters found
#' @export
#' @import maxent
tune_MaxEnt = function(feature_matrix, feature_labels, nfold = 3, showall = TRUE,
          verbose = TRUE, cores = 1,l2_params=c(0,0.5,1,5,10),sgd_params=c(TRUE,rep(FALSE,4)))
{
  if (nfold < 2) stop("A minimum of two folds is required")
  recall_accuracy <- function(true_labels, predicted_labels) {
    true_labels <- as.vector(true_labels)
    predicted_labels <- as.vector(predicted_labels, mode = class(true_labels))
    analyze <- predicted_labels == true_labels
    accuracy <- length(analyze[analyze == TRUE])/length(true_labels)
    return(accuracy)
  }
  writeLines("Creating internal feature matrix")
  feature_matrix <- as(Matrix::sparseMatrix(i = feature_matrix@i, p = feature_matrix@p, x = feature_matrix@x,index1 = FALSE), "matrix.csr")
  if (verbose == TRUE)
    writeLines(paste0("Testing ",length(l2_params)," parameter configurations..."))
 # l1_params <- c(0,1,0.2,0,1,0.2,0,0)


  rand <- sample(nfold, dim(feature_matrix)[1], replace = TRUE)
  fit_accuracy <- c()
  writeLines("Entering loop")
  cl = parallel::makeCluster(cores)
  for (n in 1:length(l2_params)) {
    cv_accuracy <- c()
    ##############
    i = sort(unique(rand))
    cv_accuracy = unlist(parallel::parLapply(cl=cl, X = i , fun = function(i,feature_matrix,rand,feature_labels,l2_params,sgd_params){
   # for (i in sort(unique(rand))) {
      model <- maxent(feature_matrix[rand != i, ], feature_labels[rand !=
                                                                 i],  l2_regularizer = l2_params[n],
                      use_sgd = sgd_params[n],
                      verbose = FALSE)
      pred <- predict(model, feature_matrix[rand == i,
                                            ])
      pred <- pred[, 1]
      recall_accuracy(feature_labels[rand ==i], pred)

    },feature_matrix=feature_matrix,rand=rand,feature_labels=feature_labels,l2_params=l2_params,sgd_params=sgd_params))
    ############
    if (verbose == TRUE)
      writeLines(paste("Configuration: ", n, " Accuracy (", nfold,
                "-fold cross-validation): ", mean(cv_accuracy), sep = ""))
    fit_accuracy[n] <- mean(cv_accuracy)
  }
  parallel::stopCluster(cl)
  names <- c( "l2_regularizer", "use_sgd",
              "accuracy", "pct_best_fit")
  if (showall == FALSE) {
    optimal_fit <- which.max(fit_accuracy)
    values <- c(l2_params[optimal_fit],
                sgd_params[optimal_fit],
                max(fit_accuracy), 100)
    optimal <- rbind(values, deparse.level = 0)
    colnames(optimal) <- names
  }
  else {
    optimal <- rbind()
    best <- max(fit_accuracy)
    for (optimal_fit in 1:length(l2_params)) {
      values <- c( l2_params[optimal_fit],
                  sgd_params[optimal_fit],
                  fit_accuracy[optimal_fit], fit_accuracy[optimal_fit]/best)
      optimal <- rbind(optimal, values, deparse.level = 0)
    }
    colnames(optimal) <- names
  }
  return(optimal)
}

