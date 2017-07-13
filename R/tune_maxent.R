#' Tunes a MaxEnt classifier)
#' @param feature_matrix A matrix with features
#' @param feature_labels The labels for the matrix
#' @param nfold Number of folds for k-fold crossvalidation (k = 5 for basic run, k = 10 is accurate)
#' @param showall Show all of the configurations
#' @param verbose Output extra information
#' @param cores How many cores to use
#' @return A matrix with the best parameters found
tune_MaxEnt = function(feature_matrix, feature_labels, nfold = 3, showall = TRUE,
          verbose = TRUE, cores = 1)
{
  recall_accuracy <- function(true_labels, predicted_labels) {
    true_labels <- as.vector(true_labels)
    predicted_labels <- as.vector(predicted_labels, mode = class(true_labels))
    analyze <- predicted_labels == true_labels
    accuracy <- length(analyze[analyze == TRUE])/length(true_labels)
    return(accuracy)
  }
  feature_matrix <- as.compressed.matrix(feature_matrix)
  if (verbose == TRUE)
    writeLines("Testing 18 parameter configurations...")
  l1_params <- c(seq(0, 1, 0.2), rep(0, length(seq(0, 1, 0.2))),
                 seq(0, 1, 0.2))
  l2_params <- c(rep(0, length(seq(0, 1, 0.2))), seq(0, 1,
                                                     0.2), rep(0, length(seq(0, 1, 0.2))))
  sgd_params <- c(rep(FALSE, 12), rep(TRUE, 6))
  set_heldout_params <- c(rep(0, 6), round(dim(feature_matrix)[1]/(nfold *
                                                                     2)), rep(0, 5), rep(0, 6))
  rand <- sample(nfold, dim(feature_matrix)[1], replace = TRUE)
  fit_accuracy <- c()
  for (n in 1:length(l1_params)) {
    cv_accuracy <- c()
    ##############
    i = sort(unique(rand))
    cv_accuracy = unlist(parallel::mclapply(X = i ,mc.cores = cores, FUN = function(i){
   # for (i in sort(unique(rand))) {
      model <- maxent(feature_matrix[rand != i, ], feature_labels[rand !=
                                                                 i], l1_regularizer = l1_params[n], l2_regularizer = l2_params[n],
                      use_sgd = sgd_params[n], set_heldout = set_heldout_params[n],
                      verbose = FALSE)
      pred <- predict(model, feature_matrix[rand == i,
                                            ])
      pred <- pred[, 1]
      recall_accuracy(feature_labels[rand ==i], pred)

    }))
    ############
    if (verbose == TRUE)
      writeLines(paste("Configuration: ", n, " Accuracy (", nfold,
                "-fold cross-validation): ", mean(cv_accuracy), sep = ""))
    fit_accuracy[n] <- mean(cv_accuracy)
  }
  names <- c("l1_regularizer", "l2_regularizer", "use_sgd",
             "set_heldout", "accuracy", "pct_best_fit")
  if (showall == FALSE) {
    optimal_fit <- which.max(fit_accuracy)
    values <- c(l1_params[optimal_fit], l2_params[optimal_fit],
                sgd_params[optimal_fit], set_heldout_params[optimal_fit],
                max(fit_accuracy), 100)
    optimal <- rbind(values, deparse.level = 0)
    colnames(optimal) <- names
  }
  else {
    optimal <- rbind()
    best <- max(fit_accuracy)
    for (optimal_fit in 1:length(l1_params)) {
      values <- c(l1_params[optimal_fit], l2_params[optimal_fit],
                  sgd_params[optimal_fit], set_heldout_params[optimal_fit],
                  fit_accuracy[optimal_fit], fit_accuracy[optimal_fit]/best)
      optimal <- rbind(optimal, values, deparse.level = 0)
    }
    colnames(optimal) <- names
  }
  return(optimal)
}

