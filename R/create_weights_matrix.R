#' Create a weights matrix given a MaxEnt model
#' Given a matrix with labels and probabilities
#' @param model A MaxEnt model
#' @param matrix A matrix
#' @return A weight matrix data table
#' @import data.table
#' @export
create_weights_matrix = function(matrix, model) {
  if (class(model) != "MaxEntModel" | class(matrix) != "dfm") stop("Model is not a MaxEntModel class or matrix is not dfm!")
  weights = data.table(model$weights)
  weights[,c("Label", "Weight", "Feature") := list(as.character(Label),as.numeric(as.character(Weight)),as.character(Feature))]


  corres = data.frame(list(
    Name = matrix@Dimnames$features, Feature = as.character(seq(1:length(
      matrix@Dimnames$features
    )))
  ))
  t = merge(weights, corres, by = "Feature", all.x = TRUE)
  t = as.data.frame.matrix(stats::xtabs(data = t, Weight ~ Name + Label))
  colnames(t) %<>% trimws(.)
  return(t)
}




