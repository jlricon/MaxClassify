#' Create a weights matrix given a MaxEnt model
#' Given a matrix with labels and probabilities
#' @param model A MaxEnt model
#' @param matrix A matrix
#' @return A weight matrix data table
#' @import data.table
#' @export
create_weights_matrix = function(matrix, model) {
  if (class(model) != "MaxEntModel" | class(matrix) != "dfmSparse") stop("Model is not a MaxEntModel class or matrix is not dfm!")
  weights = data.table(model$weights) %>% dplyr::mutate(
      Label = as.character(Label),
      Weight = as.numeric.factor(Weight),
      Feature = as.character(Feature)
    )

  corres = data.frame(list(
    Name = matrix@Dimnames$features, Feature = as.character(seq(1:length(
      matrix@Dimnames$features
    )))
  ))
  t = merge(weights, corres, by = "Feature", all.x = TRUE)
  return(as.data.frame.matrix(xtabs(data = t, Weight ~ Name + Label)))
}

#' Create a matrix from a vector of text. This function does not do any text cleaning.
#' @param Text A vector of strings to be made into a matrix
#' @param labels Labels for classification
#' @param minDocFreq Minimum frequency of a word to be included (count or fraction)
#' @param maxDocFreq Maxmimum frequency of a word to be included (count or fraction)
#' @param minWordLength Minimum wordlength for a word to be included
#' @param maxWordLength Maximum wordlength for a word to be included
#' @param weighting Weighting to use. Choose between frequency, relFreq, relMaxFreq, logFreq, tfidf (See dfm_weight)
#' @param ngrams Vector that specifies type of ngrams to generate
#' @import magrittr
#' @import quanteda
#' @return A document feature matrix
#' @export
create_training_matrix = function(Text,labels, minDocFreq=1, maxDocFreq=Inf, minWordLength=0, maxWordLength=Inf,
                                    weighting=  "frequency", ngrams=1)
{
  matrix = dfm(corpus(Text,docvars = data.frame(labels = labels)),ngrams = ngrams, tolower = FALSE) %>%
    dfm_select(selection = "keep",min_nchar = minWordLength, max_nchar = maxWordLength ) %>%
    dfm_trim(min_docfreq = minDocFreq, max_docfreq = maxDocFreq) %>% dfm_weight(weighting)
  matrix@settings$weighting = weighting
  matrix = matrix[,sort(colnames(matrix))]
  gc()
  return(matrix)
}

#' Transforms a vector of text to the same format of a given training matrix (Retain same words)
#' @param training_matrix The matrix that was used to train the model
#' @param Text Text to be adapted
#' @param ngrams Vector that specifies type of ngrams to generate
#' @return An adapted matrix
#' @import quanteda
#' @import magrittr
#' @export
create_test_matrix = function(Text, training_matrix, ngrams = 1){
  matrix = dfm(corpus(Text),ngrams = ngrams, tolower = FALSE) %>% dfm_select(features = colnames(orig_mat),padding = TRUE, valuetype = "fixed") %>%
    dfm_weight(training_matrix@settings$weighting)
  gc()
  return(matrix[,sort(colnames(matrix))])

}


