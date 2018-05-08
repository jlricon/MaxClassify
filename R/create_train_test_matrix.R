#' Create a matrix from a vector of text. This function does not do any text cleaning.
#' @param text A vector of strings to be made into a matrix
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
create_training_matrix = function(text,labels, minDocFreq=1, maxDocFreq=Inf, minWordLength=0, maxWordLength=Inf,
                                  weighting=  "count", ngrams=1, features=NULL, verbose = FALSE)
{
  matrix = quanteda::dfm(quanteda::corpus(text,docvars = data.frame(labels = labels)),ngrams = ngrams, tolower = FALSE) %>%
    quanteda::dfm_select(selection = "keep",min_nchar = minWordLength, max_nchar = maxWordLength ) %>%
    quanteda::dfm_trim(.,min_docfreq = minDocFreq, max_docfreq = maxDocFreq,docfreq_type = "prop") %>% quanteda::dfm_weight(weighting)
  if (!is.null(features)) {
    matrix = quanteda::dfm_select(matrix,features,verbose = verbose)
  }
  matrix@settings$weighting = weighting
  matrix = matrix[,sort(colnames(matrix))]

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
create_test_matrix = function(Text, training_matrix, ngrams = 1, verbose = FALSE){
  matrix = dfm(corpus(Text),ngrams = ngrams, tolower = FALSE) %>% dfm_select(training_matrix,
                                                                             verbose = verbose) %>%
    dfm_weight(training_matrix@settings$weighting)

  return(matrix[,sort(colnames(matrix))])

}
