# Choose the most common words for each class
#' Returns common words for a matrix
#' @param text A vector of strings to be made into a matrix
#' @param ngrams Vector with the ngrams required
#' @param minDocFreq Minimum document frequency allowed (fraction)
#' @param batches How many batches to use for parallel computation
#' @param verbose Outputs some info
#' @return A document feature matrix
#' @export
get_common_words = function(text,labels,ngrams = 1, minDocFreq = 0.01, batches = 1, verbose = FALSE){

  t = split(text,labels)

  batches = if (batches == "auto") if (length(text) < 1000) 1 else parallel::detectCores() else batches


  if (batches == 1) {
    trainingmats = lapply(t,get_common_words_single, ngrams = ngrams,minDocFreq = minDocFreq, verbose = verbose)
  }
  else{
    cluster = parallel::makeCluster(batches)
    trainingmats = parallel::parLapply(cl = cluster, X = t, fun = get_common_words_single,ngrams = ngrams,
                                       minDocFreq = minDocFreq,verbose = verbose )
    parallel::stopCluster(cluster)
    }

  unique(unlist(trainingmats))
}

#' Returns common words for a matrix
#' @param text A vector of strings to be made into a matrix
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
get_common_words_single = function(text, minDocFreq=0, maxDocFreq = 1, minWordLength = 0, maxWordLength = Inf,
                                  weighting = "count", ngrams = 1, minCount = 1, verbose = FALSE)
{
  matrix = quanteda::dfm(quanteda::corpus(text),ngrams = ngrams, tolower = FALSE) %>%
    quanteda::dfm_select(selection = "keep", min_nchar = minWordLength, max_nchar = maxWordLength,verbose = verbose ) %>%
    quanteda::dfm_trim(.,min_docfreq = minDocFreq, max_docfreq = maxDocFreq,verbose = verbose,docfreq_type = "prop") %>% quanteda::dfm_weight(weighting)
  matrix@settings$weighting = weighting
  matrix = matrix[,sort(colnames(matrix))]

  return(matrix@Dimnames$features)
}


