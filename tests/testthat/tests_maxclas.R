
library(magrittr)
library(data.table)
library(testthat)
library(MaxClassify)
context("MaxClassify")


test_that("We can get words that appear in every sentence",{
  expect_equal(get_common_words_single(c("this is a test test","this also happens to be a test","tests are this"),minDocFreq = 1),
               c("this"))
})

test_that("We can get words that appear in half or more of the sentences",{

  expect_equal(get_common_words_single(c("bla bla","ble bla","test bla","test a"),minDocFreq = 0.5),
               c("bla","test"))
})

txt = c("bla bla","ble bla","test bla","test a",
        "bla2 bla2","ble bla2","test2 bla2","test2 a")
txt_label = c("a","a","a","a","b","b","b","b")

test_that("We can get words that appear in half or more of sentences, for different classes",{
  expect_equal(get_common_words(txt,txt_label,minDocFreq = 0.5),c("bla","test","bla2","test2"))


})

# Now test that the whole pipeline works
train_txt_fruit = c("this text is about bananas","bananas are a kind of yellow fruit","mango is clearly the best",
                    "best ones actually are lumquats")
train_txt_doggos = c("labradors are nice dogs","labradors are kind like golden retrievers","best doggos are shibes",
                     "these dogs have yellow fur")

test_txt_fruit = c("best yellow fruits are nice mango","bananas have yellow color")
test_txt_doggos = c("dogs are clearly the best","this text is about dogs")

train = c(train_txt_fruit,train_txt_doggos)
labels = c(rep("fruit", 4),rep("doggo", 4))
test  = c(test_txt_fruit,test_txt_doggos)

# First text ngram generation

train_matrix = MaxClassify::create_training_matrix(train,labels,minDocFreq = 0.25)

test_that("Train matrix has the right words",{
  expect_equal(train_matrix@Dimnames$features,c("are","bananas","best","dogs","is","kind","labradors","yellow"))
})
test_matrix = MaxClassify::create_test_matrix(test, train_matrix, ngrams = 1,verbose = TRUE)


test_that("Test matrix has the right words",{
  expect_equal(test_matrix@Dimnames$features,c("are","bananas","best","dogs","is","kind","labradors","yellow"))
})

train_matrix = MaxClassify::create_training_matrix(train,labels,ngrams = c(1),minDocFreq = 0.25)
test_matrix = MaxClassify::create_test_matrix(test, train_matrix, ngrams = c(1),verbose = TRUE)



test_that("Test matrix has the right words",{
  expect_equal(test_matrix@Dimnames$features,c("are","bananas","best","dogs","is","kind","labradors","yellow"))
})

train_matrix = MaxClassify::create_training_matrix(train,labels,ngrams = c(1,2))
test_matrix = MaxClassify::create_test_matrix(test, train_matrix, ngrams = c(1,2),verbose = TRUE)

model = MaxClassify::trainMC(train_matrix)
predicted = MaxClassify::predictMC(model,test_matrix)

test_that("Predictions are correct",{
  expect_equal(predicted$labels,c("doggo","doggo","fruit","fruit"))
})

