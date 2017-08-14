# source(GetData.R)

library(tm)
library(tokenizers)
library(filehash)
library(slam)
library(dplyr)
library(ggplot2)

#
# Data location and loading is performed via functions in GetData.R.
# A typical flow would be:
#
# 1. Locate and load data from text files into memory (see GetData.R).
# 2. Create a corpus from the loading data. I recommend the functions,
#    sampleMediaCorpus() and fullMediaCorpus() over corpusFromFiles().
#    Preprocessing of the data is performed during this step, via
#    preProcMCorpus().
# 3. Create a document-term matrix, via dtmFromMCorpus().
# 4. Perform further analysis (e.g., via exploreDTM() and related).
#

mCorpDBNm = "mCorpus"

# The file, GetData.R, should first be sourced to make the
# data locating and loading routines accessible.
#
# srcMedium: any combination of c('blogs', 'news', 'twitter')
#
# partition: specifies that data should be loaded
# from the training, validation or test data partition. The valid
# values are "train", "val" or "test", respectively.
#
corpusFromFiles <- function(
                        srcMedium = c("blogs", "news", "twitter"),
                        partition = "train") {

    srcDir <- getPartitionDir(partition)
    print(paste0("Data partition directory: ", srcDir))
    setwd(srcDir)
    fPtrn = NULL
    if ( length(srcMedium) > 0 ) {
        filterBase <- paste(srcMedium, collapse = "|")
        fPtrn <- paste0("(", filterBase, ")", ".*\\.txt$")
        print(paste0("Source file filter: ", fPtrn))
    }
    corpDBNm <- paste0(mCorpDBNm, "_", partition, ".db")
    print(paste0("Perm corpus db name: ", corpDBNm))
    
    mCorpus <- PCorpus(DirSource(srcDir, pattern = fPtrn),
                       dbControl = list(dbName=corpDBNm, dbType="DB1"))
    mCorpus <- preprocMCorpus(mCorpus)
    mCorpus
}

vCorpusFromDF <- function(sourceDF,
                           srcMedium = c("blogs", "news", "twitter")) {
    sourceDF <- filter(sourceDF, msource %in% srcMedium)
    mCorpus <- VCorpus(VectorSource(sourceDF$mdata))
}

pCorpusFromDF <- function(sourceDF,
                         srcMedium = c("blogs", "news", "twitter"),
                         partition = "train") {

    dbDir <- getPartitionDir(partition)
    print(paste0("Perm corpus directory: ", dbDir))
    setwd(dbDir)
    corpDBNm <- paste0(mCorpDBNm, "_", partition, ".db")
    print(paste0("Perm corpus name: ", corpDBNm))
    
    sourceDF <- filter(sourceDF, msource %in% srcMedium)
    mCorpus <- PCorpus(VectorSource(sourceDF$mdata),
                       dbControl = list(dbName=corpDBNm, dbType="DB1"))
}

# Summary of preprocessing performed here:
# - None at present -
#
preprocMCorpus <- function(mCorpus) {

    # mCorpus <- tm_map(mCorpus, removeNumbers)
    
    # Punctuation, numbers, case...
    # mCorpus <- tm_map(mCorpus, removePunctuation)
    # mCorpus <- tm_map(mCorpus, content_transformer(tolower))

    # Bad words
    # n-grams containing many of the customary stopwords actually 
    # improve "predict next word" cability, so take care to remove
    # only those "bad" words that we don't want to predict.
    #
    # mCorpus <- tm_map(mCorpus, removeWords, tm::stopwords())

    # Whitespace
    # mCorpus <- tm_map(mCorpus, stripWhitespace)

    # Other cleanup (as we learn more about the training data)...
    mCorpus
}

# Create a compatible n-gram tokenizer. Base the approach on that shared
# in the tm FAQ, but use tokenizers::tokenize_ngrams because, for me, it's
# simpler.
#
# This approach is motivated by (http://tm.r-forge.r-project.org/faq.html#Bigrams):
# BigramTokenizer <-
#     function(x)
#         unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#
# `ngramTokenizer` accepts a single Document from a Corpus and
# returns a function that accepts the "n" in "n-gram". The anonymous
# function does the following:
# - Tokenize the document into sentences, converting to lowercase,
#   and removing: non alphanumeric characters, punctuation.
#   from each sentence and converts all characters to lowercase.
# - Tokenize each sentence into n-grams.
# - Return a named vector of n-grams.
#
# n - number of words in each gram (i.e., the "n" in "n-gram")
# x - a Document from a Corpus. This will be passed to the
#     anonymous function by `tm_map`
#
ngramTokenizer <- function(n) {
    function(x) {
        sentences <- tokenize_sentences(content(x), 
                                        lowercase = TRUE, 
                                        strip_punctuation = TRUE)
        ngs <- tokenize_ngrams(unlist(sentences), FALSE, n, stopwords = getBadWords())
        ngChar <- unlist(ngs)
        ngChar
    }
}

getNGrams <- function(inChar, n) {
    sentences <- tokenize_sentences(inChar, 
                                    lowercase = TRUE, 
                                    strip_punctuation = TRUE)
    ngs <- tokenize_ngrams(unlist(sentences), FALSE, n, stopwords = getBadWords())
    ngChar <- unlist(ngs)
    ngChar
}

# Create a corpus from a sampling of the media data.
#
sampleMediaCorpus <- function(sampleSize = 100, linesToSkip = 0,
                              mediaSource = c("blogs", "news", "twitter"),
                              dataPartition = "train",
                              useDB = FALSE) {
    mList <- sampleProjData(nLines = sampleSize, skipLines = linesToSkip,
                            srcMedium = mediaSource, partition = dataPartition)
    mDF <- mListToDF(mList)
    if ( useDB == TRUE )
        mCorpus <- pCorpusFromDF(mDF)
    else
        mCorpus <- vCorpusFromDF(mDF)
    
    mCorpus <- preprocMCorpus(mCorpus)
    mCorpus
}

# Create a corpus from all of the available media data.
#
fullMediaCorpus <- function(mediaSource = c("blogs", "news", "twitter"), 
                            dataPartition = "train",
                            useDB = TRUE) {

    mList <- loadProjData(srcMedium = mediaSource, partition = dataPartition)
    mDF <- mListToDF(mList)
    if ( useDB == TRUE )
        mCorpus <- pCorpusFromDF(mDF)
    else
        mCorpus <- vCorpusFromDF(mDF)

    mCorpus <- preprocMCorpus(mCorpus)
    mCorpus
}

dtmFromMCorpus <- function(mCorpus, n=3) {
    # Lowercase transformation is done during sentence tokenization.
    # Remove numbers here, and take all word lengths (e.g. "a" should be
    # included).
    DocumentTermMatrix(mCorpus, control = list(tokenize=ngramTokenizer(n), 
                                               language="en",
                                               tolower=FALSE,
                                               removeNumbers=TRUE,
                                               wordLengths=c(1, Inf)))
}

# 
# This now uses `slam` algorithms to get summary stats on the DTM.
# Turns out using things like "wFreq <- colSums(as.matrix(dtm))" was
# crushing memory and crashing my laptop.
#
# Thank you, StackOverflow: https://stackoverflow.com/questions/21921422/row-sum-for-large-term-document-matrix-simple-triplet-matrix-tm-package
#
# Returns a list where the elements are the model results of various
# quick and dirty analyses, e.g. most/least freqent words,...
#
exploreDTM <- function(dtm, freqCutoff=2) {
    resultList = list()

    # Most frequent n-grams (No longer does the least. Instead, will
    # Just prune those later.)
    termSum <- col_sums(dtm)
    resultList[["mostfreq"]] <- head(termSum[order(termSum, decreasing = TRUE)], 25)
    resultList[["leastfreq"]] <- head(termSum[order(termSum)], 25)
    resultList[["freqdist"]] <- termSum[order(termSum, decreasing = TRUE)]
    pruneProspects <- termSum[termSum < freqCutoff]
    resultList[["pruneprospects"]] <- pruneProspects[order(pruneProspects)]
    
    resultList
}

# Create a list of elements used to facilitate n-gram frequency evaluation.
# (via plots, etc).
#
# Returns a resultList with the following elements:
#
# - resultList[["mostfreq"]] - The 50 most frequently occuring n-grams.
# This is a named numeric vector, where the names are the n-gram
# contents, and the elements are the frequency/count of the n-gram's 
# appearance in the data.
#
# - resultList[["leastfreq"]] - The 50 least frequently occuring n-grams.
# This is a named numeric vector, where the names are the n-gram
# contents, and the elements are the frequency/count of the n-gram's 
# appearance in the data.
# 
#
frequencyList <- function(sampleSize = 1000, linesToSkip = 0, 
                          mediaSource = "blogs", dataPartition = "train",
                          n) {
    mCorpus <- sampleMediaCorpus(sampleSize, linesToSkip, 
                                 mediaSource, dataPartition, FALSE)
    dtm <- dtmFromMCorpus(mCorpus, n)
    freqList <- exploreDTM(dtm)
    freqList
}

#
# Run the exploratory analysis. This function is here to document the
# series of steps.
#
runExploratoryAnalysis <- function() {
    # Trigrams
    triFreqList <- frequencyList(sampleSize = 5000, mediaSource = "blogs", n=3)
    freqPlot(triFreqList)
    # Bigrams
    biFreqList <- frequencyList(sampleSize = 5000, mediaSource = "blogs", n=2)
    freqPlot(biFreqList)
    # Unigrams
    uniFreqList <- frequencyList(sampleSize = 5000, mediaSource = "blogs", n=1)
    freqPlot(uniFreqList)
}

distPlot <- function(dtmAnalysis, whichGram) {
    plot(x=seq(1,length(dtmAnalysis$freqdist)), y=dtmAnalysis$freqdist, 
         type="l", xlab="", ylab="", main=whichGram)
}

freqPlot <- function(dtmAnalysis) {
    mostFreq <- data.frame("ngram"=factor(names(dtmAnalysis$mostfreq), names(dtmAnalysis$mostfreq)),
                        "Frequency"=dtmAnalysis$mostfreq,
                        "which"=rep("Most", length(dtmAnalysis$mostfreq)))
    leastFreq <- data.frame("ngram"=factor(names(dtmAnalysis$leastfreq), names(dtmAnalysis$leastfreq)),
                           "Frequency"=dtmAnalysis$leastfreq,
                           "which"=rep("Least", length(dtmAnalysis$leastfreq)))
    
    mlFreq <- rbind(mostFreq, leastFreq)
    
    g <- ggplot(mlFreq, aes(x=ngram, y=Frequency)) + geom_col() +
        theme(axis.text.x = element_text(angle = 90))
    g
}



















