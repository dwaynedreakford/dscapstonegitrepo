library(tm)
library(tokenizers)
library(filehash)
library(dplyr)
library(ggplot2)

projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
dataDir <- paste0(projDir, "data/")
useDB = TRUE
pCorpDBNm = "mCorpus.db"

# mLang: *one* of "en_US", "de_DE", "fi_FI", "ru_RU"
# srcMedium: any combination of c('blogs', 'news', 'twitter')
#
corpusFromFiles <- function(workDir = dataDir,
                         mLang = "en_US",
                         srcMedium = c("blogs", "news", "twitter")) {

    tgtDir <- paste0(workDir, "final/", mLang, "/")
    fPtrn = NULL
    if ( length(srcMedium) > 0 ) {
        filterBase <- paste(srcMedium, collapse = "|")
        fPtrn <- paste0("(", filterBase, ")", "\\.txt$")
    }
    setwd(workDir)
    mCorpus <- PCorpus(DirSource(tgtDir, pattern = fPtrn),
                       dbControl = list(dbName=pCorpDBNm, dbType="DB1"))
    mCorpus <- preprocMCorpus(mCorpus)
    mCorpus
}

vCorpusFromDF <- function(sourceDF,
                           srcMedium = c("blogs", "news", "twitter")) {
    sourceDF <- filter(sourceDF, msource %in% srcMedium)
    mCorpus <- VCorpus(VectorSource(sourceDF$mdata))
}

pCorpusFromDF <- function(sourceDF,
                         srcMedium = c("blogs", "news", "twitter")) {
    sourceDF <- filter(sourceDF, msource %in% srcMedium)
    setwd(dataDir)
    mCorpus <- PCorpus(VectorSource(sourceDF$mdata),
                       dbControl = list(dbName=pCorpDBNm, dbType="DB1"))
}

preprocMCorpus <- function(mCorpus) {

    # TODO
    # - Sep hyphenated and slash words (must preceed 'removePunctuation')
    # - Add EOS markers (must preceed 'removePunctuation')
    #

    # Punctuation, numbers, case...
    mCorpus <- tm_map(mCorpus, removePunctuation)
    mCorpus <- tm_map(mCorpus, removeNumbers)
    mCorpus <- tm_map(mCorpus, content_transformer(tolower))

    # Stopwords
    # n-grams containing stopwords actually improve "predict next word"
    # cability, so don't remove them here.
    # mCorpus <- tm_map(mCorpus, removeWords, tm::stopwords())

    # Whitespace
    mCorpus <- tm_map(mCorpus, stripWhitespace)

    # Other cleanup (as we learn more about the training data)...
    mCorpus
}

# Create a corpus from a sampling of the media data.
#
# TODO: Use a variant of this to separate and load training, validation
# and test data sets / corpora.
#
sampleMediaCorpus <- function(sampleSize = 100, linesToSkip = 0,
                              mediaSource = c("blogs", "news", "twitter"),
                              usDB = FALSE) {
    mList <- sampleProjData(nLines = sampleSize, skipLines = linesToSkip,
                            srcMedium = mediaSource)
    mDF <- mListToDF(mList)
    if ( usDB == TRUE )
        mCorpus <- pCorpusFromDF(mDF)
    else
        mCorpus <- vCorpusFromDF(mDF)
    
    mCorpus <- preprocMCorpus(mCorpus)
    mCorpus
}

# Create a corpus from all of the available media data.
#
fullMediaCorpus <- function(mediaSource = c("blogs", "news", "twitter")) {

    mList <- loadProjData(srcMedium = mediaSource)
    mDF <- mListToDF(mList)
    mCorpus <- pCorpusFromDF(mDF)
    mCorpus <- preprocMCorpus(mCorpus)

    mCorpus
}

dtmFromMCorpus <- function(mCorpus, n=3) {
    mTokenizer <- ngramTokenizer(n)
    DocumentTermMatrix(mCorpus, control = list(tokenize=mTokenizer, language="en"))
}

# Create a compatible n-gram tokenizer. Base the approach on that shared
# in the tm FAQ, but use tokenizers::tokenize_ngrams because, for me, it's
# simpler.
#
# From the tm FAQ (http://tm.r-forge.r-project.org/faq.html#Bigrams):
# BigramTokenizer <-
#     function(x)
#         unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#
# n - document from a Corpus
#
ngramTokenizer <- function(n) {
    function(x) tokenize_ngrams(content(x), TRUE, n, simplify = TRUE)
}

# Returns a list where the elements are the model results of various
# quick and dirty analyses, e.g. most/least freqent words,...
#
exploreDTM <- function(dtm) {
    resultList = list()

    # Most and least frequent n-grams
    wFreq <- colSums(as.matrix(dtm))
    resultList[["mostfreq"]] <- head(wFreq[order(wFreq, decreasing = TRUE)], 25)
    resultList[["leastfreq"]] <- tail(wFreq[order(wFreq, decreasing = TRUE)], 25)
    resultList[["freqdist"]] <- wFreq[order(wFreq, decreasing = TRUE)]
    
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
frequencyList <- function(sampleSize = 1000, linesToSkip = 0, mediaSource = "blogs", n) {
    mCorpus <- sampleMediaCorpus(sampleSize, linesToSkip, mediaSource)
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



















