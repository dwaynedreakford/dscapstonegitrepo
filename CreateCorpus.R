library(tm)
library(tokenizers)
library(filehash)
library(dplyr)
library(ggplot2)

projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
dataDir <- paste0(projDir, "data/")
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
sampleMediaCorpus <- function(sampleSize = 100, linesToSkip = 0,
                              mediaSource = c("blogs", "news", "twitter")) {
    mList <- sampleProjData(nLines = sampleSize, skipLines = linesToSkip,
                            srcMedium = mediaSource)
    mDF <- mListToDF(mList)
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
# Some observations:
# - Most of the most frequent words have no associations >corlimit=0.5
# - Significant portion of least frequent words have associations >corlimit=0.5
# - Probably worthwhile to run an algorithm to find most frequently associated words.
#
exploreDTM <- function(dtm) {
    resultList = list()

    # Most and least frequent words
    wFreq <- colSums(as.matrix(dtm))
    resultList[["mostfreq"]] <- head(wFreq[order(wFreq, decreasing = TRUE)], 50)
    resultList[["leastfreq"]] <- tail(wFreq[order(wFreq, decreasing = TRUE)], 50)

    # Most frequent associated words
    # resultList[["mostfreqassoc"]] <- list()
    # for (w in names(wFreq[order(wFreq, decreasing = TRUE)])) {
    #     assocs <- findAssocs(dtm, w, 0.5)[[1]]
    #     if (length(assocs) > 0) {
    #         resultList[["mostfreqassoc"]][[w]] <- assocs
    #         if ( length(resultList[["mostfreqassoc"]]) == 50 ) break
    #     }
    # }
    # Least frequent associated words
    # resultList[["leastfreqassoc"]] <- list()
    # for (w in names(wFreq[order(wFreq)])) {
    #     assocs <- findAssocs(dtm, w, 0.5)[[1]]
    #     if (length(assocs) > 0) {
    #         resultList[["leastfreqassoc"]][[w]] <- assocs
    #         if ( length(resultList[["leastfreqassoc"]]) == 50 ) break
    #     }
    # }

    resultList
}

exploratoryPlots <- function(dtmAnalysis) {

    WFreq <- data.frame("word"=factor(names(dtmAnalysis$mostfreq), names(dtmAnalysis$mostfreq)),
                        "freq"=dtmAnalysis$mostfreq)
    g <- ggplot(WFreq, aes(x=word, y=freq)) + geom_col() +
        theme(axis.text.x = element_text(angle = 90))
    g
}

# Create a corpus
# Export a DTM
# Analyze the DTM
runTests <- function() {
    mCorpus <- sampleMediaCorpus(sampleSize = 300)

    dtm2 <- dtmFromMCorpus(mCorpus, n=2)
    dtmAnalysis2 <- exploreDTM(dtm2)

    dtm3 <- dtmFromMCorpus(mCorpus, n=3)
    dtmAnalysis3 <- exploreDTM(dtm3)
}
















