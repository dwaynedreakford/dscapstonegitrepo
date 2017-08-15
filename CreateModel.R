# Create the next-word prediction model for the Data Science Capstone Project
# sponsored by SwiftKey and provided by Johns Hopkins University.

projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
srcDir <- paste0(projDir, "capstonegitrepo/")
source(paste0(srcDir, "GetData.R"))
source(paste0(srcDir, "CreateCorpus.R"))

library(data.table)

# Don't include ngrams that occur less than `freqCutoff` times in the
# training data.
# TODO: Look at frequency of frequencies for input on `freqCutoff`
freqCutoff1 <- 5
freqCutoffN <- 2

# Highest order of the model (and of ngram table to create).
maxN <- 4

# Create and save to disk the ngram tables used for scoring
# during prediction.
#
createNGramTables <- function(sampleSize=1000, mSource=c("blogs", "news", "twitter"), partition) {
    workDir <- getPartitionDir(partition)
    setwd(workDir)
    print(paste(c("Working directory:", workDir), collapse=" "))
    
    print("Creating ngram tables...")
    print(paste(c("maxN:", maxN, "sampleSize:", sampleSize), collapse=" "))
    
    # "n-1" term vector (used to calc scores for the n term vector).
    minus1tc <- integer()
    for ( nVal in 1:maxN ) {
        
        # Create the DTM and n term vector.
        print(paste(c("Creating DTM... n=", nVal), collapse=""))
        mdtm <- dtmFromMCorpus(sampleMediaCorpus(sampleSize=sampleSize, mediaSource = mSource), nVal)
        tc <- col_sums(mdtm)
        print(paste(c("Total ", nVal, "-grams: ", length(tc)), collapse=""))
        rm(mdtm)
        gc()
        
        # Prune terms with counts below the frequency cutoff.
        freqCutoff <- ifelse(nVal==1, freqCutoff1, freqCutoffN)
        tc <- tc[tc > freqCutoff-1]
        print(paste("Frequency cutoff: ", freqCutoff), collapse="")
        print(paste(c("Remaining ", nVal, "-grams: ", length(tc)), collapse=""))
        
        # Create and write the n-gram lookup table.
        # Columns: ngram, score
        ngScore <- numeric(0)
        ngPrefix <- character(0)
        ngNw <- character(0)
        if ( nVal == 1 ) {
            # unigram score
            ngScore <- tc/sum(tc)
        } else {
            # ngram (where n > 1) score
            ngPrefix <- sapply(strsplit(names(tc), " ", fixed=TRUE),
                            function(x) paste0(x[1:nVal-1], collapse=" "))
            ngNw <- sapply(strsplit(names(tc), " ", fixed=TRUE),
                               function(x) x[nVal])
            prfixCt <- minus1tc[ngPrefix]
            ngScore <- tc/prfixCt
        }
        if ( nVal ==1 )
            ngTbl <- data.table(nextword=names(tc), score=ngScore)
        else
            ngTbl <- data.table(prefix=ngPrefix, nextword=ngNw, score=ngScore)
        ngTblFile <- getNgTblFileNm(nVal)
        print(paste(c("Writing: ", ngTblFile), collapse=""))
        setwd(getPartitionDir("train"))
        saveRDS(ngTbl, file = ngTblFile)
        rm(ngTbl)
        gc()
        
        # Remember the "n-1" term vector and remove the n term vector.
        minus1tc <- tc
        rm(tc)
        gc()
    } # for (nVal in 1:maxN)
}

loadNgTables <- function(partition) {
    setwd(getPartitionDir(partition))
    ngTables <- list()
    for ( nVal in 1:maxN ) {
        ngTblFile <- getNgTblFileNm(nVal)
        print(paste0("Loading ngram table from file: ", ngTblFile, collapse = ""))
        ngTbl <- readRDS(ngTblFile)
        ngTables[[nVal]] <- ngTbl
    }
    ngTables
}

getNgTblFileNm <- function(nVal) {
    paste0("ng_", nVal, "_tbl.rds")
}

# Get the last `howMany` words from `inText`.
# Returns a character vector of length 1 containing
# the last three words, separated by " ".
#
lastNWords <- function(howMany, inText) {
    if ( length(inText) != 1 ) stop("Argument `inText` must be a character vector of length 1.")
    if ( howMany > maxN ) stop(paste0("Argument `howMany` cannot be greater than `maxN` (", maxN, ")", collapse=""))
    
    wordTokens <- getNGrams(inText, 1)
    lastwords <- character(0)
    if ( length(wordTokens) >= howMany ) {
        begin <- length(wordTokens)-howMany+1
        end <- length(wordTokens)
        lastwords <- wordTokens[begin:end]
    } else {
        lastwords <- wordTokens
    }
    paste0(lastwords, collapse = " ")
}

# Predict the next word...
#
# - `nOrder` is the order (e.g., 3, 4) of ngram lookup table with 
#    which to start the search for a matching `nOrder-1` word prefix. 
#    For example: 
#    + To predict the third word, the input must be two words, and 
#      we start the search with the trigram lookup table, seeking
#      rows with matching two-word prefix.
#    + To predict the next word given no input, we simply return 
#      the highest ranking unigram scores (from the unigram table).
#
# - `ngPrefix` should be a character vector of length 1 produced by
#   `lastNWords`
#
sbAlpha <- 0.4
nextWordScores <- function(nOrder, ngPrefix, ngTables, alphaPow=1) {
    if ( length(ngTables) < nOrder ) 
        stop(paste0(nOrder, " Ngram tables are needed to predict based on ", nOrder-1, " words."))

    print(paste0("Seeking next-word scores for prefix: ", ngPrefix))
    print(paste0("nOrder=", nOrder))
    
    # Get the lookup table.
    ngTable <- ngTables[[nOrder]]
    sbScores <- data.frame(nextword=character(0), score=numeric(0))
    
    # If nOrder is 1, just rank unigrams
    if ( nOrder == 1 ) {
        sbScores <- head(ngTable[order(-score)], 40)
        if ( nrow(sbScores) > 0 ) {
            sbScores[, "score"] <- sbScores$score*alphaPow
        }
    }
    # Otherwise, seek a match among the prefixes. Continue
    # the search in the `nOrder-1` ngram table if none are found.
    else if ( nOrder > 1 ) {
        sbScores <- ngTable[prefix==ngPrefix]
        if ( nrow(sbScores) > 0 ) {
            sbScores[, "score"] <- sbScores$score*alphaPow
        }
        else {
            minus1gram <- lastNWords(nOrder-2, ngPrefix)
            sba <- sbAlpha * alphaPow
            sbScores <- nextWordScores(nOrder-1, minus1gram, ngTables, sba)
        }
    }
    
    head(sbScores[order(-score)], 40)
}

predictionFlow <- function(prefixLen, testText, ngTables) {
    # To predict based on the last n-1 words, we need the n-gram table.
    # I.e., to predict the third word, we need two words of input
    predInput <- lastNWords(prefixLen, testText)
    sbScores <- nextWordScores(prefixLen+1, predInput, ngTables)
    sbScores[order(-score)]
}









