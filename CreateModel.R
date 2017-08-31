# Create the next-word prediction model for the Data Science Capstone Project
# sponsored by SwiftKey and provided by Johns Hopkins University.

library(data.table)
library(dplyr)

# projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
# dataDir <- paste0(projDir, "data/")

# Don't include ngrams that occur less than `freqCutoff` times in the
# training data.
# TODO: Look at frequency of frequencies for input on `freqCutoff`
freqCutoff <- 3

# Highest order of the model (and of ngram table to create).
maxN <- 4

# Create and save to disk the ngram tables used for scoring
# during prediction. To include all available data for the 
# specified `mSource`, set `sampleSize=Inf`.
#
createNGramTables <- function(sampleSize=1000, mSource=c("blogs", "news"), partition) {
    workDir <- getPartitionDir(partition)
    setwd(workDir)
    print(paste(c("Working directory:", workDir), collapse=" "))
    
    print("Creating ngram tables...")
    print(paste0(c("maxN: ", maxN), collapse=""))
    print(paste0(c("sampleSize: ", sampleSize), collapse=""))
    startTime <- Sys.time()
    print(paste0("Start time: ", startTime))
    
    # "n-1" term vector (used to calc scores for the n term vector).
    minus1tc <- integer()
    for ( nVal in 1:maxN ) {
        
        # Create the DTM and n term vector.
        print(paste(c("Creating DTM... n=", nVal), collapse=""))
        mdtm <- NULL
        if ( sampleSize==Inf )
            mdtm <- dtmFromMCorpus(
                fullMediaCorpus(mediaSource = mSource, dataPartition = partition, useDB = FALSE),
                nVal)
        else
            mdtm <- dtmFromMCorpus(
                sampleMediaCorpus(sampleSize = sampleSize, mediaSource = mSource, dataPartition = partition),
                nVal)
        
        tc <- col_sums(mdtm)
        print(paste(c("Total ", nVal, "-grams: ", length(tc)), collapse=""))
        rm(mdtm)
        gc()
        
        # Prune terms with counts below the frequency cutoff.
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
    
    endTime <- Sys.time()
    print(paste0("End time: ", endTime))
    difftime(endTime, startTime)
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

loadEvalTables <- function(partition) {
    setwd(getPartitionDir(partition))
    evalTables <- list()
    for ( nVal in 1:maxN ) {
        evalTblFile <- getEvalTblFileNm(nVal)
        # print(paste0("Loading eval table from file: ", evalTblFile, collapse = ""))
        evalTbl <- readRDS(evalTblFile)
        evalTables[[nVal]] <- evalTbl
    }
    evalTables
}

getNgTblFileNm <- function(nVal) {
    paste0("ng_", nVal, "_tbl.rds")
}

getEvalTblFileNm <- function(nVal) {
    paste0("ng_", nVal, "_eval_tbl.rds")
}

# Get the last `howMany` words from `inText`.
# Returns a character vector of length 1 containing
# the last three words, separated by " ".
#
lastNWords <- function(howMany, inText) {
    if ( length(inText) != 1 ) stop("Argument `inText` must be a character vector of length 1.")
    if ( howMany > maxN ) stop(paste0("Argument `howMany` cannot be greater than `maxN` (", maxN, ")", collapse=""))
    
    if ( howMany == 0 )
        return(character(1))
    
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

scoresTable <- function() {
    data.frame(
        prefix=character(0), 
        nextword=character(0), 
        score=numeric(0), 
        stringsAsFactors = FALSE)
}

# Predict the next word...
#
# - `ngLevel` is the order (e.g., 3, 4) of ngram lookup table with 
#    which to start the search for a matching `ngLevel-1` word prefix. 
#    For example: 
#    + To predict the third word, the input must be two words, and 
#      we start the search with the trigram lookup table, seeking
#      rows with matching two-word prefix.
#    + To predict the next word given no input, we simply return 
#      the highest ranking unigram scores (from the unigram table).
#
# - `ngPrefix` is the `ngLevel -1`-word string used to predict the next
#    word. This should be a character vector of length 1 produced by
#   `lastNWords()`.
#
# Returns a scores table (see `scoresTable()`)
#
nextWordScores <- function(ngLevel, ngPrefix, ngTables, numResults, cumAlpha=1) {
    if ( length(ngTables) < ngLevel ) 
        stop(paste0(ngLevel, " Ngram tables are needed to predict based on the most recent ", ngLevel-1, " words."))

    if ( ngLevel > 1 )
        print(paste0("Seeking next-word scores for prefix: ", ngPrefix))
    else
        print(paste0("Ranking unigrams (no prefix)"))
    print(paste0("ngLevel=", ngLevel))
    
    # Get the lookup table and create the scores table.
    ngTable <- ngTables[[ngLevel]]
    sbScores <- scoresTable()
    
    # If ngLevel is 1, just rank unigrams.
    # The bit with `tmpScores` is to create the `prefix` variable/column
    # in the result scoring table (`prefix`) is not present in the 
    # unigram table.
    if ( ngLevel == 1 ) {
        tmpScores <- head(ngTable[order(-score)], numResults)
        if ( nrow(tmpScores) > 0 ) {
            sbScores <- data.table(
                prefix=character(nrow(tmpScores)), 
                nextword=tmpScores$nextword, 
                score=tmpScores$score, 
                stringsAsFactors = FALSE)
            sbScores[, "score"] <- sbScores$score*cumAlpha
            rm(tmpScores)
        }
    }
    # Otherwise, seek a match among the prefixes.
    else if ( ngLevel > 1 ) {
        sbScores <- ngTable[prefix==ngPrefix]
        if ( nrow(sbScores) > 0 ) {
            sbScores[, "score"] <- sbScores$score*cumAlpha
        }
    }
    
    sbScores    
}

# Call the prediction algorithm, given the specified prefix length
# and input text. The input text should contain at least `prefixLen`
# words. This function attempts to return at least `numResults` 
# results.
#
# The returned value is a scoring table, where each row includes:
# - `prefix` if applicable (otherwise NA)
# - `nextword` the prediced next word
# - `score` the score associated with the prediction
#
# The results are evaluated and, if we have less than `numResults` 
# scores and the current ngram order is >= 1, the prediction algorithm 
# is called again, at the next lowest ngram order.
#
predictionFlow <- function(ngLevel, inputText, ngTables, numResults=20) {

    sbScores <- scoresTable()
    sbAlpha <- 1.0
    words <- ""
    if ( ngLevel > 1 )
        words <- getNGrams(inputText, 1)
    while ( ngLevel > 0 ) {
        # Get the predictions
        # To predict based on the last n-1 words, we need the n-gram table.
        # E.g., to predict based on the last 2 words, we need the 3-gram table.
        if ( ngLevel == 1 ) {
            tmpScores <- nextWordScores(ngLevel, "", ngTables, numResults, sbAlpha)
        }
        else {
            prefixLen <- ngLevel-1
            inWords <- words[(length(words)-prefixLen+1):length(words)]
            tmpScores <- nextWordScores(ngLevel, paste0(inWords, collapse=" "), ngTables, numResults, sbAlpha)
        }
        if ( nrow(tmpScores) > 0 ) {
            if ( nrow(sbScores) > 0 ) {
                # Remove scores for `nextword`s already in the scoring table.
                dupPredictions <- tmpScores$nextword %in% sbScores$nextword
                tmpScores <- tmpScores[!(dupPredictions)]
            }
            sbScores <- rbind(sbScores, tmpScores,
                              deparse.level = 0, make.row.names = FALSE, stringsAsFactors = FALSE)
            rm(tmpScores)
        }
        
        if ( nrow(sbScores) >= numResults )
            break

        # If we need to call `nextWordScores` again to get more results, 
        # we need to call it with the next lowest ngram order, and the
        # resulting scores need to be scaled by `sbAlpha` in proportion
        # to how many levels we "back off".
        ngLevel <- ngLevel - 1
        sbAlpha <- sbAlpha * 0.4
    }
    
    head(sbScores[order(sbScores$score, decreasing = TRUE)], numResults)
}

ngTablesSize <- function (ngTables) {
    ngTablesSize <- sum(sapply(ngTables, function(x) object.size(x)))
    print(paste0("Ngram Tables Size: ", format(ngTablesSize/1000000, digits=5), " Mb"))
    for ( idx in 1:length(ngTables) ) {
        print(paste0(idx, "-gram Table Size (M): ", 
                     format(object.size(ngTables[idx]), units="Mb", justify="right")))
    }
}

evalSBOModels <- function(sampleSize=Inf,
                          mediaSource=c("blogs", "news"), partition, profiling=FALSE) {

    saveDir <- getPartitionDir(partition)
    print(paste0("Saving eval tables to: ", saveDir,
                 collapse=""))
    
    # Enable execution and memory profiling
    if ( profiling == TRUE ) {
        profileFile <- paste0(saveDir, "Rprof.out")
        print(paste0("Saving profiling data to: ", profileFile))
        Rprof(filename=profileFile, memory.profiling=TRUE)
    }

    # Load the ngram model tables
    ngTables <- loadNgTables("train")
    print(paste0("Generating eval tables for all available (", length(ngTables), ") model levels...",
                 collapse=""))

    # Evaluate each ngram model's accuracy
    # (and performance characteristics, which should about the same
    # for each).
    for ( ngLevel in length(ngTables):1 ) {
        sboResults <- evalSBOModel(ngLevel, ngTables, sampleSize, mediaSource, partition)
        resultFile <- paste0(saveDir, getEvalTblFileNm(ngLevel), collapse="")
        print(paste0("Writing eval table: ", resultFile, collapse=""))
        saveRDS(sboResults, file = resultFile)
        rm(sboResults)
    }
    
    # Turn off profiling
    if ( profiling == TRUE ) Rprof(filename=NULL)
}

evalSBOModel <- function(ngLevel, ngTables, 
                                sampleSize=1000, mediaSource=c("blogs", "news"), partition) {
    
    mList <- list()
    if ( sampleSize == Inf )
        mList <- loadProjData(dataDir, "en_US", mediaSource, partition)
    else
        mList <- sampleProjData(dataDir, "en_US", mediaSource, partition, sampleSize)
    
    inputLen <- ngLevel-1
    sboResults <- data.table(
        medium = character(0),
        nlevel = integer(0),
        input = character(0),
        nextword = character(0),
        top3 = logical(0),
        top5 = logical(0),
        top15 = logical(0)
    )
    for ( medium in mediaSource ) {
        for ( mLine in mList[[medium]] ) {
            mWords <- getNGrams(mLine, 1)
            if ( length(mWords) < inputLen ) {
                print(paste0("Not enough words (", length(mWords), 
                             ") available for nLevel=", ngLevel, " prediction. Skipping line.",
                             collapse=""))
                next
            }
            # print(paste0("Word range: 1 to ", (length(mWords)-inputLen), collapse=""))
            for ( wordIdx in 1:(length(mWords)-inputLen) ) {
                # print(paste0("wordIdx: ", wordIdx, collapse=""))
                inputWords <- ""
                if ( ngLevel > 1 )
                    inputWords <- paste0(mWords[wordIdx:(wordIdx+inputLen-1)], collapse=" ")
                nextWord <- mWords[wordIdx+inputLen]
                if ( length(nextWord) == 0 ) next
                
                sbScores <- predictionFlow(ngLevel, inputWords, ngTables, 15)
                top3 <- nextWord %in% sbScores$nextword[1:3]
                top5 <- ifelse(top3, TRUE, nextWord %in% sbScores$nextword[4:5])
                top15 <- ifelse(top5, TRUE, nextWord %in% sbScores$nextword[6:15])
                sbResult <- data.table(
                    "medium" = medium,
                    nlevel = ngLevel,
                    input = inputWords,
                    nextword = nextWord,
                    "top3" = top3,
                    "top5" = top5,
                    "top15" = top15
                )
                sboResults <- rbindlist(list(sboResults, sbResult))
                rm(sbResult)
                rm(sbScores)
            }
        }
    }
    
    sboResults
}


# print.data.frame(evalSBOAccuracy(evalTables))
evalSBOAccuracy <- function(sboEvalTables) {
    accuracyList <- list()
    for ( ngLevel in maxN:1 ) {
        evalTbl <- sboEvalTables[[ngLevel]]
        accuracyList[[as.character(ngLevel)]] <- evalTbl %>% 
            summarise(Top3 = round(mean(top3) * 100, 2),
                      Top5 = round(mean(top5) * 100, 2), 
                      Top15 = round(mean(top15) * 100, 2))
    }
    accuracyTbl <- rbindlist(accuracyList)
    accuracyTbl$Label <- paste0(nrow(accuracyTbl):1, "-gram Accuracy (%)")
    # row.names(accuracyTbl) <- paste0(nrow(accuracyTbl):1, "-gram Accuracy: ")
    accuracyTbl
}

#
# Set dataDir and projDir before calling
#
profileSBOModel <- function(saveDir = getwd(), inputText) {

    # Load the ngram model tables
    profileFile <- paste0(saveDir, "/NgTableLoad.Rprof.out")
    print(paste0("Saving table load profiling data to: ", profileFile))
    Rprof(filename=profileFile, memory.profiling=TRUE)
    
    ngTables <- loadNgTables("train")
    Rprof(filename=NULL)

    # Parse input and predict
    profileFile <- paste0(saveDir, "/Predict.Rprof.out")
    print(paste0("Saving parse/prediction profiling data to: ", profileFile))
    Rprof(filename=profileFile, memory.profiling=TRUE)
    
    inWords <- getNGrams(inputText, 1)
    inputLen <- 0
    predInput <- ""
    if ( length(inWords) >= maxN-1 ) {
        inputLen <- maxN-1
        predInput <- inWords[(length(inWords)-inputLen+1):length(inWords)]
    }
    else {
        if ( length(inWords) > 0 ) {
            inputLen <- length(inWords)
            predInput <- inWords
        }
    }
    # print(paste0("Prediction input: (", paste0(predInput, collapse=" "), ")"))
    # print(paste0("Input length: (", inputLen, ")"))
    
    # Predict
    sbScores <- predictionFlow(inputLen+1, paste0(predInput, collapse=" "), ngTables, 5)

    Rprof(filename=NULL)
    sbScores
}

runProfilerExample <- function(inputText = "I wonder how many times") {
    projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
    dataDir <- paste0(projDir, "data/")
    profileSBOModel(saveDir = dataDir, inputText)
}

viewProfileExample <- function() {
    profvis::profvis(prof_input = "~/Documents/Projects/DataScience/CapstoneProject_JHSK/data//NgTableLoad.Rprof.out")
    profvis::profvis(prof_input = "~/Documents/Projects/DataScience/CapstoneProject_JHSK/data//Predict.Rprof.out")
}




















