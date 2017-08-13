# source(CreateCorpus.R)
library(data.table)

# Create the next-word prediction model for the Data Science Capstone Project
# sponsored by SwiftKey and provided by Johns Hopkins University.


# Don't include ngrams that occur less than `freqCutoff` times in the
# training data.
# TODO: Look at frequency of frequencies for input on `freqCutoff`
freqCutoff1 <- 5
freqCutoffN <- 3

# Create and save to disk the ngram tables used for scoring
# during prediction.
# - maxN - Highest order of ngram table to create.
#
createNGramTables <- function(maxN = 3, sampleSize=1000, workDir = getPartitionDir("train")) {
    print("Creating ngram tables...")
    print(paste(c("maxN:", maxN, "sampleSize:", sampleSize), collapse=" "))
    
    # "n-1" term vector (used to calc scores for the n term vector).
    minus1tc <- integer()
    for ( nVal in 1:maxN ) {
        
        # Create the DTM and n term vector.
        print(paste(c("Creating DTM... n=", nVal), collapse=""))
        mdtm <- dtmFromMCorpus(sampleMediaCorpus(sampleSize=sampleSize, mediaSource = "blogs"), nVal)
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
        if ( nVal == 1 ) {
            # unigram score
            ngScore <- tc/sum(tc)
        } else {
            # ngram (where n > 1) score
            prfix <- sapply(strsplit(names(tc), " ", fixed=TRUE),
                            function(x) paste0(x[1:nVal-1], collapse=" "))
            prfixCt <- minus1tc[prfix]
            ngScore <- tc/prfixCt
        }
        ngTbl <- data.table(ng = names(tc), score = ngScore)
        ngTblFile <- paste0("ng_", nVal, "_tbl.rds")
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








