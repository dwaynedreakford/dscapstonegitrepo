library(reader)

# The overall data acquisition flow is:
#
# 1. downloadProjData()
# 2. partitionProjData() - into separate files for training, validation and test
# 3. loadProjData() or sample projData() - into in-memory character vectors
#
# The resulting list(s) of character vectors can be transformed into
# a data frame via mListToDF(). This form is useful to provide input to
# NLP data mining routines, like those of the tm package. 
#
# Functions defined in CreateCorpus.R make use of those defined here
# to load the data from flat (.txt) files and create corpora, document-
# term matrices and perform further analysis.
#

projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
dataDir <- paste0(projDir, "data/")

courseDataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
dataFileNm <- "Coursera-SwiftKey.zip"

# From Luis von Ahn (https://www.cs.cmu.edu/~biglou/)
# "A list of 1,300+ English terms that could be found offensive."
badwordsUrl <- "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
badwordsFileNm <- "badwords.txt"

#
# Download and unzip the course data files.
#
# Download "bad words" list.
#
downloadProjData <- function(dataUrl = courseDataUrl,
                             workDir = dataDir) {
    setwd(workDir)
    download.file(dataUrl, destfile = dataFileNm)
    unzip(dataFileNm)
    
    download.file(badwordsUrl, badwordsFileNm)
}

# Get "bad words" as a vector.
getBadWords <- function(workDir = dataDir) {
    setwd(workDir)
    reader::reader(badwordsFileNm)
}

# Partition the data files into train, validation and test sets.
# Split each media file into train, validation and test files, 
# where:
# - [file]_train.txt is approximately 80% the size of [file].txt
# - [file]_val.txt is approximately 10% the size of [file].txt
# - [file]_test.txt is approximately 10% the size of [file].txt
#
# The split is done based on number of lines in each file,
# as opposed to the exact size of the files, so the size
# delineations are approximate.
#
# trainDir <- paste0(dataDir, "train/")
# valDir <- paste0(dataDir, "val/")
# testDir <- paste0(dataDir, "test/")
#
partitionProjData <- function(workDir = dataDir,
                              mLang = "en_US",
                              srcMedium = c("blogs", "news", "twitter")) {
    
    setwd(workDir)
    srcDir <- paste0(workDir, "final/", mLang, "/")
    for(medium in srcMedium) {
        # Read the whole file into a char vector.
        srcFile <- paste0(srcDir, mLang, ".", medium, ".txt")
        print(paste0("Reading: ", srcFile))
        srcChars <- reader::reader(srcFile)
        
        # Shuffle the char vector and write the appropriate
        # portions to training, validation and test files.
        # This is valid because the predictive model takes
        # advantage of the Markovian assumption, that the
        # next word can be predicted based solely on the
        # previous few words.
        set.seed(6239)
        idxList = list()
        allIdxs <- sample(1:length(srcChars))
        idxList[["train"]] <- head(allIdxs, floor(length(allIdxs) * 0.8))
        idxList[["val"]] <- tail(allIdxs, floor(length(allIdxs) * 0.2))
        idxList[["test"]] <- tail(idxList[["val"]], floor(length(idxList[["val"]]) * 0.5))
        idxList[["val"]] <- head(idxList[["val"]], floor(length(idxList[["val"]]) * 0.5))
        
        # Write the training, validation and test files.
        for ( partition in c("train", "val", "test")  ) {
            outDir <- getPartitionDir(partition)
            if ( !dir.exists(outDir) ) {
                if (!dir.create(outDir)) stop(paste0(geterrmessage(), ": Could not create ", outDir))
            }
            outFile <- getMDataFileNm(outDir, medium, partition)
            print(paste0("Writing: ", outFile))
            write(srcChars[idxList[[partition]]], outFile)
        }
        
        rm(srcChars)
        rm(allIdxs)
        rm(idxList)
    }
}

getPartitionDir <- function(partition) {
    pDir <- paste0(dataDir, partition, "/")
    pDir
}

getMDataFileNm <- function(pDir, medium, partition) {
    pFile <- paste0(pDir, medium, "_", partition, ".txt")
    pFile
}

# Loads the full content from the specified sources and
# returns a list of character vectors, where the list elements are
# named by the data source from which the data was loaded.
#
# The `partition` argument specifies that data should be loaded
# from the training, validation or test data partition. The valid
# values are "train", "val" or "test", respectively.
#
# If all media sources are loaded via: projData <- loadProjData()
# then the text loaded from the blogs file is accessible via:
# projData[["blogs"]]
#
loadProjData <- function(workDir = dataDir,
                         mLang = "en_US",
                         srcMedium = c("blogs", "news", "twitter"),
                         partition = "train") {
    
    srcDir <- getPartitionDir(partition)
    resultList = list()
    for(medium in srcMedium) {
        srcFile <- getMDataFileNm(srcDir, medium, partition)
        print(paste0("Loading from file: ", srcFile))
        resultList[[medium]] <- reader::reader(srcFile)
    }
    resultList
}

# Loads a sampling of the content from specified data sources and
# returns a list, as described for 'loadProjData'.
#
sampleProjData <- function(workDir = dataDir,
                         mLang = "en_US",
                         srcMedium = c("blogs", "news", "twitter"),
                         partition = "train",
                         nLines = 100, skipLines = 0) {

    srcDir <- getPartitionDir(partition)
    resultList = list()
    for(medium in srcMedium) {
        srcFile <- getMDataFileNm(srcDir, medium, partition)
        print(paste0("Loading from file: ", srcFile))
        resultList[[medium]] <- n.readLines(srcFile, nLines, skip = skipLines, header = FALSE)
    }
    resultList
}

# Collapse list of character vectors into a data frame.
# Each list element becomes two columns, where:
# - element name becomes the value in the msource column
# - vector values become values in the mdata column
#
mListToDF <- function(mList) {

    resultDF = data.frame("msource" = character(0), "mdata" = character(0))
    for ( mSource in names(mList) ) {
        resultDF <- rbind(resultDF,
            data.frame("msource" = rep(mSource, length(mList[[mSource]])),
                       "mdata" = mList[[mSource]],
                       stringsAsFactors = FALSE))
    }
    resultDF
}

# --- The following functions help answer Week 1 Quiz questions ---

# Id longest line among all the loaded datasets
longestLine <- function(fullData) {
    maxIdxs <- sapply(fullData, function(x) which.max(nchar(x)))
    maxEntryIdx <- maxIdxs[which.max(maxIdxs)]
    print(maxEntryIdx)
    nchar(fullData[[names(maxEntryIdx)]][maxEntryIdx])
}

loveVsHate <- function(fullData) {
    loveLines <- sum(grepl("love",fullData[["twitter"]], fixed = TRUE))
    hateLines <- sum(grepl("hate",fullData[["twitter"]], fixed = TRUE))
    round(loveLines/hateLines, 3)
}

getBioTweet <- function(fullData) {
    grep("biostats",fullData[["twitter"]], fixed = TRUE, value = TRUE)
}

countChessFighterTweets <- function(fullData) {
    sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",
         fullData[["twitter"]], fixed = TRUE))
}









