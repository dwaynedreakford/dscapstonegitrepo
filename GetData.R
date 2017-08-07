library(reader)

courseDataUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
dataFileNm <- "Coursera-SwiftKey.zip"
projDir <- "~/Documents/Projects/DataScience/CapstoneProject_JHSK/"
dataDir <- paste0(projDir, "/data/")

# Download and unzip the course data files
downloadProjData <- function(dataUrl = courseDataUrl,
                             workDir = dataDir) {
    setwd(workDir)
    download.file(dataUrl, destfile = dataFileNm)
    unzip(dataFileNm)
}

# Loads the full content from the specified sources and
# returns a list of character vectors, where the list elements are
# named by the data source from which the data was loaded e.g.:
# If all media sources are loaded via: projData <- loadProjData()
# then the text loaded from the blogs file is accessible via:
# projData[["blogs"]]
#
loadProjData <- function(workDir = dataDir,
                         mLang = "en_US",
                         srcMedium = c("blogs", "news", "twitter")) {
    setwd(workDir)
    tgtDir <- paste0(workDir, "final/", mLang, "/")
    resultList = list()
    for(medium in srcMedium) {
        tgtFile <- paste0(tgtDir, mLang, ".", medium, ".txt")
        print(tgtFile)
        resultList[[medium]] <- reader::reader(tgtFile)
    }
    resultList
}

# Loads a sampling of the content from specified data sources and
# returns a list, as described for 'loadProjData'.
#
sampleProjData <- function(workDir = dataDir,
                         mLang = "en_US",
                         srcMedium = c("blogs", "news", "twitter"),
                         nLines = 100, skipLines = 0) {
    setwd(workDir)
    tgtDir <- paste0(workDir, "final/", mLang, "/")
    resultList = list()
    for(medium in srcMedium) {
        tgtFile <- paste0(tgtDir, mLang, ".", medium, ".txt")
        print(tgtFile)
        resultList[[medium]] <- n.readLines(tgtFile, nLines, skip = skipLines, header = FALSE)
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









