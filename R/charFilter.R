charFilter <- function(userchar, realworld) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Left merge default and realworld dataframe
  mergedData <- dplyr::left_join(userchar, realworld, by = c("character"))
  # if the word contains 2 characters.
  # then we calculate its MPMI(Modified Point Mutual Infomation)
  if (length(strsplit(as.character(userchar$character[1]),"")[[1]]) == 2) {
    mergedData <- mergedData[stats::complete.cases(mergedData), ]
    mergedData$log <- with(mergedData, test_log/real_log)
    mergedData <- mergedData[c("character", "log")]
    mergedData <- mergedData[order(mergedData$log, decreasing = T),]
  }
  rownames(mergedData) <- NULL
  return(mergedData)
}

