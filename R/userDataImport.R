#' @name userDataImport
#' @title Occurrence and frequency of the next word
#'
#' @description This is a function that lists the next word appearing frequency
#' by a given chinese character based on real world
#' chinese character frequency Statistics table.
#' @param df user import file
#' @param ChineseCharFilter Drop non-Chinese characters. Default is True.
#' @return A dataframe containing the next word and its occurrence and frequency
#' @export
#' @examples
#' userDataImport(drugnames)

userDataImport <- function(df, ChineseCharFilter = TRUE) {
  if (!(is.data.frame(df) & is.data.frame(df) == 1)) {
    warning("'df' must be a dataframe with single column.")
  } else {
    colnames(df) <- c("Candidate List")
    userdf_t2h <<- paste(df$`Candidate List`, collapse = "") #GLOBAL
    userdf_split <<- strsplit(userdf_t2h, "")[[1]] # GLOBAL
    seg_words <- strsplit(userdf_t2h, "")
    tableWord <- table(seg_words)
    segWords <<- seg_words[[1]][-1] #GLOBAL
    tableWord <- as.data.frame(tableWord)
    tableWord <- tableWord[order(tableWord$Freq, decreasing = T),]
    colnames(tableWord)[1] <- c("character")
    tableWord$pct <- with(tableWord, Freq/sum(tableWord$Freq))

    if (ChineseCharFilter == T) {
      tableWord <- tableWord[tableWord$character %in%
                               regmatches(as.character(tableWord$character),
                                          regexpr("^[\u4e00-\u9fa5]{0,}$",
                                                  as.character(tableWord$character),
                                                  perl = TRUE)),]
      rownames(tableWord) <- NULL
    } else {
      return(cat("Character fequency table generazation complete\n"))
    }
    return(cat("Character fequency table generazation complete\n"))
  }
}
