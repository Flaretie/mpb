#' @name userDataImport
#' @title Occurrence and frequency of the next word
#'
#' @description Processing user data
#' @param df user import file
#' @param ChineseCharFilter Drop non-Chinese characters. Default is True.
#' @return A dataframe containing the next word and its occurrence and frequency
#' @export
#' @examples
#' \donttest{
#' further examples for users (not used for checks)
#' userDataImport(drugnames)
#' }

userDataImport <- function(df, ChineseCharFilter = TRUE) {
  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop("Package \"crayon\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!(is.data.frame(df) & is.data.frame(df) == 1)) {
    warning("'df' must be a dataframe with single column.")
  } else {
    colnames(df) <- c("Candidate List")
    userdf_t2h <- paste0(df$`Candidate List`, collapse = "") #GLOBAL
    #assign("userdf_t2h", paste0(df$`Candidate List`, collapse = ""), envir = .GlobalEnv)
    userdf_split <- strsplit(userdf_t2h, "")[[1]] # GLOBAL
    #assign("userdf_split", strsplit(userdf_t2h, "")[[1]], envir = .GlobalEnv)
    seg_words <- strsplit(userdf_t2h, "")
    tableWord <- table(seg_words)
    segWords <- seg_words[[1]][-1] #GLOBAL
    #assign("segWords", seg_words[[1]][-1], envir = .GlobalEnv)
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
      cat(">>>Generating Character fequency table...\n")
      return(cat("DONE ", crayon::green(cli::symbol$tick), "\n"))
    }
    cat(">>>Generating Character fequency table...\n")
    return(cat("DONE ", crayon::green(cli::symbol$tick), "\n"))
  }
}
