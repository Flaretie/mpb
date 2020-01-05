#' @name mpmiExcraction
#' @title Extract words based on MPMI+BE alogrithm
#' @description Extract words based on MPMI+BE alogrithm
#' @param df One column dataframe.
#' @param ChineseCharFilter Drop non-Chinese characters. Default is True.
#' @param lambda Weight distribution between PMI and BE, which will directly affect the results.
#' @param optics Automatically optimize the result, select TURE if you don't know how to choose lambda.
#' @param steps Number of iterations in the optimization process.
#' @param threshold threshold for the final score.
#' @param bayesianCutoff threshold for the bayesian probabilty value in word extension process
#'
#' @return List of extracted strings.
#' @export
#' @examples
#' \dontrun{
#' mpmiExcraction(drugnames,
#'                ChineseCharFilter = T,
#'                lambda = 0.25,
#'                optics = T,
#'                steps = 100,
#'                threshold = 30,
#'                bayesianCutoff = 0.6)
#' }

mpmiExcraction <- function(df, ChineseCharFilter = T, lambda = 0.25 , optics, steps = 100, threshold,bayesianCutoff){
  userDataImport(df, ChineseCharFilter = T)
  # global: userone_char, userdf_t2h, segWords

  # process user file here.
  cat(">>> Start Processing import file...", "\n")
  assign("userOneChar", onecharFreq(userdf_t2h), envir = .GlobalEnv)
  #userOneChar <<- onecharFreq(userdf_t2h)
  #userTwoChar <<- ncharFreq(userdf_t2h, 2, userOneChar)
  assign("userTwoChar", ncharFreq(userdf_t2h, 2, userOneChar), envir = .GlobalEnv)
  userThreeChar <- ncharFreq(userdf_t2h, 3)
  userFourChar <- ncharFreq(userdf_t2h, 4)
  userFiveChar <- ncharFreq(userdf_t2h, 5)
  cat("DONE ", crayon::green(cli::symbol$tick), "\n")

  # mining seed word
  twoCharFilter <- seed(userTwoChar)

  # process real world data.
  cat(">>> Merging and Filtering data...", "\n")

  #threeCharFilter <<- suppressWarnings(charFilter(userThreeChar, rwstats::threeChar))
  #fourCharFilter <<- suppressWarnings(charFilter(userFourChar, rwstats::fourChar))
  #fiveCharFilter <<- suppressWarnings(charFilter(userFiveChar, rwstats::fiveChar))
  assign("threeCharFilter", suppressWarnings(charFilter(userThreeChar, rwstats::threeChar)), envir = .GlobalEnv)
  assign("fourCharFilter", suppressWarnings(charFilter(userFourChar, rwstats::fourChar)), envir = .GlobalEnv)
  assign("fiveCharFilter", suppressWarnings(charFilter(userFiveChar, rwstats::fiveChar)), envir = .GlobalEnv)
  cat("DONE ", crayon::green(cli::symbol$tick), "\n")


  #
  cat(">>> Branch Entropy calculating...", "\n")

  #ptm <- proc.time()
  twoCharFilter$BE  <- unlist(pbapply::pblapply(twoCharFilter$character,
                                                   FUN =  function(x) branchEntropy(x)))

  #by(twoCharFilter,
  #   twoCharFilter$character,
  #   FUN = function(x) branchEntropy(as.character(x)))
  #proc.time() - ptm

  cat("DONE ", crayon::green(cli::symbol$tick), "\n")

  cat(">>> Normalization...", "\n")
  outliers <- graphics::boxplot(twoCharFilter$log, plot=FALSE)$out
  twoCharFilter <- twoCharFilter[-which(twoCharFilter$log %in% outliers),]
  twoCharFilter$normLog <- with(twoCharFilter, normalize(twoCharFilter$log))
  twoCharFilter$normBE <- with(twoCharFilter, normalize(BE))
  twoCharFilter$score <- with(twoCharFilter,(1-lambda)*normLog-lambda*normBE)
  cat("DONE ", crayon::green(cli::symbol$tick), "\n")
  assign("twoCharFilter", twoCharFilter, envir = .GlobalEnv)

  Optics(steps, optics, threshold, lambda,bayesianCutoff)
}





seed <- function(userTwoChar) {
  cat(">>> Mining seeds...", "\n")
  userTwoChar <- userTwoChar[userTwoChar$test_log > 0,]
  userTwoChar <- userTwoChar[order(userTwoChar$test_log, decreasing = T),]
  userTwoCharPriority <- userTwoChar[userTwoChar$test_log >= as.double(stats::quantile(userTwoChar$test_log)[3]),]


  twoCharFilter <-suppressWarnings(charFilter(userTwoChar[userTwoChar$test_log < as.double(stats::quantile(userTwoChar$test_log)[3]),],
                                              two_char))
  twoCharFilter <- twoCharFilter[twoCharFilter$log >= as.double(stats::quantile(twoCharFilter$log)[2]),][,c("character")]
  userTwoCharPriority<- userTwoCharPriority[, c("character")]
  userTwoChar <- append(levels(droplevels(userTwoCharPriority)), twoCharFilter)
  tempchar <- ncharFreq(userdf_t2h, 2, userOneChar)
  userTwoChar <- tempchar[userTwoChar %in% tempchar$character,]
  userTwoChar$character <- as.character(userTwoChar$character)
  userTwoChar$test_log <- with(userTwoChar, log2(pct/(A*B)))
  twoCharFilter <- suppressWarnings(charFilter(userTwoChar, two_char))

  userTwoChar_drop <- userTwoChar[which(userTwoChar$character %in% twoCharFilter$character==F),]
  userTwoChar_drop <- userTwoChar_drop[,c("character","test_log")]
  colnames(userTwoChar_drop) <- c("character","log")

  twoCharFilter <-dplyr::bind_rows(userTwoChar_drop, twoCharFilter)
  cat("DONE ", crayon::green(cli::symbol$tick), "\n")
  return(twoCharFilter)
}

Optics <- function(steps, optics, threshold, lambda,bayesianCutoff) {
  if (optics == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Optimizating [:bar] :percent in :elapsed",
      total = steps, clear = FALSE, width= 60
    )
    cat(">>> Word mining...", "\n")
    num_char <- list()
    optlist <- list()
    n <- 100/steps
    for (i in 1:steps) {
      twoCharFilter2 <- twoCharFilter
      twoCharFilter2$score <- with(twoCharFilter2,
                                   (1-0.01*i*n)*normLog-0.01*i*n*normBE)
      ifelse(threshold <= 100 & threshold >= 1,
             twoCharFilter2 <- twoCharFilter2[twoCharFilter2$score > stats::quantile(twoCharFilter2$score,
                                                                              probs = 1:100/100)[threshold],],
             warning("Threshold must be an integer between 1 and 100"))

      cl <- nextWordMiner(twoCharFilter2,2,bayesianCutoff)
      cl$cont <- unique(cl$cont[cl$cont %in% threeCharFilter$character == TRUE])
      tri_char_final <- threeCharFilter[threeCharFilter$character %in% cl$cont,]

      # mining word length: 4
      cl2 <- nextWordMiner(tri_char_final,3,bayesianCutoff)
      cl2$cont <- unique(cl2$cont[cl2$cont %in% fourCharFilter$character == TRUE])
      qua_char_final <- fourCharFilter[fourCharFilter$character %in% cl2$cont,]

      # mining word length: 5
      cl3 <- nextWordMiner(qua_char_final,4,bayesianCutoff)
      cl3$cont <- unique(cl3$cont[cl3$cont %in% fiveCharFilter$character == TRUE])
      five_char_final <- fiveCharFilter[fiveCharFilter$character %in% cl3$cont,]


      # generate final list and save
      final_dn <- list()
      final_dn <- append(final_dn,cl$stop)
      final_dn <- append(final_dn,cl2$stop)
      final_dn <- append(final_dn,cl3$stop)

      #final_dn <- append(final_dn,cl$cont)
      #final_dn <- append(final_dn,cl2$cont)
      #final_dn <- append(final_dn,cl3$cont)
      final_dn <- unique(unlist(final_dn))
      optlist <- append(optlist, final_dn)
      num_char <- append(num_char, length(final_dn))
      pb$tick()
      Sys.sleep(1 / 100)
    }
    #optlist <<- as.data.frame(table(unlist(optlist)))
    assign("optlist", as.data.frame(table(unlist(optlist))), envir = .GlobalEnv)
    num_char <- as.data.frame(unlist(num_char))
    return(cat("DONE ", crayon::green(cli::symbol$tick), "\n",
               ">>>Extracted word has been saved as", crayon::bgWhite(crayon::black("optlist"))))
  } else {
    cat(">>> Word mining...", "\n")
    twoCharFilter2 <- twoCharFilter
    twoCharFilter2$score <- with(twoCharFilter2, (1-lambda)*normLog-lambda*normBE)
    twoCharFilter2 <- twoCharFilter2[twoCharFilter2$score > stats::quantile(twoCharFilter2$score, probs = 1:100/100)[threshold],]

    cl <- nextWordMiner(twoCharFilter2,2, bayesianCutoff)
    cl$cont <- unique(cl$cont[cl$cont %in% threeCharFilter$character == TRUE])
    tri_char_final <- threeCharFilter[threeCharFilter$character %in% cl$cont,]

    # mining word length: 4
    cl2 <- nextWordMiner(tri_char_final,3, bayesianCutoff)
    cl2$cont <- unique(cl2$cont[cl2$cont %in% fourCharFilter$character == TRUE])
    qua_char_final <- fourCharFilter[fourCharFilter$character %in% cl2$cont,]

    # mining word length: 5
    cl3 <- nextWordMiner(qua_char_final,4, bayesianCutoff)
    cl3$cont <- unique(cl3$cont[cl3$cont %in% fiveCharFilter$character == TRUE])
    five_char_final <- fiveCharFilter[fiveCharFilter$character %in% cl3$cont,]


    # generate final list and save
    final_dn <- list()
    final_dn <- append(final_dn,cl$stop)
    final_dn <- append(final_dn,cl2$stop)
    final_dn <- append(final_dn,cl3$stop)

    #final_dn <- append(final_dn,cl$cont)
    #final_dn <- append(final_dn,cl2$cont)
    #final_dn <- append(final_dn,cl3$cont)

    #final_dn <<- unique(unlist(final_dn))
    assign("final_dn", unique(unlist(final_dn)), envir = .GlobalEnv)
    return(cat("DONE ", crayon::green(cli::symbol$tick), "\n",
               ">>>Extracted word has been saved as", crayon::bgWhite(crayon::black("final_dn"))))
  }
}


