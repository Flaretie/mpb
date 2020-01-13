#' drugnames
#'
#' This dataframe contains examples of user-imported data.
#'
#' @format A data frame with 1 variables:
#' \describe{
#' \item{\code{V1}}{Target string.}
#' }
#'
"drugnames"

#' Frequency, PMI and BE dataframe of Chinese words wtih two characters.
#'
#' This dataframe contains Chinese Word with two characters with its branch entropy and pmi value
#'
#' @format A data frame with 1 variables:
#' \describe{
#' \item{\code{character}}{chinese word with two characters.}
#' \item{\code{freq}}{Occurence times of target word in real world.}
#' \item{\code{pct}}{Frequency of target word in real world.}
#' \item{\code{A}}{entropy of first character in target word.}
#' \item{\code{B}}{entropy of second character in target word.}
#' \item{\code{real_log}}{pmi value of target word.}
#' }
"two_char"

