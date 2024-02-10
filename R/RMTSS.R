#' RSS MSS TSS Function
#'
#' Prints the RSS, MSS, TSS and MSS / TSS
#'
#' @param model A fit model
#' @param val   The data that is being checked
#'
#' @return      NULL
#' @export
#'
#' @examples
#' library(MATH4753NATH24)
RMTSS <- function(model, val){

  RSS = sum((val    - model    )^2)
  MSS = sum((model  - mean(val))^2)
  TSS = sum((val    - mean(val))^2)

  print(paste0("RSS: ", RSS))
  print(paste0("MSS: ", MSS))
  print(paste0("TSS: ", TSS))
  print(paste0("MSS / TSS: ", MSS / TSS))
}
