#' Forsys threshold helper function
#'
#' Convert Forsys threshold into format expected by Patchmax
#'
#' @param stand_threshold 
#' @keywords internal
#' @return
#' @export
#'
#' @examples
#' 
forsys_helper_prep_threshold <- function(stand_threshold){
  if(!is.null(stand_threshold)){
    message('PatchMax assumes project stand threshold operator is ">="')
    x <- stand_threshold %>%
      stringr::str_split('>=|<=|>|<|==', simplify = T)
    if(length(x) != 2){
      message('Error in PatchMax threshold statment; using NULL values')
    } else {
      out <- list(
        st_threshold = stringr::str_trim(x[1]),
        st_threshold_value = as.numeric(x[2])
      )
    }
  } else {
    out <- NULL
  }
  return(out)
}