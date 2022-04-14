#' Forsys threshold helper function
#'
#' Convert Forsys threshold into format expected by Patchmax
#'
#' @param stand_threshold 
#'
#' @return
#' @export
#'
#' @examples
#' 
forsys_helper_prep_threshold <- function(stand_threshold){
  if(!is.null(stand_threshold)){
    message('PatchMax currently assumes that the project stand threshold operator is ">" and overwrites any other operator provided (e.g., "<")')
    x <- stand_threshold %>%
      stringr::str_split('>=|<=|>|<|==', simplify = T)
    if(length(x) != 2){
      message('Error in PatchMax threshold statment; using NULl values')
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