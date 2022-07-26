#' Merge a list of `gson` object
#' 
#' @note Do not merge `gson` objects of different annotation types
#'
#' @param glist a list of `gson` objects
#' @param ... additional `gson` object
#'
#' @return a `gson` instance
#' @export
#'
#' @examples
#' \dontrun{
#'   merge_gson(gson1, gson2, gson3)
#' }
merge_gson = function(..., glist){
  gson_list = c(list(...), glist)
  if (sum(!sapply(gson_list, FUN = inherits, what = "GSON")) > 0){
    simpleError("All input are expected to be a GSON object.")
  }
  
  slotname = methods::slotNames(gson_list[[1]])
  result = gson_list[[1]]
  for (i in seq_along(slotname)){
    name = slotname[[i]]
    value = methods::slot(result, name)
    
    if (is.data.frame(value)) methods::slot(result, name, check = TRUE) = purrr::reduce(
      .x = lapply(gson_list, methods::slot, name = name),
      .f = dplyr::bind_rows
    ) %>%
      unique()
    
    if (is.character(value)) methods::slot(result, name, check = TRUE) = purrr::reduce(
      .x = lapply(gson_list, methods::slot, name = name),
      .f = c
    ) %>%
      unique() %>%
      paste(sep = "; ")
  }
  return(result)
}