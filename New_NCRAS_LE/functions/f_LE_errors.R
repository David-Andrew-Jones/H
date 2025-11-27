#' Catch errors
#'
#' Catch errors
#'
#' @param list List of outcome dataframes
#'
#' @return Summary table of errors
#' @examples
#' f_LE_errors()
#' @export
f_LE_errors <- function(list){

        purrr::map(list, 2) %>%
                discard(is.null) %>%
                purrr::map(1) %>%
                bind_rows( .id = "characteristics")

}
