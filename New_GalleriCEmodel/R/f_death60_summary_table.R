#' Summary table across groups for probability of survival past 60
#'
#' Summary table across groups for probability of survival past 60
#'
#' @param list List of outcome dataframes
#'
#' @return Summary table
#' @examples
#' f_death60_summary_table()
#' @export
f_death60_summary_table <- function(list){

        purrr::map(list, 1) %>%
                discard(is.null) %>%
                bind_rows( .id = "characteristics") %>%
                separate_wider_delim(characteristics, delim = ".", names = c("Sex", "NCRAS_Draw", "Stage", "Age")) %>%
                rename(`2.5%` = `lower`,
                       `97.5%` = `upper`) %>%
                mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
                unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = FALSE) %>%
                mutate(CrI = paste0("(", CrI_temp, ")")) %>%
                select(Sex, NCRAS_Draw, Stage, Age, median, CrI)

}
