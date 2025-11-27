#' Summary table across groups
#'
#' Summary table across groups
#'
#' @param list List of outcome dataframes
#'
#' @return Summary table
#' @examples
#' f_LE_summary_table()
#' @export
f_LE_summary_table <- function(list){

        purrr::map(list, 1) %>%
                discard(is.null) %>%
                bind_rows( .id = "characteristics") %>%
                separate_wider_delim(characteristics, delim = ".", names = c("Sex", "NCRAS_Draw", "Stage", "Age", "haz_ratio", "cfDNA_status")) %>%
                rename(`2.5%` = `lower`,
                       `97.5%` = `upper`) %>%
                mutate(across(`variable`:`97.5%`, unlist)) %>%
                mutate(across(c(median,`2.5%`,`97.5%` ), \(x) round(x, 2) )) %>%
                unite(CrI_temp, c(`2.5%`, `97.5%`), sep = " to ", remove = FALSE) %>%
                mutate(CrI = paste0("(", CrI_temp, ")")) %>%
                unite(median_CrI, c(median, CrI), sep = " ", remove = FALSE) %>%
                group_by(Sex, NCRAS_Draw, Stage, Age, haz_ratio, cfDNA_status) %>%
                arrange(as.factor(Stage), .by_group = TRUE) %>%
                ungroup()

}
