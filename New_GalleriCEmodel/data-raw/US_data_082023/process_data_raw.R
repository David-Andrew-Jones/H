# Code to generate .Rd data files ----
# clear workspace ----

library(devtools)

setwd("~/GalleriCEmodel/data-raw/US_data_082023")

l_data_file <- list.files(path = path.expand("~/GalleriCEmodel/data-raw/US_data_082023"), pattern="csv") %>%
  map(~ as.data.frame(read_csv(.x, col_names = FALSE, show_col_types = FALSE)))

names(l_data_file) <- tools::file_path_sans_ext(
    list.files(path = path.expand("~/GalleriCEmodel/data-raw/US_data_082023"), pattern="csv")
  )

# transfer to data folder ----
purrr::walk2(l_data_file, names(l_data_file), function(df, name){
  assign(name, df)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
})
