library(readr)
library(httr)
library(purrr)
library(dplyr)

base_url <- "https://catalogue.data.gov.bc.ca/api/3/action/package_show"

record_get <- function(record_id) {
  res <- GET(base_url, query = list(id = record_id))
  warn_for_status(res)
  content(res)$result
}

get_record_id_from_url <- function(url) {
  map_chr(url,
          ~strsplit(.x, "catalogue\\.data\\.gov\\.bc\\.ca/dataset/")[[1]][2])
}

cl_sources <- read_csv("data/sources.csv",
                       col_types = cols(
                         hierarchy = col_integer(),
                         exclude = col_character(),
                         manual_download = col_character(),
                         name = col_character(),
                         alias = col_character(),
                         category = col_character(),
                         url = col_character(),
                         file_in_url = col_character(),
                         layer_in_file = col_character(),
                         query = col_character(),
                         metadata_url = col_character(),
                         info_url = col_character(),
                         preprocess_operation = col_character(),
                         preprocess_layer_alias = col_character(),
                         notes = col_character(),
                         license = col_character()
                       ))

cl_sources_new <- cl_sources %>%
  mutate(databc_record = map(get_record_id_from_url(metadata_url),
       record_get),
       license = map2_chr(databc_record, license,
                           ~ifelse(is.null(.x), .y,
                                   .x[["license_title"]])),
       # the following pattern of dealing with NULLs is only available in purrr >= 0.2.2.9000
       layer_in_file = map_chr(databc_record, "object_name", .null = NA_character_)) %>%
  select(-databc_record)

write_csv(cl_sources_new, "data/sources.csv", na = "")
