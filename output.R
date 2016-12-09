files_list <- list.files("out", "\\.(rds$|feather$)", full.names = TRUE)
file.copy(from = files_list, to = "../land-designations-shinyapp/app/data", overwrite = TRUE)
