rm(list=ls())
source("credentials.R")

pkgs <- c("data.table", 
          "dplyr",
          "stringi", 
          "httr", 
          "tools", 
          "magrittr", 
          "lubridate", 
          "jsonlite",
          "crul",
          "parallel")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))  {
    cat("######################################################################\n")
    cat("Thees are the missing packages: ", paste0(new.pkg, collapse=", "), "\n")
    cat("######################################################################\n")
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.us.r-project.org')
  }
  sapply(pkg, require, character.only = TRUE)
}

ipak(pkgs)

options(scipen = 999)




#### Investigation of the Missing Bundles ####
missing_errors <- base::readRDS("/mnt/data/allstate_log_github/missing_execution_ids_to_investigate_10_31_2023.rds")
missing_errors

existing_support_bundles <- paste0(data_directory, 'support-bundles')
existing_support_bundles <- list.files(existing_support_bundles, recursive=FALSE)
existing_support_bundles <- existing_support_bundles[!grepl("\\.zip", existing_support_bundles)]

bundles_failed_to_download <- setdiff(missing_errors, existing_support_bundles)

missing_errors

summaries <- list.files(paste0(data_directory, 'support-bundle-summary-ml'), full.name=TRUE)

df <- lapply(summaries, function(target) {
  df <- read.csv(target)
}) %>% do.call(rbind, .)

file_types <- lapply(df$File_Path, function(target) {
  x <- stringi::stri_split(target, regex="/") %>% unlist()
  x <- x[length(x)]
}) %>% unlist()

file_types[grep("logjam", file_types)] <- "logjam.log"
table(file_types)
