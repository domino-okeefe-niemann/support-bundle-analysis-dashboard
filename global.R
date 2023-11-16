rm(list=ls())

if(dir.exists('/mnt/data/')) {
  data_directory_prefix <- '/mnt/data/'
} else {
  data_directory_prefix <- '/domino/datasets/local/'
}
domino_project_name <- list.files(data_directory_prefix, recursive=FALSE)[1]
credentials_path <- paste0(data_directory_prefix, domino_project_name,'/credentials.R')
source(credentials_path)

# Load the required packages
#### GLOBAL.R ####
#cat("THIS IS THE CURRENT WORKING DIRECTORY:", getwd(), "\n")
pkgs <- c("shiny", "shinydashboard", "DT", "digest", "data.table", "highcharter", "viridis", "shinyjs", "dplyr",
          "stringi", "httr", "tools", "magrittr", "lubridate", "jsonlite", "shinycssloaders", "rintrojs", "shinyWidgets", "crul", "parallel")

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

# THESE ONLY WORK IN WORKSPACE. These environments do not exist in the environment where the app is deployed.
# Consider injecting some environment variables when creating the prod level design?
#domino_project_name <- system("echo $DOMINO_PROJECT_NAME", intern=TRUE)
#domino_url <- system("echo $RSTUDIO_HTTP_REFERER", intern=TRUE)
#domino_url <- stringi::stri_extract(domino_url, regex="(?<=https:\\/\\/)([^\\/]+)")


#### Precreate all the needed folders (for dependencies/data/etc...) ####
# if(!dir.exists('./data')) {
#   dir.create("./data")
# }

#### FOR FIRST RUN! ####
# If the regex_lookup doesn't yet exist, create it out of prediscussed "default" patterns.
if(!file.exists(paste0(data_directory, 'regex_lookup.csv'))) {

  # Define Cluster error pattern
  cluster_error_pattern = c(
    'failed error: pods(.*)',
    '"message": "Error(.*)"',
    'Internal server error(.*)',
    '\\[error\\](.*)',
    'MountVolume\\.SetUp failed',
    'Failed to create pod sandbox',
    'failed to get memory utilization',
    'error determining status: rpc error: code',
    'Unable to attach or mount volumes:',
    'Error creating:'
  )
  cluster_df <- data.frame(error_type=rep("cluster", length(cluster_error_pattern)), regex_pattern=cluster_error_pattern)
  
  #cluster_error_pattern <- paste0(cluster_error_pattern, collapse="|")
  
  # Define Domino error pattern
  domino_error_pattern = c(
    # Matches the string "ERROR: " followed by any number of characters .* until the end of the line.
    # The () in the pattern captures the text after the "ERROR: " string and returns it as a group.
    '-ERROR(.*)',
    'repository not found(.*)',
    '.*message.*level=error.*'
  )
  #domino_error_pattern <- paste0(domino_error_pattern, collapse="|")
  domino_df <- data.frame(error_type=rep("domino", length(domino_error_pattern)), regex_pattern=domino_error_pattern)
  
  # Define User error pattern
  user_error_pattern = c(
    'TypeError:(.*)'#,
    #'[error](.*)'
  )
  #user_error_pattern <- paste0(user_error_pattern, collapse="|")
  user_df <- data.frame(error_type=rep("user", length(user_error_pattern)), regex_pattern=user_error_pattern)
  
  
  regex_df <- rbind(cluster_df, domino_df, user_df)
  
  write.csv(regex_df, paste0(data_directory, "regex_lookup.csv"), row.names = FALSE)
} else {
  regex_df <- read.csv(paste0(data_directory, "regex_lookup.csv"))
}

if(!dir.exists(paste0(data_directory, "support-bundles"))) {
  dir.create(paste0(data_directory, "support-bundles"))
}

if(!dir.exists(paste0(data_directory, "support-bundle-summary"))) {
  dir.create(paste0(data_directory, "support-bundle-summary"))
}

if(!dir.exists(paste0(data_directory, "support-bundle-summary-ml"))) {
  dir.create(paste0(data_directory, "support-bundle-summary-ml"))
}

if(!dir.exists(paste0(data_directory, "resource-usage-by-day"))) {
  dir.create(paste0(data_directory, "resource-usage-by-day"))
}

if(!dir.exists(paste0(data_directory, "regex_backups"))) {
  dir.create(paste0(data_directory, "regex_backups"))
}
# Really only for first run.. Creates a bank of preselect filters for resource usage report based on the past two years of report data
# Will be stored as .rds for any subsequent runs
if(!file.exists(paste0(data_directory, "usage_report_filter_options.rds"))) {
  tst_url <- paste0("https://", domino_url, "/admin/generateUsageReport")
  
  start_date <- Sys.Date() - lubridate::years(2)
  end_date <- Sys.Date()
  
  payload <- paste0("start-date=", format(start_date, '%m/%d/%Y'), "&end-date=", format(end_date, '%m/%d/%Y'))
  #payload <- "start-date=09/15/2023&end-date=09/22/2023"
  
  headers <- c(
    'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'Content-Type'= 'application/x-www-form-urlencoded',
    'X-Domino-Api-Key'= domino_user_api_key
  )
  #cat("Is the error here?\n")
  #cat(tst_url, "\n")
  response <- httr::POST(url = tst_url, body=payload, add_headers(headers))
  #cat("Or post getting the response?\n")
  plain_text <- base::rawToChar(response$content)
  report_df <- read.csv(textConnection(plain_text))
  colnames(report_df) <- gsub("\\.", "_", colnames(report_df)) %>% gsub("__", "_", .) %>% tolower()
  
  # Save out most recent schema for future pulls
  filter_options <- c("status", "hardware_tier")
  precreated_usage_report_filters <- lapply(filter_options, function(col) {
    options <- unique(report_df[col])
  }) %>% stats::setNames(filter_options)
  
  base::saveRDS(precreated_usage_report_filters, file = paste0(data_directory, "usage_report_filter_options.rds"))
} else {
  precreated_usage_report_filters <- base::readRDS(paste0(data_directory, "usage_report_filter_options.rds"))
}

error_types <- c ("cluster", "domino", "user")
tasteful_ryg <- c("#B81D13", "#EFB700", "#008450")





#### HELPER FUNCTIONS ####
##### DOWNLOADING RESOURCE USAGE REPORTS BY A DATE RANGE #####
download_usage_report_from_api <- function(date_range) {
   # Loop through each, downloading the file, parsing it by date, and finally saving into the appropriate dataset path
   resource_usage_url <- paste0("https://", domino_url, "/admin/generateUsageReport")
   
   payload <- paste0("start-date=", format(date_range[1], '%m/%d/%Y'), "&end-date=", format(date_range[2], '%m/%d/%Y'))
   #payload <- "start-date=09/15/2023&end-date=09/22/2023"
   
   headers <- c(
     'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
     'Content-Type'= 'application/x-www-form-urlencoded',
     'X-Domino-Api-Key'= domino_user_api_key
   )
   #cat("Is the error here?\n")
   #cat(resource_usage_url, "\n")
   response <- httr::POST(url = resource_usage_url, body=payload, add_headers(headers))
   #cat("Or post getting the response?\n")
   plain_text <- base::rawToChar(response$content)
   report_df_slice <- read.csv(textConnection(plain_text))
   
   return(report_df_slice)
 }


##### REPORT_DF FEATURE ENGINEERING #####
resource_usage_feature_engineering_pipeline <- function(df) {
  colnames(df) <- gsub("\\.", "_", colnames(df)) %>% gsub("__", "_", .) %>% tolower()
  df$date <- format(as.POSIXct(df$queued_timestamp/1000), format= "%Y-%m-%d")
  
  # colnames(df) <- gsub("\\.", "_", colnames(df)) %>% gsub("__", "_", .) %>% tolower()
  # Save out most recent schema for future pulls
  # filter_options <- c("status", "hardware_tier")
  # filter_save <- lapply(filter_options, function(col) {
  #   options <- unique(df[col])
  # }) %>% stats::setNames(filter_options)
  #base::saveRDS(filter_save, file = "./data/usage_report_filter_options.rds")
  
  if("is_batch" %in% colnames(df)){ 
    df$run_type <- base::ifelse(
      df$is_batch, "batch",
      base::ifelse(df$is_endpoint, "endpoint", "notebook")
    )
    
    df$is_batch <- NULL
    df$is_endpoint <- NULL
    df$is_notebook <- NULL
  }
    
  df$started_time[which(is.na(df$started_time) | df$started_time == "")] <- df$queued_time[which(is.na(df$started_time) | df$started_time == "")]
  df$started_timestamp[which(is.na(df$started_timestamp) | df$started_timestamp == "")] <- df$queued_timestamp[which(is.na(df$started_timestamp) | df$started_timestamp == "")]
  
  # Entries which do not have a completed timestamp/time BUT have a starting timestamp AND the duration it was running at the time of the report
  completed_na_w_duration_idx <- which(is.na(df$completed_timestamp) & df$run_duration_within_reporting_period_s_ > 0)
  df$completed_timestamp[completed_na_w_duration_idx] <- df$started_timestamp[completed_na_w_duration_idx] + df$run_duration_within_reporting_period_s_[completed_na_w_duration_idx]*1000
  df$completed_time[completed_na_w_duration_idx] <- format(as.POSIXct(df$completed_timestamp[completed_na_w_duration_idx]/1000), format="%Y-%m-%dT%H:%M:%OS3Z")
  
  # Entries which do not have a completed timestamp/time
  # For purposes of visuals, we'll show the hour in which a "Failed" process runs. Ceiling date each of the start times!
  completed_na_no_duration_idx <- which(is.na(df$completed_timestamp))
  df$completed_time[completed_na_no_duration_idx] <- lubridate::ymd_hms(df$started_time[completed_na_no_duration_idx]) %>% floor_date(., unit="hour") %>% format(., format="%Y-%m-%dT%H:%M:%OS3Z")
  df$completed_timestamp[completed_na_no_duration_idx] <- lubridate::ymd_hms(df$completed_time[completed_na_no_duration_idx]) %>% as.numeric(.) * 1000
  
  df$time_to_boot_s <- base::round((df$started_timestamp - df$queued_timestamp) / 1000, 0)
  
  return(df)
}



##### REGEX PATTERN SEARCH #####
###### 1. SEND A REQUEST, GET A SUCCESS RESPONSE ######
identify_support_bundle_errors <- function(file_paths=file_paths, regex_pattern_df=regex_pattern_df) {
  cluster_error_pattern <- regex_pattern_df$regex_pattern[which(regex_pattern_df$error_type == "cluster")] %>% paste0(., collapse="|")
  domino_error_pattern <- regex_pattern_df$regex_pattern[which(regex_pattern_df$error_type == "domino")] %>% paste0(., collapse="|")
  user_error_pattern <- regex_pattern_df$regex_pattern[which(regex_pattern_df$error_type == "user")] %>% paste0(., collapse="|")
  
  associated_node_ip <- NA
  ###### 3. CRAWL FILES FOR POTENTIAL ERRORS ######
  metadata_errors <- lapply(file_paths, function(target_file_name) {
    target_file <- readLines(target_file_name)
    
    associated_node <- target_file[grep("assignedNodeName", target_file)]
    associated_node <- stringi::stri_extract(associated_node, regex="ip-[0-9]+-[0-9]+-[0-9]+-[0-9]+\\..*\\.compute.internal")
    if(length(associated_node) > 0) associated_node_ip <<- associated_node
    
    cluster_error_lines <- grep(cluster_error_pattern, target_file)
    if(length(cluster_error_lines) > 0){
      cat("CLUSTER HIT\n")
      cat("TARGET FILE: ", target_file_name, "\n")
      target_file_subset <- target_file[cluster_error_lines]
      cluster_errors <- stringi::stri_extract(target_file_subset, regex=cluster_error_pattern)
      cluster_errors <- cluster_errors[which(!is.na(cluster_errors))]
      cluster_error_description <- target_file[cluster_error_lines]
      cluster_data <- data.frame('Error'=cluster_errors, 'Line_Number'=cluster_error_lines, 'Context'=cluster_error_description)
      cluster_data$Error_Type <- "cluster"
    }
    
    domino_error_lines <- grep(domino_error_pattern, target_file)
    if(length(domino_error_lines) > 0){
      cat("DOMINO HIT\n")
      cat("TARGET FILE: ", target_file_name, "\n")
      target_file_subset <- target_file[domino_error_lines]
      domino_errors <- stringi::stri_extract(target_file_subset, regex=domino_error_pattern)
      #domino_errors <- domino_errors[which(!is.na(domino_errors))]
      domino_error_description <- target_file[domino_error_lines]
      domino_data <- data.frame('Error'=domino_errors, 'Line_Number'=domino_error_lines, 'Context'=domino_error_description)
      domino_data$Error_Type <- "domino"
    }
    
    user_error_lines <- grep(user_error_pattern, target_file)
    if(length(user_error_lines) > 0){
      cat("USER ERROR HIT\n")
      cat("TARGET FILE: ", target_file_name, "\n")
      target_file_subset <- target_file[user_error_lines]
      user_errors <- stringi::stri_extract(target_file_subset, regex=user_error_pattern)
      user_errors <- user_errors[which(!is.na(user_errors))]
      user_error_description <- target_file[user_error_lines]
      user_data <- data.frame('Error'=user_errors, 'Line_Number'=user_error_lines, 'Context'=user_error_description)
      user_data$Error_Type <- "user"
    }
    
    out <- data.frame(
      Error = character(0),
      Line_Number = integer(0),
      Context = character(0),
      Error_Type = character(0)
    )
    
    if (length(cluster_error_lines) > 0) out <- rbind(out, cluster_data)
    if (length(domino_error_lines) > 0) out <- rbind(out, domino_data)
    if (length(user_error_lines) > 0) out <- rbind(out, user_data)
    
    if(nrow(out) > 0) {
      out$File_Path <- target_file_name
    } else {
      # # Machine learning API
      # url <- model_rest_url # located in credentials.R (same with model_api_key)
      # response <- httr::POST(
      #   url,
      #   authenticate(model_api_key, model_api_key, type = "basic"),
      #   body=toJSON(list(data=list(text = target_file)), auto_unbox = TRUE),
      #   content_type("application/json")
      # )
      # predictions <- content(response)
      # predictions <- unname(unlist(predictions$result))
      # 
      # num_errors <- length(predictions[which(predictions != "none")])
      # # Identify any potential errors which are not none. If they exist, turn it into a dataframe. If not, just cbind that stuff
      # if(num_errors > 0) {
      #   error_lines <- which(predictions != "none")
      #   errors <- predictions[error_lines]
      #   error_description <- target_file[error_lines]
      #   data <- data.frame('Error'=errors, 'Line_Number'=error_lines, 'Context'=error_description)
      #   data$File_Path <- target_file_name
      #   out <- data
      # } else {
      #   out <- cbind(out, File_Path = character(0))
      # }
      out <- cbind(out, File_Path = character(0))
    }
    return(out)
    
  }) %>% do.call(rbind, .)
  
  datetime_str <- stringi::stri_extract(metadata_errors$Context, regex="\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+Z")
  metadata_errors$Date_Time <- lubridate::ymd_hms(datetime_str, tz = "UTC")
  if(nrow(metadata_errors) > 0) {
    metadata_errors$Node <- associated_node_ip
  } else {
    metadata_errors <- cbind(metadata_errors, Node = character(0))
  }
  return(metadata_errors)
}

#target_file <- paste0(data_directory, "support-bundles/65393d24b9c768436d831668.zip")
####DOWNLOAD SUPPORT BUNDLES IN PARALLEL####
unzip_single <- function(target_file) {
  # Check to see if this file is a newly downloaded zip
  is_zip <- grepl('\\.zip', target_file)
  # Extract the base name without the .zip extension
  
  # If it is, unzip into the /mnt/data/<project-name>/support-bundle/ directory named as its execution id
  if(is_zip) {
    base_name <- tools::file_path_sans_ext(basename(target_file))
    final_dest_dir <- file.path(dest_dir, base_name)
    
    # Ensure the directory exists
    if (!dir.exists(final_dest_dir)) {
      dir.create(final_dest_dir, recursive = TRUE)
      unzip(target_file, exdir = final_dest_dir)
    }
  } else {
    final_dest_dir <- target_file
  }
  
  final_dest_dir_files <- list.files(final_dest_dir, full.names=TRUE)
  
  if(length(final_dest_dir_files) > 0) {
    metadata_errors <- identify_support_bundle_errors(file_paths=final_dest_dir_files, regex_pattern_df = regex_pattern_df) %>% base::suppressWarnings()
  } else {
    metadata_errors <- data.frame(
      Error = character(0),
      Line_Number = integer(0),
      Context = character(0),
      Error_Type = character(0),
      File_Path = character(0),
      Date_Time = as.POSIXct(character(0)),  # assuming you want a datetime format
      Node = character(0)
    )
  }
  
  execution_id <- stringi::stri_extract(target_file, regex="(?<=/support-bundles/).*")
  execution_id <- gsub("\\.zip", "", execution_id)
  
  if(nrow(metadata_errors) > 0) {
    cat("Hit! Target file: ", target_file, "\n")
    metadata_errors$execution_id <- execution_id
  }

  summary_directory <- paste0(data_directory, "support-bundle-summary")
  summary_csv_path <- paste0(summary_directory, "/", execution_id, "-summary.csv")
  
  if(!dir.exists(summary_directory)) {
    dir.create(summary_directory)
  }
  write.csv(metadata_errors, summary_csv_path, row.names=FALSE)
  
  return(metadata_errors)
}