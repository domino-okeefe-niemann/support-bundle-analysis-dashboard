rm(list=ls())

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

# final_output <- c()
# for(target_file in file_paths) {
#   cat("Target File: ", target_file, "\n")
#   target_output <- unzip_single(target_file)
#   final_output <- c(final_output, target_output)
# }


unzip_create_summary_in_parallel <- function(file_paths) {
  
  # Function to unzip a single file
  unzip_single <- function(target_file) {
    # Check to see if this file is a newly downloaded zip
    is_zip <- grepl('\\.zip', target_file)
    # Extract the base name without the .zip extension
    
    if(is_zip) {
      base_name <- tools::file_path_sans_ext(basename(target_file))
      final_dest_dir <- file.path(dest_dir, base_name)
  
      # Ensure the directory exists
      if (!dir.exists(final_dest_dir)) {
        dir.create(final_dest_dir, recursive = TRUE)
      }
  
      unzip(target_file, exdir = final_dest_dir)
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
  
  # Get the number of cores available
  no_cores <- parallel::detectCores() - 1  # using one less to leave a core free
  
  # Use parLapply to unzip in parallel
  cl <- makeCluster(no_cores)
  
  dest_dir <<- paste0(data_directory, 'support-bundles')
  
  clusterExport(cl, "dest_dir")
  clusterExport(cl, "identify_support_bundle_errors")
  clusterExport(cl, 'regex_pattern_df')
  clusterExport(cl, 'data_directory')
  clusterEvalQ(cl, {
    library(magrittr)
  })
  results <- parLapply(cl, file_paths, unzip_single) %>% do.call(rbind, .)
  #zip_files <- list.files(path = dest_dir, pattern = "\\.zip$", full.names = TRUE)
  # Delete the zip files
  #file.remove(zip_files)
  stopCluster(cl)
  
  return(results)
}

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

##

domino_project_name <- "allstate_log_github"
domino_url <- 'prod-field.cs.domino.tech'
domino_user_api_key <- system("echo $DOMINO_USER_API_KEY", intern=TRUE)
data_directory <- paste0("/mnt/data/", domino_project_name, "/test_dir/")



resource_usage_url <- paste0("https://", domino_url, "/admin/generateUsageReport")
date_range <- c(as.Date(Sys.Date())-lubridate::days(7), as.Date(Sys.Date()))
date_range <- c(as.Date('2022-01-01'), as.Date(Sys.Date()))
payload <- paste0("start-date=", format(date_range[1], '%m/%d/%Y'), "&end-date=", format(date_range[2], '%m/%d/%Y'))
#payload <- "start-date=09/15/2023&end-date=09/22/2023"

headers <- c(
  'Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
  'Content-Type'= 'application/x-www-form-urlencoded',
  'X-Domino-Api-Key'= domino_user_api_key
)

response <- httr::POST(url = resource_usage_url, body=payload, add_headers(headers))
plain_text <- base::rawToChar(response$content)
report_values <- read.csv(textConnection(plain_text))


all_executions <- report_values$Run.id[which(report_values$Status %in% c("Failed", "Error"))]

# The support bundle directories are names via their execution ids
existing_bundles <- list.dirs(paste0(data_directory, "support-bundles/"), full.names=FALSE)
#download_list <- list.dirs('/mnt/data/allstate_log_github/support-bundles/', full.names=FALSE)[1:1000]
#download_list <- stringi::stri_extract(download_list, regex="(?<=support-bundle-).*")[2:length(download_list)]
download_list <- setdiff(all_executions, existing_bundles)
regex_pattern_df <- read.csv(paste0(data_directory, 'regex_lookup.csv'))

# Download the files via async
urls <- paste0("https://", domino_url, "/v4/admin/supportbundle/", download_list)
headers <- c("X-Domino-Api-Key" = domino_user_api_key,
             "accept" = "application/json")


#num_batches <- 
#ceiling(length(urls) / num_batches)
x <- 120
indices <- (seq_along(urls) - 1) %/% x + 1
# Split the URLs based on the group numbers
url_list <- split(urls, indices)

a <- Sys.time()
for (idx in seq_along(url_list)) {
  target_url <- url_list[[idx]]
  cat("Progress: ", base::round(idx/length(url_list)*100, 0), "%\n")
  cc <- crul::Async$new(
    urls = target_url,
    headers = headers
  )
  
  execution_ids <- sapply(target_url, function(singles) stringi::stri_extract(singles, regex="(?<=supportbundle\\/).*")) %>% as.vector()
  
  support_bundle_dir <- paste0(data_directory, 'support-bundles/')
  if(!dir.exists(support_bundle_dir)) {
    dir.create(support_bundle_dir)
  }
  
  
  zip_paths <- paste0(support_bundle_dir, execution_ids,".zip")
  res <- cc$get(disk=zip_paths)
}


all_file_paths <- list.files(paste0(data_directory, "support-bundles/"), full.names=TRUE)
a <- Sys.time()
out <- unzip_create_summary_in_parallel(file_paths=all_file_paths)
b <- Sys.time()



write.csv(out, '/mnt/data/allstate_log_github/error_analysis_all_time.csv', row.names=FALSE)

