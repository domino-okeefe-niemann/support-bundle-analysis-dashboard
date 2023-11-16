#### SERVER.R ####
# Define the server logic
server <- function(input, output, session) {
  current_error_type <- reactiveVal("All")
  desired_execution_id <- reactiveVal(NULL)
  
  # Function to get the list of files (to be used by reactivePoll)
  getDirList <- function() {
    bundles_summary_directory <- paste0(data_directory, "support-bundle-summary/")
    list.files(path = bundles_summary_directory)
  }
  
  # Create a reactivePoll that checks the list of files every 1 second (1000 ms)
  file_list <- reactivePoll(1000, session,
                            checkFunc = function() {
                              # Use digest to create a hash that will change if the file list changes
                              digest::digest(getDirList())
                            },
                            valueFunc = function() {
                              # Return the current list of files
                              getDirList()
                            }
  )
  
  # Create a reactivePoll that checks the list of files every 1 second (1000 ms)
  fileList <- reactivePoll(1000, session,
                           checkFunc = function() {
                             # Use digest to create a hash that will change if the file list changes
                             digest::digest(getDirList())
                           },
                           valueFunc = function() {
                             # Return the current list of files
                             getDirList()
                           }
  )
  
  ##### RESOURCE USAGE REPORT ####
  reactiveDate <- shiny::reactive({
    invalidateLater(1000 * 60 * 60) # Check each hour for a date update (since application will only open once!)
    Sys.Date()
  })
  
  output$generate_report_btn_ui <- shiny::renderUI({
    shiny::actionButton("generate_report_btn", 
                        "Generate Report", 
                        style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
  })
  
  output$date_range_ui <- shiny::renderUI({
    if(!is.null(reactiveDate())) {
      shiny::dateRangeInput(inputId = "date_range_select",
                            label = "Date Range",
                            start = as.Date(reactiveDate() - lubridate::days(7)),
                            end = as.Date(reactiveDate()),
                            width = "100%",
                            min = as.Date('2020-01-01'),
                            max = as.Date(reactiveDate())
      )
    }
  })
  
  output$status_multiselect_ui <- shiny::renderUI({
    if(!is.null(precreated_usage_report_filters)) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "status_multiselect",
                                label = "Workload Status",
                                choices = c("All", setdiff(unlist(precreated_usage_report_filters$status), NA)),
                                selected = c("Error", "Failed"),
                                multiple = TRUE)
      )
    }
  })
  
  output$hardware_tier_multiselect_ui <- shiny::renderUI({
    if(!is.null(precreated_usage_report_filters)) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "hardware_tier_multiselect",
                                label = "Hardware Tier",
                                choices = c("All", setdiff(unlist(precreated_usage_report_filters$hardware_tier), NA)),
                                selected = "All",
                                multiple = TRUE)
      )
    }
  })
  
  ##### Initially hidden filters: these will stay hidden until the initial report is updated.
  # The idea here is that the user will generate a report and then include all users for that relevant timestep.
  # To prevent a bunch of API pulls, there will only be a complete rerendering if the date select range is changed
  output$starting_user_multiselect_ui <- shiny::renderUI({
    if(!is.null(report_df0())) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "starting_user_multiselect",
                                label = "User",
                                choices = c("All", setdiff(report_df0()$starting_user, NA)),
                                selected = "All",
                                multiple = TRUE)
      )
    }
  })
  
  output$project_name_multiselect_ui <- shiny::renderUI({
    if(!is.null(report_df0())) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "project_name_multiselect",
                                label = "Project Name",
                                choices = c("All", setdiff(report_df0()$project_name, NA)),
                                selected = "All",
                                multiple = TRUE)
      )
    }
  })
  
  output$target_col_select_ui <- shiny::renderUI({
    if(!is.null(report_df0())) {
      target_cols_choices <- names(report_df0())[sapply(report_df0(), function(col) length(unique(col)) <= 20 & !is.numeric(col))]
      names(target_cols_choices) <- lapply(target_cols_choices, function(x) toTitleCase(gsub("_", " ", x))) %>% stats::setNames(target_cols_choices)
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "target_col_select",
                                label = "Secondary Category",
                                choices = target_cols_choices,
                                selected = "status",
                                multiple = FALSE)
      )
    }
  })
  
  output$usage_report_filters <- shiny::renderUI({
    list(
      shiny::uiOutput("date_range_ui"),
      shiny::uiOutput("status_multiselect_ui"),
      shiny::uiOutput("hardware_tier_multiselect_ui"),
      shiny::uiOutput("starting_user_multiselect_ui"),
      shiny::uiOutput("project_name_multiselect_ui"),
      shiny::uiOutput("target_col_select_ui"),
      shiny::HTML("<br>"),
      shiny::fluidRow(
        column(10, shiny::uiOutput("generate_report_btn_ui")),
      )
    )
  })
  
  
  usage_filters <- shiny::reactiveValues(show_contents=FALSE)
  
  output$show_contents <- reactive({usage_filters$show_contents})
  outputOptions(output, "show_contents", suspendWhenHidden = FALSE)
  
  ##### RESOURCE USAGE REPORT FILTERS #####
  # Observes the clicking of the "generate report" button. Can be seen as a "gatekeeper" to hold off the update of 
  # filters until the user selects "generate report".
  
  shiny::observeEvent(input$generate_report_btn, {
      if(usage_filters$show_contents == FALSE) {
        usage_filters$show_contents = TRUE
        usage_filters$start_date <- input$date_range_select[1]
        usage_filters$end_date <- input$date_range_select[2]
        usage_filters$status <- input$status_multiselect
        usage_filters$hardware_tier <- input$hardware_tier_multiselect
        usage_filters$starting_user <- "All"
        usage_filters$project_name <- "All"
        usage_filters$target_col <- "status"
        #cat("DATE RANGE SELECTED!", paste(usage_filters$start_date, usage_filters$end_date), "\n")
        shinyjs::addClass(id="generate_report_btn", class="grayed-out-button", asis=TRUE)
        #cat("Updated the button\n")
        #browser()
      } else {
        usage_filters$start_date <- input$date_range_select[1]
        usage_filters$end_date <- input$date_range_select[2]
        usage_filters$status <- input$status_multiselect
        usage_filters$hardware_tier <- input$hardware_tier_multiselect
        usage_filters$starting_user <- input$starting_user_multiselect
        usage_filters$project_name <- input$project_name_multiselect
        usage_filters$target_col <- input$target_col_select
        shinyjs::addClass(id="generate_report_btn", class="grayed-out-button", asis=TRUE)
      }
      # usage_report_filters <- base::readRDS('./data/usage_report_filter_options.rds')
      output$bundle_error_count <- renderUI({
        fluidRow(
          bs4Dash::bs4Card(
            width = 12, # Maximum width to fill the container
            maximizable = TRUE,
            # Action Buttons
              fluidRow(
                align = "center", 
                column(
                  width = 6,
                  actionButton("analyze_support_bundles", 
                               "Analyze Support Bundles",
                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                               width = "100%")
                ),
                column(
                  width = 6, 
                  actionButton("reset_category_filter", 
                               "Reset Secondary Category Filter",
                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                               width = "100%")
                )
              ),
            br(),
            fluidRow(
              align = "center",
              column(
                width = 6,
                div(class = "inline-radio",
                    shiny::radioButtons("use_cached_analysis", 
                                        "Use Cached Analytics From Previous Analyses?",
                                        choices = c("Yes"=TRUE, "No"=FALSE))
                )
              ),
              column(
                width = 6,
                div(class = "inline-radio",
                    shiny::radioButtons("use_ml_model",
                                        "Use Model to Identify Errors?",
                                        choices = c("Yes"=TRUE, "No"=FALSE),
                                        selected = FALSE
                    )
                )
              )
            ),
            HTML("<br>"),
            # DTOutput
            fluidRow(width=12,
                     align = "center",
                     DT::DTOutput("report_render"),
                     HTML("<br>"),
                     shiny:: downloadButton("report_render_downloader", "Download")
            )
          )
        )
      })
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$date_range_select, 
                 input$hardware_tier_multiselect, 
                 input$status_multiselect, 
                 input$starting_user_multiselect, 
                 input$project_name_multiselect, 
                 input$target_col_select), {
                   shinyjs::removeClass(id="generate_report_btn", class="grayed-out-button", asis=TRUE)
                 })
  
  ##### RESOURCE USAGE REPORT DATA #####
  report_df0 <- shiny::reactive({
    if (!is.null(usage_filters$start_date) & !is.null(usage_filters$end_date)) {
      # Create a list of dates
      #browser()
      date_list <- seq(usage_filters$start_date, usage_filters$end_date, by="days")
      
      # Identify all which already exist in the directory "{dataset_path}/resource-usage-by-day"
        # Import and upload all that exist
      existing_dates <- list.files(paste0(data_directory, "resource-usage-by-day/"))
      
      #existing_dates <- c("2023-09-29.csv", "2023-09-30.csv", "2023-10-02.csv")
      existing_dates <- stringi::stri_extract(existing_dates, regex="[0-9]{4}-[0-9]{2}-[0-9]{2}(?=\\.csv)")
      
      # Take a setdiff to find dates which need to be downloaded
      dates_to_download <- setdiff(as.character(date_list), existing_dates)
      
      # Create a list of start/end dates to minimize redundant data pulled for API
      date_vector <- as.Date(dates_to_download, format = "%Y-%m-%d")
      
      # Calculate differences between dates
      diff_dates <- c(0, diff(date_vector))
      
      # Split the vector based on consecutive days
      split_idx <- cumsum(diff_dates != 1)
      
      # Split date_vector based on split_idx
      date_lists <- split(date_vector, split_idx)
      date_lists <- lapply(date_lists, function(date) {
        if(length(date) == 1){
          return (rep(date, 2))
        } else {
          return(date)
        }
      })
      ##### Loop through each date range, pulling from the API #####
      report_df_api <- data.frame()
      
      # Edge case: The above returns a list of length 1 with an empty Date entry (registers as length 0) when no dates are found
      if(length(date_lists[[1]]) > 0) {
        for (idx in seq_along(date_lists)) {
          date_range <- c(date_lists[[idx]][1], date_lists[[idx]][length(date_lists[[idx]])])
          report_df_slice <- download_usage_report_from_api(date_range)
          report_df_api <- rbind(report_df_api, report_df_slice)
        }
      }
      
      ##### Feature Engineering #####
      if(nrow(report_df_api) > 0){
        report_df_api <- resource_usage_feature_engineering_pipeline(report_df_api)
      }
      
      # Don't really know what to do with faulty dates so I'm just gonna toss for now (very few occurences)
      report_df_api <- report_df_api[which(report_df_api$date %in% date_list),]
      
      ##### Read in cached usage reports #####
      report_df_csv <- lapply(existing_dates, function(date) {
        date_csv <- read.csv(paste0(data_directory, "resource-usage-by-day/", date, ".csv"))
      }) %>% do.call(rbind, .)
      
      if(!is.null(report_df_csv)) {
        if(length(setdiff(colnames(report_df_api), colnames(report_df_csv))) > 0){
          report_df_csv <- resource_usage_feature_engineering_pipeline(report_df_csv)
        }
      }
      
      # Save newly found dates into csv format by day
      date_splits <- split(report_df_api, report_df_api$date)
      lapply(seq_along(date_splits), function(idx) {
        date_df <- date_splits[[idx]]
        target_file_path <- paste0(data_directory, "resource-usage-by-day/", unique(date_df$date), ".csv")
        
        # Edge case: newest download of from API gives us new information (going to usually be on most recent day)
        # Issue: In saving this, data future pulls of the API will mistakenly be looked over as there is already a csv
        # Solution: Potentially hash each row and use as key? We're then getting into DB territory...
        # if(file.exists(target_file_path)) {
        #   existing_file <- read.csv(target_file_path)
        #   new_files_found <- setdiff(date_df$run_id, existing_file$run_id)
        #   if(length(new_files_found) > 0) {
        #     date_df <- rbind(existing_file, date_df[which(date_df$run_id %in% new_files_found),])
        #   }
        # }
        
        write.csv(date_df, paste0(data_directory, "resource-usage-by-day/", unique(date_df$date), ".csv"), row.names = FALSE)
      })
      
      # Bind the results together (if there was any cached data)
      if(!is.null(report_df_csv)) {
        report_df_api <- rbind(report_df_csv, report_df_api)
      }
      
      # Chuck out any weird data and order by date
      report_df_api <- report_df_api[order(report_df_api$date),]
      report_df_api <- report_df_api[which(report_df_api$date %in% date_list),]
      
      
      # Convert continuous variable "time_to_boot_s" and transform into categorical variable of form "Xs to Ys"
      # This is NOT in feature engineering function as this is not a mapped feature (changes depending on dataset)
      #browser()
      n <- 5
      breaks_range <- cut(report_df_api$time_to_boot_s, breaks=n, include.lowest=TRUE)
      no_brackets <- gsub("\\[|\\]", "", breaks_range)
      no_parenthesis <- gsub("\\(|\\)", "", no_brackets)
      no_negative_time <- gsub("-[0-9]+\\.[0-9]+", "0", no_parenthesis)
      # 2. Split string at comma
      entry <- no_negative_time[1]
      output_string <- lapply(no_negative_time, function(entry) {
        numbers <- strsplit(entry, ",")[[1]]
        modified_numbers <- paste0(numbers, "s")
        output_string <- paste(modified_numbers[1], "to", modified_numbers[2])
      }) %>% unlist()
      report_df_api$time_to_boot <- output_string
      
      
      
      return(report_df_api)
    }
  })
  
  report_df <- shiny::reactive({
    if(!is.null(report_df0())) {
      report_df <- report_df0()
      #browser()
      
      if(!is.null(usage_filters$status) & !("All" %in% usage_filters$status)) {
        report_df <- report_df[which(report_df$status %in% usage_filters$status),]
      }
      if(!is.null(usage_filters$hardware_tier) & !("All" %in% usage_filters$hardware_tier)) {
        report_df <- report_df[which(report_df$hardware_tier %in% usage_filters$hardware_tier),]
      }
      if(!is.null(usage_filters$starting_user) & !("All" %in% usage_filters$starting_user)) {
        report_df <- report_df[which(report_df$starting_user %in% usage_filters$starting_user),]
      }
      if(!is.null(usage_filters$project_name) & !("All" %in% usage_filters$project_name)) {
        report_df <- report_df[which(report_df$project_name %in% usage_filters$project_name),]
      }
      
      shiny::validate(
        need(nrow(report_df) > 0, "Selected filters yield no results. Please try with a different selection.")
      )
      # Recalculate the "time to boot." We want this to be done each time the filters are set since
      # the bins change depending on report_df subset. The final target_col conditional is done after
      # so the newest calculations of "time_to_boot" is used.
      n <- 3
      #summary(report_df$time_to_boot_s)
      #breaks <- seq(min(report_df$time_to_boot_s), max(report_df$time_to_boot_s), length.out = n + 1)
      #browser()
      Q1 <- quantile(report_df$time_to_boot_s, 0.25)
      Q3 <- quantile(report_df$time_to_boot_s, 0.75)
      #IQR <- Q3 - Q1
      Lower_Bound <- min(report_df$time_to_boot_s)
      Upper_Bound <- max(report_df$time_to_boot_s)
      
      # Evenly spaced bins between Q1 and Q3
      bins <- seq(Q1, Q3, length.out = n + 1)
      
      # Include outliers bins
      breaks <- unique(unname(c(Lower_Bound, bins, Upper_Bound)))
      
      if(length(breaks) > 1) {
        # Bin the data using the defined breaks
        breaks_range <- cut(report_df$time_to_boot_s, breaks=breaks, include.lowest=TRUE, right=FALSE)
        no_brackets <- gsub("\\[|\\]", "", breaks_range)
        no_parenthesis <- gsub("\\(|\\)", "", no_brackets)
        no_negative_time <- gsub("-[0-9]+\\.[0-9]+", "0", no_parenthesis)
        # 2. Split string at comma
        entry <- no_negative_time[1]
        output_string <- lapply(no_negative_time, function(entry) {
          numbers <- strsplit(entry, ",")[[1]]
          modified_numbers <- sapply(numbers, function(x) {
            base::ifelse(as.numeric(x) >= 60, paste0(as.character(base::round(as.numeric(x)/60, 1)), "min"), paste0(x, "s"))
          })
          #modified_numbers <- paste0(numbers, "s")
          output_string <- paste(modified_numbers[1], "to", modified_numbers[2])
        }) %>% unlist()
        report_df$time_to_boot <- output_string
      } else {
        report_df$time_to_boot <- base::ifelse(as.numeric(report_df$time_to_boot_s) >= 60, paste0(as.character(base::round(as.numeric(report_df$time_to_boot_s)/60, 1)), "min"), paste0(report_df$time_to_boot_s, "s"))
      }
      
      
      if(!is.null(usage_filters$target_col)) {
        report_df$target_col <- report_df[[usage_filters$target_col]]
      }

      return(report_df)
    }
  })
  
  output$report_render <- DT::renderDT({
        df <- report_df()
        df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
        first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
        last_cols <- setdiff(colnames(df), first_cols)
        df <- df[,c(first_cols, last_cols)]
        colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
        colnames(df)
  
        DT::datatable(df,
                      rownames = FALSE,
                      filter = "top",
                      class = "compact",
                      selection = list(target = 'row', selected = 'indices'),
                      extensions = c("Buttons", "Scroller"),
                      options = list(
                        scrollX = TRUE,
                        sScrollY = '75vh',
                        scrollCollapse = TRUE,
                        dom = 'Bfrtip',
                        buttons = c('colvis'),
                        initComplete = JS(
                          "function(settings) {",
                          "$(settings.nTHead).css('background-color', '#f5f5f5');",
                          "}"
                        )
                      )
        )
  })
  
  observe({
    # JavaScript which adds class into colvis button such that it's scrollable rather than a long list of buttons
    script <- "
      $(document).on('click', '.dt-button', function() {
        setTimeout(function() {
          $('.dt-button-collection').addClass('limit-height');
        }, 10);
      });
    "
    session$sendCustomMessage(type = 'jsCode', message = script)
  })
  
  output$report_render_downloader <- downloadHandler(
    filename = function() {
      paste0("resource-usage-report-captured-",Sys.Date(),".csv")
    },
    content = function(con) {
      if(!is.null(report_df())) {
        df <- report_df()
        colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
        write.csv(df, con, row.names = FALSE)
        #return(df)
      }
    }
  )
  
  ##### VISUALS/AGGREGATION TABLES #####
  # Donut Chart of Run Types (Batch, Endpoint, Notebook)
  report_subset_agg_dt <- shiny::reactive({
    if(!is.null(report_df())) {
      #browser()
      df <- data.table(report_df())
      
      known_counts <- table(df$target_col)
      known_counts["unknown"] <- nrow(df) - sum(known_counts)
      #browser()
      report_subset_df <- data.frame(
        target_col = names(known_counts),
        count = as.vector(known_counts)
      )
      
      report_subset_df <- report_subset_df[which(report_subset_df$count > 0),]
      report_subset_df$color <- viridis::viridis(nrow(report_subset_df))
      report_subset_df$perc <- base::round(report_subset_df$count / sum(report_subset_df$count)*100, 1)
      return(report_subset_df)
    }
  })
  
  output$run_type_donut_chart <- highcharter::renderHighchart({
    if(!is.null(report_subset_agg_dt()) & !is.null(usage_filters$target_col) &!is.null(usage_filters$start_date) & !is.null(usage_filters$end_date)) {
      #browser()
      highchart() %>%
        hc_tooltip(formatter= JS("function () { return '<b>' + this.point.target_col + '</b> ' +
                                     ' <br /> Proportion: ' + this.point.perc + '%' +
                                     ' <br /> Count: ' + this.point.count
                                                                  ;}")) %>%
        highcharter::hc_add_series(type = "pie", data = report_subset_agg_dt(), hcaes(x=target_col, y=perc, color=color), size = "80%", name = "Percentage:", 
                                   innerSize = "60%", id = 'versions') %>%
        hc_title(text=HTML(paste0(tools::toTitleCase(gsub("_", " ", usage_filters$target_col))))) %>%
        hc_subtitle(text = paste0(format(usage_filters$start_date, "%B %d, %Y"), "-", format(usage_filters$end_date, "%B %d, %Y"))) %>%
        highcharter::hc_plotOptions(
          pie=list(
            dataLabels=list(
              enabled=TRUE, 
              style=list(
                fontSize="1.4em"), 
              formatter = JS("function () {
                             // display only if larger than 1
                            return this.y > 1 ? '<b>' + this.point.name + ':</b> ' +
                            this.y + '%' : null; }"))),
          series = list(
            cursor = "pointer",
            point = list(
              events = list(
                click = JS("function() {
                                            Shiny.setInputValue('clicked_target_col_val_donut', this.target_col);
                                        }"
                )
              )
            )
          )
        )
      
    }
  })
  
  # shiny::observeEvent(input$clicked_target_col_val_donut, {
  #   cat("CLICKED!!! ", input$clicked_target_col_val_donut, "\n")
  # })
  
  # Time series of running processes
  usage_time_series_df <- shiny::reactive({
    if(!is.null(report_df())){
      #browser()
      target_df <- report_df()
      
      dt1 <- strptime(max(na.omit(lubridate::ymd_hms(target_df$started_time))), format="%Y-%m-%d %H:%M:%S")
      dt2 <- strptime(min(na.omit(lubridate::ymd_hms(target_df$completed_time))), format="%Y-%m-%d %H:%M:%S")
      time_diff <- difftime(dt1, dt2, units="secs")
      # If the largest break is minute, go in secs,
      if(time_diff < 60*60*24*7) {
        time_units <- "hour"
        num_secs <- 60*60
      } else {#if (time_diff < 60*60*24*30) {
        time_units <- "day"
        num_secs <- 60*60*24
      }
      
      
      target_df$started_time[which(is.na(target_df$started_time) | target_df$started_time == "")] <- target_df$queued_time[which(is.na(target_df$started_time) | target_df$started_time == "")]
      target_df$started_time_rounded <- lubridate::ymd_hms(target_df$started_time) %>% floor_date(., unit= time_units)
      target_df$completed_time_rounded <- lubridate::ymd_hms(target_df$completed_time) %>% ceiling_date(., unit= time_units)
      target_df$time_diff <- target_df$completed_time_rounded - target_df$started_time_rounded
      target_df$completed_time_rounded <- sapply(seq_along(target_df$completed_time_rounded), function(idx, time_units) {
        time_diff <- target_df$time_diff[[idx]]
        target_end <- target_df$completed_time_rounded[[idx]]
        if(time_diff == 0) {
          target_end <- target_end + lubridate::seconds(num_secs)
        }
        
        return(target_end)
      },
      time_units=time_units) %>% as.POSIXct(.)
      # target_df$time_diff <- base::ifelse(target_df$time_diff > 0, target_df$time_diff, 3600)
      # target_df$run_type <- base::ifelse(
      #                target_df$is_batch, "batch",
      #                   base::ifelse(target_df$is_notebook, "notebook",
      #                                   base::ifelse(target_df$is_endpoint, "endpoint", "unknown"))
      #                 )
      
      dt <- data.table(target_df)
      
      start_time_agg <- dt[, .(start_count = .N), by=.(started_time_rounded, target_col)]
      complete_time_agg <- dt[, .(complete_count = .N), by=.(completed_time_rounded, target_col)]
      
      
      date_time <- seq(min(na.omit(target_df$started_time_rounded)), max(na.omit(target_df$completed_time_rounded)), by=paste0("1 ", time_units))
      
      target_col_unique <- as.vector(unname(unique(target_df$target_col)))
      
      #run_type <- unique(target_df$run_type)
      out <- data.table::CJ(date_time, target_col_unique)
      
      out <- data.table::merge.data.table(out, start_time_agg, by.x=c("target_col_unique", "date_time"), by.y=c("target_col", "started_time_rounded"), all.x = TRUE)
      out <- data.table::merge.data.table(out, complete_time_agg, by.x=c("target_col_unique", "date_time"), by.y=c("target_col", "completed_time_rounded"), all.x = TRUE)
      out[is.na(out)] <- 0
      
      target_col_partition <- split(out, out$target_col_unique)
      idx <- 1
      new_out <- lapply(seq_along(target_col_partition), function(idx) {
        out_subset <- target_col_partition[[idx]]
        out_subset$start_count_cum <- cumsum(out_subset$start_count)
        out_subset$end_count_cum <- cumsum(out_subset$complete_count)
        out_subset$running_during_timestamp <- out_subset$start_count_cum - out_subset$end_count_cum
        
        return(out_subset)
      }) %>% do.call(rbind, .)
      

      is_negative <- tryCatch({
        length(which(new_out$running_during_timestamp < 0)) == 0
      }, error = function(e) FALSE)
      
      shiny::validate(
        need(is_negative, "Oops! Aggregations yielded a negative time duration.")
      )
      
      
      new_out$timestamp <- as.numeric(new_out$date_time) * 1000
      new_out$date_time <- as.POSIXct(new_out$date_time, origin="1970-01-01", tz="UTC")
      # browser()
      
      # Because the above aggregation rely on "ceiling rounding" for closing date, some of the close times may round
      # to the next day. This does not make sense when looking at anything in the future (these should just be seen as being open at time of aggregation)
      new_out <- new_out[which(new_out$date_time <= Sys.Date()),]
      return(new_out)
    }
  })
  
  output$rolling_count_running_processes_time_series <- highcharter::renderHighchart({
    if(!is.null(usage_time_series_df())) {
      target_df <- usage_time_series_df()
      
      # time_diff <- difftime(target_df$date_time[2], target_df$date_time[1], unit='hours')
      # 
      # if (time_diff = 1) {
      #   time_unit <- "hours"
      # } else if(time_diff == 24) {
      #   time_unit <- "days"
      # }
      # browser()
      hc <- target_df %>%
        hchart(
          "area",
          hcaes(x=timestamp, y=running_during_timestamp, group=target_col_unique, opacity=0.7),
          stacking="normal"
        ) %>%
        hc_colors(viridis::viridis(length(unique(target_df$target_col_unique)))) %>%
        hc_title(text=paste0("Historical Runtimes")) %>%
        hc_xAxis(title=list(text='Date'),
                 type ="datetime", 
                 labels = list(
                   format = '{value:%Y-%b-%e<br> %H:%M}'
                 )
        ) %>%
        hc_yAxis(title=list(text='Running Processes')) %>%
        hc_chart(
          zoomType = "xy") %>%
        hc_plotOptions(
          series = list(
            cursor = "pointer",
            point = list(
              events = list(
                click = JS("function() {
                  Shiny.setInputValue('clicked_target_col_val', this.target_col_unique);
                  Shiny.setInputValue('clicked_target_col_date', this.timestamp);
                }")
              )
            )
          )
        )
      
      return(hc)
    }
  })
  
  ##### FILTERING VIA CHART CLICKS #####
  # Every time a category is selected on the charts, the following code captures that category and filters
  # the usage report datatable render via proxy
  last_update <- shiny::reactiveVal("none")
  
  shiny::observeEvent(c(input$clicked_target_col_val, input$clicked_target_col_date), {
    if(!is.null(input$clicked_target_col_val)) {
      
      #cat("Made it into the clicked_target_col_val for the data table proxy\n")
      proxy <- dataTableProxy("report_render")
      #browser()
      #cat("Selected Date:", format(as.POSIXct(input$clicked_target_col_date/1000), format="%Y-%m-%dT%H:%M:%OS3Z"), "\n")
      target_timestamp <- input$clicked_target_col_date
      target_df <- report_df()
      target_df <- target_df[which(target_df$target_col == input$clicked_target_col_val),]
      
      dt1 <- strptime(max(na.omit(lubridate::ymd_hms(target_df$started_time))), format="%Y-%m-%d %H:%M:%S")
      dt2 <- strptime(min(na.omit(lubridate::ymd_hms(target_df$completed_time))), format="%Y-%m-%d %H:%M:%S")
      time_diff <- difftime(dt1, dt2, units="secs")
      # If the largest break is minute, go in secs,
      if(time_diff < 60*60*24*7) {
        time_units <- "hour"
        num_secs <- 60*60
      } else {#if (time_diff < 60*60*24*30) {
        time_units <- "day"
        num_secs <- 60*60*24
      }
      
      target_df$started_timestamp_rounded <- as.numeric(lubridate::ymd_hms(target_df$started_time) %>% floor_date(., unit=time_units))*1000
      target_df$completed_timestamp_rounded <- as.numeric(lubridate::ymd_hms(target_df$completed_time) %>% ceiling_date(., unit=time_units))*1000
      
      
      target_df$completed_timestamp_rounded <- sapply(seq_along(target_df$completed_timestamp_rounded), function(idx, time_units) {
        target_begin <- target_df$started_timestamp_rounded[[idx]]
        target_end <- target_df$completed_timestamp_rounded[[idx]]
        if(target_begin == target_end) {
          target_end <- as.numeric(target_end/1000 + lubridate::seconds(num_secs))*1000
        }
        
        return(target_end)
      },
      time_units=time_units)
      
      target_df <- target_df[which(target_df$started_timestamp_rounded <= target_timestamp & target_df$completed_timestamp_rounded > target_timestamp),]
      
      target_df$started_timestamp_rounded <- NULL
      target_df$completed_timestamp_rounded <- NULL
      rownames(target_df) <- NULL
      target_df <- target_df[, which(!(colnames(target_df) %in% c("date", "target_col")))]
      first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
      last_cols <- setdiff(colnames(target_df), first_cols)
      target_df <- target_df[,c(first_cols, last_cols)]
      colnames(target_df) <- gsub("_", " ", colnames(target_df)) %>% tools::toTitleCase()
      
      replaceData(proxy, target_df, resetPaging = FALSE, rownames=FALSE)
      
      last_update("time_series")
    }
  })
  
  shiny::observeEvent(input$clicked_target_col_val_donut, {
    if(!is.null(input$clicked_target_col_val_donut)) {
      #browser()
      cat("Made it into the clicked_target_col_val_donut for the data table proxy\n")
      proxy <- dataTableProxy("report_render")
      df <- report_df()
      df <- df[which(df$target_col == input$clicked_target_col_val_donut),]
      #rownames(df) <- NULL
      df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
      first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
      last_cols <- setdiff(colnames(df), first_cols)
      df <- df[,c(first_cols, last_cols)]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
      
      replaceData(proxy, df, resetPaging = FALSE, rownames=FALSE)
      
      last_update("donut")
    }
  })
  
  shiny::observeEvent(input$reset_category_filter, {
    proxy <- dataTableProxy("report_render")
    #browser()
    df <- report_df()
    df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
    first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
    last_cols <- setdiff(colnames(df), first_cols)
    df <- df[,c(first_cols, last_cols)]
    colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
    #colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
    replaceData(proxy, df, resetPaging = FALSE, rownames=FALSE)
    
    # Created for the edge case that the same category is clicked after the reset (since rerender is reactive on a DIFFERENT value being taken on by last_update)
    last_update("none")
    
    proxy <- highcharter::highchartProxy("rolling_count_running_processes_time_series")
    hcpxy_redraw(proxy)
    runjs("Shiny.setInputValue('clicked_target_col_val', null);")
    runjs("Shiny.setInputValue('clicked_target_col_date', null);")
    runjs("Shiny.setInputValue('clicked_target_col_val_donut', null);")
  })
  
  metadata_df <- reactiveVal(NULL)
  
  selected_execution_ids <- reactiveVal(NULL)
  support_button_counter <- reactiveVal(0)
  
  ##### Bundle Analysis Pipeline #####
  shiny::observeEvent(input$analyze_support_bundles, {
    # Update the uiOutput
    #cat("This is what the support bundle click is: ", support_button_counter(), "\n")
    output$bundle_error_count <- renderUI({
      if (support_button_counter()  > 0 & !is.null(execution_ids_no_errors())) {  # Adjust the condition as per your needs
        fluidRow(
          bs4Dash::bs4Card(
            maximizable = TRUE,
            width = 6, # Maximum width to fill the container
            # Action Buttons
            fluidRow(
              align = "center",
              column(
                width = 6,
                actionButton("analyze_support_bundles",
                             "Analyze Support Bundles",
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              ),
              column(
                width = 6,
                actionButton("reset_category_filter",
                             "Reset Secondary Category Filter",
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              )
            ),
            fluidRow(
              align = "center",
              column(
                width = 6,
                div(class = "inline-radio",
                    shiny::radioButtons("use_cached_analysis",
                                        "",
                                        choices = c("Use Cached Analytics"=TRUE, "Recreate Error Analysis"=FALSE))
                )
              ),
              column(
                width = 6,
                div(class = "inline-radio",
                    shiny::radioButtons("use_ml_model",
                                        "Use Model to Identify Errors?",
                                        choices = c("Yes"=TRUE, "No"=FALSE),
                                        selected = FALSE
                    )
                )
              )
            ),
            HTML("<br>"),
            # DTOutput
            fluidRow(width=10,
                     align = "center",
                     DT::DTOutput("report_render", width="95%"),
                     HTML("<br>"),
                     shiny:: downloadButton("report_render_downloader", "Download")
            )
          ),
          bs4Dash::bs4TabCard(
            maximizable = TRUE,
            width= 6,
            #style = "width: 50%; margin-left: auto; margin-right: auto;",
            shiny::tabPanel("Summary",
                            shiny::uiOutput("executions_with_errors_progress_bar"),
                            br(),
                            shinycssloaders::withSpinner(highcharter::highchartOutput(height = "600px", "error_summary_bar_chart")),
                            br(),
                            textAreaInput("executions_no_errors_found", "Execution Ids without Found Errors", value = execution_ids_no_errors(), rows = length(unlist(execution_ids_no_errors()))+1),
                            textAreaInput("executions_failed_to_download", "Support Bundle Download Failures", value = execution_ids_failed_downloads_str(), rows = length(unlist(execution_ids_failed_downloads_str()))+1)
                            ),
            shiny::tabPanel(height = "600px", "Data",
                            DT::DTOutput("error_render"),
                            HTML("<br>"),
                            shiny:: downloadButton("error_render_downloader", "Download")
                            ),
            shiny::tabPanel("Context", htmlOutput("error_row_contents"))
          )
        )
      } else {
        fluidRow(
          bs4Dash::bs4Card(
            width = 12, # Maximum width to fill the container
            maximizable=TRUE,
            # DTOutput
            # Action Buttons
            fluidRow(
              align = "center",
              column(
                width = 6,
                actionButton("analyze_support_bundles",
                             "Analyze Support Bundles",
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              ),
              column(
                width = 6,
                actionButton("reset_category_filter",
                             "Reset Secondary Category Filter",
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              )
            ),
            fluidRow(
              align = "center",
              column(
                width = 6,
                div(class = "inline-radio",
                    shiny::radioButtons("use_cached_analysis",
                                        "",
                                        choices = c("Use Cached Analytics"=TRUE, "Recreate Error Analysis"=FALSE))
                )
              ),
              column(
                width = 6,
                div(class = "inline-radio",
                    shiny::radioButtons("use_ml_model",
                                        "Use Model to Identify Errors?",
                                        choices = c("Yes"=TRUE, "No"=FALSE),
                                        selected = FALSE
                    )
                )
              )
            ),
            fluidRow(width=10,
                     align = "center",
                     DT::DTOutput("report_render", width="95%"),
                     HTML("<br>"),
                     shiny:: downloadButton("report_render_downloader", "Download")
            )
          )
        )
      }
    })

    support_button_counter(support_button_counter() + 1)


    #proxy <- dataTableProxy("report_render")
    # selectRows(proxy = proxy,
    #            selected = input$table_rows_current)
    if(last_update() == "donut") {
      row_subset <- report_df()[which(report_df()$target_col == input$clicked_target_col_val_donut),]
    } else if (last_update() == "time_series") {
      row_subset <- report_df()
      row_subset <- row_subset[which(row_subset$target_col == input$clicked_target_col_val),]

      target_timestamp <- input$clicked_target_col_date
      row_subset$started_timestamp_hour <- as.numeric(lubridate::ymd_hms(row_subset$started_time) %>% floor_date(., unit="hour"))*1000
      row_subset$completed_timestamp_hour <- as.numeric(lubridate::ymd_hms(row_subset$completed_time) %>% ceiling_date(., unit="hour"))*1000
      row_subset <- row_subset[which(row_subset$started_timestamp_hour <= target_timestamp & row_subset$completed_timestamp_hour >= target_timestamp),]
    } else if (last_update() == "none") {
      row_subset <- report_df()
    }
    #browser()
    target_rows <- input$report_render_rows_selected
    target_rows <- target_rows[which(!is.na(sapply(target_rows, as.numeric)))] %>% as.numeric()
    if(length(target_rows) == 0) {
      selected_execution_ids(unique(row_subset$run_id))
    } else {
      selected_execution_ids(unique(row_subset$run_id[target_rows]))
    }
    download_list <- selected_execution_ids()


    # if(length(download_list) == 0) {
    #   download_list <- unique(row_subset$run_id)
    # }
    #cat("DOWNLOAD RUN ID LIST: ", paste0(download_list, collapse=", "), "\n")
    output_df <- data.frame()
    #browser()
    # urls <- paste0("https://", domino_url, "/v4/admin/supportbundle/", download_list)
    # if(length(urls) > 1000) {
    #   num_batches <- 10
    #   x <- ceiling(length(urls) / num_batches)
    #   indices <- (seq_along(urls) - 1) %/% x + 1
    #   # Split the URLs based on the group numbers
    #   url_list <- split(urls, indices)
    # } else {
    #   url_list <- list(urls)
    # }

    # Now loop through each id, pull execution file, perform summary analysis

    withProgress(message='Downloading and Extracting Bundle summary', detail="Please wait...", value=0, {
      #.  ############# BEGINNING OF PARALLELIZED PROCESS ###################
      output_df <- data.frame()
      all_executions <- selected_execution_ids()#report_values$Run.id[which(report_values$Status %in% c("Failed", "Error"))]

      #browser()
      # Identify all of the already existing summary files, load them into output_df
      # setdiff with all_executions for other downloads
      if(dir.exists(paste0(data_directory, 'support-bundle-summary')) & input$use_cached_analysis == TRUE) {
        #browser()
        all_existing_summary <- list.files(paste0(data_directory, 'support-bundle-summary'), full.names = TRUE)
        all_existing_ids <- stringi::stri_extract(all_existing_summary, regex="(?<=\\/support-bundle-summary\\/).*(?=-summary.csv)")

        target_existing_ids <- intersect(all_existing_ids, all_executions)
        all_executions <- setdiff(all_executions, target_existing_ids)

        if(length(target_existing_ids) > 0) {
          output_df <- lapply(target_existing_ids, function(target_id) {
            output <- read.csv(paste0(data_directory, 'support-bundle-summary/', target_id, '-summary.csv'))
          }) %>% do.call(rbind, .)
        }
      }


      # THE NEXT FUTURE IMPLEMENTATION: Perform regex extraction again WITHOUT downloading old bundles again
      already_existing_bundles <- list.files(paste0(data_directory, 'support-bundles'), full.names = FALSE)
      existing_bundle_ids <- already_existing_bundles[!grepl("\\.zip", already_existing_bundles)]
      relevant_existing_bundle_ids <- intersect(existing_bundle_ids, all_executions)

      # The support bundle directories are names via their execution ids
      download_list <- setdiff(all_executions, relevant_existing_bundle_ids)

      # Now avoid all the execution ids which already have support bundle zip files downloaded


      regex_pattern_df <<- regex_reactive_df()

      # Download the files via async
      if (length(download_list) > 0) {
        urls <- paste0("https://", domino_url, "/v4/admin/supportbundle/", download_list)
      } else {
        urls <- list()
      }
      headers <- c("X-Domino-Api-Key" = domino_user_api_key,
                   "accept" = "application/json")


      # Maximum number of connections is 128-3 (stdin/out/console) with 5 for wiggle room
      x <- 120
      indices <- (seq_along(urls) - 1) %/% x + 1
      # Split the URLs based on the group numbers
      url_list <- split(urls, indices)

      # Loop through each batch of urls, using crul to ansychronously make api calls
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

      incProgress(1/2)

      #browser()
      if (input$use_cached_analysis == FALSE) {
        download_list <- all_executions
      }

      if(length(download_list) > 0) {
        all_file_paths <- paste0(data_directory, 'support-bundles/', download_list, '.zip')
        all_file_paths <- all_file_paths[file.exists(all_file_paths)]
      } else {
        all_file_paths <- c()
      }

      if(length(relevant_existing_bundle_ids) > 0) {
        bundle_paths <- paste0(data_directory, 'support-bundles/', relevant_existing_bundle_ids)
        all_file_paths <- c(all_file_paths, bundle_paths)
      }
      #all_file_paths <- list.files(paste0(data_directory, "support-bundles/"), full.names=TRUE)

      # Get the number of cores available
      no_cores <- parallel::detectCores() - 1  # using one less to leave a core free

      # Use parLapply to unzip in parallel
      cl <- parallel::makeCluster(no_cores)
      dest_dir <<- paste0(data_directory, 'support-bundles')

      parallel::clusterExport(cl, "dest_dir")
      parallel::clusterExport(cl, "identify_support_bundle_errors")
      parallel::clusterExport(cl, 'regex_pattern_df')
      parallel::clusterExport(cl, 'data_directory')
      # parallel::clusterExport(cl, 'model_rest_url')
      # parallel::clusterExport(cl, 'model_api_key')

      parallel::clusterEvalQ(cl, {
        library(magrittr)
        library(httr)
        library(jsonlite)
      })

      #browser()
      out <- parallel::parLapply(cl, all_file_paths, unzip_single) %>% do.call(rbind, .)
      parallel::stopCluster(cl)

      #browser()
      output_df <- rbind(output_df, out)
      
      if(input$use_ml_model) {
        ml_execution_ids <- setdiff(download_list, output_df$execution_id)
  
        # PRE-CACHED: Check the already existing runs first
        if(dir.exists(paste0(data_directory, 'support-bundle-summary-ml')) & input$use_cached_analysis == TRUE) {
          #browser()
          all_existing_summary <- list.files(paste0(data_directory, 'support-bundle-summary-ml'), full.names = TRUE)
          all_existing_ids <- stringi::stri_extract(all_existing_summary, regex="(?<=\\/support-bundle-summary-ml\\/).*(?=-summary.csv)")
          
          target_existing_ids <- intersect(all_existing_ids, ml_execution_ids)
          ml_execution_ids <- setdiff(ml_execution_ids, target_existing_ids)
          
          if(length(target_existing_ids) > 0) {
            ml_output_df <- lapply(target_existing_ids, function(target_id) {
              output <- read.csv(paste0(data_directory, 'support-bundle-summary-ml/', target_id, '-summary.csv'))
            }) %>% do.call(rbind, .)
          }
        }
        
        
        
        # If there are any existing execution ids left...
        if(length(ml_execution_ids) > 0) {
          ml_execution_ids <- paste0(ml_execution_ids, collapse="|")
          ml_support_bundles <- all_file_paths[grepl(ml_execution_ids, all_file_paths) & !grepl("\\.zip", all_file_paths)]
    
          # # Now, identify any non-error "Error/Failed" files and push them through the model api
          # # Machine learning API
          all_files <- list.files(ml_support_bundles, full.names=TRUE)
          all_files <- all_files[grep("logjam|events|execution", all_files)]
          # #a <- Sys.time()
          model_file_errors <- lapply(all_files, function(target_file_name) {
            target_file_name <- all_files[56]
            target_file <- readLines(target_file_name)
            url <- model_rest_url # located in credentials.R (same with model_api_key)
            response <- httr::POST(
              url,
              authenticate(model_api_key, model_api_key, type = "basic"),
              body=toJSON(list(data=list(text = target_file)), auto_unbox = TRUE),
              content_type("application/json")
            )
    
            predictions <- content(response)
            predictions <- unname(unlist(predictions$result))
    
            num_errors <- length(predictions[which(predictions != "none")])
            # Identify any potential errors which are not none. If they exist, turn it into a dataframe. If not, just cbind that stuff
            if(num_errors > 0) {
              error_lines <- which(predictions != "none")
              error_type <- predictions[error_lines]
              error <- target_file[error_lines]
              error_description <- target_file[error_lines]
              data <- data.frame('Error'=errors, 'Line_Number'=error_lines, 'Context'=error_description, 'Error_Type'=error_type)
              data$File_Path <- target_file_name
              datetime_str <- stringi::stri_extract(data$Context, regex="\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+Z")
              data$Date_Time <- lubridate::ymd_hms(datetime_str, tz = "UTC")
              data$execution_id <- lapply(data$File_Path, function(file_path) {
                file_path <- stringi::stri_split(file_path, regex="/") %>% unlist()
                execution_id <- file_path[length(file_path)-1]
              })
    
              associated_node <- target_file[grep("assignedNodeName", target_file)]
              associated_node <- stringi::stri_extract(associated_node, regex="ip-[0-9]+-[0-9]+-[0-9]+-[0-9]+\\..*\\.compute.internal")
              if(length(associated_node) > 0) {
                data$Node <- associated_node
              } else {
                data$Node <- NA
              }
    
              out <- data
            } else {
              out <- data.frame(
                Error = character(0),
                Line_Number = integer(0),
                Context = character(0),
                Error_Type = character(0),
                File_Path = character(0),
                Node = character(0)
              )
            }
          }) %>% do.call(rbind, .)
          
          browser()
          # Save out the identified errors
          lapply(unique(model_file_errors$execution_id), function(target_id) {
            error_subset <- model_file_errors[which(model_file_errors$execution_id == target_id),]
            summary_directory <- paste0(data_directory, "support-bundle-summary-ml")
            summary_csv_path <- paste0(summary_directory, "/", target_id, "-summary.csv")
            
            write.csv(error_subset, summary_csv_path, row.names=FALSE)
          })
          
          output_df <- rbind(output_df, model_file_errors)
        }
      }
      #
      #
      # browser()
      #zip_files <- list.files(path = dest_dir, pattern = "\\.zip$", full.names = TRUE)
      # Delete the zip files
      incProgress(1/2)
      #   ############################# END #################################

    }) # withProgress Wrapper
    #################### END OF FOR LOOP ########################
    output_df$Execution_Status <- report_df()$status[match(output_df$execution_id, report_df()$run_id)]

    metadata_df(output_df)
    #browser()
  })


  ##### Reactive to Trigger Modal for switching on ML capabilities #####
  observeEvent(input$use_ml_model, {
    # Check if the radio button is toggled to "True"
    if(!is.null(input$use_ml_model) & input$use_ml_model == TRUE) {
      # Show modal dialog when toggled to "True"
      showModal(modalDialog(
        title = "Use Model to Identify Errors?",
        "Using the model to enhance regular expression patterns may require additional time. Continue?",
        footer = tagList(
          actionButton("cancel", "Cancel"),
          actionButton("continue", "Continue")
        )
      ))
    }
  })

  # # Handling the 'Continue' action
  observeEvent(input$continue, {
    removeModal()
  })

  # # Revert to "False" if "Cancel" is pressed
  observeEvent(input$cancel, {
    updateRadioButtons(session, "use_ml_model", selected = FALSE)
    removeModal()
  })

  
  
  ##### Errors Captured Table #####
  metadata_agg <- shiny::reactive({
    if(!is.null(metadata_df())) {
      #browser()
      shiny::validate(
        need(nrow(metadata_df()) > 0, "No errors identified in the selected bundles.")
      )
      highest_vote <- data.frame(execution_id=unique(metadata_df()$execution_id))

      execution_id <- unique(metadata_df()$execution_id)[2]
      highest_vote$Error_Type <- lapply(unique(metadata_df()$execution_id), function(execution_id) {
        cat(execution_id, "\n")
        target_rows <- metadata_df()[which(metadata_df()$execution_id == execution_id),]
        most_recent_log <- target_rows[which(target_rows$Date_Time == max(target_rows$Date_Time, na.rm=TRUE)),]
        
        # Edge case: none of the execution ids have timestamps. Just choose the highest vote. This prioritizes errors with timestamps
        if(nrow(most_recent_log) == 0) {
          most_recent_log <- target_rows
        }
        # Just in case there are multiple errors caught at the same most recent time
        highest_vote <- table(most_recent_log$Error_Type)
        
        # # Edge case: all returned hits are found without timestamps
        # if(length(highest_vote) == 0) {
        #   highest_vote <- table(target_rows$Error)
        # }
        
        highest_vote <- names(highest_vote[which(highest_vote == max(highest_vote))])
        
        # This can be where we have a hierarchy of errors
        if(length(highest_vote) > 1) {
          highest_vote <- highest_vote[1]
        }
        
        return(highest_vote)
      }) %>% unlist()
      
      #browser()
      metadata_agg <- as.data.table(highest_vote)
      metadata_agg <- metadata_agg[, .N, by=Error_Type]
      all_errors <- data.frame("Error_Type"=error_types)
      metadata_agg <- data.table::merge.data.table(all_errors, metadata_agg, by="Error_Type", all.x=TRUE)
      metadata_agg[is.na(metadata_agg)] <- 0
      metadata_agg$Error_Type <- tools::toTitleCase(metadata_agg$Error_Type)
      
      return(metadata_agg)
    }
  })
  
  execution_ids_failed_downloads <- shiny::reactive({
    if(!is.null(metadata_df()) & !is.null(selected_execution_ids())) {
      support_bundle_dirs <- list.files(paste0(data_directory, "support-bundles"), full.name=TRUE)
      support_bundle_dirs <- support_bundle_dirs[!grepl("\\.zip", support_bundle_dirs)]
      support_bundle_dirs <- support_bundle_dirs[grep(paste0(selected_execution_ids(), collapse="|"), support_bundle_dirs)]
      find_empty_dirs <- function(dir_list) {
        dir <- "/mnt/data/allstate_log_github/support-bundles/653d31a42f1a8933ba692edf"
        sapply(dir_list, function(dir) {
          # List files in the directory, recursive=FALSE ensures we only check the top level
          files <- list.files(dir, recursive = FALSE)
          
          # Return TRUE if no files are found, else FALSE
          return(length(files) == 0)
        })
      }
      
      empty_support_bundles <- support_bundle_dirs[find_empty_dirs(support_bundle_dirs)]
      
      failed_ids <- lapply(empty_support_bundles, function(target_bundle) {
        id <- stringi::stri_split(target_bundle, regex="/") %>% unlist()
        id <- id[length(id)]
        return(id)
      }) %>% unlist()

      return(failed_ids)
    }
  })
  
  execution_ids_no_errors <- shiny::reactive({
    if(!is.null(metadata_df()) & !is.null(selected_execution_ids())){
      no_errors_found <- setdiff(selected_execution_ids(), metadata_df()$execution_id)
      no_errors_found <- setdiff(no_errors_found, execution_ids_failed_downloads())
      error_free_ids <- paste0(no_errors_found, collapse="\n")
      return(error_free_ids)
    }
  })
  
  execution_ids_failed_downloads_str <- shiny::reactive({
    if(!is.null(execution_ids_failed_downloads())) {
      #browser()
      execution_ids_failed_downloads_str <- execution_ids_failed_downloads()[which(execution_ids_failed_downloads() != "NA")]
      execution_ids_failed_downloads_str <- paste0(execution_ids_failed_downloads_str, collapse="\n")
      return(execution_ids_failed_downloads_str)
    }
  })
  
  output$error_summary_bar_chart <- highcharter::renderHighchart({
    if(!is.null(metadata_agg())) {
      df <- metadata_agg()
      
      
      
      df$colors <- viridis::viridis(nrow(df))
      hc <-
        df %>%
        hchart(
          'column',
          hcaes(x=Error_Type, y=N, color=colors, opacity=0.8),
          stacking="normal"
        ) %>%
        #hc_title(text=paste0("Errors")) %>%
        #hc_subtitle(text=paste0("Click a category to drill down into it.")) %>%
        hc_legend(FALSE) %>%
        # hc_tooltip(
        #   pointFormat = "<span style='color:{point.color}'>●</span> {series.name}: <b>{point.y}</b><br/>",
        #   borderWidth = 0,
        #   backgroundColor = 'none',
        #   shadow = FALSE,
        #   style = list(fontSize = "16px", fontWeight = "bold", color = "#000000"),
        #   useHTML = TRUE
        # ) %>%
        hc_xAxis(title=list(text='Error Type')) %>%
        hc_yAxis(title=list(text=''))
    }
  })
  
  output$error_render <- DT::renderDT({
    if(!is.null(metadata_df())){
      shiny::validate(
        need(nrow(metadata_df()) > 0, "No errors identified in the selected bundles.")
      )
      #browser()
      df <- metadata_df()
      df <- df[c("execution_id", "Execution_Status", "Node", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase(.)
      
      DT::datatable(df,rownames = FALSE, class = "compact", filter="top",
                    options = list(server=FALSE, scrollX = TRUE, sScrollY = '75vh',
                                   selection = list(target = 'row', selected='indices', mode='single'),
                                   scrollCollapse = TRUE, searching = TRUE,
                                   paging = TRUE, pageLength=50, lengthMenu=c(10,20,50,100)
                                   , bInfo=TRUE),
                    extensions = list("Scroller"))
      #browser()
    }
  })
  
  
  output$error_render_downloader <- downloadHandler(
    filename = function() {
      paste0("support-bundle-errors-captured-",Sys.Date(),".csv")
    },
    content = function(con) {
      if(!is.null(metadata_df())) {
        df <- metadata_df()
        df <- df[c("execution_id", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
        colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase(.)
        write.csv(df, con, row.names = FALSE)
      }
    }
  )
  
  
  
  
  
  current_path_contents <- shiny::reactiveVal()
  shiny::observeEvent(input$error_render_rows_selected, {
    #browser()
    file_path <- metadata_df()$File_Path[input$error_render_rows_selected[length(input$error_render_rows_selected)]]
    contents <- readLines(file_path)
    mentioned_lines <- metadata_df()$Line_Number[which(metadata_df()$File_Path == file_path)]
    contents[mentioned_lines] <- paste0('<span style="color: darkred; background-color: lightcoral;">', contents[mentioned_lines], "</span>")
    contents <- sapply(seq_along(contents), function(x) paste0(x, ": ", contents[x], "<br>"))
    contents <- c(paste0(file_path, ":<br><br>"), contents)
    #cat(contents, "\n")
    
    current_path_contents(contents)
  })
  
  output$error_row_contents <- renderText({
    # Check if the file input is not NULL
    if (!is.null(current_path_contents())) {
      return(current_path_contents())
    } else {
      return(NULL)
    }
  })
  
  output$executions_with_errors_progress_bar <- shiny::renderUI({
    if(!is.null(metadata_df())) {
      #browser()
      errors <- length(unique(metadata_df()$execution_id))
      total <- length(selected_execution_ids())
      percentage <- base::round(errors/total*100, 0)
      
      shinyWidgets::progressBar(
        id = "executions_progress_bar",
        value = errors,
        total = total,
        title = "Executions with Found Errors:",
        display_pct = TRUE,
        striped = TRUE
      )
    }
  })
  
  
  #### SUPPORT BUNDLE ANALYSIS - SUMMARY ####
  output$csv_file_dropdown_ui <- shiny::renderUI({
    if(!is.null(fileList())) {
      target_files <- fileList()[grep("\\.csv", fileList())]
      names(target_files) <- stringi::stri_extract(target_files, regex=".*(?=-summary)")

      support_bundle_summary <- paste0(data_directory, "support-bundle-summary/", fileList())
      file_sizes <- file.info(support_bundle_summary)
      largest_file <- rownames(file_sizes)[which(file_sizes$size == max(file_sizes$size))]
      largest_file <- stringi::stri_extract(largest_file, regex="(?<=/support-bundle-summary/).*")
      support_bundle_csv_files(largest_file)
      
      shiny::selectInput(inputId = "csv_file_dropdown",
                            label = h5("Support Bundle Summary"),
                            choices = target_files,
                            selected = largest_file,
                            multiple = FALSE,
                            selectize = TRUE)
    }
  })
  
  output$error_totals_barchart <- highcharter::renderHighchart({
    if(!is.null(support_bundle_df())) {
      support_bundle_dt <- data.table::data.table(support_bundle_df())
      support_bundle_dt$is_date_na <- base::ifelse(!is.na(support_bundle_dt$Date_Time), 'Timestamp', 'No Timestamp')
      sup_agg <- support_bundle_dt[, .N, .(Error_Type, is_date_na)]
      
      all_poss <- data.table::CJ(c("Timestamp", "No Timestamp"), error_types)
      colnames(all_poss) <- c("is_date_na", "Error_Type")
      sup_agg <- data.table::merge.data.table(sup_agg, all_poss, by=c("Error_Type", "is_date_na"), all.y = TRUE)
      sup_agg$N[which(is.na(sup_agg$N))] <- 0
      
      sup_agg$Error_Type <- tools::toTitleCase(sup_agg$Error_Type)
      bin_labels <- sort(unique(sup_agg$Error_Type))
      sup_agg <- as.data.frame(sup_agg)
      
      hc <-
        sup_agg %>%
        hchart(
          'bar',
          hcaes(x=Error_Type, y=N, group=is_date_na, opacity=0.8),
          stacking="normal"
        ) %>%
        hc_colors(c(tasteful_ryg[1], tasteful_ryg[3])) %>%
        hc_title(text=paste0("Errors")) %>%
        hc_subtitle(text=paste0("Click a category to drill down into it.")) %>%
        hc_legend(FALSE) %>%
        hc_xAxis(title=list(text='Error Type'), categories=bin_labels) %>%
        hc_yAxis(title=list(text='')) %>%
        hc_plotOptions(
          series = list(
            cursor = "pointer",
            point = list(
              events = list(
                click = JS("function() {
                Shiny.setInputValue('clicked_bar', this.Error_Type);
              }")
              )
            )
          )
        )
      return(hc)
    }
  })
  
  
  observeEvent(input$clicked_bar, {
    # Update the reactive value with the category of the clicked bar
    current_error_type(input$clicked_bar)
  })
  
  support_bundle_csv_files <- shiny::reactiveVal()
  
  observeEvent(input$reset_btn, {
    current_error_type("All")
    support_bundle_csv_files(input$csv_file_dropdown)
  })
  
  df_time_series <- shiny::reactive({
    if(!is.null(support_bundle_subset())) {
      output_df <- support_bundle_subset()[which(!is.na(support_bundle_subset()$Date_Time)),]
      
      shiny::validate(
        need(nrow(output_df) > 0, "No errors caught have associated dates")
      )
      
      return(output_df)
    }
  })
  
  output$time_series_chart <- renderHighchart({
    if(!is.null(df_time_series())) {
      target_df <- df_time_series()
      dt1 <- strptime(max(target_df$Date_Time, na.rm=TRUE), format="%Y-%m-%d %H:%M:%S")
      dt2 <- strptime(min(target_df$Date_Time, na.rm=TRUE), format="%Y-%m-%d %H:%M:%S")
      time_diff <- difftime(dt1, dt2, units="secs")
      # If the largest break is minute, go in secs,
      if(time_diff < 60*60) {
        time_units <- "sec"
      } else if (time_diff < 60*60*24) {
        time_units <- "hour"
      } else {#if (time_diff < 60*60*24*30) {
        time_units <- "day"
      }
      # Else if the largest gap is hours, go mins,
      # Else if the largest gap is days, go hours
      #browser()
      target_df$Date_Time <- as.POSIXct(target_df$Date_Time) %>% floor_date(., unit=time_units)
      dt <- data.table::data.table(target_df)
      dt_agg <- dt[, .N, by=.(Error_Type, Date_Time)]
      
      # min_datetime <- dt[, .(min_date_time=min(Date_Time)), by=execution_id]
      # map_to_date_time <- min_datetime$min_date_time
      # names(map_to_date_time) <-  min_datetime$execution_id
      # dt$time_after_start <- as.numeric(dt$Date_Time) - as.numeric(map_to_date_time[dt$execution_id])
      
      
      time_interval <- seq(min(dt_agg$Date_Time), max(dt_agg$Date_Time), by = paste("1", time_units)) %>% floor_date(., unit=time_units)
      all_types <- unique(dt_agg$Error_Type)
      all_poss <- data.table::CJ(Date_Time=time_interval, Error_Type=all_types)
      
      dt_agg <- data.table::merge.data.table(dt_agg, all_poss, by=c("Date_Time", "Error_Type"), all.y=TRUE)
      dt_agg$N[which(is.na(dt_agg$N))] <- 0
      dt_agg$Date_Time <- as.integer(dt_agg$Date_Time) * 1000
      
      #browser()
      dt_agg %>%
        hchart("line",
               hcaes(x=Date_Time, y=N, group=Error_Type)) %>%
        hc_xAxis(title=list(text=paste0('Date (by ', tools::toTitleCase(time_units),')')), type = 'datetime') %>%
        hc_colors(viridis::viridis(length(unique(dt_agg$Error_Type)))) %>%
        hc_chart(zoomType = "x") %>%
        hc_title(text=paste("Errors Over Time"))
    }
  })
  
  support_bundle_df <- shiny::reactive({
    if(!is.null(support_bundle_csv_files()) & support_bundle_csv_files() != "") {
      #browser()
      support_bundle_df <- lapply(support_bundle_csv_files(), function(target_file) {
        #cat("READING IN THE FOLLOWING FILE: ", target_file, "\n")
        df <- read.csv(paste0(data_directory, "support-bundle-summary/", target_file))
        return(df)
      }) %>% do.call(rbind, .)
      
      return(support_bundle_df)
    }
  })
  
  support_bundle_subset <- shiny::reactive({
    if(!is.null(current_error_type()) & !is.null(support_bundle_df())) {
      if(current_error_type() != "All") {
        support_bundle_subset <- support_bundle_df()[which(support_bundle_df()$Error_Type == tolower(current_error_type())),]
      } else {
        support_bundle_subset <- support_bundle_df()
      }
      
      #browser()
      
      # shiny::validate(
      #   need(try(nrow(support_bundle_subset)> 0 & !is.na(support_bundle_subset)), paste0("No errors caught in target support bundle"))
      # )
      
      return(support_bundle_subset)
    } 
  })
  
  output$bundle_error_dt_render <- DT::renderDT({
    if(!is.null(support_bundle_subset())){
      df <- support_bundle_subset()
      df <- df[c("Node", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
      colnames(df) <- gsub("_", " ", colnames(df))
      
      DT::datatable(df,rownames = FALSE, class = "compact", filter="top",
                    options = list(scrollX = TRUE, sScrollY = '75vh',
                                   scrollCollapse = TRUE, searching = TRUE,
                                   paging = TRUE, pageLength=50, lengthMenu=c(10,20,50,100)
                                   , bInfo=TRUE),
                    extensions = list("Scroller"))
    }
  })
  
  output$bundle_error_dt_render_downloader <- downloadHandler(
    filename = function() {
      paste0("support-bundle-errors-captured-",Sys.Date(),".csv")
    },
    content = function(con) {
      if(!is.null(support_bundle_subset())) {
        df <- support_bundle_subset()
        df <- df[c("Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
        colnames(df) <- gsub("_", " ", colnames(df))
        write.csv(df, con, row.names = FALSE)
      }
    }
  )
  
  output$bundle_content_summary_select_ui <- shiny::renderUI({
    if(!is.null(support_bundle_csv_files())) {
      #browser()
      execution_id <- stringi::stri_extract(support_bundle_csv_files(), regex=".*(?=-summary\\.csv)")
      directories <- paste0(data_directory, "support-bundles/", execution_id)
      path_choices <- list.files(directories, full.names=TRUE)
      names(path_choices) <- list.files(directories, full.names=FALSE)
      
      shiny::validate(
        need(length(path_choices) > 0, as.character("No associated files found. This tends to be the result of a (500) Internal Server Error on pull of support bundle and is likely due to this execution being one that had failed to start. Please select another bundle."))
      )
      
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "bundle_content_summary_select",
                                label = "Choose File",
                                choices = path_choices,
                                selected = path_choices[1],
                                multiple = FALSE)
      )
    }
  })
  
  output$bundle_summary_content <- renderText({
    # Check if the file input is not NULL
    if (!is.null(input$bundle_content_summary_select)) {
      # Read the file content
      #browser()
      bundle_summary_content <- readLines(input$bundle_content_summary_select, warn = FALSE)
      if(length(bundle_summary_content) > 0) {
        # This is formatting the bundle content
        #bundle_summary_content <- paste0("<b>", seq_along(bundle_summary_content), ":</b>&nbsp;&nbsp;", bundle_summary_content)
        # Compute the maximum number of digits
        max_digits <- nchar(max(seq_along(bundle_summary_content)))
        
        # Generate the strings with adjusted spaces
        bundle_summary_content <- mapply(function(index, content) {
          spaces_needed <- max_digits - nchar(index)
          spaces <- paste(rep("&nbsp;", times = spaces_needed + 2), collapse = "") # 2 additional spaces for clarity
          paste0("<b>", index, ":</b>", spaces, content)
        }, seq_along(bundle_summary_content), bundle_summary_content)
      }
      # Convert the content to a single string and return
      paste(bundle_summary_content, collapse = "<br>")
    } else {
      return(NULL)
    }
  })
  
  ##### UPLOAD TAB #####
  ##### ERROR REGEX MATCHING UPDATE TABLE #####
  
  regex_reactive_df <- reactiveVal(regex_df)
  
  output$regex_lookup_dt <- DT::renderDT({
    if(!is.null(regex_reactive_df())){
      # shiny::validate(
      #   need(nrow(regex_df()) > 0, "No errors identified in the selected bundles.")
      # )
      
      df <- regex_reactive_df()
      #browser()
      #df <- df[c("execution_id", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase(.)
      
      DT::datatable(df, 
                    editable = TRUE,
                    rownames = FALSE, 
                    class = "compact",
                    filter = "top",
                    options = list(#autoWidth = TRUE, 
                                   scrollX = TRUE, 
                                   sScrollY = '75vh',
                                   scrollCollapse = TRUE, 
                                   searching = TRUE,
                                   paging = TRUE, 
                                   pageLength = 50, 
                                   lengthMenu = c(10, 20, 50, 100),
                                   bInfo = TRUE,
                                   columnDefs = list(
                                     list(className = 'dt-center', targets = "_all")
                                   ),
                                   rowCallback = JS(
                                     "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                     $(nRow).attr('id', 'row_' + aData[0]);
                                   }"
                                   )
                    ), 
                    extensions = list("Scroller"), 
                    selection = "single" # Allow single row selection for deleting
      )
    }
  })
  
  proxy <- dataTableProxy("regex_lookup_dt")
  
  observeEvent(input$add_row, {
    new_row <- data.frame(error_type = "EDIT HERE", regex_pattern = "EDIT HERE") # Sample new row
    updated_df <- rbind(regex_reactive_df(), new_row)
    regex_reactive_df(updated_df)
    
    replaceData(proxy, updated_df, resetPaging = FALSE)
  })
  
  # Delete the selected row from the DataTable
  observeEvent(input$delete_row, {
    selected_row <- input$regex_lookup_dt_rows_selected
    if (!is.null(selected_row)) {
      updated_df <- regex_reactive_df()[-selected_row, ]
      regex_reactive_df(updated_df)
      replaceData(proxy, updated_df, resetPaging = FALSE)
    }
  })
  
  observeEvent(input$regex_lookup_dt_cell_edit, {
    info <- input$regex_lookup_dt_cell_edit
    modified_df <- regex_reactive_df()
    
    # Update the changed cell in the data frame
    modified_df[info$row, info$col+1] <- info$value
    regex_reactive_df(modified_df)
    
    # Update the table using the proxy
    replaceData(proxy, modified_df, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$save_table, {
    write.csv(regex_reactive_df(), paste0(data_directory, "regex_lookup.csv"), row.names = FALSE)
    
    # Triggers a "grayed out" back to a "normal" state so user knows that the click went through
    shinyjs::addClass(id="save_table", class="grayed-out-button", asis=TRUE)
    Sys.sleep(1)
    shinyjs::removeClass(id="save_table", class="grayed-out-button", asis=TRUE)
  })
  
  
  shiny::observe({
    num_mins <- 30
    # Every 30 minutes, create a backup of regex_reactive_df()
    regex_backups_dir <- paste0(data_directory, "regex_backups")
    target_df <- shiny::isolate(regex_reactive_df())
    write.csv(target_df, gsub(" ", "_", paste0(regex_backups_dir, "/", Sys.time(), ".csv")), row.names=FALSE)
    
    # Clear out any old regex saves that are older than a week.
    days_to_keep <- 7
    num_files <- days_to_keep * 24 * 60 / num_mins 
    existing_files <- list.files(regex_backups_dir, full.names=TRUE)
    if(length(existing_files) > num_files) {
      files_to_delete <- sort(existing_files)[(num_files+1):length(existing_files)]
      file.remove(files_to_delete)
    }
    
     # Check every 30 minutes
    invalidateLater(1000 * 60 * num_mins)
  })
  
  # Function to get the list of files (to be used by reactivePoll)
  get_support_bundle_dirs <- function() {
    all_available_support_bundles <- list.files(paste0(data_directory, 'support-bundles/'))
    return(all_available_support_bundles)
  }
  
  # Create a reactivePoll that checks the list of files every 1 second (1000 ms)
  support_bundle_dirs <- reactivePoll(1000, session,
                            checkFunc = function() {
                              # Use digest to create a hash that will change if the file list changes
                              digest::digest(get_support_bundle_dirs())
                            },
                            valueFunc = function() {
                              # Return the current list of files
                              get_support_bundle_dirs()
                            }
  )
  
  
  
  
  execution_ids <- shiny::reactive({
    if(!is.null(support_bundle_dirs())) {
      #browser()
      available_support_bundles <- support_bundle_dirs()[!grepl("\\.zip",support_bundle_dirs())]
    }
  })
  
  output$support_bundle_ids_multiselect_ui <- shiny::renderUI({
    if (!is.null(execution_ids())) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "support_bundle_ids_multiselect",
                                label = "Choose Support Bundle Ids",
                                choices = execution_ids(),
                                selected = execution_ids()[1],
                                multiple = TRUE)
      )
    }
  })
  
  
  
  output$support_bundle_file_select_ui <- shiny::renderUI({
    if(!is.null(input$support_bundle_ids_multiselect)) {
      #browser()
      execution_ids()
      
      directories <- paste0(data_directory, "support-bundles/", input$support_bundle_ids_multiselect)
      path_choices <- list.files(directories, full.names=TRUE)

      shiny::validate(
        need(length(path_choices) > 0, as.character("No associated files found. This tends to be the result of a (500) Internal Server Error on pull of support bundle. Refer to documentation to manually extract by execution id."))
      )

      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "support_bundle_file_select",
                                label = "Choose File",
                                choices = path_choices,
                                selected = path_choices[1],
                                multiple = FALSE)
      )
    }
  })
  
  output$regex_file_content <- renderText({
    # Check if the file input is not NULL
    if (!is.null(input$support_bundle_file_select)) {
      # Read the file content
      #browser()
      regex_file_content <- readLines(input$support_bundle_file_select, warn = FALSE)
      if(length(regex_file_content) > 0) {
        #regex_file_content <- paste0("<b>", seq_along(regex_file_content), ":</b>&nbsp;&nbsp;", regex_file_content)
        # Compute the maximum number of digits
        max_digits <- nchar(max(seq_along(regex_file_content)))
        
        # Generate the strings with adjusted spaces
        regex_file_content <- mapply(function(index, content) {
          spaces_needed <- max_digits - nchar(index)
          spaces <- paste(rep("&nbsp;", times = spaces_needed + 2), collapse = "") # 2 additional spaces for clarity
          paste0("<b>", index, ":</b>", spaces, content)
        }, seq_along(regex_file_content), regex_file_content)
      }
      # Convert the content to a single string and return
      paste(regex_file_content, collapse = "<br>")
    } else {
      return(NULL)
    }
  })
  
  
  #### TAKE A TOUR ####
  output$take_a_tour_resource_usage_btn_ui <- shiny::renderUI({
    if(!is.null(input$generate_report_btn)){
      if(input$generate_report_btn > 0) {
        actionButton("take_a_tour_resource_usage_btn", "Take a Tour",
                     style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
      }
    }
  })
  
  helptext <- reactive({
    tst <- data.table(
      btn  = c(
              rep("take_a_tour_resource_usage_btn", 6),
              rep("take_a_tour_bundle_summary_btn", 3),
              rep("take_a_tour_bundle_upload_btn", 5)
              ),
      step = c(seq(6), seq(3), seq(5)),
      element = c("#usage_report_filters", 
                  "#run_type_donut_chart", 
                  "#rolling_count_running_processes_time_series", 
                  "#report_render",
                  "#analyze_support_bundles",
                  "#error_summary_bar_chart",
                  # Bundle Summary
                  "#error_totals_barchart",
                  "#time_series_chart",
                  "#bundle_error_dt_render",
                  # Bundle Upload
                  #"#bundle_upload_from_execution_id",
                  #"#bundle_upload_from_zip_file",
                  "#regular_expression_table",
                  "#add_row",
                  "#delete_row",
                  "#save_table",
                  "#explore_source_files"
      ),
      intro = c("Use the toggles to update the filters which generate the resulting report. The secondary category filter determines the categorical breakdown of the graphs.",
                "Clicking any category on this chart will filter the data table below...",
                "whereas any clicks on this chart will filter by category and time.",
                "Here you can find the metadata for all workloads executed in Domino. Select a few of these (or nothing at all!) and click 'next.'",
                paste0("When you have made your selection, click here to run the bundle analysis. You will see a tabbox appear to the right.
                                  <br><br><b>Summary:</b> Counts of all error types found within selected support bundles.
                                  <br><b>Data:</b> Metadata surrounding each error identified
                                  <br><b>Context:</b> When a row in data is selected, outputs the corresponding file and highlights all caught errors.
                                  <br><br>
                                You can also use the 'Reset Secondary Filter' button to remove any filters imposed on the table by chart clicks."),
                "This is a test to see if I can actually render this out in the tutorial.",
                ###### BUNDLE SUMMARY
                "This figure displays the number of caught errors identified in the support bundles for the selected execution id. Files include logs for the domino session (domino), the underlying k8s infrastructure (cluster), and user input (user). This is further subdivided by the number errors which were accompanied by a timestamp.<br><br> Refer to the 'Customize Error Search Analysis' chart in the 'Upload' tab for regular expressions used in the bundle analysis.",
                "Here we break down the prior chart by the time in which they occured. It's important to note that these are only lines identified as errors which also contained a timestamp.",
                "Underlying data for the above charts. This table filters down to any error type selected in the above charts.",
                # Bundle Upload
                #"If you can't find an execution id in the summary tables, put it into the text box above and it will be added to the database.",
                #"You can also opt to directly upload a support bundle from your local computer. This is a great way to analyse support bundles from tenants that are different from that which the application resides.",
                "The error analysis conducted in the resource usage tab relies on regular expression patterns to identify lines which may potentially contain errors and the respective class.",
                "To add a regular expression, click the below button. You can then manually enter your selection into the table",
                "If you need to remove a pattern, click here. Be careful; the only way to undo a delete is to refresh the page. This erases all unsaved work.",
                "Save any patterns you have entered or deleted from the source table.",
                "Browse existing execution logs to identify potential errors. These can be either searched individually or multiple ids can be searched concurrently."
      )
    )
  })
  
  observeEvent(
    eventExpr = input$take_a_tour_resource_usage_btn,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[btn == "take_a_tour_resource_usage_btn"]
              )
      )
    }
  )
  
  output$take_a_tour_bundle_summary_btn_ui <- shiny::renderUI({
        actionButton("take_a_tour_bundle_summary_btn", "Take a Tour",
                     style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
  })
  
  observeEvent(
    eventExpr = input$take_a_tour_bundle_summary_btn,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[btn == "take_a_tour_bundle_summary_btn"]
              )
      )
    }
  )
  
  output$take_a_tour_bundle_upload_btn_ui <- shiny::renderUI({
    actionButton("take_a_tour_bundle_upload_btn", "Take a Tour",
                 style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
  })
  
  observeEvent(
    eventExpr = input$take_a_tour_bundle_upload_btn,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[btn == "take_a_tour_bundle_upload_btn"]
              )
      )
    }
  )
}