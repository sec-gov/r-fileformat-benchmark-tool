# This tool was created by staff of the U.S. Securities and Exchange Commission.
# Data and content created by government employees within the scope of their employment
# are not subject to domestic copyright protection. 17 U.S.C. 105.

# File Format Benchmarking Tool Version 0.1

# This file, "app.R", is a self-contained application that enables users to perform file format benchmarking via a shiny interface. To run the application, first open this file in RStudio, next install the suggested libraries, and finally press the "Run App" button. 

# The code is organized into three parts: (1) Setup, (2) User Interface, and (3) Server.
# Note: To preserve memory, and keystrokes, the "equals operator", "=", is used in lieu of R's traditional "assignment operator", "<-". 

# (1) Setup ----
# This section sets useful parameters, loads required libraries, defines the benchmarking function, and creates a table of file format names.

# One may want to perform benchmarks for various file locations, such as attached storage or local and network drives. To do so, simply assign a new "testing_location". The default creates a sub-directory of the folder containing this file.
testing_location = "tests/"
# testing_location = "~/Desktop/tests/"

# In the event X11 is unavailable, standard ggplot2 graphics can be rendered in lieu of plotly plots by toggling this variable
use_plotly = T

# Required libraries
library(tidyverse)
library(shiny)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(bit64)
library(scales)
library(shinycssloaders)
library(stringi)
library(plotly)
library(benchmarkme)

# Represses scientific notation and raises upload limit from 5MB to 10GB
options(scipen = 99,
        shiny.maxRequestSize = 10000 * 1024^2)

# Function performs read, write, and file size tests for 25 file formats. The function will work independently from the shiny application.
test_performance = function(test_df, 
                            iterations = 2,
                            tests = c(
                              "arrow_csv", "arrow_feather", "arrow_parquet", "fwrite_csv", "duckDB", "feather", "dbf", "fst", "dta", "sav", "xpt", "open.xlsx_xlsx", "readr_rds", "readr_csv", "qs", "qs2", "qdata", "ods", "RSQLite", "arff", "baseR_rds", "vroom_csv", "json", "baseR_csv", "writexl_xlsx"),
                            test_dir = "tests/"
){
  # setup ----
  
  # Loads required libraries
  library(tidyverse)
  library(microbenchmark)
  library(DBI)
  library(duckdb)
  library(RSQLite)
  library(vroom)
  library(arrow)
  library(fst)
  library(haven)
  library(RWeka)
  library(readODS)
  library(rjson)
  library(openxlsx)
  library(qs)
  
  # Creates test and result folders
  dir.create(test_dir, showWarnings = F)
  dir.create(paste0(test_dir, "/results"), showWarnings = F)
  
  # Removes previous test files
  file.remove(list.files(test_dir, full.names = T))
  
  # Ensures that only sf geometry compatible formats are tested
  if("geometry" %in% names(test_df)){
    tests = tests[tests %in% c(
      "open.xlsx_xlsx",
      "qs",
      "qs2",
      "qdata",
      "readr_rds",
      "readr_csv",
      "ods",
      "baseR_rds",
      "vroom_csv",
      "json",
      "writexl_xlsx")]
  }
  
  # This vector is used to identify which tests are input into function regardless of order
  all_tests = c(
    "arrow_csv",
    "arrow_feather",
    "arrow_parquet",
    "fwrite_csv",
    "duckDB",
    "feather",
    "dbf",
    "fst",
    "dta",
    "sav",
    "xpt",
    "open.xlsx_xlsx",
    "qs",
    "qs2",
    "qdata",
    "ods",
    "RSQLite",
    "arff",
    "readr_rds",
    "readr_csv",
    "baseR_rds",
    "vroom_csv",
    "json",
    "baseR_csv",
    "writexl_xlsx"
  )
  
  which_tests = all_tests %in% tests
  
  
  # Database Prep ----
  
  # Establishes a duckDB connection
  if("duckDB" %in% tests){ 
    try(dbDisconnect(duckDB_connection), silent = T)
    duckDB_connection <- dbConnect(duckdb::duckdb(dbdir = paste0(test_dir, "test_dbWriteTable.duckdb")))
  }
  
  # Establishes a RSQLite connection  
  if("RSQLite" %in% tests){
    try(dbDisconnect(sqlite), silent = T)
    sqlite <- dbConnect(RSQLite::SQLite(), paste0(test_dir, "test_dbWriteTable.sqlite"))
  }
  
  # Writing Benchmarks Tests ----
  
  # Lists unevaluated write tests expressions
  write_tests = alist(
    arrow::write_csv_arrow(test_df, paste0(test_dir, "test_arrow_csv.csv")),
    arrow::write_feather(test_df, paste0(test_dir, "test_arrow_feather.feather")),
    arrow::write_parquet(test_df, paste0(test_dir, "test_arrow_parquet.parquet")),
    data.table::fwrite(test_df, paste0(test_dir, "test_fwrite.csv")),
    duckdb::dbWriteTable(duckDB_connection, "tests", test_df, overwrite = T),
    feather::write_feather(test_df, paste0(test_dir, "test_feather.feather")),
    foreign::write.dbf(as.data.frame(test_df),paste0(test_dir,"test_write.dbf.dbf")),
    fst::write_fst(test_df, paste0(test_dir, "test_write_fst.fst")),
    haven::write_dta(test_df, paste0(test_dir, "test_write_dta.dta")),
    haven::write_sav(test_df, paste0(test_dir, "test_write_sav.sav")),
    haven::write_xpt(test_df, paste0(test_dir, "test_write_xpt.xpt")),
    openxlsx::write.xlsx(test_df, paste0(test_dir, "test_openxlsx.xlsx")),
    qs::qsave(test_df, paste0(test_dir, "test_qsave.qs")),
    qs2::qs_save(test_df, paste0(test_dir, "test_qs_save.qs2")),
    qs2::qd_save(test_df, paste0(test_dir, "test_qd_save.qd")),
    readODS::write_ods(test_df, paste0(test_dir, "test_write_ods.ods")),
    readr::write_rds(test_df, paste0(test_dir, "test_readr_rds.rds")),
    readr::write_csv(test_df, paste0(test_dir, "test_readr_csv.csv")),
    RSQLite::dbWriteTable(sqlite, "test", test_df, overwrite = T),
    RWeka::write.arff(test_df, paste0(test_dir, "test_write.arff.arff")),
    saveRDS(test_df, paste0(test_dir, "test_saveRDS.RDS")),
    vroom::vroom_write(test_df, paste0(test_dir, "test_vroom_write.csv")),
    write((rjson::toJSON(test_df)), paste0(test_dir, "test_rjson.json")),
    write.csv(test_df, paste0(test_dir, "test_write.csv.csv")),
    writexl::write_xlsx(test_df,paste0(test_dir, "test_writexl_xlsx.xlsx"))
  )
  
  # Runs write tests
  write_results = microbenchmark::microbenchmark(list = write_tests[which_tests],
                                                 times = iterations) %>%
    mutate(benchmark_type = "write",
           date_time = Sys.time())
  
  
  # Disconnects from and Reconnects to DBs ----
  if("duckDB" %in% tests){ 
    try(dbDisconnect(duckDB_connection), silent = T)
    duckDB_connection <- dbConnect(duckdb::duckdb(dbdir = paste0(test_dir, "test_dbWriteTable.duckdb")))
  }
  
  if("RSQLite" %in% tests){
    try(dbDisconnect(sqlite), silent = T)
    sqlite <- dbConnect(RSQLite::SQLite(), paste0(test_dir, "test_dbWriteTable.sqlite"))
  }
  
  # Reading Benchmarks Tests ----
  
  # Lists unevaluated write tests expressions
  read_tests = alist(
    arrow::read_csv_arrow(paste0(test_dir, "test_arrow_csv.csv")),
    arrow::read_feather(paste0(test_dir, "test_arrow_feather.feather")),
    arrow::read_parquet(paste0(test_dir, "test_arrow_parquet.parquet")),
    data.table::fread(paste0(test_dir, "test_fwrite.csv")),
    dbGetQuery(duckDB_connection, "SELECT * FROM tests"),
    feather::read_feather(paste0(test_dir, "test_feather.feather")),
    foreign::read.dbf(paste0(test_dir, "test_write.dbf.dbf")),
    fst::read_fst(paste0(test_dir, "test_write_fst.fst")),
    haven::read_dta(paste0(test_dir, "test_write_dta.dta")),
    haven::read_sav(paste0(test_dir, "test_write_sav.sav")),
    haven::read_xpt(paste0(test_dir, "test_write_xpt.xpt")),
    openxlsx::read.xlsx(paste0(test_dir, "test_openxlsx.xlsx")),
    qs::qread(paste0(test_dir, "test_qsave.qs")),
    qs2::qs_read(paste0(test_dir, "test_qs_save.qs2")),
    qs2::qd_read(paste0(test_dir, "test_qd_save.qd")),
    readODS::read_ods(paste0(test_dir, "test_write_ods.ods")),
    readr::read_rds(paste0(test_dir, "test_readr_rds.rds")),
    readr::read_csv(paste0(test_dir, "test_readr_csv.csv")),
    dbGetQuery(sqlite, 'SELECT * FROM test'),
    RWeka::read.arff(paste0(test_dir, "test_write.arff.arff")),
    readRDS(paste0(test_dir, "test_saveRDS.RDS")),
    vroom::vroom(paste0(test_dir, "test_vroom_write.csv"), delim = ","),
    rjson::fromJSON(file=paste0(test_dir, "test_rjson.json")),
    read.csv(paste0(test_dir, "test_write.csv.csv")),
    readxl::read_xlsx(paste0(test_dir, "test_writexl_xlsx.xlsx"))
  )
  
  # Runs read tests
  read_results = microbenchmark::microbenchmark(list = read_tests[which_tests],
                                                times = iterations) %>%
    mutate(benchmark_type = "read",
           date_time = Sys.time())
  
  # Organizes Output ----
  
  # Combines read, write, and file size results into a single dataframe
  results = bind_rows(
    read_results %>%
      left_join(
        data.frame(expr = c(as.character(unlist(read_tests))),
                   format = all_tests,
                   file_name = c(
                     paste0(test_dir, "test_arrow_csv.csv"),
                     paste0(test_dir, "test_arrow_feather.feather"),
                     paste0(test_dir, "test_arrow_parquet.parquet"),
                     paste0(test_dir, "test_fwrite.csv"),
                     paste0(test_dir, "test_dbWriteTable.duckdb"),
                     paste0(test_dir, "test_feather.feather"),
                     paste0(test_dir, "test_write.dbf.dbf"),
                     paste0(test_dir, "test_write_fst.fst"),
                     paste0(test_dir, "test_write_dta.dta"),
                     paste0(test_dir, "test_write_sav.sav"),
                     paste0(test_dir, "test_write_xpt.xpt"),
                     paste0(test_dir, "test_openxlsx.xlsx"),
                     paste0(test_dir, "test_qsave.qs"),
                     paste0(test_dir, "test_qs_save.qs2"),
                     paste0(test_dir, "test_qd_save.qd"),
                     paste0(test_dir, "test_write_ods.ods"),
                     paste0(test_dir, "test_readr_rds.rds"),
                     paste0(test_dir, "test_readr_csv.csv"),
                     paste0(test_dir, "test_dbWriteTable.sqlite"),
                     paste0(test_dir, "test_write.arff.arff"),
                     paste0(test_dir, "test_saveRDS.RDS"),
                     paste0(test_dir, "test_vroom_write.csv"),
                     paste0(test_dir, "test_rjson.json"),
                     paste0(test_dir, "test_write.csv.csv"),
                     paste0(test_dir, "test_writexl_xlsx.xlsx")
                   )
        ),
        by = "expr"),
    write_results %>%
      left_join(data.frame(expr = c(as.character(unlist(write_tests))),
                           format = all_tests,
                           file_name = c(
                             paste0(test_dir, "test_arrow_csv.csv"),
                             paste0(test_dir, "test_arrow_feather.feather"),
                             paste0(test_dir, "test_arrow_parquet.parquet"),
                             paste0(test_dir, "test_fwrite.csv"),
                             paste0(test_dir, "test_dbWriteTable.duckdb"),
                             paste0(test_dir, "test_feather.feather"),
                             paste0(test_dir, "test_write.dbf.dbf"),
                             paste0(test_dir, "test_write_fst.fst"),
                             paste0(test_dir, "test_write_dta.dta"),
                             paste0(test_dir, "test_write_sav.sav"),
                             paste0(test_dir, "test_write_xpt.xpt"),
                             paste0(test_dir, "test_openxlsx.xlsx"),
                             paste0(test_dir, "test_qsave.qs"),
                             paste0(test_dir, "test_qs_save.qs2"),
                             paste0(test_dir, "test_qd_save.qd"),
                             paste0(test_dir, "test_write_ods.ods"),
                             paste0(test_dir, "test_readr_rds.rds"),
                             paste0(test_dir, "test_readr_csv.csv"),
                             paste0(test_dir, "test_dbWriteTable.sqlite"),
                             paste0(test_dir, "test_write.arff.arff"),
                             paste0(test_dir, "test_saveRDS.RDS"),
                             paste0(test_dir, "test_vroom_write.csv"),
                             paste0(test_dir, "test_rjson.json"),
                             paste0(test_dir, "test_write.csv.csv"),
                             paste0(test_dir, "test_writexl_xlsx.xlsx")
                           )
      ),
      by = "expr")
  ) %>%
    mutate(file_size = file.info(file_name)$size)
  
  # Clean up ----
  
  # Closes database connections
  if("duckDB" %in% tests){ 
    dbDisconnect(duckDB_connection)
  }
  
  if("RSQLite" %in% tests){
    dbDisconnect(sqlite)
  }
  
  # Saves results if test data contains more than 100 rows
  if(nrow(test_df) > 100){
    if(!exists("system_info")){
      system_info = list(get_ram(),
                         get_cpu()$model_name,
                         get_cpu()$no_of_cores,
                         get_platform_info()$OS.type,
                         paste0(get_r_version()$version.string, " ", get_platform_info()$r_arch)
      )
    }
    
    # Save results
    results_list = list(res = results,
                        sys = system_info,
                        rows = nrow(test_df),
                        cols = ncol(test_df),
                        session = sessionInfo())
    saveRDS(results_list, paste0(test_dir, "/results/benchmark_results_", (if("dataset_name" %in% names(test_df)){paste0(test_df$dataset_name[1],"_")}), nrow(test_df), "_rows_", ncol(test_df), "_cols_", Sys.Date(),".RDS" ))
  }
  
  # Returns test results
  return(results)
}


# Collect system information for graphic headers
system_info = list(get_ram(),
                   get_cpu()$model_name,
                   get_cpu()$no_of_cores,
                   get_platform_info()$OS.type,
                   paste0(get_r_version()$version.string, " ", get_platform_info()$r_arch)
)


# Creates table of format labels for use in UI
file_formats = 
  data.frame(format = c(
    "arff",
    "arrow_csv",
    "baseR_csv",
    "fwrite_csv",
    "vroom_csv",
    "dbf",
    "dta",
    "duckDB",
    "arrow_feather",
    "feather",
    "fst",
    "json",
    "ods",
    "arrow_parquet",
    "qs",
    "qs2",
    "qdata",
    "baseR_rds",
    "RSQLite",
    "readr_rds",
    "readr_csv",
    "sav",
    "open.xlsx_xlsx",
    "writexl_xlsx",
    "xpt"), 
    format_name = c(
      "ARFF (ASCII)",
      "CSV (Arrow)",
      "CSV (Base R)",
      "CSV (Fwrite)",
      "CSV (Vroom)",
      "DBF (dBase)",
      "DTA (Stata)",
      "DUCKDB",
      "FEATHER (Arrow)",
      "FEATHER (Feather)",
      "FST (R, Python)",
      "JSON (rjson)",
      "ODS (OpenDocument)",
      "PARQUET (Arrow)",
      "QS (R)",
      "QS2 (R)",
      "QData (R, Python)",
      "RDS (Base R)",
      "RSQLITE",
      "RDS (readr)",
      "CSV (readr)",
      "SAV (SPSS)",
      "XLSX (Open.Xlsx)",
      "XLSX (writexl)",
      "XPT (SAS)"))

# (2) UI ----
# This section generates a single shiny user interface object

ui <- fluidPage(
  # Adds title
  titlePanel("File Format Benchmarking Tool"),
  
  # Prevents temporary error messages from appearing during loading 
  tags$style(type="text/css",
             ".shiny-output-error{ visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  # Adds upload switch and menu
  fluidRow(
    column(2, materialSwitch(
      inputId = "use_upload",
      label = "Upload Dataset",
      value = F,
      status = "success",
      right = T
    )),
    
    column(2,  materialSwitch(
      inputId = "hide_slow_formats",
      label = "Hide Slow Formats",
      value = F,
      status = "success",
      right = T
    ))),
  
  conditionalPanel(
    condition = "input.use_upload == true",
    h4("Upload CSV, XLSX, or RDS File"),
    # https://shiny.posit.co/r/reference/shiny/1.5.0/fileinput  
    fluidRow(column(12, fileInput("upload", NULL , accept = c(".csv", ".tsv", ".xlsx", ".RDS")))    
    )
  ),
  
  # Menu to adjust data generating parameters
  h4("Generate Synthetic Data"),
  fluidRow(
    column(2, numericInput("synthetic_rows",
                           "Rows",
                           value = 10^2)),
    column(2, numericInput("integer_columns",
                           "Integer Columns",
                           value = 10)),
    column(2, numericInput("integer_length",
                           "Integer Length",
                           value = 10)),
    column(2, numericInput("decimal_columns",
                           "Decimal Columns",
                           value = 10)),
    column(2, numericInput("string_columns",
                           "String Columns",
                           value = 10)),
    column(2, numericInput("string_length",
                           "String Length",
                           value = 10))
  ),
  
  # Benchmarking parameters menu
  # Note: Adjusting CPU cores is currently disabled.
  h4("Benchmark Options"),
  fluidRow(
    column(4, 
           uiOutput("test_formats_UI")),
    column(4, numericInput("iterations",
                           "Number of Iterations",
                           width = "100%",
                           value = 1)),
    column(4, numericInput("cores",
                           "Number of CPU Cores (Default: System Max; Currently Disabled)",
                           value = system_info[3],
                           width = "100%",
                           min = 1,
                           max = system_info[3]
    )),
    
  ),
  
  # Large processing button
  actionBttn(
    inputId = "go",
    label = "Benchmark Formats",
    style = "fill",
    block = T,
    color = "success"
  ),
  
  # Tabs that display graphics and tables
  tabsetPanel(
    tabPanel("Graphical Results",  
             
             radioGroupButtons(
               "sort_variable",
               NULL,
               choices = c("Sort by Read Time", 
                           "Sort by Write Time",
                           "Sort by File Size"),
               selected = c("Sort by Read Time"),
               justified = T
             )
             ,
             if(use_plotly == T){
               plotlyOutput("figure",
                            height = "600px",
                            width = "100%") %>% withSpinner
             }else{
               plotOutput("figure",
                          height = "600px",
                          width = "100%") %>% withSpinner
             }
    ),
    tabPanel("Synthetic Data", DT::dataTableOutput("synthetic_data")),
    tabPanel("Uploaded Data", DT::dataTableOutput("uploaded_data")),
    tabPanel("Raw Results", DT::dataTableOutput("raw_results")),
    tabPanel("Processed Results", DT::dataTableOutput("processed_result_table"))
  )
)

# (3) Server ----
# This section builds the shiny server function

server <- function(input, output) {

# The following commented variables are useful for troubleshooting changes. They should remain commented.
  # input = list()
  # input$integer_columns = 10
  # input$decimal_columns = 10
  # input$string_columns = 10
  # input$synthetic_rows = 10
  # input$iterations = 1
  # input$string_length = 10
  # input$test_formats = c("baseR_rds", "qs", "baseR_csv", "arrow_csv" )
  # input$test_formats = c("dta", "xpt", "sav" )
  # input$sort_variable = "Sort by File Size"
  # synthetic_df = function(){synthetic}
  # test_results = function(){test}
  # processed_results = function(){long_results}
  
  output$test_formats_UI <- renderUI({
    if(input$hide_slow_formats == T){
      selected_file_formats = file_formats %>%
        filter(!format %in% c("arff",
                              "baseR_csv",
                              "dbf",
                              "dta",
                              "json",
                              "readr_rds",
                              "sav",
                              "xpt",
                              "vroom_csv",
                              "ods",
                              "open.xlsx_xlsx",
                              "writexl_xlsx",
                              "readr_csv"
        ))
    }else{
      selected_file_formats = file_formats
    }
    
    pickerInput(
      inputId = "test_formats",
      label = "Formats", 
      choices = selected_file_formats$format %>% 
        setNames(selected_file_formats$format_name),
      selected = selected_file_formats$format,
      multiple = T,
      width = "100%",
      options = list(`actions-box` = TRUE)
    )
  })
  
  upload_data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name) %>% tolower()
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           xlsx = readxl::read_xlsx(input$upload$datapath),
           xls = readxl::read_xls(input$upload$datapath),
           rds = readRDS(input$upload$datapath),
           validate("Invalid file; Please upload a .csv, .tsv, .xls, .xlsx, or .rds file")
    )
  })
  
  synthetic_df = reactive({
    
    synthetic = data.frame(
      replicate(input$integer_columns, sample(0:100000, input$synthetic_rows, rep = TRUE)),
      replicate(input$decimal_columns, runif(input$synthetic_rows, 0.0, 1.0)),
      replicate(input$string_columns, stri_rand_strings(input$synthetic_rows, input$string_length))
    ) %>%
      setNames(names(.) %>% gsub("\\.", "_", .)) %>%
      mutate(rows_cols = paste0(nrow(.), "_", ncol(.)),
             dataset_name = "synthetic_data") %>%
      data.frame()
  })
  
  test_results = eventReactive(#ifelse(input$go == 0, 0, input$go),
    input$go,
    {
      if(input$use_upload == T){
        selected_data = upload_data()
      }else{
        selected_data = synthetic_df()
      }          
      test = test_performance(
        test_df = selected_data,
        iterations = (input$iterations),
        tests = (input$test_formats),
        test_dir = testing_location) 
    }) 
  
  processed_results = reactive({
    
    results = test_results() %>%
      ungroup %>%
      group_by(format, benchmark_type) %>%
      summarise(value = median(time/10^9, na.rm = T),
                file_size = mean(file_size/10^6, na.rm = T))
    
    long_results = bind_rows(
      results %>%
        filter(benchmark_type == "write") %>%
        select(-c(file_size)) %>%
        rename(variable = benchmark_type),
      
      results %>%
        filter(benchmark_type == "read") %>%
        select(-c(file_size)) %>%
        rename(variable = benchmark_type),
      
      results %>%
        filter(benchmark_type == "write") %>%
        select(-c(value)) %>%
        rename(variable = benchmark_type,
               value = file_size) %>%
        mutate(variable = "File Size (MB)")
    ) %>%
      mutate(variable = case_when(variable == "read" ~ "Read Time (s)",
                                  variable == "write" ~ "Write Time (s)",
                                  T ~ variable),
             variable = factor(variable, 
                               levels = c("Read Time (s)", "Write Time (s)", "File Size (MB)") 
             )) %>%
      left_join(file_formats,
                by = "format") 
    
  })
  
  output$uploaded_data = renderDataTable({
    datatable(upload_data(),
              extensions = 'Buttons',
              rownames = F,
              options = list(
                dom = 'Bft',
                pageLength = 100,
                lengthMenu = list(c(10, 100, 1000, -1), c('10', '100', '1000', 'All')),
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ))
  })
  
  output$synthetic_data = renderDataTable({
    datatable(synthetic_df(),
              extensions = 'Buttons',
              rownames = F,
              options = list(
                dom = 'Bft',
                pageLength = 100,
                lengthMenu = list(c(10, 100, 1000, -1), c('10', '100', '1000', 'All')),
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ))
  })
  
  output$raw_results = renderDataTable({
    datatable(test_results(),
              extensions = 'Buttons',
              rownames = F,
              options = list(
                dom = 'Bft',
                pageLength = 100,
                lengthMenu = list(c(10, 100, 1000, -1), c('10', '100', '1000', 'All')),
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ))
  })
  
  output$processed_result_table = renderDataTable({
    datatable(processed_results() %>%
                pivot_wider(names_from = variable),
              extensions = 'Buttons',
              rownames = F,
              options = list(
                dom = 'Bft',
                pageLength = 100,
                lengthMenu = list(c(10, 100, 1000, -1), c('10', '100', '1000', 'All')),
                buttons = c('copy', 'csv', 'excel', 'pdf')
              )) 
  })
  
  
  
  output$figure = if(use_plotly == T) {renderPlotly({
    
    fig_df = processed_results() 
    
    sort_var = (case_when(input$sort_variable == "Sort by Read Time" ~ "Read Time (s)",
                          input$sort_variable == "Sort by Write Time" ~ "Write Time (s)",
                          input$sort_variable == "Sort by File Size" ~ "File Size (MB)")) 
    levels_vector = fig_df %>%
      filter(variable == !!sort_var) %>%
      arrange(desc(value)) %>%
      pull(format_name) %>% 
      unique
    
    fig_df = fig_df %>%
      mutate(format_name = factor(format_name, 
                                  levels = levels_vector
      ))
    
    p = ggplot(fig_df,
               aes(x = value, y = format_name, fill = format_name)) +
      geom_col() +
      facet_wrap(~variable, scales = "free_x") +
      xlab("") +
      ylab("") +
      theme(legend.position = "none") + 
      viridis::scale_fill_viridis(discrete = T) +
      ggtitle(" ")
    
    ggplotly(p) %>%
      layout(title = list(text = paste0("OS: ", system_info[4], " - RAM: ", round(as.numeric(system_info[1])/1024^3, digits = 1), "GB - Cores: ", system_info[3], " - CPU: ", system_info[2], " - ", system_info[5])))
    
    
  })
  }else{
    renderPlot({
      fig_df = processed_results() 
      
      sort_var = (case_when(input$sort_variable == "Sort by Read Time" ~ "Read Time (s)",
                            input$sort_variable == "Sort by Write Time" ~ "Write Time (s)",
                            input$sort_variable == "Sort by File Size" ~ "File Size (MB)")) 
      levels_vector = fig_df %>%
        filter(variable == !!sort_var) %>%
        arrange(desc(value)) %>%
        pull(format_name) %>% 
        unique
      
      fig_df = fig_df %>%
        mutate(format_name = factor(format_name, 
                                    levels = levels_vector
        ))
      
      p = ggplot(fig_df,
                 aes(x = value, y = format_name, fill = format_name)) +
        geom_col() +
        facet_wrap(~variable, scales = "free_x") +
        xlab("") +
        ylab("") +
        theme(legend.position = "none") + 
        viridis::scale_fill_viridis(discrete = T) +
        ggtitle(paste0("OS: ", system_info[4], " - RAM: ", round(as.numeric(system_info[1])/1024^3, digits = 1), "GB - Cores: ", system_info[3], " - CPU: ", system_info[2], " - ", system_info[5]))
      
      p
    })
  }
}

# Runs application
shinyApp(ui, server)
