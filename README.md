# File Format Benchmarking Tool Version 0.1

The file, "app.R", is a self-contained application that enables users to perform file format benchmarking via a shiny interface. Please direct any questions, suggestions, or criticisms about the application to schwinnr (at) sec.gov.

## DISCLAIMER

Testing results suggest that file format performance varies significantly across data and hardware characteristics. Furthermore, as of November 2024, no single format dominates all other formats across read speed, write speed, and file size. Thus, this tool does not endorse any file format.

## Getting Started

To run the application open the "app.R" file in RStudio, install the suggested libraries, and press the "Run App" button. 

## Testing Performance

The green bar labeled "Benchmark Formats", in the middle of the shiny interface, performs read, write, and file size performance tests for the selected file formats. The default test data can be extended across multiple parameters using input menus. The "Upload Dataset" toggle accepts files up to 10GB for testing. One may want to perform benchmarks for various file locations, such as attached storage or local and network drives. To do so, simply assign a new "testing_location" in the "app.R" file. The default setting creates a "tests" sub-directory of the folder containing the "app.R" file.

## Results

The results appear in graphics and tables across tabs in the bottom half of the interface. If a dataset greater than 100 rows is tested, then raw results are saved to a "results" sub-directory of the testing folder.

## Session Info

The following sessionInfo() output reports version information for attached and loaded packages that resulted in a successful deployment of the shiny interface.

R version 4.4.0 (2024-04-24 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] fstcore_0.9.18        qs_0.27.2             openxlsx_4.2.7.1      rjson_0.2.23          readODS_2.3.0         RWeka_0.4-46         
 [7] haven_2.5.4           fst_0.9.8             arrow_17.0.0.1        vroom_1.6.5           RSQLite_2.3.7         duckdb_1.1.0         
[13] DBI_1.2.3             microbenchmark_1.5.0  benchmarkme_1.0.8     plotly_4.10.4         stringi_1.8.4         shinycssloaders_1.1.0
[19] scales_1.3.0          bit64_4.5.2           bit_4.5.0             shinyWidgets_0.8.6    DT_0.33               shiny_1.9.1          
[25] lubridate_1.9.3       forcats_1.0.0         stringr_1.5.1         dplyr_1.1.4           purrr_1.0.2           readr_2.1.5          
[31] tidyr_1.3.1           tibble_3.2.1          ggplot2_3.5.1         tidyverse_2.0.0      

loaded via a namespace (and not attached):
 [1] gridExtra_2.3         writexl_1.5.1         readxl_1.4.3          rlang_1.1.4           magrittr_2.0.3        compiler_4.4.0       
 [7] feather_0.3.5         vctrs_0.6.5           pkgconfig_2.0.3       crayon_1.5.3          fastmap_1.2.0         labeling_0.4.3       
[13] utf8_1.2.4            promises_1.3.0        rmarkdown_2.28        tzdb_0.4.0            xfun_0.48             cachem_1.1.0         
[19] jsonlite_1.8.9        blob_1.2.4            later_1.3.2           minty_0.0.1           parallel_4.4.0        R6_2.5.1             
[25] bslib_0.8.0           jquerylib_0.1.4       cellranger_1.1.0      Rcpp_1.0.13           assertthat_0.2.1      iterators_1.0.14     
[31] knitr_1.48            httpuv_1.6.15         Matrix_1.7-0          timechange_0.3.0      tidyselect_1.2.1      rstudioapi_0.16.0    
[37] yaml_2.3.10           viridis_0.6.5         stringfish_0.16.0     doParallel_1.0.17     codetools_0.2-20      lattice_0.22-6       
[43] withr_3.0.1           benchmarkmeData_1.0.4 evaluate_1.0.1        foreign_0.8-87        RcppParallel_5.1.9    rJava_1.0-11         
[49] zip_2.3.1             pillar_1.9.0          foreach_1.5.2         generics_0.1.3        hms_1.1.3             munsell_0.5.1        
[55] RApiSerialize_0.1.4   xtable_1.8-4          qs2_0.1.1             glue_1.8.0            lazyeval_0.2.2        tools_4.4.0          
[61] data.table_1.16.0     RWekajars_3.9.3-2     grid_4.4.0            crosstalk_1.2.1       colorspace_2.1-1      cli_3.6.2            
[67] fansi_1.0.6           viridisLite_0.4.2     gtable_0.3.5          sass_0.4.9            digest_0.6.37         htmlwidgets_1.6.4    
[73] memoise_2.0.1         htmltools_0.5.8.1     lifecycle_1.0.4       httr_1.4.7            mime_0.12      