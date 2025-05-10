# kid-knapping
Research compendium for 'What lithics can tell us about the evolution of learning and development' (Archaeometry).

## Getting Started
Please open the Rproj file instead of the Rcode directly to make sure relative paths work!

## File Structure
The repository is organised into four main directories: analysis, data, and figure.

### code
* `analysis.R` ... This is the R script generating the main results and figures of the experiment, especially the multi-level model of knapping performance.

### data
* `individual__data_by_participant.csv`... This csv file documents relevant attributes, particularly motor and psychometric test results, at the individual research participant level.
* `lithic_data_by_core.csv`... This csv file documents morphological and technological attributes of lithics at the core level.

### figure
* `Fig1.jpg` ... This is Fig.1 in the manuscript.
* `Fig2.png` ... This is Fig.2 in the manuscript.
* `Fig3.png` ... This is Fig.3 in the manuscript.

## Dependencies
The code has been successfully executed on on CL's PC with the following R settings.
* CL's R setting
 ```
R version 4.4.0 (2024-04-24 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rempsyc_0.1.9      see_0.9.0          performance_0.12.4 glmulti_1.0.8     
 [5] leaps_3.2          rJava_1.0-11       ggpubr_0.6.0       patchwork_1.3.0   
 [9] lmerTest_3.1-3     lubridate_1.9.3    forcats_1.0.0      stringr_1.5.1     
[13] dplyr_1.1.4        purrr_1.0.2        readr_2.1.5        tidyr_1.3.1       
[17] tibble_3.2.1       ggplot2_3.5.1      tidyverse_2.0.0    lme4_1.1-35.5     
[21] Matrix_1.7-0      

loaded via a namespace (and not attached):
 [1] utf8_1.2.4          generics_0.1.3      rstatix_0.7.2       stringi_1.8.4      
 [5] lattice_0.22-6      hms_1.1.3           magrittr_2.0.3      grid_4.4.0         
 [9] timechange_0.3.0    backports_1.4.1     fansi_1.0.6         scales_1.3.0       
[13] numDeriv_2016.8-1.1 abind_1.4-5         cli_3.6.2           rlang_1.1.5        
[17] munsell_0.5.1       splines_4.4.0       withr_3.0.2         tools_4.4.0        
[21] tzdb_0.4.0          nloptr_2.1.1        ggsignif_0.6.4      minqa_1.2.7        
[25] colorspace_2.1-0    boot_1.3-30         broom_1.0.5         vctrs_0.6.5        
[29] R6_2.5.1            lifecycle_1.0.4     car_3.1-2           MASS_7.3-60.2      
[33] insight_0.20.5      pkgconfig_2.0.3     pillar_1.9.0        gtable_0.3.5       
[37] glue_1.7.0          Rcpp_1.0.12         tidyselect_1.2.1    rstudioapi_0.17.1  
[41] farver_2.1.1        nlme_3.1-164        carData_3.0-5       compiler_4.4.0  
 ``` 

## Help

Please contact raylc1996@outlook.com if you have any questions related to the code or data.