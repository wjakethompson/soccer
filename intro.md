
# Introduction {#intro}



## European soccer format



### Domestic leagues



### UEFA Champions League



### Domestic cups



## Document Organization



## Colophon

The source code for this document can be found at <https://github.com/wjakethompson/soccer>. The document was written with [**bookdown**](https://bookdown.org) [@R-bookdown], which simplifies the process of turning multiple R markdown files into a single output file (e.g., HTML, PDF, EPUB).

This document was built with:


```r
devtools::session_info(c("bookdown", "knitr", "rmarkdown", "rvest", "purrr",
  "dplyr", "lubridate", "rstan", "ggplot2", "DT"))
#> Session info --------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.3.2 (2016-10-31)
#>  system   x86_64, linux-gnu           
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  tz       <NA>                        
#>  date     2017-02-25
#> Packages ------------------------------------------------------------------
#>  package      * version   date       source                           
#>  assertthat     0.1       2013-12-06 cran (@0.1)                      
#>  backports      1.0.5     2017-01-18 cran (@1.0.5)                    
#>  base64enc      0.1-3     2015-07-28 cran (@0.1-3)                    
#>  BH             1.62.0-1  2016-11-19 cran (@1.62.0-)                  
#>  bitops         1.0-6     2013-08-17 cran (@1.0-6)                    
#>  bookdown       0.3.9     2017-02-22 Github (rstudio/bookdown@a5ad84c)
#>  caTools        1.17.1    2014-09-10 cran (@1.17.1)                   
#>  colorspace     1.3-2     2016-12-14 cran (@1.3-2)                    
#>  curl           2.3       2016-11-24 CRAN (R 3.3.2)                   
#>  DBI            0.5-1     2016-09-10 cran (@0.5-1)                    
#>  dichromat      2.0-0     2013-01-24 cran (@2.0-0)                    
#>  digest         0.6.12    2017-01-27 CRAN (R 3.3.2)                   
#>  dplyr          0.5.0     2016-06-24 cran (@0.5.0)                    
#>  DT             0.2       2016-08-09 cran (@0.2)                      
#>  evaluate       0.10      2016-10-11 cran (@0.10)                     
#>  ggplot2        2.2.1     2016-12-30 cran (@2.2.1)                    
#>  gridExtra      2.2.1     2016-02-29 cran (@2.2.1)                    
#>  gtable         0.2.0     2016-02-26 cran (@0.2.0)                    
#>  highr          0.6       2016-05-09 cran (@0.6)                      
#>  htmltools      0.3.5     2016-03-21 cran (@0.3.5)                    
#>  htmlwidgets    0.8       2016-11-09 cran (@0.8)                      
#>  httr           1.2.1     2016-07-03 CRAN (R 3.3.2)                   
#>  inline         0.3.14    2015-04-13 cran (@0.3.14)                   
#>  jsonlite       1.2       2016-12-31 CRAN (R 3.3.2)                   
#>  knitr          1.15.1    2016-11-22 cran (@1.15.1)                   
#>  labeling       0.3       2014-08-23 cran (@0.3)                      
#>  lattice        0.20-34   2016-09-06 CRAN (R 3.3.2)                   
#>  lazyeval       0.2.0     2016-06-12 cran (@0.2.0)                    
#>  lubridate      1.6.0     2016-09-13 cran (@1.6.0)                    
#>  magrittr       1.5       2014-11-22 cran (@1.5)                      
#>  markdown       0.7.7     2015-04-22 cran (@0.7.7)                    
#>  MASS           7.3-45    2016-04-21 CRAN (R 3.3.2)                   
#>  Matrix         1.2-7.1   2016-09-01 CRAN (R 3.3.2)                   
#>  mime           0.5       2016-07-07 CRAN (R 3.3.2)                   
#>  munsell        0.4.3     2016-02-13 cran (@0.4.3)                    
#>  openssl        0.9.6     2016-12-31 CRAN (R 3.3.2)                   
#>  plyr           1.8.4     2016-06-08 cran (@1.8.4)                    
#>  purrr          0.2.2     2016-06-18 cran (@0.2.2)                    
#>  R6             2.2.0     2016-10-05 CRAN (R 3.3.2)                   
#>  RColorBrewer   1.1-2     2014-12-07 cran (@1.1-2)                    
#>  Rcpp           0.12.9.3  2017-02-22 Github (RcppCore/Rcpp@8bf15c0)   
#>  RcppEigen      0.3.2.9.0 2016-08-21 cran (@0.3.2.9)                  
#>  reshape2       1.4.2     2016-10-22 cran (@1.4.2)                    
#>  rmarkdown      1.3       2016-12-21 cran (@1.3)                      
#>  rprojroot      1.2       2017-01-16 cran (@1.2)                      
#>  rstan          2.14.1    2016-12-28 cran (@2.14.1)                   
#>  rvest          0.3.2     2016-06-17 cran (@0.3.2)                    
#>  scales         0.4.1     2016-11-09 cran (@0.4.1)                    
#>  selectr        0.3-1     2016-12-19 cran (@0.3-1)                    
#>  StanHeaders    2.14.0-1  2017-01-09 cran (@2.14.0-)                  
#>  stringi        1.1.2     2016-10-01 cran (@1.1.2)                    
#>  stringr        1.2.0     2017-02-18 cran (@1.2.0)                    
#>  tibble         1.2       2016-08-26 cran (@1.2)                      
#>  xml2           1.1.1     2017-01-24 cran (@1.1.1)                    
#>  yaml           2.1.14    2016-11-12 cran (@2.1.14)
```
