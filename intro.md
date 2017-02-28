
# Introduction {#intro}

The goal of this project is to predict winners of the major domestic European soccer leagues and the UEFA Champions League. Necessarily, this means that the real goal is to accurately predict the outcomes of individual games. To do this, I estimate team abilities using Bayesian mixed effects models. The estimated abilities can then be used to predict the outcomes of games that have yet to be played.

Before diving into the process of developing and estimating these models, it will be useful to have a brief introduction to how competitions are structured for European soccer teams. An understanding of the various competitions will help clarify why teams are or are not included in the estimation of the model, and make more obvious how predictions should be made.

## European soccer format

Unlike most American sports leagues (e.g., NFL, NBA, MLB, etc.), European soccer teams compete in a variety of domestic and international competitions. In general, these competitions fall into one of three major categories:

1. Domestic leagues
1. Domestic cups
1. FIFA/UEFA Competitions

### Domestic leagues {#dom-league-descrip}

Domestic leagues are most analogous to the regular season of the major American sports. In most domestic European soccer leagues, the league follows a double round robin structure with each team playing every other team twice: once at home and once away. Teams are awarded 3 points for a victory, 1 point for a tie, and 0 points for a loss. Once all the games have been played, the team with the most points wins the league.

### Domestic cups

European leagues also host domestic cups. For the most part, domestic tournaments consist of all eligible professional teams from a country, not just the teams from the highest league (this would be analogous to a baseball tournament where minor league and major league teams all competed against each other). These cups can either be straight knockout style tournaments (e.g., March Madness for NCAA basketball), or each leg can have two legs where the teams involved play twice, once at each team's home stadium. Because of the vast number of teams involved in these cups, entrance into the tournament is usually staggered so that the best teams are guaranteed places in the later rounds.

### FIFA and UEFA Competitions

These are the international tournaments. For this project, the focus is on the UEFA Champions League. This tournament consists of several qualifying rounds before the tournament proper (e.g., play-in games). Once the tournament proper begins, the final 32 teams are placed into 8 groups of 4 teams each. A team plays every team in its group twice, once at home and once away, with points awarded the same as for domestic leagues (see section \@ref(dom-league-descrip)). At the end of group play, the two teams with the most points from each group advance to the final 16. From this point on, the tournament follows the structure of a knockout tournament, with each round having two legs (one home and one away game). This continues until the final, which is only a single game played at a neutral location.

## Document organization

This document steps through the process that was followed to create the predictions for the major European domestic leagues and the UEFA Champions League. I start by defining two possible models for estimating club ability in Section \@ref(define-model). I then conduct a small scale simulation study in Section \@ref(simulation) to determine if one of the models is better able to recover estimates of team ability. Sections \@ref(gather-data) and \@ref(fit-model) focus on gather the data to be used and estimating the model respectively. In Section \@ref(predict), the estimates from the model are used to predict the outcomes of the domestic leagues and the UEFA Champions League, and output graphics are created. Finally, Section \@ref(conclusion) outlines the limitations of this approach and possible improvements to the model.

## Colophon

The source code for this document can be found at <https://github.com/wjakethompson/soccer>. The document was written with [**bookdown**](https://bookdown.org) [@R-bookdown], which simplifies the process of turning multiple R markdown files into a single output file (e.g., HTML, PDF, EPUB).

This document was built with:


```r
load("_data/session_info.rda")
devtools:::print.session_info(session_info)
#> Session info --------------------------------------------------------------
#>  setting  value                       
#>  version  R version 3.3.2 (2016-10-31)
#>  system   x86_64, darwin13.4.0        
#>  ui       RStudio (1.0.136)           
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  tz       America/Chicago             
#>  date     2017-02-27
#> Packages ------------------------------------------------------------------
#>  package               * version    date      
#>  assertthat              0.1        2013-12-06
#>  backports               1.0.4      2016-10-24
#>  base64enc               0.1-3      2015-07-28
#>  BH                      1.62.0-1   2016-11-19
#>  bitops                  1.0-6      2013-08-17
#>  bookdown                0.3.7      2017-01-15
#>  car                     2.1-3      2016-08-11
#>  caTools                 1.17.1     2014-09-10
#>  colorspace              1.2-6      2015-03-11
#>  curl                    2.1        2016-09-22
#>  DBI                     0.5-1      2016-09-10
#>  dichromat               2.0-0      2013-01-24
#>  digest                  0.6.11     2017-01-03
#>  dplyr                 * 0.5.0      2016-06-24
#>  DT                      0.2        2016-08-09
#>  evaluate                0.10       2016-10-11
#>  ggplot2               * 2.2.1.9000 2017-01-26
#>  gridExtra               2.2.1      2016-02-29
#>  gtable                  0.2.0      2016-02-26
#>  highr                   0.6        2016-05-09
#>  htmltools               0.3.5      2016-03-21
#>  htmlwidgets             0.7        2016-08-02
#>  httr                    1.2.1      2016-07-03
#>  inline                  0.3.14     2015-04-13
#>  jsonlite                1.2        2016-12-31
#>  knitr                   1.15.1     2016-11-22
#>  labeling                0.3        2014-08-23
#>  lattice                 0.20-34    2016-09-06
#>  lazyeval                0.2.0.9000 2016-09-19
#>  lme4                    1.1-12     2016-04-16
#>  lubridate             * 1.6.0.9009 2017-01-24
#>  magrittr                1.5        2014-11-22
#>  markdown                0.7.7      2015-04-22
#>  MASS                    7.3-45     2016-04-21
#>  Matrix                  1.2-7.1    2016-09-01
#>  MatrixModels            0.4-1      2015-08-22
#>  mgcv                    1.8-15     2016-09-14
#>  mime                    0.5        2016-07-07
#>  minqa                   1.2.4      2014-10-09
#>  munsell                 0.4.3      2016-02-13
#>  nlme                    3.1-128    2016-05-10
#>  nloptr                  1.0.4      2014-08-04
#>  nnet                    7.3-12     2016-02-02
#>  openssl                 0.9.4      2016-05-25
#>  pbkrtest                0.4-6      2016-01-27
#>  plyr                    1.8.4.9000 2016-11-03
#>  portableParallelSeeds * 0.97       2016-11-14
#>  purrr                 * 0.2.2.9000 2016-11-22
#>  quantreg                5.29       2016-09-04
#>  R6                      2.2.0      2016-10-05
#>  RColorBrewer            1.1-2      2014-12-07
#>  Rcpp                    0.12.9.1   2017-01-24
#>  RcppEigen               0.3.2.9.0  2016-08-21
#>  reshape2                1.4.2      2016-10-22
#>  rmarkdown               1.3        2016-12-21
#>  rockchalk               1.8.101    2016-02-25
#>  rprojroot               1.1        2016-10-29
#>  rstan                 * 2.14.1     2016-12-28
#>  rvest                 * 0.3.2      2016-06-17
#>  scales                  0.4.1      2016-11-09
#>  selectr                 0.3-0      2016-08-30
#>  SparseM                 1.72       2016-09-06
#>  StanHeaders           * 2.14.0-1   2017-01-09
#>  stringi                 1.1.2      2016-10-01
#>  stringr                 1.1.0      2016-08-19
#>  tibble                  1.2-15     2017-01-11
#>  xml2                  * 1.0.0      2016-06-24
#>  yaml                    2.1.14     2016-11-12
#>  source                           
#>  CRAN (R 3.3.0)                   
#>  cran (@1.0.4)                    
#>  cran (@0.1-3)                    
#>  cran (@1.62.0-)                  
#>  CRAN (R 3.3.0)                   
#>  Github (rstudio/bookdown@2211cd0)
#>  CRAN (R 3.3.0)                   
#>  cran (@1.17.1)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  cran (@0.5-1)                    
#>  CRAN (R 3.3.0)                   
#>  cran (@0.6.11)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  Github (hadley/ggplot2@2a1bf98)  
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  cran (@0.6)                      
#>  cran (@0.3.5)                    
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  cran (@1.2)                      
#>  cran (@1.15.1)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.2)                   
#>  Github (hadley/lazyeval@c155c3d) 
#>  CRAN (R 3.3.0)                   
#>  Github (hadley/lubridate@ebd90d9)
#>  CRAN (R 3.3.0)                   
#>  cran (@0.7.7)                    
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  Github (hadley/plyr@fe19241)     
#>  CRAN (R 3.3.1)                   
#>  Github (hadley/purrr@5360143)    
#>  CRAN (R 3.3.0)                   
#>  cran (@2.2.0)                    
#>  CRAN (R 3.3.0)                   
#>  Github (RcppCore/Rcpp@5a99a86)   
#>  CRAN (R 3.3.0)                   
#>  cran (@1.4.2)                    
#>  cran (@1.3)                      
#>  CRAN (R 3.3.0)                   
#>  cran (@1.1)                      
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.0)                   
#>  CRAN (R 3.3.2)                   
#>  CRAN (R 3.3.1)                   
#>  cran (@1.1.0)                    
#>  Github (hadley/tibble@3d6f8b4)   
#>  cran (@1.0.0)                    
#>  cran (@2.1.14)
```
