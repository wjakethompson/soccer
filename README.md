Soccer Predictions Using Bayesian Mixed Effects Models
======================================================

[![Travis-CI Build Status](https://travis-ci.org/wjakethompson/soccer.svg?branch=master)](https://travis-ci.org/wjakethompson/soccer)

This is source code and document text for [Soccer Predictions Using Bayesian Mixed Effects Models](https://wjakethompson.github.io/soccer/). 

Project Requirements
--------------------

The **rstan** package can be installed using the instructions provided by the Stan Development Team:

* [Mac orLinux](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux)
* [Windows](https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows).

The **portableParallelSeeds** package can be installed via

```{r}
install.packages("portableParallelSeeds", repos = "http://rweb.crmda.ku.edu/kran")
```

And the rest of the R packages used for this project can be installed via

```{r}
devtools::install_github("wjakethompson/soccer")
```

The site is built using the [**bookdown** package](https://github.com/rstudio/bookdown).
To create the site, you also need:

* [pandoc](http://johnmacfarlane.net/pandoc/)
