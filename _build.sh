#!/bin/sh

Rscript -e "bookdown::render_book('index.rmd', 'bookdown::gitbook')"
