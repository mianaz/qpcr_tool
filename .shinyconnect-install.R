#!/usr/bin/env Rscript
# Install required packages
repos <- 'https://cran.r-project.org'

packages <- c(
  'DT',
  'ggbeeswarm',
  'ggplot2',
  'ggprism',
  'ggsci',
  'ggsignif',
  'ggthemes',
  'gridExtra',
  'openxlsx',
  'rstatix',
  'shiny',
  'shinyjs',
  'shinythemes',
  'sortable',
  'tidyverse',
  'viridis'
)

# Check and install missing packages
installed <- installed.packages()[, 'Package']
missing <- packages[!packages %in% installed]

if (length(missing) > 0) {
  cat('Installing missing packages:', paste(missing, collapse = ', '), '\n')
  install.packages(missing, repos = repos)
} else {
  cat('All required packages are already installed\n')
}

