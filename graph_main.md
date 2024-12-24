

# Mouse Behavior Graphs

This is the code used by BehaviorGraphs v2.0
([github](https://github.com/JeffreyDeanBrown/BehaviorGraphs))

The use-case for this code is very narrow, at this point it is just for
the author to practice.

## setup

``` r
library(openxlsx) 
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(glue) 
library(ggformula) 
library(vctrs) 
library(snakecase) 
library(gridExtra)
library(devtools)
library(Hmisc)
library(magrittr)
install_github(repo = 'https://github.com/JeffreyDeanBrown/BehaviorGraphs',
               subdir = "BehaviorGraphs")
library(BehaviorGraphs)


# setwd("")

print("Done with Setup")
```

## Create and print the default graphs

``` r
genotype_of_the_day <- "OSKO"        # either a string or a vector of strings
filename_of_the_day <- "OSKO.pdf"    # you need one filename for each genotype

organize_data(spreadsheet_name = 'example_sheets/Behavior Data 01-08-24.xlsx') %>%
  label_the_data() %>%
    plot_default_graphs()

print_default_plots()
```

## Customize a set of graphs

This essentially just subsets your data to a list of genotypes you are
interested in

``` r
organize_data(spreadsheet_name = 'example_sheets/Behavior Data 01-08-24.xlsx') %>%
  label_the_data() %>%
    show_custom_options() # prints your genotypes_to_plot and graph_to_preview options

organize_data(spreadsheet_name = 'example_sheets/Behavior Data 01-08-24.xlsx') %>%
  label_the_data() %>%
    generate_custom_graphs(genotypes_to_plot = c("pFlp Homo (Male)", "OSKO Homo wSOD1M (Female)"),
                           graph_to_preview = "grip_plot")
```
