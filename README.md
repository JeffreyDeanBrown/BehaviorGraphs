
# Mouse Behavior Graphs

Organize and graph mouse behavior data

## Overview

The main use for this project is to take many excel sheets that were created during a mouse
behavior experiment and organize them in a way where we can create ggplots of the data. It is 
very unlikely that anyone (other than the person who requested help on this project) will find
any use out of this code.

However, this project has been really useful for practicing organizing a project and working
with git, so I will upload it to git and keep improving it.

The use case for this is very, very limited.

## Installation

``` r
# install the current verison on github
install_github(repo = 'https://github.com/JeffreyDeanBrown/BehaviorGraphs',
               subdir = "BehaviorGraphs")

# install a specific release verison (replace version_tag with the verison you want)
install_github(repo = 'https://github.com/JeffreyDeanBrown/BehaviorGraphs@version_tag',
               subdir = "BehaviorGraphs")
```

## Usage

There isn't currently any documentation for this project, but that is the next thing I am
working on. Currently, the way to use this project is to download `graph_main.qmd` and 
run that code. 

Several of the functions called in `graph_main.qmd` are just wrappers for other functions
with default values. It is not currently documented, but you can call these functions yourself
and make your own graphs. 

