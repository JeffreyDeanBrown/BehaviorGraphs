
# Mouse Behavior Graphs

Organize and graph mouse behavior data

## Overview

The main use for this project is to take many excel sheets that were created during a mouse
behavior experiment and organize them in a way where we can create ggplots of the data. It is 
very unlikely that anyone (other than the person who requested help on this project) will find
any use out of this code.

However, I will still upload this here so I can get practice working with Git and organizing
projects for github. 

The code is very specific to the excel sheet format that the experimenter was using, and
I really doubt anyone else will find it useful. However, if anyone wants to use it or has
any questions, feel free.

## Installation

The way the project is setup right now, the way to install and run this code is to download
the *Behavior Data Main.qmd* file and run the code on that file. However, if you just want
to install the package contents and use it yourself, use:

``` r
# install the current verison on github
install_github(repo = 'https://github.com/JeffreyDeanBrown/BehaviorGraphs',
               subdir = "BehaviorGraphs")

# install a specific release verison (replace version_tag with the verison you want)
install_github(repo = 'https://github.com/JeffreyDeanBrown/BehaviorGraphs@version_tag',
               subdir = "BehaviorGraphs")
```

## Usage

I plan on reformatting the code to make it more usable outside of the specific steps in 
*Behavior Data Main.qmd*, but that hasn't happened yet. WIP

