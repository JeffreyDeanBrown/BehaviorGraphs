# Glue Functions.R
# version 1.2.1
# last update 03-02-2024
#
# FUNCTIONS: label_the_data, group_the_data, group_and_plot, plot_the_data, generate_custom_graphs,
#            pull_the_data
# 
# DEPENDS: (need to fill this out)
#   
# DESCRIPTION:
#   These functions take all of the functions from other source files and 
#   "glue" them all together into one big happy function (plot_the_data)
#
#   There isn't anything to really customize easily here, if you change any of
#   this code you will likely need to change a lot of code in other source files
  
  
#-----------------------------------------------------------------------------------------

plot_the_data <- function(.data, ...){
  
      print("making graphs by genotype...")
      genotype_group <<- .data %>% group_and_plot(graph_group = "group_sex",
                                                  pdf_group = "group", ...)
      print("making graphs by sod1 type...")
      sod1_group     <<- .data %>% group_and_plot(graph_group = "sod1_factor_sex",
                                                  pdf_group = "sod1_factor", ...)
      print("making graphs of all mice by group...")
      all_group      <<- .data %>% group_and_plot(graph_group = 'all_sex',
                                                  pdf_group = 'all', ...)   #remove all from label_the_data when you have cleaned this up
      print("making graphs for all individual mice...")
      indiv_         <<- .data %>% plot_individuals(subtitle = subtitle)

      }

#-------------------------------------------------------------------------------------------------------------
# Mice will be grouped by legend_label, and the text for legend_label will be shown on the legend

group_and_plot <- function(.data, pdf_group,     # a pdf will print for this group (example: all WT mice in one PDF) 
                                  graph_group,   # a graph will render this group  (example: WT (Male), WT (female))
                                  ...){
        .data %<>% 
          mutate(.by = c(!!graph_group, genotype, week),
                 genotype_lab = glue("{genotype} (n={n_distinct(number)})")) %>% # n = (count of unique tag numbers)
               nest(.by = c( !!graph_group, !!pdf_group)) # nest uses tidy NSE
# >  .data (example: pdf_group = group, graph_group = group_sex)
#   group  group_sex    data               
# 1 WT     WT (Female)  <tibble> 
# 2 WT     WT (Male)    <tibble>
        .data %<>%
              plot_measurements(graph_subtitle = graph_group)
# >  .data
#   group  group_sex    data        weight_plot  grip_plot  [s_, t_]rotarod_plot  condition_plot      
# 1 WT     WT (Female)  <tibble>    <gtable>     <gtable>   <gtable>              <gtable>   
# 2 WT     WT (Male)    <tibble>    <gtable>     <gtable>   <gtable>              <gtable>   
        return(.data)
}

