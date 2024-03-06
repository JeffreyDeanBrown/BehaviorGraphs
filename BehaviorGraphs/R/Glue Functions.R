# Glue Functions.R
# version 1.2.1
# last update 03-02-2024
#
# FUNCTIONS: get_the_data, label_the_data, group_the_data, group_and_plot, process_all_the_data, generate_custom_graphs,
#            pull_the_data
# 
# DEPENDS: (need to fill this out)
#   
# DESCRIPTION:
#   These functions take all of the functions from other source files and 
#   "glue" them all together into one big happy function (process_all_the_data)
#
#   There isn't anything to really customize easily here, if you change any of
#   this code you will likely need to change a lot of code in other source files
  
  
  
#--------------------------------------------------------------------------------------------------

get_the_data <- function() {
        print("Opening the Excel file...")
      .data <- get_sheets() %>%
        separate_sheet() %>%
        clean_data() %>%
        analyze_data() %>%
        lookup_mouse_info()
        return(.data)}


#------------------------------------------------------------------------------------------------------

label_the_data <- function(.data, indiv_subtitle){
        
        print("Creating custom labels...")
  
        .data %<>%
          mutate(group = str_replace(genotype, " wSOD1M",""),
                 all   = "all", #this is combined with sex in create_label_co
                 all2  = "all2")
        
        .data %<>%
           create_label_cols()
        
        .data %<>%
          create_subtitle(string = {{indiv_subtitle}},
                          new_column = "subtitle")
        
        
        return(.data)
}

#----------------------------------------------------------------------------------------

#data_group is the group that you want to have as a graph for each plot
#(an example is OSKO homo (Male))
#pdf_group is used for referring to graphs on 'pdf Functions.R'
group_the_data <- function(.data, data_group, pdf_group,
                           group_subtitle, legend_name){
        working_group <- .data %>% 
            summarise_by_group(by_group = {{data_group}}) %>% 
            create_subtitle(string = {{group_subtitle}},
                            new_column = "legend") %>%
            ungroup() %>%
            group_by({{data_group}}, {{pdf_group}}) %>% nest
        return(working_group)
}


#-------------------------------------------------------------------------------------------------------------
# Mice will be grouped by group_subtitle, and the text for group_subtitle will be shown on the legend

group_and_plot <- function(working_group, data_group, pdf_group, group_subtitle){
        grouped_data <- working_group %>% 
                        group_the_data(data_group = {{data_group}},
                                       pdf_group = {{pdf_group}},
                                       group_subtitle = {{group_subtitle}})
        graph_subtitle = deparse(substitute(data_group))
        grouped_data %<>% plot_the_group(graph_subtitle = graph_subtitle)
        return(grouped_data)
}

#-----------------------------------------------------------------------------------------

process_all_the_data <- function(group_subtitle = "{.data$genotype} (n = {.data$n})",
                                 indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}"){
  
      .data <- data
      .data %<>% label_the_data(indiv_subtitle = {{indiv_subtitle}})
    
      i <- 0
      print("Do you want to print individual mice plots? [y/n] (it will be slower)")
      
      while (i != 1){
        var = readline()               #takes an input from the console
        
        #yes, the user wants to plot individuals:
        if (var == "y") {
          print("making graphs by genotype...")
          genotype_group <<- .data %>% group_and_plot(data_group = group_sex,
                                                             pdf_group = group,
                                                             group_subtitle = {{group_subtitle}})
          print("making graphs by sod1 type...")
          sod1_group     <<- .data %>% group_and_plot(data_group = sod1_group_sex,
                                                             pdf_group = sod1_group,
                                                             group_subtitle = {{group_subtitle}})
          print("making graphs for all individual mice...")
          indiv_ <<- .data %>% plot_the_indiv(subtitle = subtitle)
          suppressWarnings(print_with_indiv())
          print("Done!")
          i <- 1    #exits out of the while loop
        }
        
        #no, the user does not want to plot individuals:
        else if (var == "n") {  
          print("making graphs by genotype...")
          genotype_group <<- .data %>% group_and_plot(data_group = group_sex,
                                                             pdf_group = group,
                                                             group_subtitle = {{group_subtitle}})
          print("making graphs by sod1 type...")
          sod1_group     <<- .data %>% group_and_plot(data_group = sod1_group_sex,
                                                             pdf_group = sod1_group,
                                                             group_subtitle = {{group_subtitle}})
          print("making graphs of all mice by group...")
          all_group     <<- .data %>% group_and_plot(data_group = all_sex,
                                                      pdf_group = all,
                                                      group_subtitle = {{group_subtitle}})
          suppressWarnings(print_without_indiv())
          print("Done!")
          i <- 1    #exits out of the while loop
        }
        
        #the user did not enter yes (y) or no (n)
        else if {
          print("ERROR: please enter a 'y' or a 'n'")
      }
      
}
}

#-------------------------------------------------------------------------------------------

show_custom_options <- function(group_subtitle = "{.data$genotype} (n = {.data$n})",
                                indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}") {
        .data <- data
        .data %<>% label_the_data(indiv_subtitle = {{indiv_subtitle}})
        genotypes <- toString(unique(.data$group_sex))
        graphs <- "weight_plot, grip_plot, s_rotarod_plot, t_rotarod_plot, condition_plot"  #these were hand-copied from plot_the_group
        print(glue("THE GENOTYPES YOU CAN SELECT ARE:\n\t{genotypes} \nTHE GRAPHS YOU CAN PREVIEW ARE:\n\t{graphs}"))
}

#---------------------------------------------------------------------------------------------


generate_custom_graphs <- function(group_subtitle = "{.data$genotype_sex} (n = {.data$n})",
                                   indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}",
                                   genotypes_to_plot,
                                   graph_to_preview = "weight_plot"){
  
                    .data <- data
                    .data %<>% label_the_data(indiv_subtitle = {{indiv_subtitle}})
                    .data %<>% subset(genotype_sex %in% genotypes_to_plot)
                    custom_tibble <<- .data
                    n_mice <- unique(custom_tibble$id) %>% length
                    n_genes <- unique(custom_tibble$genotype) %>% length
                    
                    i <- 0
                    print(glue("Creating the graphs for {n_genes} groups, correct? [y/n] ({n_mice} mice total)"))
                    
                    while (i != 1){
                      var = readline()               #takes an input from the console
                      
    #yes, the setup is correct:
          if (var == "y") {
            print("making custom graphs...")
            custom_graphs     <<- .data %>% group_and_plot(data_group = all,
                                                       pdf_group = all2,
                                                       group_subtitle = {{group_subtitle}})
            print(glue(".\n.\n.\n.\n.\nDone! Now showing your selected preview graph ({graph_to_preview})\n Run print_custom_graphs() to print out all the graphs\n\n"))
            preview <- pluck(custom_graphs, graph_to_preview, 1)
            grid.arrange(preview)
            i <- 1    #exits out of the while loop
          }
          
   #no, the user made a mistake:
          if (var == "n") {  
            print("Please run show_custom_options() and double check your genotypes_to_plot variable")
            i <- 1    #exits out of the while loop
          }
          
          #the user did not enter yes (y) or no (n)
          if (all(var != "y", var != "n")) print("ERROR: please enter a 'y' or a 'n'")
          #then restarts the while loop
        }
}