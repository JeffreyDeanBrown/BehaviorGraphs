# Glue Functions.R
# version 1.2.1
# last update 03-02-2024
#
# FUNCTIONS: label_the_data, group_the_data, group_and_plot, process_all_the_data, generate_custom_graphs,
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
  
  
#-------------------------------------------------------------------------------------------------------------
# Mice will be grouped by group_subtitle, and the text for group_subtitle will be shown on the legend

group_and_plot <- function(working_group, data_group, pdf_group, group_subtitle){
        grouped_data <- working_group %>% 
                       summarise_by_group(group = {{ data_group }}, group_subtitle = {{ group_subtitle }} ) %>%
                       ungroup() %>%
                       group_by({{ data_group }}, {{ pdf_group }}) %>% nest
        graph_subtitle = deparse(substitute(data_group))
        grouped_data %<>% plot_the_group(graph_subtitle = graph_subtitle)
        return(grouped_data)
}

#--------------------------------------------------------------------------------------------------------------

# summarise_by_group will separate out your data by whatever group you want to be the 
# final graph group (i.e. it is common to graph one genotype+sex, such as OSKO homo (Male))
summarise_by_group <- function(.data, group, group_subtitle){
        
              group_by({{ group }}, week, genotype) %>%
                 mutate(grip_av_mean   = mean(grip_av, na.rm = TRUE),
                        weight_av_mean = mean(weight_av, na.rm = TRUE),
                        s_rotarod_mean = mean(s_rotarod, na.rm = TRUE),
                        t_rotarod_mean = mean(t_rotarod, na.rm = TRUE),
                        condition_mean = mean(condition, na.rm = TRUE),
                        grip_av_sd     = sd(grip_av, na.rm = TRUE),
                        weight_av_sd   = sd(weight_av, na.rm = TRUE),
                        s_rotarod_sd   = sd(s_rotarod, na.rm = TRUE),
                        t_rotarod_sd   = sd(t_rotarod, na.rm = TRUE),
                        condition_sd   = sd(condition, na.rm = TRUE),
                        n = n_distinct(number),
                        legend = glue(group_subtitle))
}

#-----------------------------------------------------------------------------------------

process_all_the_data <- function(group_subtitle = "{.data$genotype} (n = {.data$n})",
                                 indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}"){
  
      .data <- data
      .data %<>% label_the_data(indiv_subtitle = {{ indiv_subtitle }})

      print("Do you want to print individual mice plots? [y/n] (it will be slower)")
      
      repeat {
        var = readline()               #takes an input from the console
        
        #yes, the user wants to plot individuals:
        if (var == "y") {
          print("making graphs by genotype...")
          genotype_group <<- .data %>% group_and_plot(data_group = group_sex,
                                                             pdf_group = group,
                                                             group_subtitle = {{ group_subtitle }})
          print("making graphs by sod1 type...")
          sod1_group     <<- .data %>% group_and_plot(data_group = sod1_group_sex,
                                                             pdf_group = sod1_group,
                                                             group_subtitle = {{ group_subtitle }})
          print("making graphs for all individual mice...")
          indiv_ <<- .data %>% plot_the_indiv(subtitle = subtitle)
          suppressWarnings(print_with_indiv())
          
          print("Done!")
          
          break
        }
        
        #no, the user does not want to plot individuals:
        else if (var == "n") {  
          print("making graphs by genotype...")
          genotype_group <<- .data %>% group_and_plot(data_group = group_sex,
                                                             pdf_group = group,
                                                             group_subtitle = {{ group_subtitle }})
          print("making graphs by sod1 type...")
          sod1_group     <<- .data %>% group_and_plot(data_group = sod1_group_sex,
                                                             pdf_group = sod1_group,
                                                             group_subtitle = {{ group_subtitle }})
          print("making graphs of all mice by group...")
          all_group     <<- .data %>% group_and_plot(data_group = all_sex,
                                                      pdf_group = all,   #remove all from label_the_data when you have cleaned this up
                                                      group_subtitle = {{ group_subtitle }})
          suppressWarnings(print_without_indiv())
          
          print("Done!")
          
          break
        }
        
        #the user did not enter yes (y) or no (n)
        else {
          print("ERROR: please enter a 'y' or a 'n'")
      }
      
}
}

#-------------------------------------------------------------------------------------------

show_custom_options <- function(group_subtitle = "{.data$genotype} (n = {.data$n})",
                                indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}") {
        .data <- data
        .data %<>% label_the_data(indiv_subtitle = {{ indiv_subtitle }})
        genotypes <- toString(unique(.data$genotype_sex))
        graphs <- "weight_plot, grip_plot, s_rotarod_plot, t_rotarod_plot, condition_plot"  #these were hand-copied from plot_the_group
        print(glue("THE GENOTYPES YOU CAN SELECT ARE:\n\n\t{genotypes} \n\nTHE GRAPHS YOU CAN PREVIEW ARE:\n\n\t{graphs}"))
}

#---------------------------------------------------------------------------------------------


generate_custom_graphs <- function(group_subtitle = "{.data$genotype_sex} (n = {.data$n})",
                                   indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}",
                                   genotypes_to_plot,
                                   graph_to_preview = "weight_plot"){
  
                    .data <- data
                    .data %<>% label_the_data(indiv_subtitle = {{ indiv_subtitle }})
                    .data %<>% subset(genotype_sex %in% genotypes_to_plot)
                    custom_tibble <<- .data
                    n_mice <- unique(custom_tibble$id) %>% length
                    n_genes <- unique(custom_tibble$genotype) %>% length
                    
                    print(glue("Creating the graphs for {n_genes} groups, correct? [y/n] ({n_mice} mice total)"))
                    
                    repeat {
                      var = readline()               #takes an input from the console
                      
    #yes, the setup is correct:
          if (var == "y") {
            print("making custom graphs...")
            custom_graphs     <<- .data %>% group_and_plot(data_group = all,    #remove all from label_the_data when you have cleaned this up

                                                         pdf_group = all2,
                                                       group_subtitle = {{ group_subtitle }})
            print(glue(".\n.\n.\n.\n.\nDone! Now showing your selected preview graph ({graph_to_preview})\n Run print_custom_graphs() to print out all the graphs\n\n"))
            preview <- pluck(custom_graphs, graph_to_preview, 1)
            grid.arrange(preview)
            break
          }
          
   #no, the user made a mistake:
          else if (var == "n") {  
            print("Please run show_custom_options() and double check your genotypes_to_plot variable")
            break
          }
          
          #the user did not enter yes (y) or no (n)
          else {print("ERROR: please enter a 'y' or a 'n'")
          #then restarts the while loop
        }
                    }
}


