
#-------------------------------------------------------------------------------------------

show_custom_options <- function(...){
        .data <- data
        .data %<>% label_the_data(...)
        genotypes <- toString(unique(.data$genotype_sex))
        graphs <- "weight_plot, grip_plot, s_rotarod_plot, t_rotarod_plot, condition_plot"  #these were hand-copied from plot_the_group
        print(glue("THE GENOTYPES YOU CAN SELECT ARE:\n\n\t{genotypes} \n\nTHE GRAPHS YOU CAN PREVIEW ARE:\n\n\t{graphs}"))
}

#---------------------------------------------------------------------------------------------


generate_custom_graphs <- function(genotypes_to_plot,
                                   graph_to_preview = "weight_plot", ...){
  
                    .data <- data
                    .data %<>% label_the_data(...)
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
            custom_graphs     <<- .data %>% group_and_plot(graph_group = 'all',    #remove all from label_the_data when you have cleaned this up
                                                           pdf_group = 'all2', ...)
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

