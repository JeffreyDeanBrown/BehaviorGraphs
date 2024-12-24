
#-------------------------------------------------------------------------------------------
show_custom_options <- function(.data, measurements = c("grip_plot", "weight_plot", "s_rotarod_plot",
                                                    "t_rotarod_plot", "condition_plot"), ...){
    genotypes <- paste(unique(.data$genotype_sex), collapse = ", ")
    graphs <- paste(unique(measurements), collapse = ", ")

    print(glue("THE GENOTYPES YOU CAN SELECT ARE:\n\n\t{genotypes} \n\nTHE GRAPHS YOU CAN PREVIEW ARE:\n\n\t{graphs}"))
}

#---------------------------------------------------------------------------------------------

generate_custom_graphs <- function(.data, genotypes_to_plot,
                                   graph_to_preview = "weight_plot",
                                   measurements = c("grip_plot", "weight_plot", "s_rotarod_plot",
                                                    "t_rotarod_plot", "condition_plot"), ...){

    custom_tibble <<- subset(.data, genotype_sex %in% genotypes_to_plot)
    n_mice <- unique(custom_tibble$id) %>% length
    n_genes <- unique(custom_tibble$genotype)
    custom_tibble %<>% mutate(custom = "       ") # this will become the graph subtitle (i.e. no subtitle)
    
    print(glue("Creating the graphs for {length(n_genes)} genotypes [{paste(n_genes, collapse = ', ')}],",
               " correct? [y/n] ({n_mice} mice total)"))
    
    repeat {
      var = readline()
    #yes, the setup is correct:
          if (var == "y") {
            print("making custom graphs...")
            custom_graphs     <<- custom_tibble %>% group_and_plot(graph_group = 'custom', # also graph_subtitle
                                                           pdf_group = 'all', ...)
            print(glue(".\n.\n.\n.\n.\nDone! Now showing your selected preview graph ({graph_to_preview})",
                       "\n Do you want to print these graphs to a pdf?[y/n]\n\n"))
            pluck(custom_graphs, graph_to_preview, 1) %>% grid.arrange()
            
            do.print = readline()
              if (do.print == "y"){
                printer(.data          = custom_graphs,
                        group_to_print = "all",
                        pdf_names      = "custom graphs.pdf",
                        lookup_vector  = custom_graphs$all,
                        colnames       = measurements,
                        print.indivs   = FALSE)  # don't get too crazy now
                print("your graphs are now saved in PDFs/custom graphs.pdf");break}
              else if (do.print == "n"){print("ok have a nice day");break}
              else {print("ERROR: please enter a 'y' or a 'n'")}
          }
   #no, the user made a mistake:
          else if (var == "n") {print("Please run show_custom_options() and double check your genotypes_to_plot variable"); break}
   #the user did not enter yes (y) or no (n)
          else {print("ERROR: please enter a 'y' or a 'n'")}
    }
}

