
print_default_plots <- function(measurements = c("grip_plot", "weight_plot", "s_rotarod_plot", "t_rotarod_plot", "condition_plot")){

  print("Do you want to print individual mice plots? [y/n] (it will be slower)")

  repeat {
    want.indivs <- readline()
    if (want.indivs == "y"){want.indivs = TRUE; break}
    else if (want.indivs == "n"){want.indivs = FALSE; break}
    else{print("ERROR: please enter a 'y' or a 'n'")}
  }

  print("rendering graphs...(this will take a few seconds)")
  suppressWarnings(dir.create("PDFs"))

  printer(.data          = genotype_group,
          group_to_print = c("pFlp", genotype_of_the_day),      # group[i] will print with pdf_name[i]
          pdf_names      = c("pFlp.pdf", filename_of_the_day),
          lookup_vector  = genotype_group$group, # rows in lookup_vector which contain group_to_print will print
          colnames       = measurements,         # columns in colnames will print
          print.indivs   = want.indivs)

  printer(.data          = sod1_group,
          group_to_print = c("SOD1mut", "SOD1wt"),
          pdf_names      = c("mutant SOD1.pdf", "wildtype SOD1.pdf"),
          lookup_vector  = sod1_group$sod1_factor,
          colnames       = measurements,
          print.indivs   = want.indivs)

  printer(.data          = all_group,
          group_to_print = "all",
          pdf_names      = "all mice.pdf",
          lookup_vector  = all_group$all,
          colnames       = measurements,
          print.indivs   = FALSE)  # don't get too crazy now

  print("Done!")
}





printer <- function(.data, group_to_print, pdf_names, print.indivs, lookup_vector, colnames){
  for (x in c(1:length(group_to_print))){

    list_to_print <- subset(.data, subset = str_detect(lookup_vector, group_to_print[x]), select = colnames) %>%
               unlist(recursive= FALSE, use.names = FALSE)

    if(print.indivs){
      indiv_lookup_vector <- deparse(substitute(lookup_vector)) %>%
         sub('^.*\\$','indiv_\\$',.) %>% parse(text = .) %>% eval   # example: genotype_group$group ---> indiv_$group

      ind_list <- subset(indiv_, subset = str_detect(indiv_lookup_vector, group_to_print[x]), select = colnames) %>%
                    unlist(recursive= FALSE, use.names = FALSE)

      list_to_print <- append(list_to_print, ind_list)
    }

      suppressWarnings(pdf(glue("PDFs/{pdf_names[x]}")))
      lapply(list_to_print, print)
      dev.off()
  }
}

