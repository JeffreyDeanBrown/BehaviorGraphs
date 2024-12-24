
label_the_data <- function(.data, 
                           indiv_subtitle = "Mouse  #{.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}"){
        
  print("Creating custom labels...")

    .data %<>%
      mutate(group = str_replace(genotype, " wSOD1M",""),
             all   = "all", 
             sex            = glue("{str_detect(id, 'M') %>%                     # id is labeled M/F based on sex
                                        ifelse('Male','Female')}"),
             sod1_factor    = str_detect(genotype, 'wSOD1M') %>%
                              ifelse('SOD1mut','SOD1wt') %>%
                              as.factor(),
             subtitle        = glue(indiv_subtitle),
             sod1_factor_sex = glue("{sod1_factor} ({sex})"),
             all_sex         = glue("{all} ({sex})"),
             genotype_sex    = glue("{genotype} ({sex})"),
             group_sex       = glue("{str_remove(genotype,' wSOD1M')} ({sex})"))
  return(.data)
}

