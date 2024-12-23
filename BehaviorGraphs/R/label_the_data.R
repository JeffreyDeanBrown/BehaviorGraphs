# Labeling Functions.R
# version 1.2
# last update 03-02-2024
#
# FUNCTIONS: create_label_cols, new_column_by_replace, new_column_by_combine,
#            create_subtitle, average_by_group, spread_by_group, summarize_by_group
# 
# DEPENDS: (need to fill this out)
# 
# DESCRIPTION:
#   these functions create new columns that have custom columns, such as 
#   combining genotype and sex into one label "genotype (sex)", which is used for 
#   graphs and grouping, mostly
#   
#   somethings from create_label_cols can be easily edited to change the final graphs,
#   but for the most part these new columns and labels are referenced by other
#   source code files and should only be changed if you also update a lot of other code
#   


#---------------------------------------------------------------------------------------------------------------

label_the_data <- function(.data, 
                           indiv_subtitle = "Mouse \n {.data$number}, {.data$sex}, {.data$genotype}\n From cage: {.data$cage}")
        {
        
        print("Creating custom labels...")
  
            .data %<>%
              mutate(group = str_replace(genotype, " wSOD1M",""),
                     all   = "all", #this is used by plot_the_data and generate_custom_graphs 
                     all2  = "all2",
                     sex            = glue("{str_detect(id, 'M') %>%                     # id is labeled M/F
                                                ifelse('Male','Female')}"),              # based on sex
                     sod1_factor    = str_detect(genotype, 'wSOD1M') %>%
                                      ifelse('SOD1mut','SOD1wt') %>%
                                      as.factor(),
                     subtitle        = glue(indiv_subtitle),
                     sod1_factor_sex = glue("{sod1_factor} ({sex})"),
                     all_sex         = glue("{all} ({sex})"),
                     genotype_sex    = glue("{genotype} ({sex})"),
                     group_sex       = glue("{str_remove(genotype,' wSOD1M')} ({sex})")
                     )
        return(.data)
        }

