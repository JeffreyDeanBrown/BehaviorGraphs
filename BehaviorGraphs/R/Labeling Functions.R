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

label_the_data <- function(.data, indiv_subtitle){
        
        print("Creating custom labels...")
  
        .data %<>%
          mutate(group = str_replace(genotype, " wSOD1M",""),
                 all   = "all", #this is combined with sex in create_label_co
                 all2  = "all2",
                 subtitle = glue(indiv_subtitle)
                 )
        
        .data %<>%
           create_label_cols()


        return(.data)
}

#----------------------------------------------------------------------------------------


create_label_cols <- function(.data){
  
  .data %<>%
              new_column_by_replace(new_column = 'sod1_group',
                        source_column = 'genotype',
                        search = " wSOD1M",              #NOTE: if you change has_it or doesnt, you must also
                        has_it = "SOD1mut",              #      replace the string in "pdf Functions.R"
                                                         #      (i.e. find and replace "SOD1mut" with your new string)
                        doesnt = "SOD1wt") %>%
              new_column_by_replace(new_column = 'sex',
                          source_column = 'id',
                          search = "M",
                          has_it = "Male",
                          doesnt = "Female") %>%
              new_column_by_combine(new_column = 'group_sex',
                          source_column = 'genotype',
                          search = " wSOD1M",
                          replace = "",
                          combine_column = 'sex') %>%
              new_column_by_combine(new_column = 'sod1_group_sex',
                          source_column = 'sod1_group',
                          search = " ",
                          replace = " ",
                          combine_column = 'sex') %>%
              new_column_by_combine(new_column = 'all_sex',
                                    source_column = 'all',
                                    search = " ",
                                    replace = " ",
                                    combine_column = 'sex') %>%
              new_column_by_combine(new_column = 'genotype_sex',
                                    source_column = 'genotype',
                                    search = " ",
                                    replace = " ",
                                    combine_column = 'sex')
  return(.data)
}


#----------------------------------------------------------------------------------------------------------------

new_column_by_replace <- function(.data, new_column, source_column, search, has_it, doesnt)
          {
            x <- .data %>% pluck(source_column) %>%         
              str_detect(search) %>%
                as.character() %>%
                  str_replace("TRUE", has_it) %>%                 
                  str_replace("FALSE",doesnt)
            y <- tibble(x, .name_repair = ~ c(new_column))  #the only way I could find to name new_column as masked$data
            return(cbind(.data, y))
  }


#------------------------------------------------------------------------------------------------------------------

new_column_by_combine <- function(.data, new_column, source_column, search, replace, combine_column)
{
            .source <- .data %>% pluck(source_column) %>%
               str_replace({{search}}, {{replace}})
            .combine <- pluck(.data, combine_column)
             .y <- paste(.source,glue("({.combine})"))   #source_column (combine_column)
             y <- tibble(.y, .name_repair = ~ c(new_column))  #again, it's a pain in the butt to use masked data as a column name
            return(cbind(.data, y))
}

#--------------------------------------------------------------------------------------------------------------

average_by_group <- function(.data, group){
            grouped_data <- group_by(.data, {{group}}, week, genotype) %>%
                 mutate(grip_av_mean   = mean(grip_av, na.rm = TRUE),
                        weight_av_mean = mean(weight_av, na.rm = TRUE),
                        s_rotarod_mean = mean(s_rotarod, na.rm = TRUE),
                        t_rotarod_mean = mean(t_rotarod, na.rm = TRUE),
                        condition_mean = mean(condition, na.rm = TRUE),
                        n = n_distinct(number))
            return(grouped_data)
}   

#---------------------------------------------------------------------------------------

spread_by_group <- function(.data, group){
            grouped_data <- group_by(.data, {{group}}, week, genotype) %>%
                 mutate(grip_av_sd   = sd(grip_av, na.rm = TRUE),
                        weight_av_sd = sd(weight_av, na.rm = TRUE),
                        s_rotarod_sd = sd(s_rotarod, na.rm = TRUE),
                        t_rotarod_sd = sd(t_rotarod, na.rm = TRUE),
                        condition_sd = sd(condition, na.rm = TRUE))
            return(grouped_data)
}   

#--------------------------------------------------------------------------------------------------------------------------------
# summarise_by_group will separate out your data by whatever group you want to be the 
# final graph group (i.e. it is common to graph one genotype+sex, such as OSKO homo (Male))
summarise_by_group <- function(.data, by_group, weeks = 11:24){
              mean <- subset(.data, week %in% {{weeks}}) %>%
                          average_by_group(group = {{by_group}})
              
              sd   <- subset(.data, week %in% {{weeks}}) %>%
                          spread_by_group(group = {{by_group}}) %>%
                          subset(select = c(grip_av_sd, weight_av_sd, s_rotarod_sd,
                                            t_rotarod_sd, condition_sd)) #only need the data
              return(cbind(mean, sd))
  
}









