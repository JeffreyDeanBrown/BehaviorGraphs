# Import Functions.R
# version 1.2
# last update 12-18-2024
#
# FUNCTIONS: organize_data, get_sheets, separate_lut, clean_data, average_data, lookup_mouse_info
# 
# DEPENDS: none
#
# VARIABLES: spreadsheet_name, lut_col_keep, rotarod_time, rotarod_speed, weeks_to_keep, week, weight_data,
#            grip_data, kept_columns
# 
# DESCRIPTION:
#   These functions mostly pertain to taking an excel workbook of mice data and 
#   extracting the data.
#   ***description is WIP***
#   
#--------------------------------------------------------------------------------------------------

organize_data <- function(spreadsheet_name, ...) {
        print("Opening the Excel file...") 
.data <- get_sheets(spreadsheet_name) %>%    # returns sheets as a list
        separate_lut(...) %>%                # separates the 1st sheet (lookup table) to df "mouse_lut"
        clean_data(...) %>%                  # cleans up names, pulls weeks_to_keep, rowbinds sheets to one df
        average_data(...) %>%                # averages all weight and grip measurements for each week
        lookup_mouse_info()                  # pulls mouse number, cage, and genotype from mouse_lut
        rm(mouse_lut, inherits=TRUE)
        return(.data)}


#--------------------------------------------------------------------------------------------------

get_sheets <- function(spreadsheet_name) {
     .data <- sapply(          #apply function(x) to everything in the list x=getSheetNames(spreadsheet_name)
                getSheetNames(spreadsheet_name), 
                function(x)                     
                  read.xlsx(spreadsheet_name,  
                            sheet = x,
                            fillMergedCells = TRUE))
     return(.data)
}


#--------------------------------------------------------------------------------------------------------------
# pulls out the first sheet to use as a look up table (lut) for labeling
separate_lut <- function(.data,
                         lut_col_keep = c('number#','Genotype','Cage','F/M'), ...)
                           {

        mouse_lut <<- pluck(.data,1) %>%
                      subset(select = {{ lut_col_keep }}) %>%
                      rename_with(to_snake_case)
        
        .data <- .data[-1]                                       #remove .data[1]
        
        return(.data)
}


#----------------------------------------------------------------------------------------------------------

clean_data <- function(.data,
                       rotarod_time = "rotarod.time.sec",     # name on original excel file but 
                       rotarod_speed = "rotarod.speed.rpm",   # with spaces replaced with dots
                       weeks_to_keep = c(11:24),
                       week = "week", ...){
          clean <- function(x){                                  
            
                      x <- filter(x,week %in% weeks_to_keep)   
                      
                      x <- filter(x,!if_all(week, is.na))      #some sheets have extra charts floating around 
                      
                      x %<>% mutate_if(is.character, as.numeric)          
                }
        
          clean_list <- lapply(.data, clean)                  
          
          clean_list %<>% map_dfr(~ .x,            # takes the list of sheets and makes 1 dataframe 
                                  .id = "id")      # with a column "id" that has the sheet name
                                                                
          
          clean_list %<>% dplyr::rename(t_rotarod = {{ rotarod_time }},        
                                        s_rotarod = {{ rotarod_speed }})
          return(clean_list)
}


#--------------------------------------------------------------------------------------------------------------------

average_data <- function(.data,
                         weight_data = c("Weight.1","Weight.2","Weight.3"),  # only include indiv weight & grip
                         grip_data = c("GS1","GS2","GS3","GS4","GS5"),       # not the averages too!
                         kept_columns = c('id','week','t_rotarod','s_rotarod',
                                          'weight_av','weight_sd','grip_av',
                                          'grip_sd','condition'), ...)
{
          .data %<>%
              mutate(
                    weight_av = subset({{ .data }}, select = weight_data) %>%
                                rowMeans(na.rm = TRUE),
                    grip_av = subset({{ .data }}, select = grip_data) %>%
                                rowMeans(na.rm = TRUE),
                    weight_sd = subset({{ .data }}, select = weight_data) %>%
                                apply(1, sd, na.rm = TRUE),
                    grip_sd = subset({{ .data }}, select = grip_data) %>%
                                apply(1, sd, na.rm = TRUE)) %>%
                    subset(select = {{ kept_columns }})                                 
                  
         return(.data)
}


#---------------------------------------------------------------------------------------------------------------------

lookup_mouse_info <- function(.data){
  .data %<>%                                   
      mutate(                                       #  add new columns
        
        
        number = mouse_lut$number[           #  new column 'number' is a vector made from the vector mice_info$number
                  match(x = .data$id,        #  the offset of the mice_info$number vector used in 'number' is what
                    table = mouse_lut$f_m)], #  .data$id values match which mice_info$id values
                     

        genotype = mouse_lut$genotype[            #  ditto
                    match(x = .data$id,
                      table = mouse_lut$f_m)],
        
        cage = mouse_lut$cage[                    #  ditto
               match(x = .data$id,
                 table = mouse_lut$f_m)]
                             
      )
      
      .data$genotype %<>% trimws
      return(.data)
     }

