# Import Functions.R
# version 1.2
# last update 02-29-2024
#
# FUNCTIONS: get_sheets, separate_sheet, clean_data, analyze_data, lookup_mouse_info
# 
# DEPENDS: (need to fill this out)
# 
# DESCRIPTION:
#   These functions mostly pertain to taking an excell workbook of mice data and 
#   extracting the data.
# 
#   These functions were not really designed to be edited and editing will require
#   editing of other source files which work off of this code
#   

get_sheets <- function() {
     .data <- sapply(
                getSheetNames(paste(spreadsheet_name)), #makes a vector of the names within the spreadsheet
                function(x)                             #      performs the following function, replacing x
                  read.xlsx(paste(spreadsheet_name),    #      with a sheet name, and repeating for all sheetnames
                            sheet = x,
                            fillMergedCells = TRUE))
     return(.data)
}


#--------------------------------------------------------------------------------------------------------------
 
separate_sheet <- function(.data,
                           kept_lut_col = c('number#','Genotype','Cage','F/M'),
                           lut_name = 'mice_info') {

        lut_name <<- pluck(.data,1) %>%
                      subset(select = {{kept_lut_col}}) %>%
                      rename_with(to_snake_case)
        
        .data <- .data[-1]                                       #remove .data[1]
        
        return(.data)
}


#----------------------------------------------------------------------------------------------------------

clean_data <- function(.data,
                     rotarod_time = "rotarod.time.sec",
                     rotarod_speed = "rotarod.speed.rpm",
                     week = "week",
                     weeks_to_keep = c(11:24)){
          
          clean <- function(x){                                  
            
                      x <- filter(x,week %in% weeks_to_keep)      
                      
                      x <- filter(x,!if_all(week, is.na))                  
                      
                      x %<>% mutate_if(is.character, as.numeric)          
                }
        
          clean_list <- lapply(.data, clean)                  
          
          
          clean_list %<>% map_dfr(~ .x,                        
                                  .id = "id")                   
                                                                
          
          clean_list %<>% dplyr::rename(t_rotarod = {{rotarod_time}},        
                                        s_rotarod = {{rotarod_speed}})
          return(clean_list)
}


#--------------------------------------------------------------------------------------------------------------------

analyze_data <- function(.data,
                         weight_data = c("Weight.1","Weight.2","Weight.3"),
                         grip_data = c("GS1","GS2","GS3","GS4","GS5"),
                         kept_columns = c('id','week','t_rotarod','s_rotarod',        
                                          'weight_av','weight_sd','grip_av',
                                          'grip_sd','condition'))
{
          .data %<>%
              mutate(
                    weight_av = subset({{.data}}, select = weight_data) %>%
                                rowMeans(na.rm = TRUE),
                    grip_av = subset({{.data}}, select = grip_data) %>%
                                rowMeans(na.rm = TRUE),
                    weight_sd = subset({{.data}}, select = weight_data) %>%
                                apply(1, sd, na.rm = TRUE),
                    grip_sd = subset({{.data}}, select = grip_data) %>%
                                apply(1, sd, na.rm = TRUE)) %>%
                    subset(select = {{kept_columns}})                                 
                  
         return(.data)
}


#---------------------------------------------------------------------------------------------------------------------

lookup_mouse_info <- function(.data, lut = lut_name)      {    
  .data %<>%                                   
      mutate(                                       #  add new columns
        
        
        number = lut$number[                  #  new column 'number' is a vector made from the vector mice_info$number
                  match(x = .data$id,          #  the offset of the mice_info$number vector used in 'number' is what
                    table = lut$f_m)],     #       .data$id values match which mice_info$id values
                     

        genotype = lut$genotype[            #  ditto
                    match(x = .data$id,
                      table = lut$f_m)],
        
        cage = lut$cage[                    #  ditto
               match(x = .data$id,
                 table = lut$f_m)]
                             
      )
      
      .data$genotype %<>% trimws
      return(.data)
     }

