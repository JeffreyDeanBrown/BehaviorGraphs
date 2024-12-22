# plot_indiv Functions.R
# version 1.2
# last update 03-02-2024
#
# FUNCTIONS:  plot_the_indiv, plot_by_indiv
# 
# DEPENDS: (need to fill this out)
# 
# DESCRIPTION:
#   These functions plot the graphs for individual mice. 
# 
#   plot_the_indiv is the place to customize the individual mouse graphs,
#   while plot_by_indiv has all of the ggplot mechanisms used for each graph. 
#   
#   be careful when changing plot_by_indiv


#-----------------------------------------------------------------------------------------------------------------

plot_the_indiv <- function(.data, subtitle){
  
  # edit theme_update parameters to change the style of the graphs
  
  theme_update(legend.title = element_blank(),
               plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5),
               axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = 'lightgray', linewidth = 0.05),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
  
  .data %<>% group_by(id, {{ subtitle }}, group, sod1_group) %>% nest
  
  print("plotting weight for individual mice...")
  
  
  # edit title_str, yaxis_str, y_lim, y_break, or plot_ratio here to edit the
  # title, y-axis label, y-axis limit, y-axis ticks, or ratio of the graphs
  
  .data %<>% plot_by_indiv(new_column = 'weight_plot',
                                subtitle = 'subtitle',
                                measured_val = weight_av,
                                sd = weight_sd,
                                title_str = "Body Weight (grams)",
                                yaxis_str = "Weight (grams)",
                                y_lim = c(15,35),
                                y_break = c(15:35),
                                plot_ratio = 0.6)
  
  print("plotting grip strength for individual mice...")

  .data %<>% plot_by_indiv(new_column = 'grip_plot',
                                subtitle = 'subtitle',
                                 measured_val = grip_av,
                                 sd = grip_sd,
                                 title_str = "Grip Strength (grams)",
                                 yaxis_str = "Max Grip Strenth (grams)",
                                 y_lim = c(0,150),
                                 y_break = seq(0,150,10),
                                 plot_ratio = .06)
  
  print("plotting rotarod speed for individual mice...")

  .data %<>% plot_by_indiv(new_column = 's_rotarod_plot',
                             subtitle = 'subtitle',
                                 measured_val = s_rotarod,
                                 sd = 0,
                                 errbar_width = 0,
                                 title_str = 'Rotarod Speed (RPM) at time of Fall',
                                 yaxis_str = "Rotarod Speed (RPM)",
                                 y_lim = c(0,35),
                                 y_break = seq(0,35,5),
                                 plot_ratio = 0.3)
  
  print("plotting rotarod time for individual mice...")

  .data %<>% plot_by_indiv(new_column = 't_rotarod_plot',
                           subtitle = 'subtitle',
                                 measured_val = t_rotarod,
                                 sd = 0,
                                 errbar_width = 0,
                                 title_str = 'Time on Rotarod (seconds)',
                                 yaxis_str = "Time on Rotarod (Seconds)",
                                 y_lim = c(0,350),
                                 y_break = seq(0,350,25),
                                 plot_ratio = 0.03)
  
  print("plotting condition assesment for individual mice...")

  .data %<>% plot_by_indiv(new_column = 'condition_plot',
                           subtitle = 'subtitle',
                                 measured_val = condition,
                                 sd = 0,
                                 errbar_width = 0,
                                 title_str = 'Mouse Condition (rated 1 through 5)',
                                 yaxis_str = 'Mouse Condition',
                                 y_lim = c(0,8),
                                 y_break = c(1:8),
                                 plot_ratio = 1.0)
  return(.data)
}


#-------------------------------------------------------------------------------------------------------------------

plot_by_indiv <- function(.data, new_column, data_colname = 'data', subtitle,
                          measured_val, title_str, yaxis_str, y_lim, y_break,
                          sd, plot_ratio, errbar_width = 0.3){
  
  t1 <- pluck(.data, data_colname)
  t2 <- pluck(.data, subtitle)
  
  x <- map2(
    t1, t2, 
    ~ ggplot(data = .x,
             aes(x = week,
                 y = {{ measured_val }},
                 label = week, na.rm = TRUE)) +
      ggtitle({{ title_str }},          #### graph title
              paste(glue("{.y}"))) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      coord_fixed(ratio = {{ plot_ratio }}) +
      scale_y_continuous({{ yaxis_str }},   #### y-axis title
                         limits = {{ y_lim }},
                         breaks = {{ y_break }}) +
      scale_x_continuous("Age (weeks)",
                         breaks = c(11:24)) + 
      geom_errorbar(aes(ymin = {{ measured_val }} - {{ sd }},
                        ymax = {{ measured_val }} + {{ sd }}),
                    linewidth = errbar_width,
                    width = 0.2))
  
  y <- tibble(x, .name_repair = ~ c(new_column))
  
  return(cbind(.data, y))
  
}






