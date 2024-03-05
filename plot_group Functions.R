# plot_group Functions.R
# version 1.2
# last update 03-02-2024
#
# FUNCTIONS:  plot_the_group, plot_by_group
# 
# DEPENDS: (need to fill this out)
#
# DEPENDENTS: show_custom_options() needs to be updated if a new_column is updated
# 
# DESCRIPTION:
#   These functions plot the graphs for grouped mice. 
# 
#   plot_the_group is the place to customize the grouped mouse graphs,
#   while plot_by_group has all of the ggplot mechanisms used for each graph. 
#   
#   be careful when changing plot_by_group


#-------------------------------------------------------------------------------------------------


plot_the_group <- function(.data, graph_subtitle){
  #theme for the graphs. edit this to edit the style of the graph
  theme_update(legend.title = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = 'lightgray', linewidth = 0.05),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
  
  print("plotting the mice by weight...")
  
  # edit title_str, yaxis_str, y_lim, y_break, or plot_ratio here to edit the
  # title, y-axis label, y-axis limit, y-axis ticks, or ratio of the graphs
  .data %<>% plot_by_group(new_column = 'weight_plot',
                                graph_subtitle = {{graph_subtitle}},
                                measured_val = weight_av_mean,
                                sd = weight_av_sd,
                                title_str = "Body Weight (grams)",
                                yaxis_str = "Weight (grams)",
                                y_lim = c(15,35),
                                y_break = c(15:35),
                                plot_ratio = 0.6)

  print("plotting the mice by grip...")
  .data %<>% plot_by_group(new_column = 'grip_plot',
                                 graph_subtitle = {{graph_subtitle}},
                                 measured_val = grip_av_mean,
                                 sd = grip_av_sd,
                                 title_str = "Grip Strength (grams)",
                                 yaxis_str = "Max Grip Strenth (grams)",
                                 y_lim = c(0,150),
                                 y_break = seq(0,150,10),
                                 plot_ratio = .06)

  print("plotting the mice by rotarod...")
  .data %<>% plot_by_group(new_column = 's_rotarod_plot',
                                 graph_subtitle = {{graph_subtitle}},
                                 measured_val = s_rotarod_mean,
                                 sd = s_rotarod_sd,
                                 title_str = 'Rotarod Speed (RPM) at time of Fall',
                                 yaxis_str = "Rotarod Speed (RPM)",
                                 y_lim = c(0,35),
                                 y_break = seq(0,35,5),
                                 plot_ratio = 0.3)

  .data %<>% plot_by_group(new_column = 't_rotarod_plot',
                                 graph_subtitle = {{graph_subtitle}},
                                 measured_val = t_rotarod_mean,
                                 sd = t_rotarod_sd,
                                 title_str = 'Time on Rotarod (seconds)',
                                 yaxis_str = "Time on Rotarod (Seconds)",
                                 y_lim = c(0,350),
                                 y_break = seq(0,350,25),
                                 plot_ratio = 0.03)
  
  print("plotting the mice by condition...")
  .data %<>% plot_by_group(new_column = 'condition_plot',
                                 graph_subtitle = {{graph_subtitle}},
                                 measured_val = condition_mean,
                                 sd = condition_sd,
                                 title_str = 'Mouse Condition (rated 1 through 5)',
                                 yaxis_str = 'Mouse Condition',
                                 y_lim = c(0,8),
                                 y_break = c(1:8),
                                 plot_ratio = 1.0)
  return(.data)
}



#---------------------------------------------------------------------------------------------------------------


plot_by_group <- function(.data, new_column, data_colname = 'data', graph_subtitle,
                          measured_val, group_legend = legend, title_str, yaxis_str, y_lim, y_break,
                          sd, plot_ratio){
  t1 <- pluck(.data, data_colname)
  t2 <- pluck(.data, graph_subtitle)
  
  x <- map2(
    t1, t2, 
    ~ ggplot(data = .x,
             aes(x = week,
                 y = {{measured_val}},
                 group = {{group_legend}},
                 color = {{group_legend}}, na.rm = TRUE), shape='.') +
      ggtitle({{title_str}},          #### graph title
              paste(glue("{.y}"))) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      coord_fixed(ratio = {{plot_ratio}}) +
      scale_y_continuous({{yaxis_str}},   #### y-axis title
                         limits = {{y_lim}},
                         breaks = {{y_break}}) +
      scale_x_continuous("Age (weeks)",
                         breaks = c(11:24)) + 
      geom_errorbar(aes(ymin = {{measured_val}} - {{sd}},
                        ymax = {{measured_val}} + {{sd}}),
                    linewidth = 0.3,
                    width = 0.2))
  
  y <- lapply(x, function(x){ggplotGrob(x)})
  y <- tibble(y, .name_repair = ~ c(new_column))
  
  
  return(cbind(.data, y))
  
}




