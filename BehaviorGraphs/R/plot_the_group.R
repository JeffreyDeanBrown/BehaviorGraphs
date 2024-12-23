# plot_group Functions.R
# version 1.2
# last update 03-02-2024
#
# FUNCTIONS:  plot_the_group, plotter_gg
# 
# DEPENDS: (need to fill this out)
#
# DEPENDENTS: show_custom_options() needs to be updated if a new_column is updated
# 
# DESCRIPTION:
#   These functions plot the graphs for grouped mice. 
# 
#   plot_the_group is the place to customize the grouped mouse graphs,
#   while plotter_gg has all of the ggplot mechanisms used for each graph. 
#   
#   be careful when changing plotter_gg


#-------------------------------------------------------------------------------------------------


plot_measurements <- function(.data, graph_subtitle){                        #move stats to a layer and remove it from earlier functions!
  #theme for the graphs. edit this to edit the style of the graph         # also only make one plot function, but pipe in differently grouped data (either indiv or w/e)
  theme_update(legend.title = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = 'lightgray', linewidth = 0.05),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())
  
  print("plotting the mice by weight...")
  
  # edit title_, yaxis_str, y_lim, y_break, or plot_ratio here to edit the
  # title, y-axis label, y-axis limit, y-axis ticks, or ratio of the graphs
  .data %<>% plotter_gg(new_column = 'weight_plot',
                                graph_subtitle = graph_subtitle,
                                measured_val = weight_av,
                                title_str = "Body Weight (grams)",
                                yaxis_str = "Weight (grams)",
                                y_lim = c(15,35),
                                y_break = c(15:35),
                                plot_ratio = 0.6)

  print("plotting the mice by grip...")
  .data %<>% plotter_gg(new_column = 'grip_plot',
                                 graph_subtitle = graph_subtitle,
                                 measured_val = grip_av,
                                 title_str = "Grip Strength (grams)",
                                 yaxis_str = "Max Grip Strenth (grams)",
                                 y_lim = c(0,150),
                                 y_break = seq(0,150,10),
                                 plot_ratio = .06)

  print("plotting the mice by rotarod...")
  .data %<>% plotter_gg(new_column = 's_rotarod_plot',
                                 graph_subtitle = graph_subtitle,
                                 measured_val = s_rotarod,
                                 title_str = 'Rotarod Speed (RPM) at time of Fall',
                                 yaxis_str = "Rotarod Speed (RPM)",
                                 y_lim = c(0,35),
                                 y_break = seq(0,35,5),
                                 plot_ratio = 0.3)

  .data %<>% plotter_gg(new_column = 't_rotarod_plot',
                                 graph_subtitle = graph_subtitle,
                                 measured_val = t_rotarod,
                                 title_str = 'Time on Rotarod (seconds)',
                                 yaxis_str = "Time on Rotarod (Seconds)",
                                 y_lim = c(0,350),
                                 y_break = seq(0,350,25),
                                 plot_ratio = 0.03)
  
  print("plotting the mice by condition...")
  .data %<>% plotter_gg(new_column = 'condition_plot',
                                 graph_subtitle = graph_subtitle,
                                 measured_val = condition,
                                 title_str = 'Mouse Condition (rated 1 through 5)',
                                 yaxis_str = 'Mouse Condition',
                                 y_lim = c(0,8),
                                 y_break = c(1:8),
                                 plot_ratio = 1.0)
  return(.data)
}



#---------------------------------------------------------------------------------------------------------------


plotter_gg <- function(.data, new_column, graph_subtitle, measured_val, title_str,
                       yaxis_str, y_lim, y_break, plot_ratio){
  list1 <- pluck(.data, 'data')
  list2 <- pluck(.data, graph_subtitle)
  
  x <- suppressWarnings(map2(
    list1, list2, 
    ~ ggplot(data = .x,
             aes(x = week, y = {{ measured_val }},
                 color = genotype_lab, na.rm = TRUE)) + #:genotype_lab: genotype (n={n_distinct(number)})
      ggtitle({{ title_str }},   # graph title
              glue("{.y}")) +    # subtitle = graph_subtitle
      geom_line(stat = 'summary', fun = 'mean', linewidth = 1, na.rm = TRUE) +
      geom_pointrange(stat = 'summary', fun.data = 'mean_cl_boot', linewidth = 1) +
      coord_fixed(ratio = {{ plot_ratio }}) +
      scale_y_continuous({{ yaxis_str }},   #### y-axis title
                         limits = {{ y_lim }},
                         breaks = {{ y_break }}) +
      scale_x_continuous("Age (weeks)",
                         breaks = c(11:24))))
  
  y <- lapply(x, function(x){ggplotGrob(x)})
  y <- tibble(y, .name_repair = ~ c(new_column))
  
  
  return(cbind(.data, y))
  
}


#---------------------------------------------------------------------------------------------------------------

