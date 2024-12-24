
plot_measurements <- function(.data, graph_subtitle){

  theme_update(legend.title = element_blank(),
               axis.line = element_line(colour = "black"),
               panel.grid.major = element_line(colour = 'lightgray', linewidth = 0.05),
               panel.grid.minor = element_blank(),
               panel.background = element_blank())

  print("plotting the mice by weight...")

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





plotter_gg <- function(.nested_tibble, new_column, graph_subtitle, measured_val, title_str,
                       yaxis_str, y_lim, y_break, plot_ratio){

  list1 <- pluck(.nested_tibble, 'data')
  list2 <- pluck(.nested_tibble, graph_subtitle)

  plots <- map2(list1, list2, ~
    ggplot(data = .x, aes(x = week, y = {{ measured_val }}, color = genotype_lab, na.rm = TRUE)) + #:genotype_lab: genotype (n={n_distinct(number)})
    labs(title = title_str, subtitle =  glue("{.y}")) +
    geom_line(stat = 'summary', fun = 'mean', linewidth = 1, na.rm = TRUE) +
    geom_pointrange(stat = 'summary', fun.data = 'mean_cl_boot', linewidth = 0.7, na.rm = TRUE) +
    coord_fixed(ratio = {{ plot_ratio }}) +
    scale_y_continuous(name = {{ yaxis_str }}, limits = {{ y_lim }}, breaks = {{ y_break }}) +
    scale_x_continuous(name = "Age (weeks)", breaks = c(11:24))
   )

  plots %<>% tibble(.name_repair = ~c(new_column))
  return(cbind(.nested_tibble, plots))

}

