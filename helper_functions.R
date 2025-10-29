plot_moderator_analysis <- function(data, moderator_var, x_label = NULL, include_error_types = FALSE){
  # capture the unquoted variable
  mod_quo <- enquo(moderator_var)
  mod_name <- quo_name(mod_quo)
  
  # compute quantiles (1–100)
  if (include_error_types){
    eff_data <- data %>% 
      mutate(
        moderator = ntile(!!mod_quo, 100)
      ) %>% 
      group_by(moderator, mistake_made_in_losing_position_posterror, mistake_flipped_sign_posterror) %>% 
      summarize(
        mean_pes = mean(pes, na.rm = TRUE),
        mean_se_pes = sd(pes, na.rm = TRUE) / sqrt(n()),
        sd_pes = sd(pes, na.rm = TRUE),
        mean_pea = mean(pea, na.rm = TRUE),
        effsize = effectsize::cohens_d(move_time_control, move_time_posterror)
      ) %>% 
      mutate(
        mean_upper = mean_pes + 1.96*mean_se_pes,
        mean_lower = mean_pes - 1.96*mean_se_pes
      ) %>% 
      janitor::clean_names()
    
  } else {
    eff_data <- data %>% 
      mutate(
        moderator = ntile(!!mod_quo, 100)
      ) %>% 
      group_by(moderator) %>%
      summarize(
        mean_pes = mean(pes, na.rm = TRUE),
        mean_se_pes = sd(pes, na.rm = TRUE) / sqrt(n()),
        sd_pes = sd(pes, na.rm = TRUE),
        mean_pea = mean(pea, na.rm = TRUE),
        effsize = effectsize::cohens_d(move_time_control, move_time_posterror)
      ) %>% 
      mutate(
        mean_upper = mean_pes + 1.96*mean_se_pes,
        mean_lower = mean_pes - 1.96*mean_se_pes
      ) %>% 
      janitor::clean_names()
    
  }
  
  
  # find actual moderator values at every 10th quantile
  quantile_labels <- data %>%
    summarize(q = quantile(!!mod_quo, probs = seq(0.05, 0.95, 0.15), na.rm = TRUE)) %>%
    pull(q) 
  
  if (include_error_types){
    ggplot(eff_data, aes(x = moderator, y = mean_pes, group = sign_flipped_posterror, color = sign_flipped_posterror)) +
      geom_point(alpha = 0.7) +
      geom_errorbar(
        aes(ymin = mean_lower, ymax = mean_upper)
      )+
      geom_smooth(se = FALSE) +
      scale_x_continuous(
        breaks = seq(5, 95, by = 10),
        labels = round(quantile_labels, 2)
      ) +
      theme_classic() +
      labs(
        y = "PES",
        x = ifelse(is.null(x_label), glue::glue("{mod_name} (quantile bins → actual values on axis)"), x_label)
      )
  } else {
    ggplot(eff_data, aes(x = moderator, y = mean_pes)) +
      geom_point() +
      geom_errorbar(
        aes(ymin = mean_lower, ymax = mean_upper)
      )+
      geom_smooth(se = FALSE) +
      scale_x_continuous(
        breaks = seq(5, 95, by = 15),
        labels = round(quantile_labels, 1)
      ) +
      theme_classic() +
      labs(
        y = "PES",
        x = ifelse(is.null(x_label), glue::glue("{mod_name} (quantile bins → actual values on axis)"), x_label)
      )+
      theme(
        text = element_text(size = 25),            # base text size
        axis.title = element_text(size = 25),      # axis titles
        axis.text = element_text(size = 20),       # axis labels
      )
  }
}

plot_moderator_grouped <- function(data, moderator_var){
  # capture the unquoted variable
  mod_quo <- enquo(moderator_var)
  mod_name <- quo_name(mod_quo)
  
  # compute quantiles (1–100)
  eff_data <- data %>% 
    mutate(
      moderator = ntile(!!mod_quo, 100)
    ) %>% 
    group_by(moderator, time_format) %>% 
    summarize(
      mean_pes = mean(pes, na.rm = TRUE),
      sd_pes = sd(pes, na.rm = TRUE),
      mean_pea = mean(pea, na.rm = TRUE),
      effsize = effectsize::cohens_d(move_time_control, move_time_posterror)
    ) %>% 
    janitor::clean_names()
  
  colnames(eff_data) <- c("moderator", "time_format", "mean_pes", "sd_pes", "mean_pea", "cohen_d")

  # find actual moderator values at every 10th quantile
  quantile_labels <- data %>%
    summarize(q = quantile(!!mod_quo, probs = seq(0.05, 0.95, 0.1), na.rm = TRUE)) %>%
    pull(q)
  
  ggplot(eff_data, aes(x = moderator, y = mean_pes, group = time_format, color = time_format)) +
    geom_point(alpha = 0.7) +
    geom_smooth(se = FALSE) +
    # scale_x_continuous(
    #   breaks = seq(5, 95, by = 10),
    #   labels = round(quantile_labels, 2)
    # ) +
    theme_classic() +
    labs(
      y = "PES",
      x = glue::glue("{mod_name}")
    )
}
