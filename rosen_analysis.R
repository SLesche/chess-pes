library(tidyverse)

data <- read.csv("rosen_eval.csv")

data <- data %>% 
  mutate(game_id = str_remove(Site, "https://lichess.org/"))

move_data <- data %>% 
  select(game_id, Moves)

parse_moves <- function(move_string){
  split_moves = strsplit(move_string, "\\d+\\.+ ")
  
  clean_moves = split_moves[[1]][-1]
  
  return(clean_moves)
}

clean_move_data <- move_data %>% 
  as_tibble() %>% 
  mutate(clean_moves = map(Moves, parse_moves)) %>% 
  select(game_id, clean_moves) %>% 
  unnest(cols = clean_moves)


clean_move_data <- clean_move_data %>% 
  group_by(game_id) %>% 
  mutate(move_num = row_number()) %>% 
  ungroup() %>% 
  mutate(color = ifelse(move_num %% 2, "white", "black"))

parse_time <- function(move_string){
  time_string = str_extract(move_string, "\\[%clk \\d+\\:\\d+:\\d+]")
  
  clock_time = str_extract(time_string, "\\d+\\:\\d+:\\d+")
  
  time = lubridate::period_to_seconds(lubridate::hms(clock_time))
  return(time)
}

parse_eval <- function(move_string){
  eval_string = str_extract(move_string, "\\[%eval -?\\d\\.\\d+\\]")
  
  eval = as.numeric(str_extract(eval_string, "-?\\d\\.\\d+"))
  
  return(eval)
}

clean_move_data <- clean_move_data %>% 
  mutate(clk_time = parse_time(clean_moves))

clean_move_data <- clean_move_data %>% 
  mutate(eval = parse_eval(clean_moves))

clean_move_data <- clean_move_data %>% 
  group_by(game_id) %>% 
  mutate(prev_eval = lag(eval)) %>% 
  group_by(game_id, color) %>% 
  mutate(prev_time = lag(clk_time)) %>% 
  ungroup()

clean_move_data <- clean_move_data %>% 
  left_join(., data %>% select(game_id, TimeControl, WhiteElo, BlackElo, Result))

clean_move_data <- clean_move_data %>% 
  mutate(increment = as.numeric(str_remove(TimeControl, "^\\d+\\+")),
         time_control = as.numeric(str_remove(TimeControl, "\\+\\d+$")))

clean_move_data <- clean_move_data %>% 
  mutate(
    move_time = prev_time - clk_time + increment,
    move_eval = eval - prev_eval
  ) %>% 
  mutate(move_time = ifelse(move_time < 0, NA, move_time)) %>% 
  mutate(
    corrected_move_eval = ifelse(color == "black", -1 * move_eval, move_eval)
  ) %>% 
  group_by(game_id, color) %>% 
  mutate(
    prev_own_move_eval = lag(corrected_move_eval)
  ) %>% 
  group_by(game_id) %>% 
  mutate(prev_opp_move_eval = lag(corrected_move_eval)) %>% 
  ungroup()

clean_move_data <- clean_move_data %>% 
  mutate(
    mistake = factor(ifelse(move_eval < -2, 1, 0))
  ) %>% 
  group_by(game_id) %>% 
  mutate(
    prev_opp_mistake = factor(lag(mistake))
  ) %>% 
  group_by(game_id, color) %>% 
  mutate(prev_own_mistake = factor(lag(mistake))) %>% 
  ungroup() 

clean_move_data <- clean_move_data %>% 
  mutate(
    abs_eval = abs(eval),
  )

binned_summary <- clean_move_data %>% 
  mutate(
    move_num_quantile = move_num - (move_num %% 20) + 20,
    cut_eval = cut(abs_eval, 50),
    cut_own_move_eval = cut(prev_own_move_eval, 50),
    cut_opp_move_eval = cut(prev_opp_move_eval, 50)
  ) %>% 
  group_by(
    move_num_quantile,
    cut_eval,
    cut_own_move_eval,
    cut_opp_move_eval,
    color,
  ) %>% 
  summarize(
    mean_move_time = mean(move_time, na.rm = TRUE),
    n = n()
  ) %>% 
  filter(
    n > 100
  ) %>% 
  na.omit()

binned_summary <- binned_summary %>% 
  mutate(
    abs_eval = parse_number(as.character(cut_eval)),
    prev_own_move_eval = parse_number(as.character(cut_own_move_eval)),
    prev_opp_move_eval = parse_number(as.character(cut_opp_move_eval)),
  )

# Make sure that the correction for eval is correct. Should easiness of position 
# also apply to the "worse" party?
# Maybe add color as predictor

# Lets make some plots
clean_move_data %>% 
  filter(!is.na(prev_own_mistake)) %>%
  group_by(prev_own_mistake) %>% 
  slice_sample(n = 20000) %>% 
  ungroup() %>% 
  ggplot(aes(x = abs(prev_eval), y = move_time, group = prev_own_mistake, color = prev_own_mistake))+
  geom_smooth(method = "loess")

# Move time over move number
summary(lm(move_time ~ prev_own_mistake + abs_eval + sq_eval + color + move_num + sq_move_num, data = clean_move_data))
