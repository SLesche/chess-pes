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
    mistake = factor(ifelse(corrected_move_eval < -2 & ((color == "black" & eval > 0) | (color == "white" & eval < 0)), 1, 0))
  ) %>% 
  group_by(game_id) %>% 
  mutate(
    prev_opp_mistake = factor(lag(mistake))
  ) %>% 
  group_by(game_id, color) %>% 
  mutate(prev_own_mistake = factor(lag(mistake))) %>% 
  ungroup() 

clean_data <- clean_move_data %>% 
  filter(!is.na(move_num), !is.na(eval), !is.na(clk_time)) 

posterror_move_data <- clean_data %>% 
  filter(
    prev_own_mistake == 1
  )

library(MatchIt)

# Example: match sampled_data to full_data by move_num and eval
match_result <- matchit(group ~ move_num + eval + clk_time, 
                        data = rbind(
                          posterror_move_data %>% mutate(group = 1),
                          clean_data %>% filter(
                            prev_own_mistake != 1
                          ) %>% mutate(group = 0)
                        ),
                        method = "nearest", 
                        distance = "mahalanobis")

matched_data <- match.data(match_result) %>%
  filter(group == 0) # matched rows from full_data
# Make sure that the correction for eval is correct. Should easiness of position 
# also apply to the "worse" party?
# Maybe add color as predictor

full_data <- match.data(match_result)

full_data %>% 
  filter(move_time < 50, clk_time < 600) %>% 
  ggplot(
    aes(x = move_time, fill = factor(group), group = factor(group))
  )+
  geom_density(
   alpha = 0.5 
  )

full_data %>% 
  filter(move_time < 50) %>% 
  group_by(group) %>% 
  summarize(mean_move_time = mean(move_time, na.rm = TRUE))

brms::brm(move_time ~ group, full_data %>% 
            filter(move_time < 50, clk_time < 600))

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
