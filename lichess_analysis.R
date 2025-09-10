library(bigchess)
# Replace with your PGN file path
pgn_file <- "eval_games.pgn"

# Read all games into a data frame
games <- read.pgn(
  pgn_file,
  # n.moves = FALSE,
  # stat.moves = TRUE,
  )

test <- read_pgn_moves("eval_games.pgn")

data <- test %>% 
  mutate(
    move_num = as.numeric(str_extract(Move, "^\\d+")),
    color = ifelse(str_detect(Move, "\\.{3}"), "black", "white"),
  ) %>% 
  mutate(
    player_moving = ifelse(color == "white", White, Black),
    player_moving_elo = ifelse(color == "white", WhiteElo, BlackElo),
    opponent_elo = ifelse(color == "white", BlackElo, WhiteElo),
  ) %>% 
  select(
    -contains("White"), -contains("Black"), - Move
  )
