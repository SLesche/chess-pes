read_pgn_moves <- function(con, add.tags = NULL, quiet = F, max.lines = NULL) {
  # Standard tags to read
  tags <- c("Event", "Site", "Date", "Round", "White", "Black", "Result", 
            "WhiteElo", "BlackElo", "TimeControl", add.tags)
  
  # Read file lines
  if (!is.null(max.lines)){
    al <- readLines(con, n = max.lines)
  } else {
    al <- readLines(con)
  }
  
  # Extract headers
  s <- "^\\[([\\S]+)\\s\"([^\"]+)\"\\]$"
  tmp1 <- gsub(s, "\\1", al, perl = T)
  tmp2 <- gsub(s, "\\2", al, perl = T)
  tmp3 <- grepl("^\\[[^%]+\\]$", al, perl = T)
  tmp4 <- cumsum(grepl("\\[Event ", al))
  tmp1[!tmp3] <- "Movetext"
  r2 <- data.frame(tmp1, tmp2, tmp3, tmp4, stringsAsFactors = F)
  
  # Split games
  game_ids <- unique(tmp4)
  all_games <- list()
  
  for (gid in game_ids) {
    game_lines <- r2[r2$tmp4 == gid, ]
    
    # Extract header info
    WhiteElo <- as.integer(game_lines$tmp2[game_lines$tmp1 == "WhiteElo"])
    White <- game_lines$tmp2[game_lines$tmp1 == "White"]
    
    BlackElo <- as.integer(game_lines$tmp2[game_lines$tmp1 == "BlackElo"])
    Black <- game_lines$tmp2[game_lines$tmp1 == "Black"]
    
    TimeControl <- game_lines$tmp2[game_lines$tmp1 == "TimeControl"]
    
    # Combine movetext into single string
    moves_text <- paste(game_lines$tmp2[game_lines$tmp1 == "Movetext"], collapse = " ")
    
    # Extract moves with eval and clock
    move_pattern <- "(\\d+\\.\\.?.*?)\\s*\\{ \\[%eval ([^\\]]+)\\] \\[%clkc ([^\\]]+)\\] \\}"
    moves <- str_match_all(moves_text, move_pattern)[[1]]
    
    if(length(moves) == 0) next
    
    dt <- data.table(
      Id = gid,
      Move = moves[,2],
      Eval = moves[,3],
      Clock = as.numeric(moves[,4]),
      WhiteElo = WhiteElo,
      White = White,
      BlackElo = BlackElo,
      Black = Black,
      TimeControl = TimeControl
    )
    
    all_games[[gid]] <- dt
  }
  
  result <- rbindlist(all_games)
  if (!quiet) message(paste(Sys.time(), " - Parsed", length(all_games), "games"))
  return(result)
}

library(stringr)
library(data.table)

read_pgn_moves_stream <- function(con, add.tags = NULL, quiet = FALSE, max.games = Inf) {
  # Standard tags
  tags <- c("Event", "Site", "Date", "Round", "White", "Black", "Result", 
            "WhiteElo", "BlackElo", "TimeControl", add.tags)
  
  all_games <- list()
  gid <- 0
  game_lines <- character()
  
  # Read line by line
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if (grepl("^\\[Event ", line)) {
      # New game starts -> process the previous one
      if (length(game_lines) > 0) {
        gid <- gid + 1
        all_games[[gid]] <- parse_single_game(game_lines, gid)
        if (!quiet && gid %% 100 == 0) 
          message(Sys.time(), " - Parsed ", gid, " games")
        if (gid >= max.games) break
      }
      game_lines <- line
    } else {
      game_lines <- c(game_lines, line)
    }
  }
  
  # Process last game
  if (length(game_lines) > 0 && gid < max.games) {
    gid <- gid + 1
    all_games[[gid]] <- parse_single_game(game_lines, gid)
  }
  
  result <- rbindlist(all_games, fill = TRUE)
  if (!quiet) message(Sys.time(), " - Parsed ", length(all_games), " games total")
  return(result)
}

# Helper: parse a single game’s lines
parse_single_game <- function(game_lines, gid) {
  s <- "^\\[([\\S]+)\\s\"([^\"]+)\"\\]$"
  tmp1 <- gsub(s, "\\1", game_lines, perl = TRUE)
  tmp2 <- gsub(s, "\\2", game_lines, perl = TRUE)
  tmp3 <- grepl("^\\[[^%]+\\]$", game_lines, perl = TRUE)
  tmp1[!tmp3] <- "Movetext"
  r2 <- data.frame(tmp1, tmp2, tmp3, stringsAsFactors = FALSE)
  
  WhiteElo <- as.integer(r2$tmp2[r2$tmp1 == "WhiteElo"])
  White    <- r2$tmp2[r2$tmp1 == "White"]
  BlackElo <- as.integer(r2$tmp2[r2$tmp1 == "BlackElo"])
  Black    <- r2$tmp2[r2$tmp1 == "Black"]
  TimeControl <- r2$tmp2[r2$tmp1 == "TimeControl"]
  
  moves_text <- paste(r2$tmp2[r2$tmp1 == "Movetext"], collapse = " ")
  move_pattern <- "(\\d+\\.\\.?.*?)\\s*\\{ \\[%eval ([^\\]]+)\\] \\[%clkc ([^\\]]+)\\] \\}"
  moves <- str_match_all(moves_text, move_pattern)[[1]]
  
  if (length(moves) == 0) return(NULL)
  
  dt <- data.table(
    Id = gid,
    Move = moves[,2],
    Eval = moves[,3],
    Clock = as.numeric(moves[,4]),
    WhiteElo = WhiteElo,
    White = White,
    BlackElo = BlackElo,
    Black = Black,
    TimeControl = TimeControl
  )
  return(dt)
}


