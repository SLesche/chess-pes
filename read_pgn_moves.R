read_pgn_moves <- function(con, add.tags = NULL, quiet = F) {
  # Standard tags to read
  tags <- c("Event", "Site", "Date", "Round", "White", "Black", "Result", 
            "WhiteElo", "BlackElo", "TimeControl", add.tags)
  
  # Read file lines
  al <- readLines(con)
  
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

