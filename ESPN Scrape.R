# The NBA has been interesting lately. Use the following page as a jumping point:
#   
#   https://www.espn.com/nba/schedule/_/date/20200910
# 
# This page contains links with a gameID -- you will need those. Take those gameID values and scrape the play-by-play tables for each of the games on that main page. 
# 
# The ideal return is going to be each game having a complete data frame.

library(httr)
library(rvest)
library(stringr)
link <- "https://www.espn.com/nba/schedule/_/date/20200910"

nba <- read_html(link)
nba_links <- nba%>% 
  html_nodes("a[href*='/nba/game?gameId=']") %>% 
  html_attr("href") %>% 
  unique() %>% 
  str_replace(., "/nba/game", "") %>% 
  paste("https://www.espn.com/nba/playbyplay", ., sep ="")

get_games <- function(x) {
  read_html(x) %>% 
    html_table(fill = TRUE)
}

# Get the games
games <- lapply(nba_links, get_games)

combine_quarters_2 <- function(x) {
  table_length <- length(x) - 2
  datalist <- list()
  for (y in 2:table_length) {
    iteration <- y - 1
    datalist[[iteration]] <- x[[y]]
  }
  out <- do.call(rbind, datalist)
  out <- out[,c(1,3,4)]
}

combine_quarters_1 <- function(x) {
  table_length <- length(x) - 1
  datalist <- list()
  for (y in 2:table_length) {
    iteration <- y - 1
    datalist[[iteration]] <- x[[y]]
  }
  out <- do.call(rbind, datalist)
  out <- out[,c(1,3,4)]
}

# LA vs. Houston
LAA_HOU <- games[[1]]
LAA_HOU <- combine_quarters_2(LAA_HOU)

# Denver vs. LA
DEN_LAC <- games[[2]]
DEN_LAC <- combine_quarters_2(DEN_LAC)

# Boston vs. Toronto 
BOS_TOR <- games[[3]]
BOS_TOR <- combine_quarters_1(BOS_TOR)

# Houston vs. LA
HOU_LAA <- games[[4]]
HOU_LAA <- combine_quarters_2(HOU_LAA)

# LA vs. Denver 
LAC_DEN <- games[[5]]
LAC_DEN <- combine_quarters_2(LAC_DEN)

# Miami vs. Boston 
MIA_BOS <- games[[6]]
MIA_BOS <- combine_quarters_2(MIA_BOS)

# Denver vs. LA
DEN_LAC <- games[[7]]
DEN_LAC <- combine_quarters_2(DEN_LAC)