library(tidyverse)
library(nbastatR)
library(lubridate)

logs <- game_logs(seasons = c(2014:2023), result_types = "team", season_types = "Regular Season")

gl <- logs %>%
    arrange(dateGame,idGame) %>%
    mutate(dateGame = as_date(dateGame)) %>%
    left_join(logs, 
              by = c("idGame" = "idGame", "slugTeam" = "slugOpponent")) %>%
    select(5,13,8,54,21,
           45,90,34,79,
           23,24,26,27,35,36,
           37,38,39,40,43,41,42,44,
           68,69,71,72,80,81,
           82,83,84,85,88,86,87,89)

colnames(gl) <- c("Date", "teamLoc", "teamName", "opptName", "teamRslt", 
                  "teamPTS", "opptPTS", "teamMin", "opptMin", 
                  "teamFGM", "teamFGA", "team3PM", "team3PA", "teamFTM",
                  "teamFTA", "teamORB", "teamDRB", "teamTRB", "teamAST",
                  "teamTOV", "teamSTL", "teamBLK", "teamPF", 
                  "opptFGM", "opptFGA", "oppt3PM", "oppt3PA", "opptFTM", 
                  "opptFTA", "opptORB", "opptDRB", "opptTRB", "opptAST", 
                  "opptTOV", "opptSTL", "opptBLK", "opptPF")

gl <- gl %>%
    mutate(fg3m_delta = team3PM - oppt3PM,
           ftm_delta = teamFTM - opptFTM)

df <- gl %>%
    group_by(fg3m_delta) %>%
    mutate(win = if_else(teamRslt=="W", 1, 0), loss = if_else(teamRslt=="L", 1, 0)) %>%
    summarise(across(c(win:loss),sum)) %>%
    mutate(win_pct = win/(win+loss))

df %>%
    ggplot(aes(fg3m_delta, win_pct)) +
    geom_col()
    





