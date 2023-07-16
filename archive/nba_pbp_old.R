library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)
library(future)

#### Part 1 ####

game_logs <- game_logs(seasons = 2020)

games <- game_logs %>%
    select(idGame, slugTeam, slugOpponent, locationGame) %>%
    mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
           slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
    select(-c(slugTeam, slugOpponent, locationGame)) %>%
    distinct(idGame, .keep_all = TRUE)

play_logs_all <- play_by_play_v2(game_ids = unique(games$idGame))

play_logs_all

play_logs_all %>%
    select(idGame, numberEvent, numberPeriod, timeQuarter, descriptionPlayHome, descriptionPlayVisitor) %>%
    add_count(idGame, numberEvent, numberPeriod, timeQuarter, descriptionPlayHome, descriptionPlayVisitor) %>%
    filter(n > 1) %>%
    head(10)

play_logs_all %>%
    select(idGame, numberEvent, numberPeriod, timeQuarter, descriptionPlayHome, descriptionPlayVisitor) %>%
    filter(idGame == 21900924,
           numberEvent > 75, 
           numberEvent < 90)

new_pbp <- play_logs_all %>%
    distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
    group_by(idGame) %>%
    mutate(numberEvent = row_number()) %>%  # new numberEvent column with events in the right order
    ungroup() %>%
    select(idGame, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,                   
           slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, minuteRemainingQuarter,          
           secondsRemainingQuarter, descriptionPlayHome, numberEvent, descriptionPlayVisitor, scoreHome, scoreAway) %>%
    mutate(shotPtsHome = case_when(
        numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,                               # Note 1
        numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,                                 # Note 2
        numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    mutate(shotPtsAway = case_when(
        numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
        numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
        numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    group_by(idGame) %>%
    mutate(ptsHome = cumsum(shotPtsHome),
           ptsAway = cumsum(shotPtsAway)) %>%
    ungroup() %>%
    mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%                       # Note 3 
    mutate(secsStartQuarter = case_when(                                                                        # Note 4
        numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
        TRUE ~ 2880 + (numberPeriod - 5) * 300
    )) %>%
    mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter),  # Note 5
           secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
    left_join(games %>%
                  select(idGame, slugTeamHome, slugTeamAway)) %>%
    select(idGame, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1, slugTeamPlayer2, 
           slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2, namePlayer3, 
           descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway) %>%
    mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
           marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
           timeQuarter = str_pad(timeQuarter, width = 5, pad = 0))

subs_made <- new_pbp %>%
    filter(numberEventMessageType == 8) %>%        # Note 6
    mutate(slugTeamLocation = ifelse(slugTeamPlayer1 == slugTeamHome, "Home", "Away")) %>%
    select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1,
           slugTeamLocation, playerOut = namePlayer1, playerIn = namePlayer2) %>%
    pivot_longer(cols = starts_with("player"),
                 names_to = "inOut",
                 names_prefix = "player",
                 values_to = "namePlayer") %>%
    group_by(idGame, numberPeriod, slugTeamPlayer, namePlayer) %>%
    filter(row_number() == 1) %>%
    ungroup()

others_qtr <- new_pbp %>%
    filter(numberEventMessageType != 8) %>%                             
    filter(!(numberEventMessageType == 6 & numberEventActionType %in% c(10, 11, 16, 18, 25))) %>%     # Note 7
    filter(!(numberEventMessageType == 11 & numberEventActionType == 1)) %>%   # Note 8
    pivot_longer(cols = starts_with("namePlayer"),
                 names_to = "playerNumber",
                 names_prefix = "namePlayer",
                 values_to = "namePlayer") %>%
    mutate(slugTeamPlayer = case_when(playerNumber == 1 ~ slugTeamPlayer1,
                                      playerNumber == 2 ~ slugTeamPlayer2,
                                      playerNumber == 3 ~ slugTeamPlayer3,
                                      TRUE ~ "None")) %>%
    mutate(slugTeamLocation = ifelse(slugTeamPlayer == slugTeamHome, "Home", "Away")) %>%
    filter(!is.na(namePlayer),
           !is.na(slugTeamPlayer)) %>%
    anti_join(subs_made %>%
                  select(idGame, numberPeriod, slugTeamPlayer, namePlayer)) %>%    # remove players that were subbed in the quarter
    distinct(idGame, numberPeriod, namePlayer, slugTeamPlayer, slugTeamLocation)

lineups_quarters <- subs_made %>%
    filter(inOut == "Out") %>%
    select(idGame, numberPeriod, slugTeamPlayer, namePlayer, slugTeamLocation) %>%
    bind_rows(others_qtr) %>%
    arrange(idGame, numberPeriod, slugTeamPlayer)

lineups_quarters

lineups_quarters %>%
    count(idGame, numberPeriod, slugTeamPlayer) %>%
    filter(n != 5)

lineups_quarters %>%
    filter(idGame == 21900023,
           numberPeriod == 5,
           slugTeamPlayer == "DEN")

missing_players_ot <- tribble(
    ~idGame,   ~slugTeamPlayer,          ~namePlayer,     ~numberPeriod,
    21900023,        "DEN",           "Malik Beasley",          5,
    21900120,        "MIN",          "Treveon Graham",          5,
    21900272,        "ATL",         "De'Andre Hunter",          5,
    21900409,        "WAS",               "Ish Smith",          5,
    21900502,        "GSW",              "Damion Lee",          5,
    21900550,        "OKC",       "Terrance Ferguson",          5,
    21900563,        "DET",              "Tony Snell",          5,
    21900696,        "SAC",         "Harrison Barnes",          5,
    21900787,        "ATL",         "De'Andre Hunter",          5,
    21900892,        "HOU",             "Eric Gordon",          5
) %>%
    left_join(games %>%
                  select(idGame, slugTeamHome, slugTeamAway)) %>%
    mutate(slugTeamLocation = ifelse(slugTeamHome == slugTeamPlayer, "Home", "Away")) %>%
    select(-c(slugTeamHome, slugTeamAway))

lineups_quarters <- lineups_quarters %>%
    bind_rows(missing_players_ot) %>%
    arrange(idGame, numberPeriod, slugTeamPlayer)

lineup_subs <- new_pbp %>%
    filter(numberEventMessageType == 8) %>%
    select(idGame, numberPeriod, timeQuarter, secsPassedGame, slugTeamPlayer = slugTeamPlayer1, playerOut = namePlayer1, 
           playerIn = namePlayer2, numberEvent) %>%
    arrange(idGame, numberEvent) %>%
    group_by(idGame, numberPeriod, slugTeamPlayer) %>%
    mutate(row1 = row_number()) %>%
    ungroup() %>%
    left_join(lineups_quarters %>%
                  group_by(idGame, numberPeriod, slugTeamPlayer) %>%
                  summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
                  ungroup() %>%
                  mutate(row1 = 1)) %>%
    select(-row1)

lineup_subs

lineup_subs <- lineup_subs %>%
    mutate(lineupBefore = str_split(lineupBefore, ", ")) %>% 
    arrange(idGame, numberEvent) %>%
    group_by(idGame, numberPeriod, slugTeamPlayer) %>%
    mutate(lineupAfter = accumulate2(playerIn, playerOut, ~setdiff(c(..1, ..2), ..3), .init = lineupBefore[[1]])[-1],
           lineupBefore = coalesce(lineupBefore, lag(lineupAfter))) %>%
    ungroup() %>% 
    mutate_all(~map_chr(., ~paste(.x, collapse = ", "))) %>%
    mutate_at(vars("numberEvent", "numberPeriod", "idGame"), ~ as.integer(.)) %>%
    mutate(secsPassedGame = as.numeric(secsPassedGame)) %>%
    arrange(idGame, numberEvent) %>%
    left_join(lineups_quarters %>%
                  distinct(idGame, slugTeamPlayer, slugTeamLocation)) %>%
    filter(!is.na(slugTeamLocation))

lineup_subs %>%
    select(lineupBefore, playerOut, playerIn, lineupAfter) %>%
    head(10)

lineup_game <- new_pbp %>%
    group_by(idGame, numberPeriod) %>%
    mutate(row1 = row_number()) %>%
    ungroup() %>%
    left_join(lineups_quarters %>%
                  group_by(idGame, numberPeriod, slugTeamLocation) %>%
                  summarise(lineupBefore = paste(sort(unique(namePlayer)), collapse = ", ")) %>%
                  ungroup() %>%
                  pivot_wider(names_from = slugTeamLocation,
                              names_prefix = "lineupInitial",
                              values_from = lineupBefore) %>%
                  mutate(row1 = 1)) %>%
    select(-row1) %>%
    left_join(lineup_subs %>%
                  mutate(lineupBeforeHome = ifelse(slugTeamLocation == "Home", lineupBefore, NA),
                         lineupAfterHome = ifelse(slugTeamLocation == "Home", lineupAfter, NA),
                         lineupBeforeAway = ifelse(slugTeamLocation == "Away", lineupBefore, NA),
                         lineupAfterAway = ifelse(slugTeamLocation == "Away", lineupAfter, NA)) %>%
                  select(idGame, numberPeriod, timeQuarter, secsPassedGame, numberEvent, slugTeamPlayer1 = slugTeamPlayer,
                         contains("Home"), contains("Away"))) %>%
    mutate_at(vars(c(lineupBeforeHome, lineupAfterHome)), ~ ifelse(!is.na(lineupInitialHome), lineupInitialHome, .)) %>%
    mutate_at(vars(c(lineupBeforeAway, lineupAfterAway)), ~ ifelse(!is.na(lineupInitialAway), lineupInitialAway, .)) %>%
    select(-starts_with("lineupInitial"))

lineup_game <- lineup_game %>%
    group_by(idGame, numberPeriod) %>%
    mutate(lineupHome = na.locf(lineupAfterHome, na.rm = FALSE),
           lineupAway = na.locf(lineupAfterAway, na.rm = FALSE),
           lineupHome = ifelse(is.na(lineupHome), na.locf(lineupBeforeHome, fromLast = TRUE, na.rm = FALSE), lineupHome),
           lineupAway = ifelse(is.na(lineupAway), na.locf(lineupBeforeAway, fromLast = TRUE, na.rm = FALSE), lineupAway),
           lineupHome = str_split(lineupHome, ", "),
           lineupAway = str_split(lineupAway, ", "),
           lineupHome = map_chr(lineupHome, ~ paste(sort(.), collapse = ", ")),
           lineupAway = map_chr(lineupAway, ~ paste(sort(.), collapse = ", "))) %>%
    ungroup() %>%
    select(-c(starts_with("lineupBefore"), starts_with("lineupAfter")))

lineup_game

#### Part 2 ####

# lineup_game <- read_csv("LineupGame0107.csv",
#                         col_types = cols(timeQuarter = "c"))

lineup_game %>%
    filter(idGame == 21900001,
           numberPeriod == 1,
           timeQuarter == "03:44") %>%
    select(starts_with("descriptionPlay"), starts_with("pts"))

lineup_game %>%
    filter(idGame == 21900001,
           numberPeriod == 1,
           timeQuarter == "03:44") %>%
    group_by(idGame, secsPassedGame) %>%
    mutate(ptsHome = ptsHome[row_number() == max(row_number())],
           ptsAway = ptsAway[row_number() == max(row_number())]) %>%
    ungroup() %>%
    select(starts_with("descriptionPlay"), starts_with("pts"))

lineup_game %>%
    filter(idGame == 21900078,
           numberPeriod == 4,
           timeQuarter == "00:06") %>%
    select(starts_with("descriptionPlay"), starts_with("pts"))

lineup_game %>%
    filter(idGame == 21900078,
           numberPeriod == 4,
           timeQuarter == "00:06") %>%
    mutate(canSub = case_when(numberEventMessageType == 5 & numberEventActionType %in% c(1, 2) ~ 1,     # dead ball turnovers
                              numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                              numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                              TRUE ~ 0)) %>%
    group_by(idGame, secsPassedGame) %>%
    mutate(subOpp = cumsum(canSub)) %>%
    group_by(idGame, secsPassedGame, subOpp) %>%
    mutate(ptsHome = ptsHome[row_number() == max(row_number())],
           ptsAway = ptsAway[row_number() == max(row_number())]) %>%
    ungroup() %>%
    select(starts_with("descriptionPlay"), starts_with("pts"), subOpp)

lineup_game %>%
    filter(idGame == 21900914,
           numberPeriod == 2,
           timeQuarter == "00:00") %>%
    select(numberEvent, starts_with("descriptionPlay"), starts_with("pts"))

lineup_game %>%
    filter(idGame == 21900914) %>%
    mutate(canSub = case_when(numberEventMessageType == 5 & numberEventActionType %in% c(1, 2) ~ 1,     # dead ball turnovers
                              numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                              numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                              TRUE ~ 0)) %>%
    group_by(idGame, secsPassedGame) %>%
    mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                              as.character(numberEvent)),
           numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                              numberNew)) %>%            # Note 1
    mutate(numberNew = str_split(numberNew, ", "),
           numberNew = map(numberNew, ~as.numeric(.)),
           numberNew = map2_dbl(numberNew, numberEvent, ~ max(.x[.x <= .y]))) %>%  # Note 2
    ungroup() %>%    
    arrange(idGame, numberNew, numberEvent) %>%
    select(-numberNew) %>%
    group_by(idGame) %>%
    mutate(newptsHome = cumsum(shotPtsHome),
           newptsAway = cumsum(shotPtsAway)) %>%         # Note 3
    group_by(idGame, secsPassedGame) %>%
    mutate(subOpp = cumsum(canSub)) %>%
    group_by(idGame, secsPassedGame, subOpp) %>%
    mutate(hasFouls = sum(numberEventMessageType == 3)) %>%    # see if there is a foul in the sequence
    mutate(ptsHome = ifelse(hasFouls > 0,
                            newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                            newptsHome),     # take last value when the event is a free throw
           ptsAway = ifelse(hasFouls > 0,
                            newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                            newptsAway)) %>%
    ungroup() %>%
    filter(numberPeriod == 2,
           timeQuarter == "00:00") %>%
    select(numberEvent, starts_with("descriptionPlay"), starts_with("pts"))

lineup_game_stats <- lineup_game %>%
    mutate(canSub = case_when(numberEventMessageType == 5 & !numberEventActionType %in% c(1, 2) ~ 1,    # dead ball turnovers
                              numberEventMessageType == 6 & numberEventActionType != 16 ~ 1,            # fouls
                              numberEventMessageType == 11 & numberEventActionType != 4 ~ 1,
                              numberEventMessageType == 7 & numberEventActionType == 5 ~ 1,             # kickballs
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayHome), "OFF:") ~ 1,
                              numberEventMessageType == 4 & numberEventActionType == 0 & !str_detect(str_to_upper(descriptionPlayVisitor), "OFF:") ~ 1,
                              TRUE ~ 0)) %>%
    mutate(secsPassedGame2 = ifelse(timeQuarter == "12:00" &
                                        (str_detect(str_to_lower(descriptionPlayHome), "technical") |
                                             str_detect(str_to_lower(descriptionPlayVisitor), "technical")),
                                    secsPassedGame + 0.5, secsPassedGame)) %>%    # Note 4
    group_by(idGame, numberPeriod, secsPassedGame) %>%
    mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                              as.character(numberEvent)),
           numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                              numberNew)) %>%
    mutate(numberNew = str_split(numberNew, ", "),
           numberNew = map(numberNew, ~as.numeric(.)),
           numberNew = map2_dbl(numberNew, numberEvent, ~ max(.x[.x <= .y]))) %>%
    ungroup() %>%
    arrange(idGame, numberNew, numberEvent) %>%
    group_by(idGame) %>%
    mutate(newptsHome = cumsum(shotPtsHome),
           newptsAway = cumsum(shotPtsAway)) %>%
    group_by(idGame, numberPeriod, secsPassedGame2) %>%
    mutate(subOpp = cumsum(canSub)) %>%
    group_by(idGame = as.character(idGame), numberPeriod = as.character(numberPeriod), subOpp, secsPassedGame2 = as.character(secsPassedGame2)) %>%
    mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
    mutate(newptsHome = ifelse(hasFouls > 0,
                               newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                               newptsHome),
           newptsAway = ifelse(hasFouls > 0,
                               newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                               newptsAway)) %>%
    ungroup() %>%
    select(-hasFouls) %>%
    select(-c(numberNew, secsPassedGame2)) %>%
    mutate_all(~ as.character(.)) %>%
    mutate(secsPassedGame = as.numeric(secsPassedGame),
           numberEvent = as.numeric(numberEvent))

lineup_game_stats

lineup_stats <- lineup_game_stats %>%
    select(idGame, numberEvent, slugTeamHome, slugTeamAway, numberPeriod, timeQuarter, secsPassedGame, 
           newptsHome, newptsAway, lineupHome, lineupAway) %>%
    pivot_longer(cols = starts_with("lineup"),
                 names_to = "lineupLocation",
                 names_prefix = "lineup",
                 values_to = "lineup") %>%
    mutate(ptsTeam = ifelse(lineupLocation == "Home", newptsHome, newptsAway),
           ptsOpp = ifelse(lineupLocation == "Away", newptsHome, newptsAway),
           slugTeam = ifelse(lineupLocation == "Home", slugTeamHome, slugTeamAway),
           slugOpp = ifelse(lineupLocation == "Away", slugTeamHome, slugTeamAway)) %>%
    distinct(idGame, slugTeam, slugOpp, numberPeriod, timeQuarter, secsPassedGame, ptsTeam, ptsOpp, lineup, 
             teamLocation = lineupLocation, numberEvent) %>%
    arrange(idGame, numberEvent)

lineup_stats 

lineup_stats <- lineup_stats %>%
    group_by(idGame, slugTeam) %>%
    mutate(lineupChange = lineup != lag(lineup),
           lineupChange = coalesce(lineupChange, FALSE)) %>%
    group_by(idGame, slugTeam) %>%
    mutate(lineupStint = cumsum(lineupChange)) %>%
    ungroup() %>%
    arrange(idGame, lineupStint, numberEvent) %>%
    group_by(idGame, slugTeam, lineup, lineupStint) %>%
    summarise(initialScoreTeam = ptsTeam[row_number() == min(row_number())],
              initialScoreOpp = ptsOpp[row_number() == min(row_number())],
              finalScoreTeam = ptsTeam[row_number() == max(row_number())],
              finalScoreOpp =  ptsOpp[row_number() == max(row_number())],
              initialTime = secsPassedGame[row_number() == min(row_number())],
              finalTime = secsPassedGame[row_number() == max(row_number())]) %>%
    ungroup() %>%
    arrange(idGame, lineupStint) %>%
    group_by(idGame, slugTeam) %>%                              
    mutate(finalTime = ifelse(row_number() == max(row_number()), finalTime, lead(initialTime))) %>%  
    ungroup()

lineup_stats

lineup_stats <- lineup_stats %>%
    mutate(across(c(contains("Score")), ~ as.numeric(.), .names = "{col}")) %>%
    mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
           totalScoreOpp = finalScoreOpp - initialScoreOpp,
           netScoreTeam = totalScoreTeam - totalScoreOpp,
           totalTime = finalTime - initialTime) %>%
    arrange(idGame, lineupStint)

indiv_stats <- lineup_stats %>%
    separate_rows(lineup, sep = ", ") %>%
    group_by(namePlayer = lineup, idGame, slugTeam) %>%
    summarise(totalPlusMinus = sum(netScoreTeam),
              totalSecs = sum(totalTime)) %>%
    ungroup() %>%
    arrange(-totalPlusMinus)

indiv_stats

indiv_stats %>%
    group_by(namePlayer) %>%
    summarise(seasonPM = sum(totalPlusMinus),
              seasonSecs = sum(totalSecs)) %>%
    ungroup() %>%
    arrange(-seasonPM) %>%
    mutate(seasonMin = paste0(floor(seasonSecs / 60), ":", str_pad(round(seasonSecs %% 60, 0), side = "left", width = 2, pad = 0))) %>%
    select(-seasonSecs)

lineup_stats %>%
    group_by(lineup) %>%
    summarise(seasonPM = sum(netScoreTeam),
              seasonSecs = sum(totalTime)) %>%
    ungroup() %>%
    arrange(-seasonPM) %>%
    mutate(seasonMin = paste0(floor(seasonSecs / 60), ":", str_pad(round(seasonSecs %% 60, 0), side = "left", width = 2, pad = 0))) %>%
    select(-seasonSecs)

#### Part 3 ####

# lineup_game <- read_csv("LineupGame0107.csv",
#                         col_types = cols(timeQuarter = "c"))
# 
# lineup_stats <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBAblog/master/LineupStatsNBA.csv")

lineup_stats %>%
    filter(str_detect(lineup, "Ben Simmons") &
               str_detect(lineup, "Joel Embiid") &
               str_detect(lineup, "Al Horford")) %>%
    summarise(plusMinus = sum(netScoreTeam))

lineup_stats %>%
    filter(str_detect(lineup, "Giannis Antetokounmpo") &
               !str_detect(lineup, "Lopez|Ilyasova|D.J. Wilson")) %>%
    summarise(plusMinus = sum(netScoreTeam),
              totalTime = sum(totalTime)) %>%
    ungroup() %>%
    mutate(totalTime = paste0(floor(totalTime / 60), ":", str_pad(round(totalTime %% 60, 0), side = "left", width = 2, pad = 0)))

lineup_stats %>%
    filter(str_detect(lineup, "(?=.*Seth Curry)(?=.*Porzingis)(?=.*Doncic)(?=.*Finney-Smith)(?=.*Hardaway Jr.)")) %>%
    summarise(plusMinus = sum(netScoreTeam),
              totalTime = sum(totalTime)) %>%
    ungroup() %>%
    mutate(totalTime = paste0(floor(totalTime / 60), ":", str_pad(round(totalTime %% 60, 0), side = "left", width = 2, pad = 0)))

players_wanted <- c("Seth Curry", "Porzingis", "Doncic", "Finney-Smith", "Hardaway Jr.")
paste(map_chr(players_wanted, ~ glue("(?=.*{.x})")), collapse = "")

function_players <- function(with = NULL, without = NULL){
    lineup_stats %>%
        filter(if (!is.null(with)) str_detect(lineup, paste(map_chr(with, ~ glue("(?=.*{.x})")), collapse = "")) else TRUE) %>%
        filter(if (!is.null(without)) !str_detect(lineup, paste(without, collapse = "|")) else TRUE) %>%
        summarise(games = n_distinct(idGame), 
                  totalTime = sum(totalTime),
                  plusMinus = sum(netScoreTeam)) %>%
        mutate(totalTime = paste0(floor(totalTime / 60), ":", str_pad(round(totalTime %% 60, 0), side = "left", width = 2, pad = 0)),
               with_plr = paste(with, collapse = ", "),
               without_plr = paste(without, collapse = ", ")) %>%
        select(with_plr, without_plr, everything())
}

function_players("Zion Williamson", c("Jaxson Hayes", "Derrick Favors", "Jahlil Okafor", "Nicolo Melli"))

function_players_group <- function(with = NULL, without = NULL){
    lineup_stats %>%
        filter(if (!is.null(with)) str_detect(lineup, paste(map_chr(with, ~ glue("(?=.*{.x})")), collapse = "")) else TRUE) %>%
        filter(if (!is.null(without)) !str_detect(lineup, paste(without, collapse = "|")) else TRUE) %>%
        group_by(lineup) %>%
        summarise(games = n_distinct(idGame), 
                  totalTime = sum(totalTime),
                  plusMinus = sum(netScoreTeam)) %>%
        ungroup() %>%
        arrange(-totalTime) %>%
        mutate(totalTime = paste0(floor(totalTime / 60), ":", str_pad(round(totalTime %% 60, 0), side = "left", width = 2, pad = 0)))
}

function_players_group("Bam Adebayo", c("Kelly Olynyk", "Meyers Leonard", "Chris Silva", "Udonis Haslem", "James Johnson"))

lineup_combinations <- lineup_stats %>%
    select(idGame, slugTeam, lineup, lineupStint, netScoreTeam, totalTime) %>%
    separate_rows(lineup, sep = ", ") %>%
    group_by(idGame, slugTeam, lineupStint, netScoreTeam, totalTime) %>%
    summarise(combinations = combn(lineup, m = 2, simplify = FALSE)) %>%
    ungroup() %>%
    mutate(combinations = map_chr(combinations, ~ paste(sort(.), collapse = ", ")))

lineup_combinations

lineup_combinations %>%
    group_by(combinations) %>%
    summarise(totalTime = sum(totalTime),
              plusMinus = sum(netScoreTeam)) %>%
    ungroup() %>%
    arrange(-totalTime) %>%
    mutate(totalTime = paste0(floor(totalTime / 60), ":", str_pad(round(totalTime %% 60, 0), side = "left", width = 2, pad = 0)))

#### Part 4 ####

game_logs <- game_logs(seasons = 2020)

games <- game_logs %>%
    select(idGame, slugTeam, slugOpponent, locationGame) %>%
    mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
           slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
    select(-c(slugTeam, slugOpponent, locationGame)) %>%
    distinct(idGame, .keep_all = TRUE)

plan(multiprocess)
shots_2020 <- teams_shots(teams = unique(game_logs$nameTeam),
                          seasons = 2020)

# lineup_game <- read_csv("LineupGame0107.csv",
#                         col_types = cols(timeQuarter = "c"))
# 
# lineup_stats <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBAblog/master/LineupStatsNBA.csv")

assist_pct_players <- final_poss_pack %>%
    filter(numberEventMessageType == 1) %>%
    mutate(lineup = ifelse(slugTeamPlayer1 == slugTeamHome, lineupHome, lineupAway)) %>%
    select(idGame, numberEvent, slugTeamPlayer1, scorer = namePlayer1, assist = namePlayer2, lineup) %>%
    mutate(lineup = str_split(lineup, ", ")) %>%
    mutate(not_involved = map2(assist, lineup, ~ setdiff(.y, .x)),
           not_involved = map2(scorer, not_involved, ~ setdiff(.y, .x)),
           not_involved = map_chr(not_involved, ~ paste(sort(.), collapse = ", ")))

assist_pct_players

assist_pct_players <- assist_pct_players %>%
    separate_rows(not_involved, sep = ", ") %>%
    pivot_longer(cols = c("scorer", "assist", "not_involved"),
                 names_to = "participation",
                 values_to = "namePlayer") %>%
    distinct(idGame, numberEvent, slugTeamPlayer1, participation, namePlayer) %>%
    count(namePlayer, participation)

assist_pct_players

assist_pct_players %>%
    pivot_wider(names_from = participation,
                values_from = n) %>%
    mutate(pct_assist = assist / (assist + not_involved)) %>%
    arrange(-pct_assist) %>%
    left_join(lineup_stats %>%
                  separate_rows(lineup, sep = ", ") %>%
                  group_by(namePlayer = lineup) %>%
                  summarise(totalTime = sum(totalTime),
                            games = n_distinct(idGame)) %>%
                  ungroup()) %>%
    filter(games >= 25,
           round((totalTime / games / 60), 1) >= 20) %>%
    select(-c(totalTime, games))

rebound_pct_lineups <- final_poss_pack %>%
    filter(numberEventMessageType == 4 & numberEventActionType == 0) %>%
    pivot_longer(cols = starts_with("lineup"),
                 names_to = "lineupLocation",
                 names_prefix = "lineup",
                 values_to = "lineup") %>%
    select(idGame, numberEvent, slugTeamHome, slugTeamAway, descriptionPlayHome, descriptionPlayVisitor, lineup, lineupLocation)

rebound_pct_lineups

rebound_pct_lineups %>%
    mutate(gotReb = case_when(descriptionPlayHome=="" & lineupLocation == "Home" ~ "team",
                              descriptionPlayVisitor=="" & lineupLocation == "Away" ~ "team",
                              TRUE ~ "opponent")) %>%
    count(lineup, gotReb) %>%
    pivot_wider(names_from = gotReb,
                values_from = n) %>%
    mutate(pct_rebound = team / (team + opponent)) %>%
    arrange(pct_rebound) %>%
    left_join(lineup_stats %>%
                  group_by(lineup) %>%
                  summarise(totalTime = sum(totalTime)) %>%
                  ungroup() %>%
                  select(lineup, totalTime)) %>%
    filter(totalTime >= 9000) %>%
    select(lineup, team, opponent, pct_rebound)

lineup_game %>%
    filter(idGame == 21900466,
           secsPassedGame == 158) %>%
    select(descriptionPlayHome, descriptionPlayVisitor)

pbp_shots <- final_poss_pack %>%
    pivot_longer(cols = starts_with("descriptionPlay"),
                 names_to = "descriptionPlayLocation",
                 values_to = "descriptionPlay",
                 names_prefix = "descriptionPlay") %>%
    filter(numberEventMessageType %in% c(1, 2),
           str_detect(descriptionPlay, "MISS|PTS")) %>%
    mutate(marginBefore = ifelse(slugTeamPlayer1 == slugTeamHome, marginBeforeHome, marginBeforeAway),
           lineupTeam = ifelse(slugTeamPlayer1 == slugTeamHome, lineupHome, lineupAway),
           lineupOpp = ifelse(slugTeamPlayer1 != slugTeamHome, lineupHome, lineupAway)) %>%
    mutate(typeEvent = ifelse(numberEventMessageType == 1, "Made Shot", "Missed Shot")) %>%
    select(idGame, slugTeam = slugTeamPlayer1, numberPeriod, timeQuarter, 
           namePlayer = namePlayer1, namePlayer2, marginBefore, lineupTeam, lineupOpp, descriptionPlay)

pbp_shots

shots_2020$idGame <- as.character(shots_2020$idGame)
shots_2020$numberPeriod <- as.character(shots_2020$numberPeriod)

shots_lineups <- shots_2020 %>%
    mutate(timeQuarter = paste(str_pad(minutesRemaining, 2, side = "left", pad = 0),
                               str_pad(secondsRemaining, 2, side = "left", pad = 0), sep = ":")) %>%
    left_join(pbp_shots) %>%
    distinct(idGame, idEvent, .keep_all = TRUE) %>%
    select(-c(yearSeason, slugSeason, idTeam, idPlayer, typeGrid, minutesRemaining, secondsRemaining, isShotAttempted, isShotMade))

shots_lineups

shots_lineups %>%
    filter(nameTeam == "Miami Heat") %>%
    mutate(withDuncan = ifelse(str_detect(lineupTeam, "Duncan Robinson"), "With", "Without")) %>%
    filter(zoneBasic == "Restricted Area") %>%
    count(typeEvent, withDuncan) %>%
    ungroup() %>%
    pivot_wider(names_from = typeEvent,
                values_from = n) %>%
    janitor::clean_names() %>%
    mutate(total_shots = made_shot + missed_shot,
           pct_restricted = made_shot / total_shots)

shots_players <- shots_lineups %>%
    filter(zoneBasic == "Restricted Area",
           typeEvent == "Made Shot") %>%
    mutate(slugOpp = ifelse(slugTeam == slugTeamHome, slugTeamAway, slugTeamHome)) %>%
    select(idGame, idEvent, slugTeam, slugOpp, lineupOpp)

shots_players

shots_players <- shots_players %>%
    left_join(lineup_stats %>%
                  separate_rows(lineup, sep = ", ") %>%
                  group_by(slugOpp = slugTeam) %>%
                  summarise(every_player = paste(sort(unique(lineup)), collapse = ", "))) %>%
    mutate_at(vars(c(lineupOpp, every_player)), ~ str_split(., ", ")) %>%
    mutate(not_in = map2(lineupOpp, every_player, ~ setdiff(.y, .x)),
           not_in = map_chr(not_in, ~ paste(sort(.), collapse = ", ")),
           lineupOpp = map_chr(lineupOpp, ~ paste(sort(.), collapse = ", "))) %>%
    select(-c(every_player, slugTeam))

shots_players

shots_individual <- shots_players %>%         
    separate_rows(lineupOpp, sep = ", ") %>%
    separate_rows(not_in, sep = ", ") %>%
    pivot_longer(cols = c("lineupOpp", "not_in"),
                 names_to = "inLineup",
                 values_to = "namePlayer") %>%
    distinct(idGame, idEvent, slugOpp, inLineup, namePlayer)

shots_individual <- shots_individual %>%
    mutate(points = 2) %>%
    group_by(namePlayer, slugOpp, inLineup) %>%
    summarise(total = sum(points)) %>%
    ungroup()

shots_individual

shots_individual %>%
    left_join(lineup_stats %>%
                  separate_rows(lineup, sep = ", ") %>%
                  group_by(slugOpp = slugTeam, namePlayer = lineup) %>%
                  summarise(totalTime = sum(totalTime)) %>%
                  ungroup() %>%
                  group_by(slugOpp) %>%
                  mutate(totalTeam = sum(totalTime)) %>%
                  ungroup() %>%
                  mutate(totalTeam = totalTeam / 5)) %>%
    filter(totalTime >= 30000) %>%
    pivot_wider(names_from = inLineup,
                values_from = total) %>%
    mutate(timeOff = totalTeam - totalTime) %>%
    mutate(withPer48 = (lineupOpp * 2880) / totalTime,
           withoutPer48 = (not_in * 2880) / timeOff) %>%
    select(Player = namePlayer, Team = slugOpp, On_Court = withPer48, Off_Court = withoutPer48) %>%
    mutate(Difference = round(Off_Court - On_Court, 1)) %>%
    arrange(-Difference)

#### Part 1.2 ####

# source("https://raw.githubusercontent.com/ramirobentes/NBAblog/master/get%20newpbp.R")

new_pbp %>%
    arrange(idGame) %>%
    filter(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS") | numberEventMessageType == 4) %>%
    select(idGame, numberEventMessageType, numberEventActionType, descriptionPlayHome, descriptionPlayVisitor) %>%
    mutate(descriptionMessage = case_when(numberEventMessageType %in% c(2, 3) ~ "Missed Shot",
                                          numberEventMessageType == 4 ~ "Rebound",
                                          TRUE ~ "Other")) %>%
    count(descriptionMessage)

new_pbp %>%
    arrange(idGame) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(prevMsgType = lag(numberEventMessageType)) %>%
    ungroup() %>%
    filter(numberEventMessageType == 4) %>%  # rebound
    count(prevMsgType)

rebounds_table <- new_pbp %>%
    filter(numberEventMessageType == 4) %>%
    mutate(slugTeamPlayer1 = ifelse(descriptionPlayHome == "", slugTeamAway, slugTeamHome)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(line_miss_rebound = row_number()) %>%
    ungroup()

missed_shots_table <- new_pbp %>%
    filter(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS")) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(line_miss_rebound = row_number()) %>%
    ungroup()

shots_rebounds_table <- missed_shots_table %>%
    bind_rows(rebounds_table) %>%
    arrange(idGame, numberPeriod, line_miss_rebound)

editables_ft_teamreb <- shots_rebounds_table %>%
    filter(lag(numberEventMessageType) == 3 & lag(numberEventActionType) %in% c(11, 13, 14, 16, 18, 19, 25, 26, 27, 28, 29)) %>%
    filter(!is.na(namePlayer1)) %>%
    select(idGame, line_miss_rebound, numberEventMessageType, numberPeriod, timeQuarter) %>%
    left_join(shots_rebounds_table %>%
                  filter(is.na(namePlayer1)) %>%
                  select(idGame, line_miss_rebound, numberEventMessageType, numberPeriod, timeQuarter), 
              by = c("idGame", "numberPeriod", "timeQuarter", "numberEventMessageType")) %>%
    filter(!is.na(line_miss_rebound.y)) %>%
    mutate(line_miss_rebound.Newx = line_miss_rebound.y,
           line_miss_rebound.Newy = line_miss_rebound.x) %>%
    pivot_longer(cols = starts_with("line_miss_rebound.New"),
                 names_to = "columnName",
                 values_to = "line_miss_rebound") %>%
    mutate(new_line_miss_rebound = if_else(columnName == "line_miss_rebound.Newx", line_miss_rebound.x, line_miss_rebound.y)) %>%
    select(idGame, numberEventMessageType, numberPeriod, timeQuarter, line_miss_rebound, new_line_miss_rebound)

shots_rebounds_table_edited <- shots_rebounds_table %>%
    left_join(editables_ft_teamreb) %>%
    mutate(line_miss_rebound = ifelse(is.na(new_line_miss_rebound), line_miss_rebound, new_line_miss_rebound)) %>%
    select(-new_line_miss_rebound) %>%
    arrange(idGame, numberPeriod, line_miss_rebound, numberEvent)

away_play_reb_new <- new_pbp %>%
    filter(numberEventMessageType == 3 & numberEventActionType == 10 & lag(numberEventMessageType == 6) &
               lag(numberEventActionType == 6)) %>%   # free throws 1 of 1, where the previous play was an away from the play foul
    select(idGame, numberPeriod, timeQuarter) %>%
    left_join(new_pbp %>%
                  select(idGame, numberPeriod, timeQuarter, numberEventMessageType, numberEventActionType, numberEvent, slugTeamPlayer1,
                         descriptionPlayHome, descriptionPlayVisitor)) %>%
    group_by(idGame, numberPeriod, timeQuarter) %>%
    # if there's a made field goal (numberEventMessageType == 1) in the same second and the team that comitted the foul (numberEventMessageType == 6 & numberEventActionType == 6) is differente than the one who made the field goal, it means that the free throw rebound is a live ball. If the team that made the shot is the same that comitted the foul, it means that they scored and then, when the ball was out of bounds and ready to be inbounded, comitted an away from the ball foul on the opposing team. These are dead ball situations and we want to keep them.
    mutate(team_made_shot = ifelse(any(numberEventMessageType == 1), slugTeamPlayer1[which(numberEventMessageType == 1)], NA),
           team_foul = ifelse(any(numberEventMessageType == 6 & numberEventActionType == 6), 
                              slugTeamPlayer1[which(numberEventMessageType == 6 & numberEventActionType == 6)], NA)) %>%
    ungroup() %>%
    filter((team_made_shot == team_foul | is.na(team_made_shot)) & numberEventMessageType == 3 & numberEventActionType == 10 &
               (str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS"))) %>%
    select(idGame, numberEvent) %>%
    mutate(away_play = 1)

all_rebounds_new <- shots_rebounds_table_edited %>%
    left_join(away_play_reb_new) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(deadBall = ifelse((lag(numberEventMessageType) == 3 & 
                                  lag(numberEventActionType) %in% c(11, 13, 14, 16, 18, 19, 25, 26, 27, 28, 29)) | 
                                 !is.na(lag(away_play)), 1, 0),
           previous = paste(lag(numberEventMessageType), lag(numberEventActionType), sep =  ), 
           isOffensive = ifelse(numberEventMessageType == 4 & slugTeamPlayer1 == lag(slugTeamPlayer1) & deadBall == 0, 1, NA)) %>%
    ungroup() %>%
    arrange(idGame, numberPeriod, line_miss_rebound, numberEvent) %>%
    # creating a new numberEvent where the rebound will always follow the missed shot
    group_by(idGame, numberPeriod, line_miss_rebound) %>%
    mutate(newNumberEvent = ifelse(numberEvent == min(numberEvent), numberEvent, min(numberEvent) + 1)) %>%
    ungroup()

new_pbp_reb <- new_pbp %>%
    anti_join(all_rebounds_new %>%
                  select(idGame, numberPeriod, numberEvent)) %>%
    bind_rows(all_rebounds_new) %>%
    mutate(newNumberEvent = coalesce(newNumberEvent, numberEvent)) %>%
    arrange(idGame, secsPassedGame, newNumberEvent, line_miss_rebound) %>%
    group_by(idGame) %>%
    mutate(numberEvent = row_number()) %>%
    ungroup() %>%
    select(-c(newNumberEvent, line_miss_rebound))

tips_putbacks <- new_pbp_reb %>%
    select(idGame, numberEvent, numberEventMessageType, timeQuarter, secsPassedGame, namePlayer1, slugTeamPlayer1,
           descriptionPlayHome, descriptionPlayVisitor) %>%
    semi_join(new_pbp_reb %>%
                  # finding rebounds where the previous play was a missed field goal e the next play was either a missed or made field goal by the same team, but the team that grabbed the rebound is not the same of the shots
                  filter(numberEventMessageType == 4 & lag(numberEventMessageType) == 2 & lead(numberEventMessageType) %in% c(1, 2) &
                             lag(slugTeamPlayer1) == lead(slugTeamPlayer1) & lead(slugTeamPlayer1) != slugTeamPlayer1),
              by = c("idGame", "secsPassedGame")) %>%
    select(-c(timeQuarter)) %>%
    filter(numberEventMessageType %in% c(1, 2, 4)) %>%
    # swap numberEvents
    group_by(idGame, secsPassedGame) %>%
    mutate(newNumberEvent = ifelse(numberEvent == min(numberEvent[which(numberEventMessageType == 4)]),
                                   max(numberEvent[which(numberEventMessageType == 4)]), numberEvent),
           newNumberEvent = ifelse(numberEvent == max(numberEvent[which(numberEventMessageType == 4)]),
                                   min(numberEvent[which(numberEventMessageType == 4)]), newNumberEvent)) %>%
    ungroup() %>%
    filter(numberEvent != newNumberEvent)

new_pbp_reb <- new_pbp_reb %>%
    left_join(tips_putbacks %>%
                  select(idGame, numberEvent, newNumberEvent)) %>%
    mutate(numberEvent = ifelse(is.na(newNumberEvent), numberEvent, newNumberEvent)) %>%
    arrange(idGame, numberEvent) %>%
    select(-newNumberEvent) %>%
    replace_na(list(deadBall = 0,
                    away_play = 0))

missed_shots <- new_pbp_reb %>%
    filter(numberEventMessageType %in% c(2, 3)) %>%
    mutate(descriptionPlay = if_else(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor),
           descriptionPlay = str_to_lower(descriptionPlay)) %>%
    filter(str_detect(descriptionPlay, "miss")) %>%
    # removing free throws when the ball goes back to the team that shot it
    filter(!(numberEventMessageType == 3 & numberEventActionType %in% c(11, 13, 14, 16, 18, 19, 25, 26, 27, 28, 29))) %>%
    filter(away_play != 1) %>%
    select(idGame, numberEvent, numberPeriod, slugTeam = slugTeamPlayer1, descriptionPlay)

off_rebounds <- new_pbp_reb %>%
    filter(isOffensive == 1)

missed_shots %>%
    count(idGame, slugTeam) %>%
    left_join(off_rebounds %>%
                  count(idGame, slugTeam = slugTeamPlayer1, name = "off_reb") ) %>%
    replace_na(list(n = 0, off_reb = 0)) %>%
    group_by(slugTeam) %>%
    summarise(across(c(n, off_reb), sum)) %>%
    mutate(oreb_pct = round((off_reb / n) * 100, 1)) %>%
    arrange(-oreb_pct) %>%
    print(as_tibble(.), n = 30)

missed_shots <- new_pbp_reb %>%
    filter(numberEventMessageType %in% c(2, 3)) %>%
    mutate(descriptionPlay = if_else(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor),
           descriptionPlay = str_to_lower(descriptionPlay)) %>%
    filter(str_detect(descriptionPlay, "miss")) %>%
    filter(!str_detect(descriptionPlay, "1 of 2|1 of 3|2 of 3|technical")) %>%
    select(idGame, numberEvent, numberPeriod, slugTeam = slugTeamPlayer1, descriptionPlay)

off_rebounds <- new_pbp_reb %>%
    # keeping away from play, flagrant 2 of 2, flagrant 3 of 2, clear path 2 of 2
    filter(isOffensive == 1 | (deadBall == 1 & previous %in% c("3 10", "3 19", "3 26", "3 29")))

missed_shots %>%
    count(idGame, slugTeam) %>%
    left_join(off_rebounds %>%
                  count(idGame, slugTeam = slugTeamPlayer1, name = "off_reb") ) %>%
    replace_na(list(n = 0, off_reb = 0)) %>%
    group_by(slugTeam) %>%
    summarise(across(c(n, off_reb), sum)) %>%
    mutate(oreb_pct = round((off_reb / n) * 100, 1)) %>%
    arrange(-oreb_pct) %>%
    print(as_tibble(.), n = 30)

#### Part 2.2 ####

logs_teams <- game_logs(2020, result_type = "team")

games <- logs_teams %>%
    select(idGame, slugTeam, slugOpponent, locationGame) %>%
    mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
           slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
    select(-c(slugTeam, slugOpponent, locationGame)) %>%
    distinct(idGame, .keep_all = TRUE)

plan(multiprocess)
play_logs_all <- play_by_play_v2(game_ids = unique(games$idGame))

new_pbp <- play_logs_all %>%
    mutate(numberOriginal = numberEvent) %>%
    distinct(idGame, numberEvent, .keep_all = TRUE) %>%   # remove duplicate events
    mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%                       
    mutate(secsStartQuarter = case_when(                                                                        
        numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
        TRUE ~ 2880 + (numberPeriod - 5) * 300
    )) %>%
    mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 720 - secsLeftQuarter, 300 - secsLeftQuarter),  
           secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
    arrange(idGame, secsPassedGame) %>%
    filter(numberEventMessageType != 18) %>%     # instant replay
    group_by(idGame) %>%
    mutate(numberEvent = row_number()) %>%  # new numberEvent column with events in the right order
    ungroup() %>%
    mutate(shotPtsHome = case_when(
        numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") ~ 1,                               
        numberEventMessageType == 1 & str_detect(descriptionPlayHome, "3PT") ~ 3,                                 
        numberEventMessageType == 1 & !str_detect(descriptionPlayHome, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    mutate(shotPtsAway = case_when(
        numberEventMessageType == 3 & !str_detect(descriptionPlayVisitor, "MISS") ~ 1,
        numberEventMessageType == 1 & str_detect(descriptionPlayVisitor, "3PT") ~ 3,
        numberEventMessageType == 1 & !str_detect(descriptionPlayVisitor, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    group_by(idGame) %>%
    mutate(ptsHome = cumsum(shotPtsHome),
           ptsAway = cumsum(shotPtsAway)) %>%
    ungroup() %>%
    left_join(games %>%
                  select(idGame, slugTeamHome, slugTeamAway)) %>%
    select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1,
           slugTeamPlayer2, slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2, 
           namePlayer3, descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway,
           descriptionPlayNeutral) %>%
    mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
           marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
           timeQuarter = str_pad(timeQuarter, width = 5, pad = 0)) %>%
    replace_na(list(descriptionPlayHome = "",
                    descriptionPlayVisitor = "",
                    descriptionPlayNeutral = ""))

rebounds_table <- new_pbp %>%
    filter(numberEventMessageType == 4) %>%
    mutate(slugTeamPlayer1 = ifelse(descriptionPlayHome == "", slugTeamAway, slugTeamHome)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(line_miss_rebound = row_number()) %>%
    ungroup()

missed_shots_table <- new_pbp %>%
    filter(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS")) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(line_miss_rebound = row_number()) %>%
    ungroup()

shots_rebounds_table <- missed_shots_table %>%
    bind_rows(rebounds_table) %>%
    arrange(idGame, numberPeriod, line_miss_rebound)

editables_ft_teamreb <- shots_rebounds_table %>%
    filter(lag(numberEventMessageType) == 3 & lag(numberEventActionType) %in% c(11, 13, 14, 16, 18, 19, 25, 26, 27, 28, 29)) %>%
    filter(!is.na(namePlayer1)) %>%
    select(idGame, line_miss_rebound, numberEventMessageType, numberPeriod, timeQuarter) %>%
    left_join(shots_rebounds_table %>%
                  filter(is.na(namePlayer1)) %>%
                  select(idGame, line_miss_rebound, numberEventMessageType, numberPeriod, timeQuarter), 
              by = c("idGame", "numberPeriod", "timeQuarter", "numberEventMessageType")) %>%
    filter(!is.na(line_miss_rebound.y)) %>%
    mutate(line_miss_rebound.Newx = line_miss_rebound.y,
           line_miss_rebound.Newy = line_miss_rebound.x) %>%
    pivot_longer(cols = starts_with("line_miss_rebound.New"),
                 names_to = "columnName",
                 values_to = "line_miss_rebound") %>%
    mutate(new_line_miss_rebound = if_else(columnName == "line_miss_rebound.Newx", line_miss_rebound.x, line_miss_rebound.y)) %>%
    select(idGame, numberEventMessageType, numberPeriod, timeQuarter, line_miss_rebound, new_line_miss_rebound)

shots_rebounds_table_edited <- shots_rebounds_table %>%
    left_join(editables_ft_teamreb) %>%
    mutate(line_miss_rebound = ifelse(is.na(new_line_miss_rebound), line_miss_rebound, new_line_miss_rebound)) %>%
    select(-new_line_miss_rebound) %>%
    arrange(idGame, numberPeriod, line_miss_rebound, numberEvent)

away_play_reb_new <- new_pbp %>%
    filter(numberEventMessageType == 3 & numberEventActionType == 10 & lag(numberEventMessageType == 6) &
               lag(numberEventActionType == 6)) %>%   # free throws 1 of 1, where the previous play was an away from the play foul
    select(idGame, numberPeriod, timeQuarter) %>%
    left_join(new_pbp %>%
                  select(idGame, numberPeriod, timeQuarter, numberEventMessageType, numberEventActionType, numberEvent, slugTeamPlayer1,
                         descriptionPlayHome, descriptionPlayVisitor)) %>%
    group_by(idGame, numberPeriod, timeQuarter) %>%
    # if there's a made field goal (numberEventMessageType == 1) in the same second and the team that comitted the foul (numberEventMessageType == 6 & numberEventActionType == 6) is different than the one who made the field goal, it means that the free throw rebound is a live ball. If the team that made the shot is the same that comitted the foul, it means that they scored and then, when the ball was out of bounds and ready to be inbounded, comitted an away from the ball foul on the opposing team. These are dead ball situations and we want to keep them.
    mutate(team_made_shot = ifelse(any(numberEventMessageType == 1), slugTeamPlayer1[which(numberEventMessageType == 1)], NA),
           team_foul = ifelse(any(numberEventMessageType == 6 & numberEventActionType == 6), 
                              slugTeamPlayer1[which(numberEventMessageType == 6 & numberEventActionType == 6)], NA)) %>%
    ungroup() %>%
    filter((team_made_shot == team_foul | is.na(team_made_shot)) & numberEventMessageType == 3 & numberEventActionType == 10 &
               (str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS"))) %>%
    select(idGame, numberEvent) %>%
    mutate(away_play = 1)

all_rebounds_new <- shots_rebounds_table_edited %>%
    left_join(away_play_reb_new) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(deadBall = ifelse((lag(numberEventMessageType) == 3 & 
                                  lag(numberEventActionType) %in% c(11, 13, 14, 16, 18, 19, 25, 26, 27, 28, 29)) | 
                                 !is.na(lag(away_play)), 1, 0),
           previous = paste(lag(numberEventMessageType), lag(numberEventActionType), sep =  ), 
           isOffensive = ifelse(numberEventMessageType == 4 & slugTeamPlayer1 == lag(slugTeamPlayer1) & deadBall == 0, 1, NA)) %>%
    ungroup() %>%
    arrange(idGame, numberPeriod, line_miss_rebound, numberEvent) %>%
    # creating a new numberEvent where the rebound will always follow the missed shot
    group_by(idGame, numberPeriod, line_miss_rebound) %>%
    mutate(newNumberEvent = ifelse(numberEvent == min(numberEvent), numberEvent, min(numberEvent) + 1)) %>%
    ungroup()

new_pbp_reb <- new_pbp %>%
    anti_join(all_rebounds_new %>%
                  select(idGame, numberPeriod, numberEvent)) %>%
    bind_rows(all_rebounds_new) %>%
    mutate(newNumberEvent = coalesce(newNumberEvent, numberEvent)) %>%
    arrange(idGame, secsPassedGame, newNumberEvent, line_miss_rebound) %>%
    group_by(idGame) %>%
    mutate(numberEvent = row_number()) %>%
    ungroup() %>%
    select(-c(newNumberEvent, line_miss_rebound))

tips_putbacks <- new_pbp_reb %>%
    select(idGame, numberEvent, numberEventMessageType, timeQuarter, secsPassedGame, namePlayer1, slugTeamPlayer1,
           descriptionPlayHome, descriptionPlayVisitor) %>%
    semi_join(new_pbp_reb %>%
                  # finding rebounds where the previous play was a missed field goal e the next play was either a missed or made field goal by the same team, but the team that grabbed the rebound is not the same of the shots
                  filter(numberEventMessageType == 4 & lag(numberEventMessageType) == 2 & lead(numberEventMessageType) %in% c(1, 2) &
                             lag(slugTeamPlayer1) == lead(slugTeamPlayer1) & lead(slugTeamPlayer1) != slugTeamPlayer1),
              by = c("idGame", "secsPassedGame")) %>%
    select(-c(timeQuarter)) %>%
    filter(numberEventMessageType %in% c(1, 2, 4)) %>%
    # swap numberEvents
    group_by(idGame, secsPassedGame) %>%
    mutate(newNumberEvent = ifelse(numberEvent == min(numberEvent[which(numberEventMessageType == 4)]),
                                   max(numberEvent[which(numberEventMessageType == 4)]), numberEvent),
           newNumberEvent = ifelse(numberEvent == max(numberEvent[which(numberEventMessageType == 4)]),
                                   min(numberEvent[which(numberEventMessageType == 4)]), newNumberEvent)) %>%
    ungroup() %>%
    filter(numberEvent != newNumberEvent)

new_pbp_reb <- new_pbp_reb %>%
    left_join(tips_putbacks %>%
                  select(idGame, numberEvent, newNumberEvent)) %>%
    mutate(numberEvent = ifelse(is.na(newNumberEvent), numberEvent, newNumberEvent)) %>%
    arrange(idGame, numberEvent) %>%
    select(-newNumberEvent) %>%
    replace_na(list(deadBall = 0,
                    away_play = 0))

pbp_possession <- new_pbp_reb %>%
    replace_na(list(isOffensive = 0)) %>%
    mutate(possession = case_when(numberEventMessageType == 3 & numberEventActionType %in% c(11, 13) ~ 1, # free throws 1 of 2 and 1 of 3
                                  numberEventMessageType %in% c(1, 2) ~ 1,  # field goal attempts
                                  numberEventMessageType == 5 & !numberEventActionType %in% c(17) ~ 1, # turnovers, except lane violation turnover
                                  isOffensive == 1 ~ -1,   # offensive rebound (restarts possession)
                                  numberEventMessageType == 3 & numberEventActionType == 20 ~ -1, # flagrant and 1 (ball goes back to shooting team)
                                  TRUE ~ 0)) %>%
    mutate(slugTeam = case_when(descriptionPlayHome == "" & is.na(slugTeamPlayer1) ~ slugTeamAway,
                                descriptionPlayVisitor == "" & is.na(slugTeamPlayer1) ~ slugTeamHome,
                                TRUE ~ slugTeamPlayer1)) %>%
    mutate(isTeamReb = isOffensive == 1 & !str_detect(descriptionPlayHome, "\\(") & !str_detect(descriptionPlayVisitor, "\\(")) %>%
    filter(numberEventMessageType != 13) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(possession = ifelse(isTeamReb & row_number() == max(row_number()), 0, possession)) %>%  # removing -1 from end of quarter team offensive rebounds
    ungroup()

pbp_possession %>%
    filter(possession %in% c(-1, 1)) %>%
    group_by(idGame, numberPeriod) %>%
    filter(possession == lag(possession) & slugTeam == lag(slugTeam)) %>%
    ungroup() %>%
    select(idGame, descriptionPlayHome, descriptionPlayVisitor, slugTeam, possession)

sequence_row <- pbp_possession %>%
    filter(possession %in% c(-1, 1)) %>%
    group_by(idGame, numberPeriod) %>%
    filter(possession == lag(possession) & slugTeam == lag(slugTeam)) %>%
    ungroup() %>%
    mutate(sequence = row_number()) %>%
    bind_rows(pbp_possession %>%
                  filter(possession %in% c(-1, 1)) %>%
                  group_by(idGame, numberPeriod) %>%
                  filter(possession == lead(possession) & slugTeam == lead(slugTeam)) %>%
                  ungroup() %>%
                  mutate(sequence = row_number())) %>%
    arrange(idGame, numberEvent) %>%
    group_by(idGame, sequence, team_consec = slugTeam) %>%
    summarise(numberEvent = map2(min(numberEvent), max(numberEvent), `:`)) %>%
    ungroup() %>%
    unnest_longer(numberEvent) %>%
    left_join(pbp_possession %>%
                  select(idGame, numberEventMessageType, numberEventActionType, numberEvent, 
                         slugTeam, descriptionPlayHome, descriptionPlayVisitor, possession, 
                         slugTeamPlayer1, slugTeamPlayer2, slugTeamPlayer3))

sequence_row

changes_consecutive <- sequence_row %>%
    # in a jump ball, the team that keeps the ball is defined by the column slugTeamPlayer3. We want to find the team that lost it, in order to add a turnover to them
    mutate(loser_jumpball = case_when(numberEventMessageType == 10 & slugTeamPlayer3 == slugTeamPlayer2 ~ slugTeamPlayer1,
                                      numberEventMessageType == 10 & slugTeamPlayer3 == slugTeamPlayer1 ~ slugTeamPlayer2,
                                      TRUE ~ "")) %>%
    mutate(slugTeamNew = ifelse(loser_jumpball != "", loser_jumpball, slugTeam),
           new_possession = case_when(numberEventMessageType == 5 & numberEventActionType == 0 & possession == 1 & slugTeamNew == team_consec ~ 0, # no turnover
                                      numberEventMessageType == 5 & numberEventActionType == 15 & possession == 1 & slugTeamNew == team_consec ~ 0, # off goaltending on last free throw
                                      numberEventMessageType == 5 & numberEventActionType == 17 & possession == 0 & slugTeamNew != team_consec ~ 1, # lane violation turnover when actually is a offensive 3 seconds violation
                                      numberEventMessageType == 3 & numberEventActionType == 19 & slugTeamNew == team_consec ~ -1, # flagrant and 1 when missing first shot
                                      numberEventMessageType == 5 & numberEventActionType == 18 & possession == 1 & slugTeamNew == team_consec ~ 0,  # jump ball violation turnover
                                      numberEventMessageType == 7 & numberEventActionType == 6 & slugTeamNew == team_consec ~ -1,  # double lane after last free throw and team wins jump ball
                                      numberEventMessageType == 10 & slugTeamNew != team_consec & !(lag(numberEventMessageType) == 7 & lag(numberEventActionType == 6)) ~ 1,  # jump ball except after a double lane
                                      TRUE ~ possession
           )) %>%
    filter(possession != new_possession) %>%  # keeping only the ones where there's a change
    select(idGame, numberEvent, slugTeamNew, new_possession)

changes_consecutive

pbp_possession_edit <- pbp_possession %>%
    left_join(changes_consecutive) %>%
    mutate(slugTeam = ifelse(is.na(slugTeamNew), slugTeam, slugTeamNew),
           possession = ifelse(is.na(new_possession), possession, new_possession)) %>%
    select(-c(slugTeamNew, new_possession))

pbp_possession_edit %>%
    filter(possession %in% c(-1, 1)) %>%
    group_by(idGame, numberPeriod) %>%
    filter(possession == lag(possession) & slugTeam == lag(slugTeam)) %>%
    ungroup()

possessions_quarter <- pbp_possession_edit %>%
    group_by(slugTeam, idGame, numberPeriod) %>%
    summarise(possessions = sum(possession)) %>%
    ungroup()

ratings_quarter <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBAblog/master/ratings-quarter-2020/data.csv")

points_quarter <- pbp_possession_edit %>%
    group_by(idGame, numberPeriod, slugTeamHome, slugTeamAway) %>%
    summarise(pointsHome = sum(shotPtsHome),
              pointsAway = sum(shotPtsAway)) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("points"),
                 names_to = "pointsLocation",
                 values_to = "points",
                 names_prefix = "points") %>%
    mutate(slugTeam = if_else(pointsLocation == "Home", slugTeamHome, slugTeamAway)) %>%
    select(idGame, numberPeriod, slugTeam, points)

comparison <- possessions_quarter %>%
    left_join(logs_teams %>%
                  distinct(idGame, slugTeam, slugOpponent, dateGame)) %>%
    left_join(points_quarter) %>%
    mutate(pts_per100 = round((points / possessions) * 100, 1),
           pts_per100 = as.character(pts_per100)) %>%
    select(idGame, dateGame, slugTeam, slugOpp = slugOpponent, numberPeriod, possessions, points, pts_per100) %>%
    left_join(ratings_quarter %>%
                  select(slugTeam, slugOpp, dateGame, numberPeriod, offrtg)) %>%
    mutate(across(c(pts_per100, offrtg), as.numeric)) %>%
    mutate(possessions_nba = round(points * 100 / offrtg, 1)) %>%
    select(idGame, dateGame, slugTeam, slugOpp, numberPeriod, points, possessions, possessions_nba, pts_per100, pts_per100_nba = offrtg)

comparison

comparison %>%
    count(difference = possessions - possessions_nba) %>%
    mutate(difference_pct = n / sum(n))








