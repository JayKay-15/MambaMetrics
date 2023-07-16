library(tidyverse)
library(lubridate)
library(zoo)
library(nbastatR)   # devtools::install_github("abresler/nbastatR")
library(future)
library(readxl)

team_logs <- game_logs(seasons = 2022, result_types = "team")
source("https://raw.githubusercontent.com/ramirobentes/NBA-in-R/master/data%20add%20pbp.R")

games <- team_logs %>%
    mutate(slugTeamHome = ifelse(locationGame == "H", slugTeam, slugOpponent),
           slugTeamAway = ifelse(locationGame == "A", slugTeam, slugOpponent)) %>%
    distinct(idGame, slugTeamHome, slugTeamAway)

plan(multiprocess)
# play_logs_all <- play_by_play_v2(game_ids = unique(games$idGame))
play_logs_all <- df %>%
    filter(substr(idGame, 1,3) == 221)

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
    select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,                   
           slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, 
           descriptionPlayHome, numberEvent, descriptionPlayVisitor, descriptionPlayNeutral) %>%
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
    mutate(numberEventNew = numberEvent) %>%
    rows_update(event_changes, by = c("idGame", "numberEvent")) %>%
    mutate(numberEvent = numberEventNew) %>%
    select(-numberEventNew) %>%
    arrange(idGame, numberEvent)


subs_made <- new_pbp %>%
    filter(numberEventMessageType == 8) %>%
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
    filter(!(numberEventMessageType == 6 & numberEventActionType %in% c(10, 11, 16, 18, 25))) %>% 
    filter(!(numberEventMessageType == 11 & numberEventActionType %in% c(1, 4))) %>%
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

lineups_quarters %>%
    count(idGame, numberPeriod, slugTeamPlayer) %>%
    filter(n != 5)

missing_players_ot <- data_missing_players %>%
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
                                    secsPassedGame + 0.005, secsPassedGame)) %>%    # Note 4
    mutate(secsPassedGame2 = ifelse(timeQuarter == "00:00" & numberEventMessageType == 3 & numberEventActionType != 10,
                                    secsPassedGame2 - 0.1,
                                    secsPassedGame2)) %>%
    group_by(idGame, numberPeriod, secsPassedGame) %>%
    mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 12, 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 11], collapse = ", "), 
                              as.character(numberEvent)),
           numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType %in% c(14, 15), 
                              paste(numberEvent[numberEventMessageType == 3 & numberEventActionType == 13], collapse = ", "),
                              numberNew)) %>%
    mutate(techs_and1 = sum(numberEventMessageType == 3 & numberEventActionType == 16) > 0 &
               sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0 & 
               sum(numberEventMessageType == 8) > 0) %>%
    mutate(numberNew = ifelse(numberEventMessageType == 3 & numberEventActionType == 10 & techs_and1, 
                              paste(numberEvent[numberEventMessageType == 6 & numberEventActionType == 2 & techs_and1], collapse = ", "), 
                              as.character(numberNew))) %>%
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
    group_by(idGame = as.character(idGame), 
             numberPeriod = as.character(numberPeriod), 
             subOpp, 
             secsPassedGame2 = as.character(secsPassedGame2)) %>%
    mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
    mutate(newptsHome = ifelse(hasFouls > 0,
                               newptsHome[row_number() == max(row_number()[numberEventMessageType == 3])],
                               newptsHome),
           newptsAway = ifelse(hasFouls > 0,
                               newptsAway[row_number() == max(row_number()[numberEventMessageType == 3])],
                               newptsAway)) %>%
    ungroup() %>%
    select(-c(secsPassedGame2, numberNew, techs_and1, hasFouls)) %>%
    mutate(across(starts_with("description"), ~ coalesce(., "")))

# Adding possession when fg attempts, ft 1 of 2 and 1 of 3 and turnovers
possession_initial <- lineup_game_stats %>%
    mutate(possession = case_when(numberEventMessageType %in% c(1, 2, 5) ~ 1,
                                  numberEventMessageType == 3 & numberEventActionType %in% c(12, 15) ~ 1,
                                  TRUE ~ 0),
           team_possession = case_when(is.na(slugTeamPlayer1) & possession == 1 & descriptionPlayHome == "" ~ slugTeamAway,
                                       is.na(slugTeamPlayer1) & possession == 1 & descriptionPlayVisitor == "" ~ slugTeamHome,
                                       TRUE ~ slugTeamPlayer1))

# lane violation when there's no description of turnover (don't shoot last free throw and consider 1st free throw 1 of 1)
lane_description_missing <- possession_initial %>%
    group_by(idGame, secsPassedGame) %>%
    filter(sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0,
           sum(numberEventMessageType == 6 & numberEventActionType == 2) > 0,
           sum(numberEventMessageType == 7 & numberEventActionType == 3) > 0,
           sum(numberEventMessageType == 1) == 0) %>%
    ungroup() %>%
    mutate(possession = ifelse(numberEventMessageType == 3 & numberEventActionType == 10, 1, possession)) %>%
    select(idGame, numberEvent, team_possession, possession)

# adding turnover to opponent of team when the challenger gets the jumpball
jumpball_turnovers <- possession_initial %>%
    group_by(idGame, numberPeriod) %>%
    mutate(prev_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA)),
           next_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA), fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(slugTeamPlayer1 = case_when(numberEventMessageType == 9 & descriptionPlayHome == "" ~ slugTeamAway,
                                       numberEventMessageType == 9 & descriptionPlayVisitor == "" ~ slugTeamHome,
                                       TRUE ~ slugTeamPlayer1)) %>%
    group_by(idGame, secsPassedGame) %>%
    mutate(team_reb_chall = sum(numberEventMessageType == 9 & numberEventActionType == 7) > 0 &
               sum(numberEventMessageType == 4 & is.na(namePlayer1)) > 0) %>% 
    ungroup() %>%
    filter(numberEventMessageType == 10 & numberEventActionType == 1 & 
               lag(numberEventMessageType) == 9 & lag(numberEventActionType) == 7 &
               slugTeamPlayer3 == lag(slugTeamPlayer1) &
               prev_poss == next_poss &
               lag(team_reb_chall) == FALSE) %>%
    mutate(team_possession = ifelse(slugTeamPlayer3 == slugTeamPlayer1, slugTeamPlayer2, slugTeamPlayer1),
           possession = 1) %>%
    select(idGame, numberEvent, team_possession, possession)

# finding when there are consecutive poss and changing the first one to zero
change_consec <- possession_initial %>%
    rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
    rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
    filter(possession == 1 | (numberEventMessageType == 6 & numberEventActionType == 30)) %>%
    group_by(idGame, numberPeriod) %>%
    filter(possession == lead(possession) & team_possession == lead(team_possession)) %>%
    ungroup() %>%
    mutate(possession = 0) %>%
    select(idGame, numberEvent, possession)

# replacing in original data
poss_pack <- possession_initial %>%
    rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
    rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
    rows_update(change_consec, by = c("idGame","numberEvent"))

# identifying start of possession
start_possessions <- poss_pack %>%
    mutate(slugTeamPlayer1 = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                       is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                       TRUE ~ slugTeamPlayer1)) %>% 
    select(idGame, numberPeriod, timeQuarter, numberEventMessageType,  slugTeamPlayer1, 
           descriptionPlayHome, descriptionPlayVisitor, numberEvent) %>%
    filter(numberEventMessageType %in% c(1:5)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(start_poss = case_when(slugTeamPlayer1 != lag(slugTeamPlayer1) & numberEventMessageType == 4 ~ timeQuarter, 
                                  slugTeamPlayer1 != lag(slugTeamPlayer1) & numberEventMessageType != 4 ~ lag(timeQuarter))) %>%
    mutate(start_poss = case_when(is.na(start_poss) & row_number() == 1 & numberPeriod <= 4 ~ "12:00", 
                                  is.na(start_poss) & row_number() == 1 & numberPeriod > 4 ~ "05:00",
                                  TRUE ~ start_poss)) %>%
    ungroup()

# add column with start of possession to the original table and identify heaves
poss_pack_start <- poss_pack %>%
    left_join(start_possessions %>%
                  select(idGame, numberEvent, start_poss)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(start_poss = ifelse(possession == 1, na.locf0(start_poss), start_poss),
           start_poss = ifelse(numberEventMessageType == 4 & numberEventActionType == 1, na.locf0(start_poss), start_poss),
           start_poss = na.locf0(start_poss, fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(heave = ifelse(numberEventMessageType %in% c(2, 5) & possession == 1 & as.integer(str_sub(start_poss, 4, 5)) <= 2 & str_starts(start_poss, "00:") & (lead(shotPtsHome) + lead(shotPtsAway) == 0), 1, 0),
           possession = ifelse(heave == 1, 0, possession))


# adding extra possessions at end of quarter when team gets the ball with more than 2 secs
last_possessions <- poss_pack_start %>%
    group_by(idGame, numberPeriod) %>%
    filter(cumsum(possession) >= max(cumsum(possession)) & possession == 1) %>%
    ungroup()

last_rebounds <- poss_pack_start %>%
    group_by(idGame, numberPeriod) %>%
    filter(numberEventMessageType == 4 & !(lag(numberEventMessageType) == 3 & lag(numberEventActionType) %in% c(18:20, 27:29))) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    mutate(rebound_team = case_when(is.na(slugTeamPlayer1) & descriptionPlayHome == "" ~ slugTeamAway,
                                    is.na(slugTeamPlayer1) & descriptionPlayVisitor == "" ~ slugTeamHome,
                                    TRUE ~ slugTeamPlayer1)) %>%
    select(idGame, numberPeriod, rebound_team, timeQuarterReb = timeQuarter)

missedft_and1_last <- poss_pack_start %>%
    semi_join(last_possessions %>%
                  select(idGame, secsPassedGame)) %>%
    group_by(idGame, secsPassedGame) %>%
    filter(sum(numberEventMessageType == 1) > 0 & sum(numberEventMessageType == 3 & numberEventActionType == 10) > 0 & sum(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS")) > 0) %>%
    ungroup() %>%
    filter(numberEventMessageType == 1) %>%
    select(idGame, numberEvent)

addit_poss_reb <- last_possessions %>%
    left_join(last_rebounds, by = c("idGame", "numberPeriod")) %>%
    left_join(missedft_and1_last %>%
                  mutate(and1_ft = 1)) %>%
    filter(numberEventMessageType == 2 | (numberEventMessageType == 3 & (str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS"))) | and1_ft == 1) %>%
    filter(rebound_team != team_possession,
           as.integer(str_sub(timeQuarterReb, 4, 5)) >= 3) %>%
    transmute(idGame, numberPeriod, start_poss = timeQuarterReb, 
              team_possession = rebound_team, possession)

addit_poss_made <- last_possessions %>%
    filter(numberEventMessageType %in% c(1, 5) | (numberEventMessageType == 3 & !str_detect(descriptionPlayHome, "MISS") & !str_detect(descriptionPlayVisitor, "MISS"))) %>%
    anti_join(missedft_and1_last) %>%
    left_join(team_logs %>%
                  distinct(idGame = as.character(idGame), .keep_all = TRUE) %>%
                  select(idGame, slugTeam, slugOpponent)) %>%
    mutate(team_possession_next = ifelse(team_possession == slugTeam, slugOpponent, slugTeam)) %>%
    filter(as.integer(str_sub(timeQuarter, 4, 5)) >= 3) %>%
    transmute(idGame, numberPeriod, start_poss = timeQuarter, 
              team_possession = team_possession_next, possession)

additional_possessions <- bind_rows(addit_poss_reb,  addit_poss_made) %>%
    mutate(numberEventMessageType = 0,
           numberEventActionType = 0,
           numberOriginal = 0,
           descriptionPlayNeutral = "Last possession of quarter") %>%
    left_join(poss_pack %>%
                  filter(numberEventMessageType == 13) %>%
                  select(-c(numberOriginal, numberEventMessageType, numberEventActionType,
                            descriptionPlayNeutral, possession, team_possession))) %>%
    mutate(numberEvent = numberEvent - 0.5)

final_poss_pack <- poss_pack_start %>%
    bind_rows(additional_possessions) %>%
    arrange(idGame, numberEvent) %>%
    select(-c(subOpp, canSub)) %>%
    mutate(across(starts_with("description"), ~ coalesce(., "")))

# changing possession when it ends in free throw (make it end at foul that led to fts)
fouls_possessions <- final_poss_pack %>%
    filter(numberEventMessageType == 3 & possession == 1) %>%
    select(idGame, secsPassedGame, player_foul = namePlayer1, team_possession, numberEvent_ft = numberEvent) %>%
    left_join(final_poss_pack %>%
                  filter(numberEventMessageType == 6 & !numberEventActionType %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
                  mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
                  select(idGame, secsPassedGame, player_foul = namePlayer2, numberEvent_foul = numberEvent, description)) %>%
    add_count(idGame, secsPassedGame, player_foul, name = "number_plays") %>%
    filter(!(number_plays > 1 & !str_detect(description, " S.FOUL |\\.PN\\)")))

missing_comp <- fouls_possessions %>%
    filter(is.na(numberEvent_foul)) %>%
    left_join(final_poss_pack %>%
                  filter(numberEventMessageType == 6 & !numberEventActionType %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
                  mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
                  select(idGame, secsPassedGame, numberEvent_foul = numberEvent, description),
              by = c("idGame", "secsPassedGame"),
              suffix = c("", "_new")) %>%
    mutate(numberEvent_foul = numberEvent_foul_new,
           description = description_new) %>%
    select(-c(numberEvent_foul_new, description_new))

fouls_possessions <- fouls_possessions %>%
    rows_update(missing_comp, by = c("idGame", "secsPassedGame", "player_foul", "team_possession", "numberEvent_ft", "number_plays")) %>%
    select(idGame, secsPassedGame, team_possession, numberEvent_ft, numberEvent_foul) %>%
    pivot_longer(cols = starts_with("numberEvent"),
                 names_to = "type_play",
                 values_to = "numberEvent",
                 names_prefix = "numberEvent_") %>%
    mutate(possession_players = ifelse(type_play == "foul", 1, 0)) %>%
    select(-type_play)

final_poss_pack <- final_poss_pack %>%
    mutate(possession_players = possession) %>%
    rows_update(fouls_possessions, by = c("idGame", "numberEvent"))

lineup_stats <- final_poss_pack %>%
    select(idGame, numberEvent, slugTeamHome, slugTeamAway, numberPeriod, timeQuarter, secsPassedGame, 
           newptsHome, newptsAway, lineupHome, lineupAway, possession_players, team_possession) %>%
    mutate(possession_home = ifelse(team_possession == slugTeamHome & possession_players == 1, 1, 0),
           possession_away = ifelse(team_possession == slugTeamAway & possession_players == 1, 1, 0)) %>%
    pivot_longer(cols = starts_with("lineup"),
                 names_to = "lineupLocation",
                 names_prefix = "lineup",
                 values_to = "lineup") %>%
    mutate(ptsTeam = ifelse(lineupLocation == "Home", newptsHome, newptsAway),
           ptsOpp = ifelse(lineupLocation == "Away", newptsHome, newptsAway),
           possTeam = ifelse(lineupLocation == "Home", possession_home, possession_away),
           possOpp = ifelse(lineupLocation == "Away", possession_home, possession_away),
           slugTeam = ifelse(lineupLocation == "Home", slugTeamHome, slugTeamAway),
           slugOpp = ifelse(lineupLocation == "Away", slugTeamHome, slugTeamAway)) %>%
    distinct(idGame, slugTeam, slugOpp, numberPeriod, timeQuarter, secsPassedGame, ptsTeam, ptsOpp,
             possTeam, possOpp, lineup, teamLocation = lineupLocation, numberEvent) %>%
    arrange(idGame, numberEvent) %>%
    group_by(idGame, slugTeam) %>%
    mutate(lineupChange = lineup != lag(lineup),
           lineupChange = coalesce(lineupChange, FALSE)) %>%
    group_by(idGame, slugTeam) %>%
    mutate(lineupStint = cumsum(lineupChange)) %>%
    ungroup() %>%
    arrange(idGame, lineupStint, numberEvent) %>%
    group_by(idGame, slugTeam, lineup, lineupStint, numberPeriod) %>%
    summarise(totalPossTeam = sum(possTeam),
              totalPossOpp = sum(possOpp),
              initialScoreTeam = ptsTeam[row_number() == min(row_number())],
              initialScoreOpp = ptsOpp[row_number() == min(row_number())],
              finalScoreTeam = ptsTeam[row_number() == max(row_number())],
              finalScoreOpp =  ptsOpp[row_number() == max(row_number())],
              initialTime = secsPassedGame[row_number() == min(row_number())],
              finalTime = secsPassedGame[row_number() == max(row_number())]) %>%
    ungroup() %>%
    arrange(idGame, lineupStint) %>%
    group_by(idGame, slugTeam) %>%                              
    mutate(finalTime = ifelse(row_number() == max(row_number()), finalTime, lead(initialTime))) %>%  
    ungroup() %>%
    mutate(across(c(contains("Score")), ~ as.numeric(.), .names = "{col}")) %>%
    mutate(totalScoreTeam = finalScoreTeam - initialScoreTeam,
           totalScoreOpp = finalScoreOpp - initialScoreOpp,
           netScoreTeam = totalScoreTeam - totalScoreOpp,
           totalTime = finalTime - initialTime) %>%
    arrange(idGame, lineupStint)

lineup_stats <- lineup_stats %>%
    left_join(lineup_stats %>%
                  filter(lineupStint == 0) %>%
                  distinct(idGame, slugTeam, starters = lineup)) %>%
    mutate(across(c(lineup, starters), ~ str_split(., ", "), .names = "{.col}_list")) %>%
    mutate(reserves = map_int(map2(lineup_list, starters_list, setdiff), length)) %>%
    select(-c(contains("list"), starters))


rm(games, event_changes, play_logs_all, new_pbp, subs_made, others_qtr,
   lineups_quarters, data_missing_players, missing_players_ot, lineup_subs, 
   lineup_game, lineup_game_stats, possession_initial, jumpball_turnovers, lane_description_missing,
   change_consec, poss_pack, start_possessions, poss_pack_start, last_possessions, fouls_possessions,
   missing_comp, last_rebounds, missedft_and1_last, addit_poss_reb, addit_poss_made, additional_possessions)









starters_teams <- final_poss_pack %>%
    filter(numberEventMessageType == 12 & numberPeriod == 1) %>%   # message type 12 corresponds to start of quarter
    distinct(idGame, starters_home = lineupHome, starters_away = lineupAway)

ctg_garbagetime <- final_poss_pack %>%
    left_join(starters_teams) %>%
    mutate(across(c(starts_with("lineup"), starts_with("starters")), ~ str_split(., ", "), .names = "{.col}_list")) %>%
    mutate(total_starters_home = map_int(map2(lineupHome_list, starters_home_list, intersect), length),
           total_starters_away = map_int(map2(lineupAway_list, starters_away_list, intersect), length),
           total_starters = total_starters_home + total_starters_away) %>%
    select(-c(contains("list"), starts_with("starters")))

ctg_garbagetime <- ctg_garbagetime %>%
    mutate(garbage_time = case_when(
        # score differential >= 25 for minutes 12-9:
        secsPassedGame >= 2160 & secsPassedGame < 2340 & abs(marginBeforeHome) >= 25 & total_starters <= 2 & numberPeriod == 4 ~ 1,
        # score differential >= 20 for minutes 9-6:
        secsPassedGame >= 2340 & secsPassedGame < 2520 & abs(marginBeforeHome) >= 20 & total_starters <= 2 & numberPeriod == 4  ~ 1,
        # score differential >= 10 for minutes 6 and under:
        secsPassedGame >= 2520 & abs(marginBeforeHome) >= 10 & total_starters <= 2 & numberPeriod == 4 ~ 1,
        TRUE ~ 0))

ctg_garbagetime <- ctg_garbagetime %>%
    group_by(idGame) %>%
    mutate(max_nongarbage = max(numberEvent[which(garbage_time == 0)])) %>%
    ungroup() %>%
    mutate(garbage_time = ifelse(garbage_time == 1 & numberEvent < max_nongarbage, 0, garbage_time)) %>%
    select(-max_nongarbage)

start_garbagetime <- ctg_garbagetime %>%
    filter(garbage_time == 1 | lead(garbage_time) == 1) %>%
    group_by(idGame, garbage_time) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(total_pts = shotPtsHome + shotPtsAway,
           margin_after = abs(ptsHome - ptsAway),
           would_start = case_when(numberEventMessageType %in% c(1, 3) & secsPassedGame >= 2160 & margin_after >= 25 & total_pts > 0 ~ 1,
                                   numberEventMessageType %in% c(1, 3) & secsPassedGame >= 2340 & secsPassedGame < 2520 & margin_after >= 20  & total_pts > 0 ~ 1,
                                   numberEventMessageType %in% c(1, 3) & secsPassedGame >= 2520 & margin_after >= 10 & total_pts > 0 ~ 1,
                                   TRUE ~ 0)) %>%
    mutate(type_start = case_when(garbage_time == 1 & numberEventMessageType == 8 ~ "sub",
                                  garbage_time == 1 & timeQuarter == "12:00" ~ "start quarter",
                                  would_start == 1 & garbage_time == 0 ~ "shots")) %>%
    group_by(idGame) %>%
    mutate(total_types = sum(!is.na(type_start)),
           has_sub = sum(type_start == "sub")) %>%
    ungroup() %>%
    mutate(type_start = ifelse(is.na(type_start) & garbage_time == 1 & total_types == 0, "time", type_start),
           type_start = ifelse(total_types == 2 & has_sub == 1 & type_start != "sub", NA, type_start)) %>%
    filter(!is.na(type_start)) %>%
    select(idGame, start_garbage = timeQuarter, type_start)










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

# ratings_quarter <- read_csv("https://raw.githubusercontent.com/ramirobentes/NBAblog/master/ratings-quarter-2020/data.csv")

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
