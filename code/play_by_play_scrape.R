library(tidyverse)
library(janitor)

make_url <- function(datatype = NULL,
                     SeasonType = "",
                     LeagueID = "",
                     Season = "",
                     IsOnlyCurrentSeason = "",
                     PlayerID = "",
                     TeamID = "",
                     GameID = "",
                     ContextMeasure = "",
                     PlayerPosition = "",
                     DateFrom = "",
                     DateTo = "",
                     GameSegment = "",
                     LastNGames = "",
                     Location = "",
                     Month = "",
                     OpponentTeamID = "",
                     Outcome = "",
                     SeasonSegment = "",
                     VSConference = "",
                     VSDivision = "",
                     RookieYear = "",
                     Period = "",
                     StartPeriod = "",
                     EndPeriod = "",
                     StartRange = "",
                     EndRange = "",
                     RangeType = "",
                     runType = "") {
    prefix <- paste0("https://stats.nba.com/stats/", datatype, "?")
    info <- list(
        SeasonType = SeasonType,
        LeagueID = LeagueID,
        Season = Season,
        IsOnlyCurrentSeason = IsOnlyCurrentSeason,
        PlayerID = PlayerID,
        TeamID = TeamID,
        GameID = GameID,
        ContextMeasure = ContextMeasure,
        PlayerPosition = PlayerPosition,
        DateFrom = DateFrom,
        DateTo = DateTo,
        GameSegment = GameSegment,
        LastNGames = LastNGames,
        Location = Location,
        Month = Month,
        OpponentTeamID = OpponentTeamID,
        Outcome = Outcome,
        SeasonSegment = SeasonSegment,
        VSConference = VSConference,
        VSDivision = VSDivision,
        RookieYear = RookieYear,
        Period = Period,
        StartPeriod = StartPeriod,
        EndPeriod = EndPeriod,
        StartRange = StartRange,
        EndRange = EndRange,
        RangeType = RangeType,
        runType = runType
    )
    
    info_str <- paste0(names(info), "=", unlist(info), sep = "&", collapse = "")
    str_len <- nchar(info_str)
    info_str <- substr(info_str, 1, str_len - 1)
    url_str <- paste0(prefix, info_str)
    return(url_str)
}


pad_id <- function(id = 21601112) {
    zeros <- 10 - nchar(id)
    
    if (zeros == 0) {
        return(id)
    }
    
    start <- rep("0", times = zeros) %>% str_c(collapse = "")
    
    glue::glue("{start}{id}") %>% as.character()
    
}


game_id <- 22200334
period_start <- 0
period_end <- 12
game_slug <- pad_id(game_id)

json_url <- make_url(datatype = "playbyplayv2",
                     GameID = game_slug,
                     StartPeriod = period_start,
                     EndPeriod = period_end)

headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
)

res <- httr::GET(json_url, httr::add_headers(.headers = headers))

json <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)

json


data <- json$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = F) %>%
    as_tibble()

json_names <- json$resultSets$headers[[1]]

actual_names <- json_names
data <- data %>% set_names(actual_names) %>% clean_names()

# actual_names <- json_names %>% resolve_nba_names()
# data <- data %>% set_names(actual_names) %>% munge_nba_data()

data




# --------------



curl_chinazi <- function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=P&SeasonType=Regular%20Season&Sorter=DATE") {
    
    
    headers = c(
        `Connection` = 'close',
        `Accept` = 'application/json, text/plain, */*',
        `x-nba-stats-token` = 'true',
        `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
        `x-nba-stats-origin` = 'stats',
        `Sec-Fetch-Site` = 'same-origin',
        `Sec-Fetch-Mode` = 'cors',
        `Referer` = 'https://downwiththechinazis.com',
        `Accept-Encoding` = 'gzip, deflate, br',
        `Accept-Language` = 'en-US,en;q=0.9'
    )
    
    headers <- c(
        `Host` = 'stats.nba.com',
        `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
        `Accept` = 'application/json, text/plain, */*',
        `Accept-Language` = 'en-US,en;q=0.5',
        `Accept-Encoding` = 'gzip, deflate, br',
        `x-nba-stats-origin` = 'stats',
        `x-nba-stats-token` = 'true',
        `Connection` = 'keep-alive',
        `Referer` = 'https =//stats.nba.com/',
        `Pragma` = 'no-cache',
        `Cache-Control` = 'no-cache'
    )
    
    res <-
        httr::GET(url,
                  httr::add_headers(.headers = headers))
    
    json <-
        res$content %>%
        rawToChar() %>%
        jsonlite::fromJSON(simplifyVector = T)
    
    json
    
}







get_pbp2 <-
    function(game_id = 21601112,
             period_start = 0,
             period_end = 12,
             return_message = T,
             ...) {
        game_slug <-
            pad_id(game_id)
        json_url <-
            make_url(
                datatype = "playbyplayv2",
                GameID = game_slug,
                StartPeriod = period_start,
                EndPeriod = period_end
            )
        
        if (return_message) {
            glue::glue("Getting play by play for game {game_id}") %>% cat(fill = T)
        }
        json <-
            json_url  %>%
            curl_chinazi()
        
        data <- json$resultSets$rowSet[[1]] %>%
            data.frame(stringsAsFactors = F) %>%
            as_tibble()
        
        json_names <- json$resultSets$headers[[1]]
        
        data <- data %>% set_names(json_names) %>% clean_names()
        
        data
        
    }


pbp2 <- get_pbp2()





get_win_prob <- function(game_id = 21601112,
                         period_start = 0,
                         period_end = 12,
                         return_message = T,
                         ...) {
    game_slug <-
        pad_id(game_id)
    json_url <-
        glue::glue(
            "https://stats.nba.com/stats/winprobabilitypbp?SeasonType=&LeagueID=&Season=&IsOnlyCurrentSeason=&PlayerID=&TeamID=&GameID={game_slug}&ContextMeasure=&PlayerPosition=&DateFrom=&DateTo=&GameSegment=&LastNGames=&Location=&Month=&OpponentTeamID=&Outcome=&SeasonSegment=&VSConference=&VSDivision=&RookieYear=&Period=&StartPeriod=0&EndPeriod=12&StartRange=0&EndRange=12&RangeType=1&Runtype=each%20second"
        ) %>%
        as.character()
    
    if (return_message) {
        glue::glue("Getting win probability and play-by-play for game {game_id}") %>% cat(fill = T)
    }
    json <-
        json_url  %>%
        curl_chinazi()
    
    data <-
        json$resultSets$rowSet[[1]] %>%
        data.frame(stringsAsFactors = F) %>%
        as_tibble()
    
    json_names <-
        json$resultSets$headers[[1]]
    
    df_metadata <-
        json$resultSets$rowSet[[2]] %>%
        data.frame(stringsAsFactors = F) %>%
        as_tibble()
    
    names_md <-
        json$resultSets$headers[[2]]
    
    df_metadata <-
        df_metadata %>%
        set_names(names_md) %>%
        clean_names() %>%
        mutate(game_date = game_date %>% lubridate::mdy()) %>%
        mutate_at(c("home_team_pts", "visitor_team_pts"),
                  funs(. %>% as.integer())
        ) %>%
        select(-dplyr::matches("pts"))
    
    names_md <- names(df_metadata)
    
    data <-
        data %>%
        set_names(json_names) %>%
        clean_names() %>%
        left_join(df_metadata, by = "game_id") %>%
        select(one_of(names_md), everything()) %>%
        suppressMessages()
    
    data
}       


win_prob <- get_win_prob()






get_fanduel <-
    function(game_id = 21700003,
             return_message = TRUE) {
        game_slug <-
            pad_id(game_id)
        json_url <-
            glue::glue(
                "https://stats.nba.com/stats/infographicfanduelplayer/?gameId={game_slug}"
            ) %>%
            as.character()
        
        if (return_message) {
            glue("Getting fanduel summary for game {game_id}") %>% cat(fill = T)
        }
        json <-
            json_url  %>%
            curl_chinazi()
        
        data <-
            json$resultSets$rowSet[[1]] %>%
            data.frame(stringsAsFactors = F) %>%
            as_tibble()
        
        json_names <-
            json$resultSets$headers[[1]]
        
        data <-
            data %>%
            set_names(json_names) %>%
            # munge_nba_data() %>%
            suppressMessages()
        
        data
        
    }



fanduel <- get_fanduel()







pbp_desc <- pbp2 %>%
    select(game_id, eventnum, eventmsgtype, eventmsgactiontype, 
           homedescription, neutraldescription, visitordescription)




pbp_full <- win_prob %>%
    left_join(pbp_desc, by =c("game_id" = "game_id", "event_num" = "eventnum")) %>%
    mutate(away_poss = if_else(home_poss_ind == 0, 1, 0),
           home_poss = if_else(home_poss_ind == 1, 1, 0),
           away_poss = coalesce(away_poss, 0),
           home_poss = coalesce(home_poss, 0),
           away_poss_change = ifelse(away_poss == 1 & lag(away_poss, default = 0) == 0, 1, 0),
           home_poss_change = ifelse(home_poss == 1 & lag(home_poss, default = 0) == 0, 1, 0),
           away_poss_cume = cumsum(away_poss_change),
           home_poss_cume = cumsum(home_poss_change)
    )

















pbp <-
    pbp2 %>%
    separate(
        "pctimestring",
        into = c("minutes_remaining_quarter", "seconds_remaining_quarter"),
        sep = "\\:",
        remove = F
    ) %>%
    mutate_at(
        c("minutes_remaining_quarter", "seconds_remaining_quarter", "period"),
        funs(. %>% as.numeric())
    ) %>%
    mutate(
        minute_game = ((period - 1) * 12) + (12 - minutes_remaining_quarter) + (((
            60 - seconds_remaining_quarter
        ) / 60) - 1),
        time_remaining = 48 - ((period - 1) * 12) - (12 - minutes_remaining_quarter) -
            ((60 - seconds_remaining_quarter) / 60 - 1)
    ) %>%
    dplyr::select(game_id:period,
                  minute_game,
                  time_remaining,
                  everything())




new_pbp <- pbp %>%
    distinct(game_id, eventnum, .keep_all = TRUE) %>%   # remove duplicate events
    mutate(seconds_left_quarter = (minutes_remaining_quarter * 60) + seconds_remaining_quarter) %>%                       
    mutate(seconds_start_quarter = case_when(                                                                        
        period %in% c(1:5) ~ (period - 1) * 720,
        TRUE ~ 2880 + (period - 5) * 300
    )) %>%
    mutate(seconds_passed_quarter = ifelse(period %in% c(1:4), 720 - seconds_left_quarter, 300 - seconds_left_quarter),  
           seconds_passed_game = seconds_passed_quarter + seconds_start_quarter) %>%
    arrange(game_id, seconds_passed_game) %>%
    filter(eventmsgtype != 18) %>%     # instant replay
    group_by(game_id) %>%
    mutate(eventnum = row_number()) %>%  # new eventnum column with events in the right order
    ungroup() %>%
    # select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, namePlayer1, namePlayer2, namePlayer3,                   
    #        slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, 
    #        descriptionPlayHome, numberEvent, descriptionPlayVisitor, descriptionPlayNeutral) %>%
    mutate(away_shot_points = case_when(
        eventmsgtype == 3 & !str_detect(visitordescription, "MISS") ~ 1,                               
        eventmsgtype == 1 & str_detect(visitordescription, "3PT") ~ 3,                                 
        eventmsgtype == 1 & !str_detect(visitordescription, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    mutate(home_shot_points = case_when(
        eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ 1,
        eventmsgtype == 1 & str_detect(homedescription, "3PT") ~ 3,
        eventmsgtype == 1 & !str_detect(homedescription, "3PT") ~ 2,
        TRUE ~ 0
    )) %>%
    group_by(game_id) %>%
    mutate(away_points = cumsum(away_shot_points),
           home_points = cumsum(home_shot_points)) %>%
    ungroup() %>%
    # left_join(games %>%
    #               select(idGame, slugTeamHome, slugTeamAway)) %>%
    # select(idGame, numberOriginal, numberEventMessageType, numberEventActionType, slugTeamHome, slugTeamAway, slugTeamPlayer1,
    #        slugTeamPlayer2, slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, namePlayer1, namePlayer2, 
    #        namePlayer3, descriptionPlayHome, descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway,
    #        descriptionPlayNeutral) %>%
    mutate(away_margin_before = away_points - home_points - away_shot_points + home_shot_points,
           home_margin_before = home_points - away_points - home_shot_points + away_shot_points,
           time_quarter = str_pad(pctimestring, width = 5, pad = 0))





# Adding possession when fg attempts, ft 1 of 2 and 1 of 3 and turnovers
possession_initial <- new_pbp %>%
    mutate(possession = case_when(eventmsgtype %in% c(1, 2, 5) ~ 1,
                                  eventmsgtype == 3 & eventmsgactiontype %in% c(12, 15) ~ 1,
                                  TRUE ~ 0),
           team_possession = case_when(is.na(player1_team_abbreviation) & possession == 1 & visitordescription == "" ~ "away",
                                       is.na(player1_team_abbreviation) & possession == 1 & homedescription == "" ~ "home",
                                       TRUE ~ player1_team_abbreviation))

# lane violation when there's no description of turnover (don't shoot last free throw and consider 1st free throw 1 of 1)
lane_description_missing <- possession_initial %>%
    group_by(game_id, seconds_passed_game) %>%
    filter(sum(eventmsgtype == 3 & eventmsgactiontype == 10) > 0,
           sum(eventmsgtype == 6 & eventmsgactiontype == 2) > 0,
           sum(eventmsgtype == 7 & eventmsgactiontype == 3) > 0,
           sum(eventmsgtype == 1) == 0) %>%
    ungroup() %>%
    mutate(possession = ifelse(eventmsgtype == 3 & eventmsgactiontype == 10, 1, possession)) %>%
    select(game_id, eventnum, team_possession, possession)

# adding turnover to opponent of team when the challenger gets the jumpball
jumpball_turnovers <- possession_initial %>%
    group_by(game_id, period) %>%
    mutate(prev_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA)),
           next_poss = zoo::na.locf0(ifelse(possession == 1, team_possession, NA), fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(player1_team_abbreviation = case_when(eventmsgtype == 9 & homedescription == "" ~ "away",
                                       eventmsgtype == 9 & visitordescription == "" ~ "home",
                                       TRUE ~ player1_team_abbreviation)) %>%
    group_by(game_id, seconds_passed_game) %>%
    mutate(team_reb_chall = sum(eventmsgtype == 9 & eventmsgactiontype == 7) > 0 &
               sum(eventmsgtype == 4 & is.na(player1_name)) > 0) %>% 
    ungroup() %>%
    filter(eventmsgtype == 10 & eventmsgactiontype == 1 & 
               lag(eventmsgtype) == 9 & lag(eventmsgactiontype) == 7 &
               player3_team_abbreviation == lag(player1_team_abbreviation) &
               prev_poss == next_poss &
               lag(team_reb_chall) == FALSE) %>%
    mutate(team_possession = ifelse(player3_team_abbreviation == player1_team_abbreviation, player2_team_abbreviation, player1_team_abbreviation),
           possession = 1) %>%
    select(game_id, eventnum, team_possession, possession)

# finding when there are consecutive poss and changing the first one to zero
change_consec <- possession_initial %>%
    rows_update(lane_description_missing, by = c("idGame", "numberEvent")) %>%
    rows_update(jumpball_turnovers, by = c("idGame", "numberEvent")) %>%
    filter(possession == 1 | (eventmsgtype == 6 & eventmsgactiontype == 30)) %>%
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
    select(idGame, numberPeriod, timeQuarter, eventmsgtype,  slugTeamPlayer1, 
           descriptionPlayHome, descriptionPlayVisitor, numberEvent) %>%
    filter(eventmsgtype %in% c(1:5)) %>%
    group_by(idGame, numberPeriod) %>%
    mutate(start_poss = case_when(slugTeamPlayer1 != lag(slugTeamPlayer1) & eventmsgtype == 4 ~ timeQuarter, 
                                  slugTeamPlayer1 != lag(slugTeamPlayer1) & eventmsgtype != 4 ~ lag(timeQuarter))) %>%
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
           start_poss = ifelse(eventmsgtype == 4 & eventmsgactiontype == 1, na.locf0(start_poss), start_poss),
           start_poss = na.locf0(start_poss, fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(heave = ifelse(eventmsgtype %in% c(2, 5) & possession == 1 & as.integer(str_sub(start_poss, 4, 5)) <= 2 & str_starts(start_poss, "00:") & (lead(shotPtsHome) + lead(shotPtsAway) == 0), 1, 0),
           possession = ifelse(heave == 1, 0, possession))


# adding extra possessions at end of quarter when team gets the ball with more than 2 secs
last_possessions <- poss_pack_start %>%
    group_by(idGame, numberPeriod) %>%
    filter(cumsum(possession) >= max(cumsum(possession)) & possession == 1) %>%
    ungroup()

last_rebounds <- poss_pack_start %>%
    group_by(idGame, numberPeriod) %>%
    filter(eventmsgtype == 4 & !(lag(eventmsgtype) == 3 & lag(eventmsgactiontype) %in% c(18:20, 27:29))) %>%
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
    filter(sum(eventmsgtype == 1) > 0 & sum(eventmsgtype == 3 & eventmsgactiontype == 10) > 0 & sum(str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS")) > 0) %>%
    ungroup() %>%
    filter(eventmsgtype == 1) %>%
    select(idGame, numberEvent)

addit_poss_reb <- last_possessions %>%
    left_join(last_rebounds, by = c("idGame", "numberPeriod")) %>%
    left_join(missedft_and1_last %>%
                  mutate(and1_ft = 1)) %>%
    filter(eventmsgtype == 2 | (eventmsgtype == 3 & (str_detect(descriptionPlayHome, "MISS") | str_detect(descriptionPlayVisitor, "MISS"))) | and1_ft == 1) %>%
    filter(rebound_team != team_possession,
           as.integer(str_sub(timeQuarterReb, 4, 5)) >= 3) %>%
    transmute(idGame, numberPeriod, start_poss = timeQuarterReb, 
              team_possession = rebound_team, possession)

addit_poss_made <- last_possessions %>%
    filter(eventmsgtype %in% c(1, 5) | (eventmsgtype == 3 & !str_detect(descriptionPlayHome, "MISS") & !str_detect(descriptionPlayVisitor, "MISS"))) %>%
    anti_join(missedft_and1_last) %>%
    left_join(team_logs %>%
                  distinct(idGame = as.character(idGame), .keep_all = TRUE) %>%
                  select(idGame, slugTeam, slugOpponent)) %>%
    mutate(team_possession_next = ifelse(team_possession == slugTeam, slugOpponent, slugTeam)) %>%
    filter(as.integer(str_sub(timeQuarter, 4, 5)) >= 3) %>%
    transmute(idGame, numberPeriod, start_poss = timeQuarter, 
              team_possession = team_possession_next, possession)

additional_possessions <- bind_rows(addit_poss_reb,  addit_poss_made) %>%
    mutate(eventmsgtype = 0,
           eventmsgactiontype = 0,
           numberOriginal = 0,
           descriptionPlayNeutral = "Last possession of quarter") %>%
    left_join(poss_pack %>%
                  filter(eventmsgtype == 13) %>%
                  select(-c(numberOriginal, eventmsgtype, eventmsgactiontype,
                            descriptionPlayNeutral, possession, team_possession))) %>%
    mutate(numberEvent = numberEvent - 0.5)

final_poss_pack <- poss_pack_start %>%
    bind_rows(additional_possessions) %>%
    arrange(idGame, numberEvent) %>%
    select(-c(subOpp, canSub)) %>%
    mutate(across(starts_with("description"), ~ coalesce(., "")))

# changing possession when it ends in free throw (make it end at foul that led to fts)
fouls_possessions <- final_poss_pack %>%
    filter(eventmsgtype == 3 & possession == 1) %>%
    select(idGame, secsPassedGame, player_foul = namePlayer1, team_possession, numberEvent_ft = numberEvent) %>%
    left_join(final_poss_pack %>%
                  filter(eventmsgtype == 6 & !eventmsgactiontype %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
                  mutate(description = ifelse(slugTeamPlayer1 == slugTeamHome, descriptionPlayHome, descriptionPlayVisitor)) %>%
                  select(idGame, secsPassedGame, player_foul = namePlayer2, numberEvent_foul = numberEvent, description)) %>%
    add_count(idGame, secsPassedGame, player_foul, name = "number_plays") %>%
    filter(!(number_plays > 1 & !str_detect(description, " S.FOUL |\\.PN\\)")))

missing_comp <- fouls_possessions %>%
    filter(is.na(numberEvent_foul)) %>%
    left_join(final_poss_pack %>%
                  filter(eventmsgtype == 6 & !eventmsgactiontype %in% c(6, 9, 11, 13, 14, 15, 16, 17)) %>%
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










headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
)

res <- httr::GET(url = "https://stats.nba.com/stats/playbyplayv2?GameID=0022200334&StartPeriod=0&EndPeriod=12", httr::add_headers(.headers=headers))

json <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)

data <- json$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = F) %>%
    as_tibble()

json_names <- json$resultSets$headers[[1]]

data <- data %>% set_names(json_names) %>% clean_names()






headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
)

res <- httr::GET(url = "https://stats.nba.com/stats/winprobabilitypbp?GameID=0022200334&StartPeriod=0&EndPeriod=12&StartRange=0&EndRange=12&RangeType=1&Runtype=each%20second", httr::add_headers(.headers=headers))

json <- res$content %>%
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = T)

data <- json$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = F) %>%
    as_tibble()

json_names <- json$resultSets$headers[[1]]

df_metadata <- json$resultSets$rowSet[[2]] %>%
    data.frame(stringsAsFactors = F) %>%
    as_tibble()

names_md <- json$resultSets$headers[[2]]

df_metadata <- df_metadata %>%
    set_names(names_md) %>%
    clean_names() %>%
    mutate(game_date = game_date %>% lubridate::mdy()) %>%
    mutate_at(c("home_team_pts", "visitor_team_pts"),
              funs(. %>% as.integer())) %>%
    select(-dplyr::matches("pts"))

names_md <- names(df_metadata)

data <- data %>%
    set_names(json_names) %>%
    clean_names() %>%
    left_join(df_metadata, by = "game_id") %>%
    select(one_of(names_md), everything()) %>%
    suppressMessages()






headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https =//stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
)

res <- httr::GET(url = "https://stats.nba.com/stats/infographicfanduelplayer/?gameId=0022200334", httr::add_headers(.headers=headers))

json <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)

data <- json$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = F) %>%
    as_tibble()

json_names <- json$resultSets$headers[[1]]

data <- data %>% set_names(json_names) %>% clean_names()






