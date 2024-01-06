library(tidyverse)
library(janitor)

options(scipen = 999999)

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





saveRDS(pbp2, "./pbp_working")
saveRDS(win_prob, "./win_prob_working")




games <- "0022300421"


#### play by play ----
scrape_nba_play_by_play <- function(games) {
    
    pbp_df <- data.frame()
    
    for (game_id in games) {
        
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
        
        res <- httr::GET(url = paste0("https://stats.nba.com/stats/playbyplayv2?GameID=",game_id,"&StartPeriod=0&EndPeriod=12"),
                         httr::add_headers(.headers=headers))
        
        json <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)
        
        data <- json$resultSets$rowSet[[1]] %>%
            data.frame(stringsAsFactors = F) %>%
            as_tibble()
        
        json_names <- json$resultSets$headers[[1]]
        
        data <- data %>% set_names(json_names) %>% clean_names()
        
        pbp_df <- bind_rows(pbp_df, data)
        
        print(paste0("Getting Game ", game_id))
        
    }
    
    return(pbp_df)
}

pbp <- scrape_nba_play_by_play(games)


#### win probability ----
scrape_nba_win_probability <- function(games) {
    
    wp_df <- data.frame()
    
    for (game_id in games) {
        
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
        
        res <- httr::GET(url = paste0("https://stats.nba.com/stats/winprobabilitypbp?GameID=",game_id,"&StartPeriod=0&EndPeriod=12&StartRange=0&EndRange=12&RangeType=1&Runtype=each%20second"),
                         httr::add_headers(.headers=headers))
        
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
            select(-dplyr::matches("pts"))
        
        names_md <- names(df_metadata)
        
        data <- data %>%
            set_names(json_names) %>%
            clean_names() %>%
            left_join(df_metadata, by = "game_id") %>%
            select(one_of(names_md), everything()) %>%
            suppressMessages()
        
        wp_df <- bind_rows(wp_df, data)
        
        print(paste0("Getting Game ", game_id))
        
    }
    
    return(wp_df)
    
}

win_prob <- scrape_nba_win_probability(games)


#### fanduel ----
scrape_fanduel <- function(game_id = "0022200334") {
    
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
    
}

fanduel <- scrape_fanduel(game_id = "0022200334")


#### full pbp ----
pbp_desc <- pbp %>%
    select(game_id, eventnum, eventmsgtype, eventmsgactiontype, 
           homedescription, neutraldescription, visitordescription,
           player1_team_abbreviation, player2_team_abbreviation, player3_team_abbreviation)

pbp_full <- win_prob %>%
    left_join(pbp_desc, by =c("game_id" = "game_id", "event_num" = "eventnum")) %>%
    separate(
        "pctimestring",
        into = c("minutes_remaining_quarter", "seconds_remaining_quarter"),
        sep = "\\:",
        remove = F
    ) %>%
    mutate(
        away_poss = if_else(home_poss_ind == 0 & !(eventmsgtype %in% c(13)), 1, 0),
        home_poss = if_else(home_poss_ind == 1 & !(eventmsgtype %in% c(13)), 1, 0),
        away_poss = coalesce(away_poss, 0),
        home_poss = coalesce(home_poss, 0),
        poss_start = case_when(
            eventmsgtype == 4 & is.na(player1_team_abbreviation) & seconds_remaining == 0 ~ 0,
            eventmsgtype %in% c(1, 4, 5, 10, 12) ~ 1,
            eventmsgtype == 3 & eventmsgactiontype %in% c(12, 15) ~ 1,
            eventmsgtype == 6 & eventmsgactiontype %in% c(4, 26) ~ 1,
            eventmsgtype == 7 & eventmsgactiontype %in% c(2) ~ 1,
            TRUE ~ 0),
        away_poss_start = if_else(away_poss == 1 &
                                      poss_start == 1, 1, 0),
        home_poss_start = if_else(home_poss == 1 &
                                      poss_start == 1, 1, 0),
        away_poss_cume = cumsum(away_poss_start),
        home_poss_cume = cumsum(home_poss_start),
        across(c(minutes_remaining_quarter, seconds_remaining_quarter, period,
                 event_num, home_pts, visitor_pts, home_score_margin, seconds_remaining),
               as.numeric),
        minute_game = ((period - 1) * 12) + (12 - minutes_remaining_quarter) + (((
               60 - seconds_remaining_quarter
           ) / 60) - 1),
        time_remaining = 48 - ((period - 1) * 12) - (12 - minutes_remaining_quarter) -
               ((60 - seconds_remaining_quarter) / 60 - 1)
    ) %>%
    distinct() %>%
    mutate(
        seconds_left_quarter = seconds_remaining,
        seconds_start_quarter = case_when(                                                                        
            period %in% c(1:5) ~ (period - 1) * 720,
            TRUE ~ 2880 + (period - 5) * 300),
        seconds_passed_quarter = ifelse(period %in% c(1:4), 720 - seconds_remaining, 300 - seconds_remaining),  
        seconds_passed_game = seconds_passed_quarter + seconds_start_quarter,
    ) %>%
    arrange(game_id, seconds_passed_game) %>%
    group_by(game_id) %>%
    mutate(event_num_new = row_number()) %>%
    ungroup() %>%
    select(game_id:visitor_team_abr, home_pct:home_score_margin,
           period, seconds_remaining, seconds_passed_game,
           event_num_new, eventmsgtype:player3_team_abbreviation,
           home_poss_ind, location, away_poss:home_poss_cume
    ) %>%
    mutate(
        away_shot_points = case_when(
            eventmsgtype == 3 & !str_detect(visitordescription, "MISS") ~ 1,                               
            eventmsgtype == 1 & str_detect(visitordescription, "3PT") ~ 3,                                 
            eventmsgtype == 1 & !str_detect(visitordescription, "3PT") ~ 2,
            TRUE ~ 0),
        home_shot_points = case_when(
            eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ 1,
            eventmsgtype == 1 & str_detect(homedescription, "3PT") ~ 3,
            eventmsgtype == 1 & !str_detect(homedescription, "3PT") ~ 2,
            TRUE ~ 0)
    ) %>%
    group_by(game_id) %>%
    mutate(
        away_points = cumsum(away_shot_points),
        home_points = cumsum(home_shot_points)
    ) %>%
    ungroup() %>%
    mutate(
        away_margin_before = away_points - home_points - away_shot_points + home_shot_points,
        home_margin_before = home_points - away_points - home_shot_points + away_shot_points
    )
    
    

pbp_stats <- pbp_full %>%
    select(game_id:player3_team_abbreviation, away_poss, away_poss_cume, home_poss, home_poss_cume) %>%
    mutate(
        away_fga = if_else(eventmsgtype == 1 & str_detect(visitordescription, "PTS") |
                               eventmsgtype == 2 & str_detect(visitordescription, "MISS"), 1, NA),
        away_fg3a = if_else(eventmsgtype == 1 & str_detect(visitordescription, "PTS") & 
                                str_detect(visitordescription, "3PT") |
                               eventmsgtype == 2 & str_detect(visitordescription, "MISS") &
                                str_detect(visitordescription, "3PT"), 1, NA),
        away_fg2a = if_else(away_fga == 1 & is.na(away_fg3a), 1, NA),
        away_fgm = if_else(eventmsgtype == 1 & str_detect(visitordescription, "PTS"), 1, NA),
        away_fg3m = if_else(eventmsgtype == 1 & str_detect(visitordescription, "PTS") & 
                                str_detect(visitordescription, "3PT"), 1, NA),
        away_fg2m = if_else(away_fgm == 1 & is.na(away_fg3m), 1, 0),
        away_reb = if_else(eventmsgtype == 4 & !is.na(player1_team_abbreviation) &
                               !is.na(visitordescription), 1, NA),
        away_oreb = if_else(away_reb == 1 & lag(away_poss == 1), 1, NA),
        away_dreb = if_else(away_reb == 1 & is.na(away_oreb), 1, NA),
        away_tov = if_else(eventmsgtype == 5 & str_detect(visitordescription, "Turnover"), 1, NA),
        away_fta = if_else(eventmsgtype == 3 & str_detect(visitordescription, "Free Throw"), 1, NA),
        away_ftm = if_else(away_fta == 1 & !str_detect(visitordescription, "MISS"), 1, NA)
    )


sum(pbp_stats$away_fga, na.rm = T)
sum(pbp_stats$away_fgm, na.rm = T)







pbp_wp <- pbp %>%
    left_join(wp_df) %>%
    distinct() %>%
    separate(
        "pctimestring",
        into = c("minutes_quarter", "seconds_quarter"),
        sep = "\\:",
        remove = F
    ) %>%
    mutate(
        across(c(minutes_quarter, seconds_quarter,
                 period, eventnum), as.numeric),
        seconds_remaining_quarter = (minutes_quarter*60) + seconds_quarter,
        seconds_start_quarter = case_when(                                                                        
            period %in% c(1:5) ~ (period - 1) * 720,
            TRUE ~ 2880 + (period - 5) * 300),
        seconds_passed_quarter = ifelse(period %in% c(1:4),
                                        720 - seconds_remaining_quarter,
                                        300 - seconds_remaining_quarter),
        seconds_passed_game = seconds_passed_quarter + seconds_start_quarter,
        seconds_remaining_game = 2880 - seconds_passed_game
    ) %>%
    select(away_abr, home_abr, team_winner,
           game_id:period, seconds_remaining_quarter, seconds_remaining_game,
           homedescription:visitordescription, 
           person1type:player3_team_abbreviation) %>%
    arrange(game_id, desc(seconds_remaining_game)) %>%
    filter(eventmsgtype != 18) %>%
    group_by(game_id) %>%
    mutate(eventnum = row_number()) %>%
    mutate(
        away_shot_points = case_when(
            eventmsgtype == 3 & !str_detect(visitordescription, "MISS") ~ 1,                               
            eventmsgtype == 1 & str_detect(visitordescription, "3PT") ~ 3,                                 
            eventmsgtype == 1 & !str_detect(visitordescription, "3PT") ~ 2,
            TRUE ~ 0),
        home_shot_points = case_when(
            eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ 1,
            eventmsgtype == 1 & str_detect(homedescription, "3PT") ~ 3,
            eventmsgtype == 1 & !str_detect(homedescription, "3PT") ~ 2,
            TRUE ~ 0),
        away_points = cumsum(away_shot_points),
        home_points = cumsum(home_shot_points),
        away_margin = away_points-home_points,
        elapsed_share = (2880 - seconds_remaining_game) / 2880,
        diff_time_ratio = away_margin / (exp(-4 * elapsed_share)),
        possession = case_when(eventmsgtype %in% c(1, 2, 5) ~ 1,
                               eventmsgtype == 3 & eventmsgactiontype %in% c(12, 15) ~ 1,
                               TRUE ~ 0),
        team_possession = case_when(person1type == 4 ~ "home",
                                    person1type == 5 ~ "away"),
        poss_start = case_when(
            eventmsgtype == 4 & is.na(player1_team_abbreviation) &
                seconds_remaining_quarter == 0 ~ 0,
            eventmsgtype %in% c(1, 4, 5, 10, 12) ~ 1,
            eventmsgtype == 3 & eventmsgactiontype %in% c(12, 15) ~ 1,
            eventmsgtype == 6 & eventmsgactiontype %in% c(4, 26) ~ 1,
            eventmsgtype == 7 & eventmsgactiontype %in% c(2) ~ 1,
            TRUE ~ 0),
        poss = if_else(poss_start == 1, team_possession, NA)
    ) %>% ungroup()

pbp_wp <- pbp_wp %>%
    fill(., poss) %>%
    select(game_id, team_winner, seconds_remaining_quarter, seconds_remaining_game,
           away_margin, diff_time_ratio, poss)

train <- pbp_wp %>%
    filter(!game_id %in% games2 & !is.na(poss)) %>%
    mutate(team_winner = if_else(team_winner == 1, "win", "loss"))

test <- pbp_wp %>%
    filter(game_id %in% games2 & !is.na(poss)) %>%
    mutate(team_winner = if_else(team_winner == 1, "win", "loss"))
    

library(caTools)
library(caret)

set.seed(214)
# Model
ctrl <- trainControl(method = "cv", number = 5, verboseIter = T, 
                     classProbs = T, summaryFunction = twoClassSummary)

log_win <- train(as.factor(team_winner) ~., data = train[-1],
                 trControl = ctrl,
                 method = "glm",
                 metric = "ROC",
                 family = "binomial")

win_pred <- predict(log_win, test[-1], type = "prob")

plot_data <- test %>% 
    bind_cols(win_pred) %>%
    filter(game_id == "0022100001") %>% 
    mutate(game_min = (2880 - seconds_remaining_game)/60,
           away_win_prob = ifelse(game_min == max(game_min) & away_margin > 0, 1,
                                  ifelse(game_min == max(game_min) & away_margin < 0, 0, win)))


score_labels <- plot_data[seq(1, nrow(plot_data), 20), ] %>%
    select(game_min, away_margin)


ggplot(data = plot_data, aes(x = game_min, y = away_win_prob)) +
    geom_line() +
    scale_x_continuous(limits = c(0,48), breaks = seq(0, 48, 6)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    geom_text(data = score_labels, aes(x = game_min, y = 0.1, label = away_margin), color = "darkblue") +
    labs(title = "Ugly Win Probability Chart",
         x = "Minutes",
         y = "Away Win Probability") +
    theme_bw()



## fix poss team after makes?







generate_headers <- function() {
    headers <- c(
        `Sec-Fetch-Site` = "same-site",
        `Accept` = "*/*",
        `Origin` = "https://www.nba.com",
        `Sec-Fetch-Dest` = "empty",
        `Accept-Language` = "en-US,en;q=0.9",
        `Sec-Fetch-Mode` = "cors",
        `Host` = "stats.nba.com",
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
        `Referer` = "https://www.nba.com/",
        `Accept-Encoding` = "gzip, deflate, br",
        `Connection` = "keep-alive"
    )
    return(headers)
}

generate_parameters <- function(year, measure_type) {
    year <- (year - 1)
    season <- sprintf("%d-%02d", year, (year + 1) %% 100)
    params <- list(
        `DateFrom` = "",
        `DateTo` = "",
        `GameSegment` = "",
        `ISTRound` = "",
        `LastNGames` = "0",
        `LeagueID` = "00",
        `Location` = "",
        `MeasureType` = measure_type,
        `Month` = "0",
        `OpponentTeamID` = "0",
        `Outcome` = "",
        `PORound` = "0",
        `PaceAdjust` = "N",
        `PerMode` = "Totals",
        `Period` = "0",
        `PlusMinus` = "N",
        `Rank` = "N",
        `Season` = season,
        `SeasonSegment` = "",
        `SeasonType` = "Regular Season",
        `ShotClockRange` = "",
        `VsConference` = "",
        `VsDivision` = ""
    )
    return(params)
}


scrape_nba_schedule <- function(seasons) {
    headers <- generate_headers()
    all_data <- data.frame()
    
    for (year in seasons) {
        params <- generate_parameters(year, "Base")
        
        res <- httr::GET(url = "https://stats.nba.com/stats/teamgamelogs",
                         httr::add_headers(.headers = headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()
        dt <- data.table::rbindlist(data$rowSet) %>% data.table::setnames(column_names)
        
        all_data <- bind_rows(all_data, dt)
        
        print(paste0("Getting Schedule for ", params$Season, " Season"))
    }
    
    statr_schedule <- all_data %>%
        clean_names() %>%
        arrange(game_date, game_id) %>%
        mutate(
            location = if_else(grepl("@", matchup) == T, "away", "home"),
            game_date = as_date(game_date),
            season_year = as.numeric(substr(season_year, 1, 4)) + 1,
            team_name = str_replace_all(team_name,
                                        "Los Angeles Clippers", "LA Clippers")
        ) %>%
        select(season_year, game_id, game_date, location, min,
               team_name, team_abbreviation, pts) %>%
        # filter(location == "away") %>%
        arrange(game_id)
    
    joined_schedule <- statr_schedule %>%
        left_join(statr_schedule %>% select(game_id, opp_name = team_name,
                                            opp_abbreviation = team_abbreviation,
                                            opp_pts = pts),
                  by = "game_id", relationship = "many-to-many") %>%
        filter(team_name != opp_name) %>%
        mutate(team_winner = if_else(pts > opp_pts, 1, 0)) %>%
        arrange(game_id, location)

    return(joined_schedule)
}


df <- scrape_nba_schedule(2022:2024)

wp_df <- df %>%
    filter(location == "away") %>%
    select(game_id, away_abr = team_abbreviation, home_abr = opp_abbreviation,
           team_winner)

games <- unique(df$game_id)
games <- games[1:25]
games2 <- games[1:5]



