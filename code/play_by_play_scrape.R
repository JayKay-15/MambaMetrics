library(tidyverse)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

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

resolve_nba_names <- function(json_names) {
    df_nba_names <- dictionary_nba_names()
    
    json_names %>% map_chr(function(name){
        no_name <- df_nba_names %>%
            filter(nameNBA == name) %>%
            nrow() == 0
        
        if (no_name) {
            glue::glue("Missing {name} in dictionary") %>% cat(fill = T)
            return(name)
            }
            
        df_nba_names %>%
            filter(nameNBA == name) %>%
            pull(nameActual) %>%
            unique() %>% .[[1]]
        })
}

munge_nba_data <- function(data) {
    if (data %>% has_name("datetimeBirth")) {
        data <-
            data %>%
            mutate(datetimeBirth = datetimeBirth %>%  parse_datetime() %>% as.Date())
    }
    
    if (data %>% has_name("timeGame")) {
        data <-
            data %>%
            separate(timeGame, into = c("hours", "minutes"), sep = "\\:") %>%
            mutate_at(c("hours", "minutes"),
                      funs(. %>% as.numeric())) %>%
            mutate(lengthGameMinutes = (hours * 60) + minutes) %>%
            dplyr::select(-one_of(c("hours", "minutes")))
    }
    
    if (data %>% has_name("minutes")) {
        if (data$minutes %>% str_count("\\:") %>% sum(na.rm = T) > 0) {
            data <- data %>%
                separate(minutes, into = c("min", "seconds"), sep = "\\:") %>%
                mutate_at(c("min", "seconds"),
                          funs(. %>% as.numeric())) %>%
                mutate(seconds = seconds / 60,
                       minExact = min + seconds) %>%
                dplyr::select(-c(min, seconds)) %>%
                dplyr::select(one_of(c("idGame", "descriptionComment", "minExact")), everything()) %>%
                suppressWarnings()
        }
    }
    
    char_names <- data %>% dplyr::select(dplyr::matches(char_words())) %>% names()
    
    num_names <-
        data %>% dplyr::select(-one_of(char_names)) %>% names()
    
    data <-
        data %>%
        mutate_at(num_names,
                  funs(. %>% as.numeric())) %>%
        suppressWarnings()
    
    if (data %>% has_name("fga") && data %>%  has_name("fg3a")) {
        data <-
            data %>%
            mutate(fg2m = fgm - fg3m,
                   fg2a = fga - fg3a,
                   pctFG2 = if_else( fg2a > 0 , fg2m / fg2a, 0 )
            )
    }
    
    if (data %>% has_name("slugMatchup")){
        data <-
            data %>%
            mutate(locationGame = case_when(slugMatchup %>% str_detect("@") ~
                                                "A",
                                            T ~ "H")) %>%
            separate(
                slugMatchup,
                into = c("remove", "slugOpponent"),
                sep = c("vs.|@"),
                remove = F
            ) %>%
            dplyr::select(-remove) %>%
            mutate_if(is.character,
                      funs(. %>% str_trim())) %>%
            dplyr::select(slugSeason:outcomeGame, locationGame, everything())
    }
    
    if (data %>% has_name("groupStartPosition")){
        data <-
            data %>%
            mutate(isStarter = !is.na(groupStartPosition)) %>%
            dplyr::select(dplyr::matches("id|name|slug|city|is"), everything())
    }
    
    if (data %>% has_name("dateGame")) {
        if (data$dateGame %>% str_detect("T") %>% sum(na.rm = T) > 0) {
            data <-
                data %>%
                mutate(dateGame = dateGame %>% substr(1,10) %>% lubridate::ymd())
        }
    }
    
    if (data %>% has_name("dateGameLast")) {
        if (data$dateGameLast %>% str_detect("T") %>% sum(na.rm = T) > 0) {
            data <-
                data %>%
                mutate(dateGameLast = dateGameLast %>% substr(1,10) %>% lubridate::ymd())
        } else {
            data <-
                data %>%
                mutate(dateGameLast = dateGameLast %>% lubridate::mdy())
        }
    }
    if (data %>% has_name("slugScore")) {
        data <-
            data %>%
            separate(slugScore, into = c("scoreAway", "scoreHome"), sep = "\\ - ", remove = F) %>%
            mutate_at(c("scoreHome", "scoreAway"),
                      funs(. %>% as.numeric())) %>%
            mutate(slugTeamLeading = case_when(marginScore == 0 ~ "Tie",
                                               marginScore < 0 ~ "Away",
                                               TRUE ~ "Home"))
    }
    
    if (data %>% has_name("nameGroup") && data %>% has_name("nameGroupValue")) {
        data <-
            data %>%
            dplyr::select(-nameGroup) %>%
            dplyr::rename(typeFilter = nameGroupValue)
    }
    
    if (data %>% has_name("timeQuarter")) {
        data <-
            data %>%
            separate(
                "timeQuarter",
                into = c("minuteRemainingQuarter", "secondsRemainingQuarter"),
                sep = "\\:",
                remove = F
            ) %>%
            mutate_at(c("minuteRemainingQuarter", "secondsRemainingQuarter"),
                      funs(. %>% as.numeric())) %>%
            
            mutate(
                minuteGame = ((numberPeriod - 1) * 12) + (12 - minuteRemainingQuarter) + (((
                    60 - secondsRemainingQuarter
                ) / 60) - 1),
                timeRemaining = 48 - ((numberPeriod - 1) * 12) - (12 - minuteRemainingQuarter) -
                    ((60 - secondsRemainingQuarter) / 60 - 1)
            ) %>%
            dplyr::select(idGame:numberPeriod, minuteGame, timeRemaining, everything())
    }
    
    if (data %>% has_name("dateGameLastPlayed")) {
        data <-
            data %>%
            mutate(dateGameLastPlayed = dateGameLastPlayed %>% substr(1,10) %>% lubridate::ymd())
    }
    
    if (data %>% has_name("slugRecordTeam")){
        data <-
            data %>%
            separate(slugRecordTeam,
                     sep = "\\-",
                     into = c("winsTeam", "lossesTeam"),
                     remove = F) %>%
            mutate_at(c("winsTeam", "lossesTeam"),
                      funs(. %>% as.numeric())) %>%
            mutate(countGamesTeam = winsTeam + lossesTeam,
                   pctWinTeam = winsTeam / (countGamesTeam)) %>%
            dplyr::select(idGame, slugRecordTeam,countGamesTeam, pctWinTeam, everything())
    }
    data <-
        data %>%
        mutate_if(is.character,
                  funs(str_trim)) %>%
        mutate_if(is.character,
                  funs(ifelse(. == "", NA, .)))
    
    logicial_names <-
        data %>% dplyr::select(dplyr::matches("^has[A-Z]|^is[A-Z]")) %>% names()
    
    if (logicial_names %>% length() > 0) {
        data <-
            data %>%
            mutate_at(logicial_names,
                      funs(. %>% as.numeric() %>% as.logical()))
    }
    
    id_names <-
        data %>% dplyr::select(dplyr::matches("idTeam", "idPlayer")) %>% names()
    
    if (id_names %>% length() > 0) {
        data <-
            data %>%
            mutate_at(id_names,
                      funs(. %>% as.numeric()))
    }
    
    if (data %>% has_name("teamName") & !data %>% has_name("cityTeam")) {
        data <-
            data %>%
            dplyr::rename(nameTeam = teamName)
    }
    
    if (data %>% has_name("namePlayerOnOff")) {
        assign_nba_players()
        data <-
            data %>%
            dplyr::select(-one_of("namePlayerOnOff")) %>%
            left_join(
                df_dict_nba_players %>% select(idPlayerOnOff = idPlayer,
                                               namePlayerOnOff = namePlayer)
            ) %>%
            dplyr::select(dplyr::matches("type[A-Z]|id[A-Z]|name[A-Z]"),
                          everything()) %>%
            suppressMessages()
        
        data <-
            data %>%
            filter(!namePlayerOnOff %>% is.na())
        
        data <-
            data %>%
            dplyr::rename(typeFilter = namePlayerOnOff)
    }
    
    if (data %>% has_name("fg3a") && data %>% has_name("fg3m")) {
        data <-
            data %>%
            mutate(pctFG3 = fg3m / fg3a)
    }
    
    if (data %>% has_name("namePlayerPasser")) {
        assign_nba_players()
        data <-
            data %>%
            dplyr::select(-one_of("namePlayerPasser")) %>%
            dplyr::rename(idPlayerPasserPassTo = idPlayerPasser) %>%
            left_join(
                df_dict_nba_players %>% select(idPlayerPasserPassTo = idPlayer,
                                               namePlayerPasserPassTo = namePlayer)
            ) %>%
            dplyr::select(dplyr::matches("type[A-Z]|id[A-Z]|name[A-Z]"),
                          everything()) %>%
            suppressMessages()
    }
    
    if (data %>% has_name("namePlayerPassTo")) {
        assign_nba_players()
        data <-
            data %>%
            dplyr::rename(idPlayerPasserPassTo = idPlayerPasser) %>%
            dplyr::select(-one_of("namePlayerPassTo")) %>%
            left_join(
                df_dict_nba_players %>% select(idPlayerPasserPassTo = idPlayer,
                                               namePlayerPasserPassTo = namePlayer)
            ) %>%
            dplyr::select(dplyr::matches("type[A-Z]|id[A-Z]|name[A-Z]"),
                          everything()) %>%
            suppressMessages()
    }
    
    
    data <-
        data %>%
        dplyr::select(-dplyr::matches("CIF")) %>%
        dplyr::select(-one_of(c("orderSort", "idLeague", "namePlayerLastFirst", "dateGameLastPlayed"))) %>%
        suppressWarnings()
    
    if (data %>% has_name("idTeam1")) {
        data <-
            data %>%
            dplyr::rename(idTeam = idTeam1)
    }
    
    if (data %>% has_name("yearSeasonFirst")) {
        data <-
            data %>%
            mutate(yearSeasonFirst = yearSeasonFirst + 1,
                   yearSeasonLast = yearSeasonLast + 1)
    }
    
    data <-
        data %>%
        mutate_at(
            .vars = data %>% select(dplyr::matches("^pts|^blk")) %>% names(),
            funs(. %>% as.numeric())
        )
    
    data <-
        data %>%
        dplyr::select(which(colMeans(is.na(.)) < 1))
    
    char_names <-
        data %>% select_if(is.character) %>% names()
    
    data <-
        data %>%
        dplyr::select(one_of(char_names), everything()) %>%
        dplyr::select(
            dplyr::matches(
                "slugTable|group[A-Z]|type[A-Z]|mode[A-Z]|id[A-Z]|name[A-Z]|year|slug[A-Z]|number[A-Z]|date|outcome|^url|gp|gs|minutes[A-Z]|passes|^fg|^pct[A-Z]"
            ),
            everything()
        )
    
    data <-
        data %>%
        mutate_if(is.numeric,
                  funs(ifelse(. %>% is.nan(), 0 , .)))
    
    data
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
data <- data %>% set_names(actual_names)

# actual_names <- json_names %>% resolve_nba_names()
# data <- data %>% set_names(actual_names) %>% munge_nba_data()

data





        
        
        
        
        
        
        
        
        
        
.get_win_prob <- function(game_id = 21601112,
         period_start = 0,
         period_end = 12,
         return_message = T,
         ...) {
    game_slug <-
        pad_id(game_id)
    json_url <-
        glue(
            "https://stats.nba.com/stats/winprobabilitypbp?SeasonType=&LeagueID=&Season=&IsOnlyCurrentSeason=&PlayerID=&TeamID=&GameID={game_slug}&ContextMeasure=&PlayerPosition=&DateFrom=&DateTo=&GameSegment=&LastNGames=&Location=&Month=&OpponentTeamID=&Outcome=&SeasonSegment=&VSConference=&VSDivision=&RookieYear=&Period=&StartPeriod=0&EndPeriod=12&StartRange=0&EndRange=12&RangeType=1&Runtype=each%20second"
        ) %>%
        as.character()
    
    if (return_message) {
        glue("Getting win probability and play-by-play for game {game_id}") %>% cat(fill = T)
    }
    json <-
        json_url  %>%
        .curl_chinazi()
    
    data <-
        json$resultSets$rowSet[[1]] %>%
        data.frame(stringsAsFactors = F) %>%
        as_tibble()
    
    json_names <-
        json$resultSets$headers[[1]]
    
    actual_names <-
        json_names %>% resolve_nba_names()
    
    df_metadata <-
        json$resultSets$rowSet[[2]] %>%
        data.frame(stringsAsFactors = F) %>%
        as_tibble()
    
    names_md <-
        json$resultSets$headers[[2]]
    
    names_md <- names_md %>% resolve_nba_names()
    
    df_metadata <-
        df_metadata %>%
        set_names(names_md) %>%
        mutate(dateGame = dateGame %>% lubridate::mdy()) %>%
        mutate_at(c("idGame", "idTeamHome", "idTeamAway", "ptsTotalTeamHome", "ptsTotalTeamAway"),
                  funs(. %>% as.integer())
        ) %>%
        select(-dplyr::matches("pts"))
    
    names_md <- names(df_metadata)
    
    
    data <-
        data %>%
        set_names(actual_names) %>%
        munge_nba_data() %>%
        dplyr::rename(slugLocationPossession = locationGame) %>%
        left_join(df_metadata) %>%
        select(one_of(names_md), everything()) %>%
        suppressMessages()
    
    data
}       




