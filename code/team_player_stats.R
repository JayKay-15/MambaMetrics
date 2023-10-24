#### team & player stats ####

library(tidyverse)
library(data.table)
library(magrittr)
library(nbastatR)
library(gt)
library(gtExtras)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

team_stats <- function(x) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                 "#d9f0d3", "#7fbf7b", "#1b7837")
    
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                     "#e7d4e8", "#af8dc3", "#762a83")
    
    if (x == 'Base') {
        
        ### team base stats ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        ### pull base ----
        params = list(
            `Conference` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `TwoWay` = '0',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        tm_base <- dt %>% select(2,8:27)
        
        ### team base ----
        tm_base %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(2, domain = c(min(tm_base$FGM), mean(tm_base$FGM), max(tm_base$FGM)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(3, domain = c(min(tm_base$FGA), mean(tm_base$FGA), max(tm_base$FGA)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(4, domain = c(min(tm_base$FG_PCT), mean(tm_base$FG_PCT), max(tm_base$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(tm_base$FG3M), mean(tm_base$FG3M), max(tm_base$FG3M)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(tm_base$FG3A), mean(tm_base$FG3A), max(tm_base$FG3A)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(tm_base$FG3_PCT), mean(tm_base$FG3_PCT), max(tm_base$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(tm_base$FTM), mean(tm_base$FTM), max(tm_base$FTM)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(tm_base$FTA), mean(tm_base$FTA), max(tm_base$FTA)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(tm_base$FT_PCT), mean(tm_base$FT_PCT), max(tm_base$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(tm_base$OREB), mean(tm_base$OREB), max(tm_base$OREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(tm_base$DREB), mean(tm_base$DREB), max(tm_base$DREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(tm_base$REB), mean(tm_base$REB), max(tm_base$REB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(tm_base$AST), mean(tm_base$AST), max(tm_base$AST)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(tm_base$TOV), mean(tm_base$TOV), max(tm_base$TOV)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(tm_base$STL), mean(tm_base$STL), max(tm_base$STL)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(tm_base$BLK), mean(tm_base$BLK), max(tm_base$BLK)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(tm_base$BLKA), mean(tm_base$BLKA), max(tm_base$BLKA)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(tm_base$PF), mean(tm_base$PF), max(tm_base$PF)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(tm_base$PFD), mean(tm_base$PFD), max(tm_base$PFD)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(tm_base$PTS), mean(tm_base$PTS), max(tm_base$PTS)),
                         palette = pal_hex, accuracy = 0.1)
        
    } else if (x == "Advanced") {
        
        ### team adv stats ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        ### pull advanced ----
        params = list(
            `Conference` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `TwoWay` = '0',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        tm_adv <- dt %>% select(2,9,11,13,14:22,24,27)
        
        ### team advanced ----
        tm_adv %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(2, domain = c(min(tm_adv$OFF_RATING), mean(tm_adv$OFF_RATING), max(tm_adv$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(3, domain = c(min(tm_adv$DEF_RATING), mean(tm_adv$DEF_RATING), max(tm_adv$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(4, domain = c(min(tm_adv$NET_RATING), 0, max(tm_adv$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(tm_adv$AST_PCT), mean(tm_adv$AST_PCT), max(tm_adv$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(tm_adv$AST_TO), mean(tm_adv$AST_TO), max(tm_adv$AST_TO)),
                         palette = pal_hex, accuracy = 0.01) %>%
            gt_color_box(7, domain = c(min(tm_adv$AST_RATIO), mean(tm_adv$AST_RATIO), max(tm_adv$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(tm_adv$OREB_PCT), mean(tm_adv$OREB_PCT), max(tm_adv$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(tm_adv$DREB_PCT), mean(tm_adv$DREB_PCT), max(tm_adv$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(tm_adv$REB_PCT), mean(tm_adv$REB_PCT), max(tm_adv$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(tm_adv$TM_TOV_PCT), mean(tm_adv$TM_TOV_PCT), max(tm_adv$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(tm_adv$EFG_PCT), mean(tm_adv$EFG_PCT), max(tm_adv$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(tm_adv$TS_PCT), mean(tm_adv$TS_PCT), max(tm_adv$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(tm_adv$PACE), mean(tm_adv$PACE), max(tm_adv$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(tm_adv$PIE), mean(tm_adv$PIE), max(tm_adv$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else if (x == "Scoring") {
        
        ### team scoring stats ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        ### pull scoring ----
        params = list(
            `Conference` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Scoring',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `TwoWay` = '0',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        tm_scr <- dt %>% select(2,8:22)
        
        ### team scoring ----
        tm_scr %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(2, domain = c(min(tm_scr$PCT_FGA_2PT), mean(tm_scr$PCT_FGA_2PT), max(tm_scr$PCT_FGA_2PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(3, domain = c(min(tm_scr$PCT_FGA_3PT), mean(tm_scr$PCT_FGA_3PT), max(tm_scr$PCT_FGA_3PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(4, domain = c(min(tm_scr$PCT_PTS_2PT), mean(tm_scr$PCT_PTS_2PT), max(tm_scr$PCT_PTS_2PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(tm_scr$PCT_PTS_2PT_MR), mean(tm_scr$PCT_PTS_2PT_MR), max(tm_scr$PCT_PTS_2PT_MR)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(tm_scr$PCT_PTS_3PT), mean(tm_scr$PCT_PTS_3PT), max(tm_scr$PCT_PTS_3PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(tm_scr$PCT_PTS_FB), mean(tm_scr$PCT_PTS_FB), max(tm_scr$PCT_PTS_FB)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(tm_scr$PCT_PTS_FT), mean(tm_scr$PCT_PTS_FT), max(tm_scr$PCT_PTS_FT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(tm_scr$PCT_PTS_OFF_TOV), mean(tm_scr$PCT_PTS_OFF_TOV), max(tm_scr$PCT_PTS_OFF_TOV)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(tm_scr$PCT_PTS_PAINT), mean(tm_scr$PCT_PTS_PAINT), max(tm_scr$PCT_PTS_PAINT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(tm_scr$PCT_AST_2PM), mean(tm_scr$PCT_AST_2PM), max(tm_scr$PCT_AST_2PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(tm_scr$PCT_UAST_2PM), mean(tm_scr$PCT_UAST_2PM), max(tm_scr$PCT_UAST_2PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(tm_scr$PCT_AST_3PM), mean(tm_scr$PCT_AST_3PM), max(tm_scr$PCT_AST_3PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(tm_scr$PCT_UAST_3PM), mean(tm_scr$PCT_UAST_3PM), max(tm_scr$PCT_UAST_3PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(tm_scr$PCT_AST_FGM), mean(tm_scr$PCT_AST_FGM), max(tm_scr$PCT_AST_FGM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(tm_scr$PCT_UAST_FGM), mean(tm_scr$PCT_UAST_FGM), max(tm_scr$PCT_UAST_FGM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1)
        
    } else if (x == "Four Factors") {
        
        ### pull four factors ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `Conference` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Four Factors',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `TwoWay` = '0',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        tm_ff <- dt %>% select(2,8:15)
        
        ### team four factors ----
        tm_ff %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(2, domain = c(min(tm_ff$EFG_PCT), mean(tm_ff$EFG_PCT), max(tm_ff$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(3, domain = c(min(tm_ff$FTA_RATE), mean(tm_ff$FTA_RATE), max(tm_ff$FTA_RATE)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(4, domain = c(min(tm_ff$TM_TOV_PCT), mean(tm_ff$TM_TOV_PCT), max(tm_ff$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(tm_ff$OREB_PCT), mean(tm_ff$OREB_PCT), max(tm_ff$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(tm_ff$OPP_EFG_PCT), mean(tm_ff$OPP_EFG_PCT), max(tm_ff$OPP_EFG_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(tm_ff$OPP_FTA_RATE), mean(tm_ff$OPP_FTA_RATE), max(tm_ff$OPP_FTA_RATE)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(tm_ff$OPP_TOV_PCT), mean(tm_ff$OPP_TOV_PCT), max(tm_ff$OPP_TOV_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(tm_ff$OPP_OREB_PCT), mean(tm_ff$OPP_OREB_PCT), max(tm_ff$OPP_OREB_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1)
        
    } else if (x == "Opponent") {
        
        ### pull opponent ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `Conference` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Opponent',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `TwoWay` = '0',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        tm_opp <- dt %>% select(2,8:28)
        
        ### team opponent ----
        tm_opp %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(2, domain = c(min(tm_opp$OPP_FGM), mean(tm_opp$OPP_FGM), max(tm_opp$OPP_FGM)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(3, domain = c(min(tm_opp$OPP_FGA), mean(tm_opp$OPP_FGA), max(tm_opp$OPP_FGA)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(4, domain = c(min(tm_opp$OPP_FG_PCT), mean(tm_opp$OPP_FG_PCT), max(tm_opp$OPP_FG_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(tm_opp$OPP_FG3M), mean(tm_opp$OPP_FG3M), max(tm_opp$OPP_FG3M)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(tm_opp$OPP_FG3A), mean(tm_opp$OPP_FG3A), max(tm_opp$OPP_FG3A)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(tm_opp$OPP_FG3_PCT), mean(tm_opp$OPP_FG3_PCT), max(tm_opp$OPP_FG3_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(tm_opp$OPP_FTM), mean(tm_opp$OPP_FTM), max(tm_opp$OPP_FTM)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(tm_opp$OPP_FTA), mean(tm_opp$OPP_FTA), max(tm_opp$OPP_FTA)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(tm_opp$OPP_FT_PCT), mean(tm_opp$OPP_FT_PCT), max(tm_opp$OPP_FT_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(tm_opp$OPP_OREB), mean(tm_opp$OPP_OREB), max(tm_opp$OPP_OREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(tm_opp$OPP_DREB), mean(tm_opp$OPP_DREB), max(tm_opp$OPP_DREB)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(tm_opp$OPP_REB), mean(tm_opp$OPP_REB), max(tm_opp$OPP_REB)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(tm_opp$OPP_AST), mean(tm_opp$OPP_AST), max(tm_opp$OPP_AST)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(tm_opp$OPP_TOV), mean(tm_opp$OPP_TOV), max(tm_opp$OPP_TOV)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(tm_opp$OPP_STL), mean(tm_opp$OPP_STL), max(tm_opp$OPP_STL)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(tm_opp$OPP_BLK), mean(tm_opp$OPP_BLK), max(tm_opp$OPP_BLK)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(tm_opp$OPP_BLKA), mean(tm_opp$OPP_BLKA), max(tm_opp$OPP_BLKA)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(tm_opp$OPP_PF), mean(tm_opp$OPP_PF), max(tm_opp$OPP_PF)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(tm_opp$OPP_PFD), mean(tm_opp$OPP_PFD), max(tm_opp$OPP_PFD)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(tm_opp$OPP_PTS), mean(tm_opp$OPP_PTS), max(tm_opp$OPP_PTS)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(22, domain = c(min(tm_opp$PLUS_MINUS), mean(tm_opp$PLUS_MINUS), max(tm_opp$PLUS_MINUS)),
                         palette = pal_hex, accuracy = 0.1)
        
    } else if (x == "Defense") {
        
        ### pull defense ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `Conference` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Defense',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `TwoWay` = '0',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashteamstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        tm_def <- dt %>% select(2,8:16)
        
        ### team defense ----
        tm_def %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(2, domain = c(min(tm_def$DEF_RATING), mean(tm_def$DEF_RATING), max(tm_def$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(3, domain = c(min(tm_def$DREB), mean(tm_def$DREB), max(tm_def$DREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(4, domain = c(min(tm_def$DREB_PCT), mean(tm_def$DREB_PCT), max(tm_def$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(tm_def$STL), mean(tm_def$STL), max(tm_def$STL)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(tm_def$BLK), mean(tm_def$BLK), max(tm_def$BLK)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(tm_def$OPP_PTS_OFF_TOV), mean(tm_def$OPP_PTS_OFF_TOV), max(tm_def$OPP_PTS_OFF_TOV)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(tm_def$OPP_PTS_2ND_CHANCE), mean(tm_def$OPP_PTS_2ND_CHANCE), max(tm_def$OPP_PTS_2ND_CHANCE)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(tm_def$OPP_PTS_FB), mean(tm_def$OPP_PTS_FB), max(tm_def$OPP_PTS_FB)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(tm_def$OPP_PTS_PAINT), mean(tm_def$OPP_PTS_PAINT), max(tm_def$OPP_PTS_PAINT)),
                         palette = pal_hex_rev, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
}
player_stats <- function(x) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### player stats base ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `College` = '',
            `Conference` = '',
            `Country` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `DraftPick` = '',
            `DraftYear` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `VsConference` = '',
            `VsDivision` = '',
            `Weight` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayerstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        pl_base <- dt %>% select(2,5,7,11:32,34,35)
        
        pl_base_gt <- pl_base %>% filter(GP >= 15)
        
        pl_base_gt %>%
            arrange(desc(PTS)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(5, domain = c(min(pl_base_gt$FGM), mean(pl_base_gt$FGM), max(pl_base_gt$FGM)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(pl_base_gt$FGA), mean(pl_base_gt$FGA), max(pl_base_gt$FGA)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(pl_base_gt$FG_PCT), mean(pl_base_gt$FG_PCT), max(pl_base_gt$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(pl_base_gt$FG3M), mean(pl_base_gt$FG3M), max(pl_base_gt$FG3M)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(pl_base_gt$FG3A), mean(pl_base_gt$FG3A), max(pl_base_gt$FG3A)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(pl_base_gt$FG3_PCT), mean(pl_base_gt$FG3_PCT), max(pl_base_gt$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(pl_base_gt$FTM), mean(pl_base_gt$FTM), max(pl_base_gt$FTM)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(pl_base_gt$FTA), mean(pl_base_gt$FTA), max(pl_base_gt$FTA)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(pl_base_gt$FT_PCT), mean(pl_base_gt$FT_PCT), max(pl_base_gt$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(pl_base_gt$OREB), mean(pl_base_gt$OREB), max(pl_base_gt$OREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(pl_base_gt$DREB), mean(pl_base_gt$DREB), max(pl_base_gt$DREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(pl_base_gt$REB), mean(pl_base_gt$REB), max(pl_base_gt$REB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(pl_base_gt$AST), mean(pl_base_gt$AST), max(pl_base_gt$AST)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(pl_base_gt$TOV), mean(pl_base_gt$TOV), max(pl_base_gt$TOV)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(pl_base_gt$STL), mean(pl_base_gt$STL), max(pl_base_gt$STL)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(pl_base_gt$BLK), mean(pl_base_gt$BLK), max(pl_base_gt$BLK)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(pl_base_gt$BLKA), mean(pl_base_gt$BLKA), max(pl_base_gt$BLKA)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(22, domain = c(min(pl_base_gt$PF), mean(pl_base_gt$PF), max(pl_base_gt$PF)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(23, domain = c(min(pl_base_gt$PFD), mean(pl_base_gt$PFD), max(pl_base_gt$PFD)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(24, domain = c(min(pl_base_gt$PTS), mean(pl_base_gt$PTS), max(pl_base_gt$PTS)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(25, domain = c(min(pl_base_gt$PLUS_MINUS), mean(pl_base_gt$PLUS_MINUS), max(pl_base_gt$PLUS_MINUS)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(26, domain = c(min(pl_base_gt$DD2), mean(pl_base_gt$DD2), max(pl_base_gt$DD2)),
                         palette = pal_hex) %>%
            gt_color_box(27, domain = c(min(pl_base_gt$TD3), mean(pl_base_gt$TD3), max(pl_base_gt$TD3)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### player stats advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `College` = '',
            `Conference` = '',
            `Country` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `DraftPick` = '',
            `DraftYear` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `VsConference` = '',
            `VsDivision` = '',
            `Weight` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayerstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        pl_adv <- dt %>% select(2,5,7,11,13,16,19,21:27,29:31,34,37,38)
        
        pl_adv_gt <- pl_adv %>% filter(GP >= 15)
        
        pl_adv_gt %>%
            arrange(desc(POSS)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(5, domain = c(min(pl_adv_gt$OFF_RATING),median(pl_adv_gt$OFF_RATING),max(pl_adv_gt$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(pl_adv_gt$DEF_RATING), median(pl_adv_gt$DEF_RATING),max(pl_adv_gt$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(pl_adv_gt$NET_RATING),median(pl_adv_gt$NET_RATING),max(pl_adv_gt$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(pl_adv_gt$AST_PCT),median(pl_adv_gt$AST_PCT),max(pl_adv_gt$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(pl_adv_gt$AST_TO),median(pl_adv_gt$AST_TO),max(pl_adv_gt$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(pl_adv_gt$AST_RATIO),median(pl_adv_gt$AST_RATIO),max(pl_adv_gt$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(pl_adv_gt$OREB_PCT),median(pl_adv_gt$OREB_PCT),max(pl_adv_gt$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(pl_adv_gt$DREB_PCT),median(pl_adv_gt$DREB_PCT),max(pl_adv_gt$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(pl_adv_gt$REB_PCT),median(pl_adv_gt$REB_PCT),max(pl_adv_gt$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(pl_adv_gt$TM_TOV_PCT),median(pl_adv_gt$TM_TOV_PCT),max(pl_adv_gt$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(pl_adv_gt$EFG_PCT),median(pl_adv_gt$EFG_PCT),max(pl_adv_gt$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(pl_adv_gt$TS_PCT),median(pl_adv_gt$TS_PCT),max(pl_adv_gt$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(pl_adv_gt$USG_PCT),median(pl_adv_gt$USG_PCT),max(pl_adv_gt$USG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(pl_adv_gt$PACE),median(pl_adv_gt$PACE),max(pl_adv_gt$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(pl_adv_gt$PIE),median(pl_adv_gt$PIE),max(pl_adv_gt$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(pl_adv_gt$POSS),median(pl_adv_gt$POSS),max(pl_adv_gt$POSS)),
                         palette = pal_hex)
        
    } else if (x == "Scoring") {
        
        ### player stats scoring ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `College` = '',
            `Conference` = '',
            `Country` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `DraftPick` = '',
            `DraftYear` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Scoring',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `VsConference` = '',
            `VsDivision` = '',
            `Weight` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayerstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        pl_scr <- dt %>% select(2,5,7,11,12:26)
        
        pl_scr_gt <- pl_scr %>% filter(GP >= 15)
        
        pl_scr_gt %>%
            arrange(desc(GP)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(5, domain = c(min(pl_scr_gt$PCT_FGA_2PT), mean(pl_scr_gt$PCT_FGA_2PT), max(pl_scr_gt$PCT_FGA_2PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(pl_scr_gt$PCT_FGA_3PT), mean(pl_scr_gt$PCT_FGA_3PT), max(pl_scr_gt$PCT_FGA_3PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(pl_scr_gt$PCT_PTS_2PT), mean(pl_scr_gt$PCT_PTS_2PT), max(pl_scr_gt$PCT_PTS_2PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(pl_scr_gt$PCT_PTS_2PT_MR), mean(pl_scr_gt$PCT_PTS_2PT_MR), max(pl_scr_gt$PCT_PTS_2PT_MR)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(pl_scr_gt$PCT_PTS_3PT), mean(pl_scr_gt$PCT_PTS_3PT), max(pl_scr_gt$PCT_PTS_3PT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(pl_scr_gt$PCT_PTS_FB), mean(pl_scr_gt$PCT_PTS_FB), max(pl_scr_gt$PCT_PTS_FB)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(pl_scr_gt$PCT_PTS_FT), mean(pl_scr_gt$PCT_PTS_FT), max(pl_scr_gt$PCT_PTS_FT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(pl_scr_gt$PCT_PTS_OFF_TOV), mean(pl_scr_gt$PCT_PTS_OFF_TOV), max(pl_scr_gt$PCT_PTS_OFF_TOV)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(pl_scr_gt$PCT_PTS_PAINT), mean(pl_scr_gt$PCT_PTS_PAINT), max(pl_scr_gt$PCT_PTS_PAINT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(pl_scr_gt$PCT_AST_2PM), mean(pl_scr_gt$PCT_AST_2PM), max(pl_scr_gt$PCT_AST_2PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(pl_scr_gt$PCT_UAST_2PM), mean(pl_scr_gt$PCT_UAST_2PM), max(pl_scr_gt$PCT_UAST_2PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(pl_scr_gt$PCT_AST_3PM), mean(pl_scr_gt$PCT_AST_3PM), max(pl_scr_gt$PCT_AST_3PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(pl_scr_gt$PCT_UAST_3PM), mean(pl_scr_gt$PCT_UAST_3PM), max(pl_scr_gt$PCT_UAST_3PM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(pl_scr_gt$PCT_AST_FGM), mean(pl_scr_gt$PCT_AST_FGM), max(pl_scr_gt$PCT_AST_FGM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(pl_scr_gt$PCT_UAST_FGM), mean(pl_scr_gt$PCT_UAST_FGM), max(pl_scr_gt$PCT_UAST_FGM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1)
        
    } else if (x == "Usage") {
        
        ### player stats usage ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `College` = '',
            `Conference` = '',
            `Country` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `DraftPick` = '',
            `DraftYear` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Usage',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `VsConference` = '',
            `VsDivision` = '',
            `Weight` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayerstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        pl_usg <- dt %>% select(2,5,7,11,12:29)
        
        pl_usg_gt <- pl_usg %>% filter(GP >= 15)
        
        pl_usg_gt %>%
            arrange(desc(USG_PCT)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(5, domain = c(min(pl_usg_gt$USG_PCT), mean(pl_usg_gt$USG_PCT), max(pl_usg_gt$USG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(pl_usg_gt$PCT_FGM), mean(pl_usg_gt$PCT_FGM), max(pl_usg_gt$PCT_FGM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(pl_usg_gt$PCT_FGA), mean(pl_usg_gt$PCT_FGA), max(pl_usg_gt$PCT_FGA)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(pl_usg_gt$PCT_FG3M), mean(pl_usg_gt$PCT_FG3M), max(pl_usg_gt$PCT_FG3M)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(pl_usg_gt$PCT_FG3A), mean(pl_usg_gt$PCT_FG3A), max(pl_usg_gt$PCT_FG3A)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(pl_usg_gt$PCT_FTM), mean(pl_usg_gt$PCT_FTM), max(pl_usg_gt$PCT_FTM)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(pl_usg_gt$PCT_FTA), mean(pl_usg_gt$PCT_FTA), max(pl_usg_gt$PCT_FTA)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(pl_usg_gt$PCT_OREB), mean(pl_usg_gt$PCT_OREB), max(pl_usg_gt$PCT_OREB)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(pl_usg_gt$PCT_DREB), mean(pl_usg_gt$PCT_DREB), max(pl_usg_gt$PCT_DREB)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(pl_usg_gt$PCT_REB), mean(pl_usg_gt$PCT_REB), max(pl_usg_gt$PCT_REB)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(pl_usg_gt$PCT_AST), mean(pl_usg_gt$PCT_AST), max(pl_usg_gt$PCT_AST)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(pl_usg_gt$PCT_TOV), mean(pl_usg_gt$PCT_TOV), max(pl_usg_gt$PCT_TOV)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(pl_usg_gt$PCT_STL), mean(pl_usg_gt$PCT_STL), max(pl_usg_gt$PCT_STL)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(pl_usg_gt$PCT_BLK), mean(pl_usg_gt$PCT_BLK), max(pl_usg_gt$PCT_BLK)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(pl_usg_gt$PCT_BLKA), mean(pl_usg_gt$PCT_BLKA), max(pl_usg_gt$PCT_BLKA)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(pl_usg_gt$PCT_PF), mean(pl_usg_gt$PCT_PF), max(pl_usg_gt$PCT_PF)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(pl_usg_gt$PCT_PFD), mean(pl_usg_gt$PCT_PFD), max(pl_usg_gt$PCT_PFD)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(22, domain = c(min(pl_usg_gt$PCT_PTS), mean(pl_usg_gt$PCT_PTS), max(pl_usg_gt$PCT_PTS)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1)
        
    } else if (x == "Opponent") {
        
        ### player stats opponent ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `College` = '',
            `Conference` = '',
            `Country` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `DraftPick` = '',
            `DraftYear` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Opponent',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Per100Possessions',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `VsConference` = '',
            `VsDivision` = '',
            `Weight` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leagueplayerondetails', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        pl_opp <- dt %>% select(6,3,8,13:33)
        
        pl_opp_gt <- pl_opp %>% filter(GP >= 15)
        
        pl_opp_gt %>%
            arrange(OPP_PTS) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(4, domain = c(min(pl_opp_gt$OPP_FGM), mean(pl_opp_gt$OPP_FGM), max(pl_opp_gt$OPP_FGM)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(5, domain = c(min(pl_opp_gt$OPP_FGA), mean(pl_opp_gt$OPP_FGA), max(pl_opp_gt$OPP_FGA)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(pl_opp_gt$OPP_FG_PCT), mean(pl_opp_gt$OPP_FG_PCT), max(pl_opp_gt$OPP_FG_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(pl_opp_gt$OPP_FG3M), mean(pl_opp_gt$OPP_FG3M), max(pl_opp_gt$OPP_FG3M)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(pl_opp_gt$OPP_FG3A), mean(pl_opp_gt$OPP_FG3A), max(pl_opp_gt$OPP_FG3A)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(pl_opp_gt$OPP_FG3_PCT), mean(pl_opp_gt$OPP_FG3_PCT), max(pl_opp_gt$OPP_FG3_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(pl_opp_gt$OPP_FTM), mean(pl_opp_gt$OPP_FTM), max(pl_opp_gt$OPP_FTM)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(pl_opp_gt$OPP_FTA), mean(pl_opp_gt$OPP_FTA), max(pl_opp_gt$OPP_FTA)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(pl_opp_gt$OPP_FT_PCT), mean(pl_opp_gt$OPP_FT_PCT), max(pl_opp_gt$OPP_FT_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(pl_opp_gt$OPP_OREB), mean(pl_opp_gt$OPP_OREB), max(pl_opp_gt$OPP_OREB)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(pl_opp_gt$OPP_DREB), mean(pl_opp_gt$OPP_DREB), max(pl_opp_gt$OPP_DREB)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(pl_opp_gt$OPP_REB), mean(pl_opp_gt$OPP_REB), max(pl_opp_gt$OPP_REB)),
                         palette = pal_hex_rev, accuracy = 0.1)  %>%
            gt_color_box(16, domain = c(min(pl_opp_gt$OPP_AST), mean(pl_opp_gt$OPP_AST), max(pl_opp_gt$OPP_AST)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(pl_opp_gt$OPP_TOV), mean(pl_opp_gt$OPP_TOV), max(pl_opp_gt$OPP_TOV)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(pl_opp_gt$OPP_STL), mean(pl_opp_gt$OPP_STL), max(pl_opp_gt$OPP_STL)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(pl_opp_gt$OPP_BLK), mean(pl_opp_gt$OPP_BLK), max(pl_opp_gt$OPP_BLK)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(pl_opp_gt$OPP_BLKA), mean(pl_opp_gt$OPP_BLKA), max(pl_opp_gt$OPP_BLKA)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(pl_opp_gt$OPP_PF), mean(pl_opp_gt$OPP_PF), max(pl_opp_gt$OPP_PF)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(22, domain = c(min(pl_opp_gt$OPP_PFD), mean(pl_opp_gt$OPP_PFD), max(pl_opp_gt$OPP_PFD)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(23, domain = c(min(pl_opp_gt$OPP_PTS), mean(pl_opp_gt$OPP_PTS), max(pl_opp_gt$OPP_PTS)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(24, domain = c(min(pl_opp_gt$PLUS_MINUS), mean(pl_opp_gt$PLUS_MINUS), max(pl_opp_gt$PLUS_MINUS)),
                         palette = pal_hex, accuracy = 0.1) 
        
    } else if (x == "Defense") {
        
        ### player stats defense ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `College` = '',
            `Conference` = '',
            `Country` = '',
            `DateFrom` = '',
            `DateTo` = '',
            `Division` = '',
            `DraftPick` = '',
            `DraftYear` = '',
            `GameScope` = '',
            `GameSegment` = '',
            `Height` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Defense',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'PerGame',
            `Period` = '0',
            `PlayerExperience` = '',
            `PlayerPosition` = '',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `StarterBench` = '',
            `TeamID` = '0',
            `VsConference` = '',
            `VsDivision` = '',
            `Weight` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashplayerstats', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        pl_def <- dt %>% select(2,5,7,11,12:24)
        
        pl_def_gt <- pl_def %>% filter(GP >= 15)
        
        pl_def_gt %>%
            arrange(desc(DEF_WS)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(5, domain = c(min(pl_def_gt$DEF_RATING), mean(pl_def_gt$DEF_RATING), max(pl_def_gt$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(6, domain = c(min(pl_def_gt$DREB), mean(pl_def_gt$DREB), max(pl_def_gt$DREB)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(pl_def_gt$DREB_PCT), mean(pl_def_gt$DREB_PCT), max(pl_def_gt$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(pl_def_gt$PCT_DREB), mean(pl_def_gt$PCT_DREB), max(pl_def_gt$PCT_DREB)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(pl_def_gt$STL), mean(pl_def_gt$STL), max(pl_def_gt$STL)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(pl_def_gt$PCT_STL), mean(pl_def_gt$PCT_STL), max(pl_def_gt$PCT_STL)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(pl_def_gt$BLK), mean(pl_def_gt$BLK), max(pl_def_gt$BLK)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(pl_def_gt$PCT_BLK), mean(pl_def_gt$PCT_BLK), max(pl_def_gt$PCT_BLK)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(pl_def_gt$OPP_PTS_OFF_TOV), mean(pl_def_gt$OPP_PTS_OFF_TOV), max(pl_def_gt$OPP_PTS_OFF_TOV)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(pl_def_gt$OPP_PTS_2ND_CHANCE), mean(pl_def_gt$OPP_PTS_2ND_CHANCE), max(pl_def_gt$OPP_PTS_2ND_CHANCE)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(pl_def_gt$OPP_PTS_FB), mean(pl_def_gt$OPP_PTS_FB), max(pl_def_gt$OPP_PTS_FB)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(pl_def_gt$OPP_PTS_PAINT), mean(pl_def_gt$OPP_PTS_PAINT), max(pl_def_gt$OPP_PTS_PAINT)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(pl_def_gt$DEF_WS), mean(pl_def_gt$DEF_WS), max(pl_def_gt$DEF_WS)),
                         palette = pal_hex, accuracy = 0.001)   
        
    } else {
        print("error")
    }
    
}
team_logs <- function(x) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### game logs ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/teamgamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_base <- dt %>%
            select(6,4,7,8,9,10:30) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_base_gt <- gl_base
        
        gl_base_gt %>%
            arrange(desc(PTS)) %>%
            head(10) %>%
            gt() %>%
            gt_theme_dark() %>%
            fmt_number(
                columns = 5,
                rows = everything(),
                decimals = 0) %>%
            gt_color_box(6, domain = c(min(gl_base_gt$FGM),median(gl_base_gt$FGM),max(gl_base_gt$FGM)),
                         palette = pal_hex) %>%
            gt_color_box(7, domain = c(min(gl_base_gt$FGA),median(gl_base_gt$FGA),max(gl_base_gt$FGA)),
                         palette = pal_hex) %>%
            gt_color_box(8, domain = c(min(gl_base_gt$FG_PCT),median(gl_base_gt$FG_PCT),max(gl_base_gt$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_base_gt$FG3M),median(gl_base_gt$FG3M),max(gl_base_gt$FG3M)),
                         palette = pal_hex) %>%
            gt_color_box(10, domain = c(min(gl_base_gt$FG3A),median(gl_base_gt$FG3A),max(gl_base_gt$FG3A)),
                         palette = pal_hex) %>%
            gt_color_box(11, domain = c(min(gl_base_gt$FG3_PCT),median(gl_base_gt$FG3_PCT),max(gl_base_gt$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_base_gt$FTM),median(gl_base_gt$FTM),max(gl_base_gt$FTM)),
                         palette = pal_hex) %>%
            gt_color_box(13, domain = c(min(gl_base_gt$FTA),median(gl_base_gt$FTA),max(gl_base_gt$FTA)),
                         palette = pal_hex) %>%
            gt_color_box(14, domain = c(min(gl_base_gt$FT_PCT),median(gl_base_gt$FT_PCT),max(gl_base_gt$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_base_gt$OREB),median(gl_base_gt$OREB),max(gl_base_gt$OREB)),
                         palette = pal_hex) %>%
            gt_color_box(16, domain = c(min(gl_base_gt$DREB),median(gl_base_gt$DREB),max(gl_base_gt$DREB)),
                         palette = pal_hex) %>%
            gt_color_box(17, domain = c(min(gl_base_gt$REB),median(gl_base_gt$REB),max(gl_base_gt$REB)),
                         palette = pal_hex) %>%
            gt_color_box(18, domain = c(min(gl_base_gt$AST),median(gl_base_gt$AST),max(gl_base_gt$AST)),
                         palette = pal_hex) %>%
            gt_color_box(19, domain = c(min(gl_base_gt$TOV),median(gl_base_gt$TOV),max(gl_base_gt$TOV)),
                         palette = pal_hex_rev) %>%
            gt_color_box(20, domain = c(min(gl_base_gt$STL),median(gl_base_gt$STL),max(gl_base_gt$STL)),
                         palette = pal_hex) %>%
            gt_color_box(21, domain = c(min(gl_base_gt$BLK),median(gl_base_gt$BLK),max(gl_base_gt$BLK)),
                         palette = pal_hex) %>%
            gt_color_box(22, domain = c(min(gl_base_gt$BLKA),median(gl_base_gt$BLKA),max(gl_base_gt$BLKA)),
                         palette = pal_hex_rev) %>%
            gt_color_box(23, domain = c(min(gl_base_gt$PF),median(gl_base_gt$PF),max(gl_base_gt$PF)),
                         palette = pal_hex_rev) %>%
            gt_color_box(24, domain = c(min(gl_base_gt$PFD),median(gl_base_gt$PFD),max(gl_base_gt$PFD)),
                         palette = pal_hex) %>%
            gt_color_box(25, domain = c(min(gl_base_gt$PTS),median(gl_base_gt$PTS),max(gl_base_gt$PTS)),
                         palette = pal_hex) %>%
            gt_color_box(26, domain = c(min(gl_base_gt$PLUS_MINUS),median(gl_base_gt$PLUS_MINUS),max(gl_base_gt$PLUS_MINUS)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### game logs advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/teamgamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_adv <- dt %>%
            select(6,4,7,8,9,11,13,15,16:24,26,28,29) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_adv_gt <- gl_adv
        
        gl_adv_gt %>%
            arrange(desc(PIE)) %>%
            head(10) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(6, domain = c(min(gl_adv_gt$OFF_RATING),median(gl_adv_gt$OFF_RATING),max(gl_adv_gt$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(gl_adv_gt$DEF_RATING),median(gl_adv_gt$DEF_RATING),max(gl_adv_gt$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(gl_adv_gt$NET_RATING),median(gl_adv_gt$NET_RATING),max(gl_adv_gt$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_adv_gt$AST_PCT),median(gl_adv_gt$AST_PCT),max(gl_adv_gt$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_adv_gt$AST_TO),median(gl_adv_gt$AST_TO),max(gl_adv_gt$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(gl_adv_gt$AST_RATIO),median(gl_adv_gt$AST_RATIO),max(gl_adv_gt$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_adv_gt$OREB_PCT),median(gl_adv_gt$OREB_PCT),max(gl_adv_gt$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_adv_gt$DREB_PCT),median(gl_adv_gt$DREB_PCT),max(gl_adv_gt$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(gl_adv_gt$REB_PCT),median(gl_adv_gt$REB_PCT),max(gl_adv_gt$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_adv_gt$TM_TOV_PCT),median(gl_adv_gt$TM_TOV_PCT),max(gl_adv_gt$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_adv_gt$EFG_PCT),median(gl_adv_gt$EFG_PCT),max(gl_adv_gt$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(gl_adv_gt$TS_PCT),median(gl_adv_gt$TS_PCT),max(gl_adv_gt$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(gl_adv_gt$PACE),median(gl_adv_gt$PACE),max(gl_adv_gt$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(gl_adv_gt$POSS),median(gl_adv_gt$POSS),max(gl_adv_gt$POSS)),
                         palette = pal_hex) %>%
            gt_color_box(20, domain = c(min(gl_adv_gt$PIE),median(gl_adv_gt$PIE),max(gl_adv_gt$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
    
}
player_logs <- function(x) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### game logs ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/playergamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_base <- dt %>%
            select(9,3,7,10,11,12,13:33) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_base_gt <- gl_base %>%
            filter(MIN >= 20)
        
        gl_base_gt %>%
            arrange(desc(PTS)) %>%
            head(10) %>%
            gt() %>%
            gt_theme_dark() %>%
            fmt_number(
                columns = 6,
                rows = everything(),
                decimals = 1) %>%
            gt_color_box(7, domain = c(min(gl_base_gt$FGM),median(gl_base_gt$FGM),max(gl_base_gt$FGM)),
                         palette = pal_hex) %>%
            gt_color_box(8, domain = c(min(gl_base_gt$FGA),median(gl_base_gt$FGA),max(gl_base_gt$FGA)),
                         palette = pal_hex) %>%
            gt_color_box(9, domain = c(min(gl_base_gt$FG_PCT),median(gl_base_gt$FG_PCT),max(gl_base_gt$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_base_gt$FG3M),median(gl_base_gt$FG3M),max(gl_base_gt$FG3M)),
                         palette = pal_hex) %>%
            gt_color_box(11, domain = c(min(gl_base_gt$FG3A),median(gl_base_gt$FG3A),max(gl_base_gt$FG3A)),
                         palette = pal_hex) %>%
            gt_color_box(12, domain = c(min(gl_base_gt$FG3_PCT),median(gl_base_gt$FG3_PCT),max(gl_base_gt$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_base_gt$FTM),median(gl_base_gt$FTM),max(gl_base_gt$FTM)),
                         palette = pal_hex) %>%
            gt_color_box(14, domain = c(min(gl_base_gt$FTA),median(gl_base_gt$FTA),max(gl_base_gt$FTA)),
                         palette = pal_hex) %>%
            gt_color_box(15, domain = c(min(gl_base_gt$FT_PCT),median(gl_base_gt$FT_PCT),max(gl_base_gt$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_base_gt$OREB),median(gl_base_gt$OREB),max(gl_base_gt$OREB)),
                         palette = pal_hex) %>%
            gt_color_box(17, domain = c(min(gl_base_gt$DREB),median(gl_base_gt$DREB),max(gl_base_gt$DREB)),
                         palette = pal_hex) %>%
            gt_color_box(18, domain = c(min(gl_base_gt$REB),median(gl_base_gt$REB),max(gl_base_gt$REB)),
                         palette = pal_hex) %>%
            gt_color_box(19, domain = c(min(gl_base_gt$AST),median(gl_base_gt$AST),max(gl_base_gt$AST)),
                         palette = pal_hex) %>%
            gt_color_box(20, domain = c(min(gl_base_gt$TOV),median(gl_base_gt$TOV),max(gl_base_gt$TOV)),
                         palette = pal_hex_rev) %>%
            gt_color_box(21, domain = c(min(gl_base_gt$STL),median(gl_base_gt$STL),max(gl_base_gt$STL)),
                         palette = pal_hex) %>%
            gt_color_box(22, domain = c(min(gl_base_gt$BLK),median(gl_base_gt$BLK),max(gl_base_gt$BLK)),
                         palette = pal_hex) %>%
            gt_color_box(23, domain = c(min(gl_base_gt$BLKA),median(gl_base_gt$BLKA),max(gl_base_gt$BLKA)),
                         palette = pal_hex_rev) %>%
            gt_color_box(24, domain = c(min(gl_base_gt$PF),median(gl_base_gt$PF),max(gl_base_gt$PF)),
                         palette = pal_hex_rev) %>%
            gt_color_box(25, domain = c(min(gl_base_gt$PFD),median(gl_base_gt$PFD),max(gl_base_gt$PFD)),
                         palette = pal_hex) %>%
            gt_color_box(26, domain = c(min(gl_base_gt$PTS),median(gl_base_gt$PTS),max(gl_base_gt$PTS)),
                         palette = pal_hex) %>%
            gt_color_box(27, domain = c(min(gl_base_gt$PLUS_MINUS),median(gl_base_gt$PLUS_MINUS),max(gl_base_gt$PLUS_MINUS)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### game logs advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/playergamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_adv <- dt %>%
            select(9,3,7,10,11,12,14,17,20,22:28,30:32,35,38,39) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_adv_gt <- gl_adv %>%
            filter(MIN >= 20)
        
        gl_adv_gt %>%
            arrange(desc(PIE)) %>%
            head(10) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(7, domain = c(min(gl_adv_gt$OFF_RATING),median(gl_adv_gt$OFF_RATING),max(gl_adv_gt$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(gl_adv_gt$DEF_RATING),median(gl_adv_gt$DEF_RATING),max(gl_adv_gt$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_adv_gt$NET_RATING),median(gl_adv_gt$NET_RATING),max(gl_adv_gt$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_adv_gt$AST_PCT),median(gl_adv_gt$AST_PCT),max(gl_adv_gt$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(gl_adv_gt$AST_TO),median(gl_adv_gt$AST_TO),max(gl_adv_gt$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_adv_gt$AST_RATIO),median(gl_adv_gt$AST_RATIO),max(gl_adv_gt$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_adv_gt$OREB_PCT),median(gl_adv_gt$OREB_PCT),max(gl_adv_gt$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(gl_adv_gt$DREB_PCT),median(gl_adv_gt$DREB_PCT),max(gl_adv_gt$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_adv_gt$REB_PCT),median(gl_adv_gt$REB_PCT),max(gl_adv_gt$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_adv_gt$TM_TOV_PCT),median(gl_adv_gt$TM_TOV_PCT),max(gl_adv_gt$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(gl_adv_gt$EFG_PCT),median(gl_adv_gt$EFG_PCT),max(gl_adv_gt$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(gl_adv_gt$TS_PCT),median(gl_adv_gt$TS_PCT),max(gl_adv_gt$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(gl_adv_gt$USG_PCT),median(gl_adv_gt$USG_PCT),max(gl_adv_gt$USG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(gl_adv_gt$PACE),median(gl_adv_gt$PACE),max(gl_adv_gt$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(gl_adv_gt$PIE),median(gl_adv_gt$PIE),max(gl_adv_gt$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
    
}
team_logs_game <- function(x, gm) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### game logs ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/teamgamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_base <- dt %>%
            select(5,6,4,7,8,9,10:30) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_base_gt <- gl_base %>%
            filter(GAME_ID == as.character(gm)) %>%
            select(-1)
        
        gl_base_gt %>%
            # filter(PLAYER_NAME == "Kevin Durant" & GAME_DATE == "2022-11-28") %>%
            arrange(desc(PTS)) %>%
            head(10) %>%
            gt() %>%
            gt_theme_dark() %>%
            fmt_number(
                columns = 5,
                rows = everything(),
                decimals = 0) %>%
            gt_color_box(6, domain = c(min(gl_base$FGM),median(gl_base$FGM),max(gl_base$FGM)),
                         palette = pal_hex) %>%
            gt_color_box(7, domain = c(min(gl_base$FGA),median(gl_base$FGA),max(gl_base$FGA)),
                         palette = pal_hex) %>%
            gt_color_box(8, domain = c(min(gl_base$FG_PCT),median(gl_base$FG_PCT),max(gl_base$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_base$FG3M),median(gl_base$FG3M),max(gl_base$FG3M)),
                         palette = pal_hex) %>%
            gt_color_box(10, domain = c(min(gl_base$FG3A),median(gl_base$FG3A),max(gl_base$FG3A)),
                         palette = pal_hex) %>%
            gt_color_box(11, domain = c(min(gl_base$FG3_PCT),median(gl_base$FG3_PCT),max(gl_base$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_base$FTM),median(gl_base$FTM),max(gl_base$FTM)),
                         palette = pal_hex) %>%
            gt_color_box(13, domain = c(min(gl_base$FTA),median(gl_base$FTA),max(gl_base$FTA)),
                         palette = pal_hex) %>%
            gt_color_box(14, domain = c(min(gl_base$FT_PCT),median(gl_base$FT_PCT),max(gl_base$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_base$OREB),median(gl_base$OREB),max(gl_base$OREB)),
                         palette = pal_hex) %>%
            gt_color_box(16, domain = c(min(gl_base$DREB),median(gl_base$DREB),max(gl_base$DREB)),
                         palette = pal_hex) %>%
            gt_color_box(17, domain = c(min(gl_base$REB),median(gl_base$REB),max(gl_base$REB)),
                         palette = pal_hex) %>%
            gt_color_box(18, domain = c(min(gl_base$AST),median(gl_base$AST),max(gl_base$AST)),
                         palette = pal_hex) %>%
            gt_color_box(19, domain = c(min(gl_base$TOV),median(gl_base$TOV),max(gl_base$TOV)),
                         palette = pal_hex_rev) %>%
            gt_color_box(20, domain = c(min(gl_base$STL),median(gl_base$STL),max(gl_base$STL)),
                         palette = pal_hex) %>%
            gt_color_box(21, domain = c(min(gl_base$BLK),median(gl_base$BLK),max(gl_base$BLK)),
                         palette = pal_hex) %>%
            gt_color_box(22, domain = c(min(gl_base$BLKA),median(gl_base$BLKA),max(gl_base$BLKA)),
                         palette = pal_hex_rev) %>%
            gt_color_box(23, domain = c(min(gl_base$PF),median(gl_base$PF),max(gl_base$PF)),
                         palette = pal_hex_rev) %>%
            gt_color_box(24, domain = c(min(gl_base$PFD),median(gl_base$PFD),max(gl_base$PFD)),
                         palette = pal_hex) %>%
            gt_color_box(25, domain = c(min(gl_base$PTS),median(gl_base$PTS),max(gl_base$PTS)),
                         palette = pal_hex) %>%
            gt_color_box(26, domain = c(min(gl_base$PLUS_MINUS),median(gl_base$PLUS_MINUS),max(gl_base$PLUS_MINUS)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### game logs advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/teamgamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_adv <- dt %>%
            select(5,6,4,7,8,9,11,13,15,16:24,26,28,29) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_adv_gt <- gl_adv %>%
            filter(GAME_ID == as.character(gm)) %>%
            select(-1)
        
        gl_adv_gt %>%
            arrange(desc(PIE)) %>%
            head(10) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(6, domain = c(min(gl_adv$OFF_RATING),median(gl_adv$OFF_RATING),max(gl_adv$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(gl_adv$DEF_RATING),median(gl_adv$DEF_RATING),max(gl_adv$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(gl_adv$NET_RATING),median(gl_adv$NET_RATING),max(gl_adv$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_adv$AST_PCT),median(gl_adv$AST_PCT),max(gl_adv$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_adv$AST_TO),median(gl_adv$AST_TO),max(gl_adv$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(gl_adv$AST_RATIO),median(gl_adv$AST_RATIO),max(gl_adv$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_adv$OREB_PCT),median(gl_adv$OREB_PCT),max(gl_adv$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_adv$DREB_PCT),median(gl_adv$DREB_PCT),max(gl_adv$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(gl_adv$REB_PCT),median(gl_adv$REB_PCT),max(gl_adv$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_adv$TM_TOV_PCT),median(gl_adv$TM_TOV_PCT),max(gl_adv$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_adv$EFG_PCT),median(gl_adv$EFG_PCT),max(gl_adv$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(gl_adv$TS_PCT),median(gl_adv$TS_PCT),max(gl_adv$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(gl_adv$PACE),median(gl_adv$PACE),max(gl_adv$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(gl_adv$POSS),median(gl_adv$POSS),max(gl_adv$POSS)),
                         palette = pal_hex) %>%
            gt_color_box(20, domain = c(min(gl_adv$PIE),median(gl_adv$PIE),max(gl_adv$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
    
}
player_logs_game <- function(x, gm) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### game logs ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/playergamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_base <- dt %>%
            select(8,9,3,7,10,11,12,13:33) %>%
            mutate(GAME_DATE = as_date(GAME_DATE)) %>%
            filter(MIN >= 20)
        
        gl_base_gt <- gl_base %>%
            filter(GAME_ID == as.character(gm)) %>%
            select(-1)
        
        gl_base_gt %>%
            arrange(desc(PTS)) %>%
            gt() %>%
            gt_theme_dark() %>%
            fmt_number(
                columns = 6,
                rows = everything(),
                decimals = 1) %>%
            gt_color_box(7, domain = c(min(gl_base$FGM),median(gl_base$FGM),max(gl_base$FGM)),
                         palette = pal_hex) %>%
            gt_color_box(8, domain = c(min(gl_base$FGA),median(gl_base$FGA),max(gl_base$FGA)),
                         palette = pal_hex) %>%
            gt_color_box(9, domain = c(min(gl_base$FG_PCT),median(gl_base$FG_PCT),max(gl_base$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_base$FG3M),median(gl_base$FG3M),max(gl_base$FG3M)),
                         palette = pal_hex) %>%
            gt_color_box(11, domain = c(min(gl_base$FG3A),median(gl_base$FG3A),max(gl_base$FG3A)),
                         palette = pal_hex) %>%
            gt_color_box(12, domain = c(min(gl_base$FG3_PCT),median(gl_base$FG3_PCT),max(gl_base$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_base$FTM),median(gl_base$FTM),max(gl_base$FTM)),
                         palette = pal_hex) %>%
            gt_color_box(14, domain = c(min(gl_base$FTA),median(gl_base$FTA),max(gl_base$FTA)),
                         palette = pal_hex) %>%
            gt_color_box(15, domain = c(min(gl_base$FT_PCT),median(gl_base$FT_PCT),max(gl_base$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_base$OREB),median(gl_base$OREB),max(gl_base$OREB)),
                         palette = pal_hex) %>%
            gt_color_box(17, domain = c(min(gl_base$DREB),median(gl_base$DREB),max(gl_base$DREB)),
                         palette = pal_hex) %>%
            gt_color_box(18, domain = c(min(gl_base$REB),median(gl_base$REB),max(gl_base$REB)),
                         palette = pal_hex) %>%
            gt_color_box(19, domain = c(min(gl_base$AST),median(gl_base$AST),max(gl_base$AST)),
                         palette = pal_hex) %>%
            gt_color_box(20, domain = c(min(gl_base$TOV),median(gl_base$TOV),max(gl_base$TOV)),
                         palette = pal_hex_rev) %>%
            gt_color_box(21, domain = c(min(gl_base$STL),median(gl_base$STL),max(gl_base$STL)),
                         palette = pal_hex) %>%
            gt_color_box(22, domain = c(min(gl_base$BLK),median(gl_base$BLK),max(gl_base$BLK)),
                         palette = pal_hex) %>%
            gt_color_box(23, domain = c(min(gl_base$BLKA),median(gl_base$BLKA),max(gl_base$BLKA)),
                         palette = pal_hex_rev) %>%
            gt_color_box(24, domain = c(min(gl_base$PF),median(gl_base$PF),max(gl_base$PF)),
                         palette = pal_hex_rev) %>%
            gt_color_box(25, domain = c(min(gl_base$PFD),median(gl_base$PFD),max(gl_base$PFD)),
                         palette = pal_hex) %>%
            gt_color_box(26, domain = c(min(gl_base$PTS),median(gl_base$PTS),max(gl_base$PTS)),
                         palette = pal_hex) %>%
            gt_color_box(27, domain = c(min(gl_base$PLUS_MINUS),median(gl_base$PLUS_MINUS),max(gl_base$PLUS_MINUS)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### game logs advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/playergamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_adv <- dt %>%
            select(8,9,3,7,10,11,12,14,17,20,22:28,30:32,35,38,39) %>%
            mutate(GAME_DATE = as_date(GAME_DATE)) %>%
            filter(MIN >= 20)
        
        gl_adv_gt <- gl_adv %>%
            filter(GAME_ID == as.character(gm)) %>%
            select(-1)
        
        gl_adv_gt %>%
            arrange(desc(PIE)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(7, domain = c(min(gl_adv$OFF_RATING),median(gl_adv$OFF_RATING),max(gl_adv$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(gl_adv$DEF_RATING),median(gl_adv$DEF_RATING),max(gl_adv$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_adv$NET_RATING),median(gl_adv$NET_RATING),max(gl_adv$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_adv$AST_PCT),median(gl_adv$AST_PCT),max(gl_adv$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(gl_adv$AST_TO),median(gl_adv$AST_TO),max(gl_adv$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_adv$AST_RATIO),median(gl_adv$AST_RATIO),max(gl_adv$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_adv$OREB_PCT),median(gl_adv$OREB_PCT),max(gl_adv$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(gl_adv$DREB_PCT),median(gl_adv$DREB_PCT),max(gl_adv$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_adv$REB_PCT),median(gl_adv$REB_PCT),max(gl_adv$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_adv$TM_TOV_PCT),median(gl_adv$TM_TOV_PCT),max(gl_adv$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(gl_adv$EFG_PCT),median(gl_adv$EFG_PCT),max(gl_adv$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(gl_adv$TS_PCT),median(gl_adv$TS_PCT),max(gl_adv$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(gl_adv$USG_PCT),median(gl_adv$USG_PCT),max(gl_adv$USG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(gl_adv$PACE),median(gl_adv$PACE),max(gl_adv$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(gl_adv$PIE),median(gl_adv$PIE),max(gl_adv$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
    
}
team_logs_team <- function(x, tm) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### game logs ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/teamgamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_base <- dt %>%
            select(6,4,7,8,9,10:30) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_base_gt <- gl_base %>%
            filter(TEAM_NAME == as.character(tm))
        
        gl_base_gt %>%
            arrange(desc(GAME_DATE)) %>%
            gt() %>%
            gt_theme_dark() %>%
            fmt_number(
                columns = 5,
                rows = everything(),
                decimals = 0) %>%
            gt_color_box(6, domain = c(min(gl_base$FGM),median(gl_base$FGM),max(gl_base$FGM)),
                         palette = pal_hex) %>%
            gt_color_box(7, domain = c(min(gl_base$FGA),median(gl_base$FGA),max(gl_base$FGA)),
                         palette = pal_hex) %>%
            gt_color_box(8, domain = c(min(gl_base$FG_PCT),median(gl_base$FG_PCT),max(gl_base$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_base$FG3M),median(gl_base$FG3M),max(gl_base$FG3M)),
                         palette = pal_hex) %>%
            gt_color_box(10, domain = c(min(gl_base$FG3A),median(gl_base$FG3A),max(gl_base$FG3A)),
                         palette = pal_hex) %>%
            gt_color_box(11, domain = c(min(gl_base$FG3_PCT),median(gl_base$FG3_PCT),max(gl_base$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_base$FTM),median(gl_base$FTM),max(gl_base$FTM)),
                         palette = pal_hex) %>%
            gt_color_box(13, domain = c(min(gl_base$FTA),median(gl_base$FTA),max(gl_base$FTA)),
                         palette = pal_hex) %>%
            gt_color_box(14, domain = c(min(gl_base$FT_PCT),median(gl_base$FT_PCT),max(gl_base$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_base$OREB),median(gl_base$OREB),max(gl_base$OREB)),
                         palette = pal_hex) %>%
            gt_color_box(16, domain = c(min(gl_base$DREB),median(gl_base$DREB),max(gl_base$DREB)),
                         palette = pal_hex) %>%
            gt_color_box(17, domain = c(min(gl_base$REB),median(gl_base$REB),max(gl_base$REB)),
                         palette = pal_hex) %>%
            gt_color_box(18, domain = c(min(gl_base$AST),median(gl_base$AST),max(gl_base$AST)),
                         palette = pal_hex) %>%
            gt_color_box(19, domain = c(min(gl_base$TOV),median(gl_base$TOV),max(gl_base$TOV)),
                         palette = pal_hex_rev) %>%
            gt_color_box(20, domain = c(min(gl_base$STL),median(gl_base$STL),max(gl_base$STL)),
                         palette = pal_hex) %>%
            gt_color_box(21, domain = c(min(gl_base$BLK),median(gl_base$BLK),max(gl_base$BLK)),
                         palette = pal_hex) %>%
            gt_color_box(22, domain = c(min(gl_base$BLKA),median(gl_base$BLKA),max(gl_base$BLKA)),
                         palette = pal_hex_rev) %>%
            gt_color_box(23, domain = c(min(gl_base$PF),median(gl_base$PF),max(gl_base$PF)),
                         palette = pal_hex_rev) %>%
            gt_color_box(24, domain = c(min(gl_base$PFD),median(gl_base$PFD),max(gl_base$PFD)),
                         palette = pal_hex) %>%
            gt_color_box(25, domain = c(min(gl_base$PTS),median(gl_base$PTS),max(gl_base$PTS)),
                         palette = pal_hex) %>%
            gt_color_box(26, domain = c(min(gl_base$PLUS_MINUS),median(gl_base$PLUS_MINUS),max(gl_base$PLUS_MINUS)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### game logs advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/teamgamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_adv <- dt %>%
            select(6,4,7,8,9,11,13,15,16:24,26,28,29) %>%
            mutate(GAME_DATE = as_date(GAME_DATE))
        
        gl_adv_gt <- gl_adv %>%
            filter(TEAM_NAME == as.character(tm))
        
        gl_adv_gt %>%
            arrange(desc(GAME_DATE)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(6, domain = c(min(gl_adv$OFF_RATING),median(gl_adv$OFF_RATING),max(gl_adv$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(7, domain = c(min(gl_adv$DEF_RATING),median(gl_adv$DEF_RATING),max(gl_adv$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(gl_adv$NET_RATING),median(gl_adv$NET_RATING),max(gl_adv$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_adv$AST_PCT),median(gl_adv$AST_PCT),max(gl_adv$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_adv$AST_TO),median(gl_adv$AST_TO),max(gl_adv$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(gl_adv$AST_RATIO),median(gl_adv$AST_RATIO),max(gl_adv$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_adv$OREB_PCT),median(gl_adv$OREB_PCT),max(gl_adv$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_adv$DREB_PCT),median(gl_adv$DREB_PCT),max(gl_adv$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(gl_adv$REB_PCT),median(gl_adv$REB_PCT),max(gl_adv$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_adv$TM_TOV_PCT),median(gl_adv$TM_TOV_PCT),max(gl_adv$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_adv$EFG_PCT),median(gl_adv$EFG_PCT),max(gl_adv$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(gl_adv$TS_PCT),median(gl_adv$TS_PCT),max(gl_adv$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(gl_adv$PACE),median(gl_adv$PACE),max(gl_adv$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(gl_adv$POSS),median(gl_adv$POSS),max(gl_adv$POSS)),
                         palette = pal_hex) %>%
            gt_color_box(20, domain = c(min(gl_adv$PIE),median(gl_adv$PIE),max(gl_adv$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
    
}
player_logs_team <- function(x, tm) {
    
    ### colors ----
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                          "#d9f0d3", "#7fbf7b", "#1b7837")
                          
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                              "#e7d4e8", "#af8dc3", "#762a83")
                              
    if (x == "Base") {
        
        ### game logs ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Base',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/playergamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_base <- dt %>%
            select(9,3,7,10,11,12,13:33) %>%
            mutate(GAME_DATE = as_date(GAME_DATE)) %>%
            filter(MIN >= 20)
        
        gl_base_gt <- gl_base %>%
            filter(TEAM_NAME == as.character(tm))
        
        gl_base_gt %>%
            arrange(desc(PTS)) %>%
            gt() %>%
            gt_theme_dark() %>%
            fmt_number(
                columns = 6,
                rows = everything(),
                decimals = 1) %>%
            gt_color_box(7, domain = c(min(gl_base$FGM),median(gl_base$FGM),max(gl_base$FGM)),
                         palette = pal_hex) %>%
            gt_color_box(8, domain = c(min(gl_base$FGA),median(gl_base$FGA),max(gl_base$FGA)),
                         palette = pal_hex) %>%
            gt_color_box(9, domain = c(min(gl_base$FG_PCT),median(gl_base$FG_PCT),max(gl_base$FG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_base$FG3M),median(gl_base$FG3M),max(gl_base$FG3M)),
                         palette = pal_hex) %>%
            gt_color_box(11, domain = c(min(gl_base$FG3A),median(gl_base$FG3A),max(gl_base$FG3A)),
                         palette = pal_hex) %>%
            gt_color_box(12, domain = c(min(gl_base$FG3_PCT),median(gl_base$FG3_PCT),max(gl_base$FG3_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_base$FTM),median(gl_base$FTM),max(gl_base$FTM)),
                         palette = pal_hex) %>%
            gt_color_box(14, domain = c(min(gl_base$FTA),median(gl_base$FTA),max(gl_base$FTA)),
                         palette = pal_hex) %>%
            gt_color_box(15, domain = c(min(gl_base$FT_PCT),median(gl_base$FT_PCT),max(gl_base$FT_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_base$OREB),median(gl_base$OREB),max(gl_base$OREB)),
                         palette = pal_hex) %>%
            gt_color_box(17, domain = c(min(gl_base$DREB),median(gl_base$DREB),max(gl_base$DREB)),
                         palette = pal_hex) %>%
            gt_color_box(18, domain = c(min(gl_base$REB),median(gl_base$REB),max(gl_base$REB)),
                         palette = pal_hex) %>%
            gt_color_box(19, domain = c(min(gl_base$AST),median(gl_base$AST),max(gl_base$AST)),
                         palette = pal_hex) %>%
            gt_color_box(20, domain = c(min(gl_base$TOV),median(gl_base$TOV),max(gl_base$TOV)),
                         palette = pal_hex_rev) %>%
            gt_color_box(21, domain = c(min(gl_base$STL),median(gl_base$STL),max(gl_base$STL)),
                         palette = pal_hex) %>%
            gt_color_box(22, domain = c(min(gl_base$BLK),median(gl_base$BLK),max(gl_base$BLK)),
                         palette = pal_hex) %>%
            gt_color_box(23, domain = c(min(gl_base$BLKA),median(gl_base$BLKA),max(gl_base$BLKA)),
                         palette = pal_hex_rev) %>%
            gt_color_box(24, domain = c(min(gl_base$PF),median(gl_base$PF),max(gl_base$PF)),
                         palette = pal_hex_rev) %>%
            gt_color_box(25, domain = c(min(gl_base$PFD),median(gl_base$PFD),max(gl_base$PFD)),
                         palette = pal_hex) %>%
            gt_color_box(26, domain = c(min(gl_base$PTS),median(gl_base$PTS),max(gl_base$PTS)),
                         palette = pal_hex) %>%
            gt_color_box(27, domain = c(min(gl_base$PLUS_MINUS),median(gl_base$PLUS_MINUS),max(gl_base$PLUS_MINUS)),
                         palette = pal_hex)
        
    } else if (x == "Advanced") {
        
        ### game logs advanced ----
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Host` = 'stats.nba.com',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Referer` = 'https://www.nba.com/',
            `Connection` = 'keep-alive'
        )
        
        params = list(
            `DateFrom` = '',
            `DateTo` = '',
            `GameSegment` = '',
            `LastNGames` = '0',
            `LeagueID` = '00',
            `Location` = '',
            `MeasureType` = 'Advanced',
            `Month` = '0',
            `OpponentTeamID` = '0',
            `Outcome` = '',
            `PORound` = '0',
            `PaceAdjust` = 'N',
            `PerMode` = 'Totals',
            `Period` = '0',
            `PlusMinus` = 'N',
            `Rank` = 'N',
            `Season` = '2022-23',
            `SeasonSegment` = '',
            `SeasonType` = 'Regular Season',
            `ShotClockRange` = '',
            `VsConference` = '',
            `VsDivision` = ''
        )
        
        res <- httr::GET(url = 'https://stats.nba.com/stats/playergamelogs', httr::add_headers(.headers=headers), query = params)
        data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
        column_names <- data$headers %>% as.character()  
        dt <- rbindlist(data$rowSet) %>% setnames(column_names)
        
        gl_adv <- dt %>%
            select(9,3,7,10,11,12,14,17,20,22:28,30:32,35,38,39) %>%
            mutate(GAME_DATE = as_date(GAME_DATE)) %>%
            filter(MIN >= 20)
        
        gl_adv_gt <- gl_adv %>%
            filter(TEAM_NAME == as.character(tm))
        
        gl_adv_gt %>%
            arrange(desc(PIE)) %>%
            gt() %>%
            gt_theme_dark() %>%
            gt_color_box(7, domain = c(min(gl_adv$OFF_RATING),median(gl_adv$OFF_RATING),max(gl_adv$OFF_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(8, domain = c(min(gl_adv$DEF_RATING),median(gl_adv$DEF_RATING),max(gl_adv$DEF_RATING)),
                         palette = pal_hex_rev, accuracy = 0.1) %>%
            gt_color_box(9, domain = c(min(gl_adv$NET_RATING),median(gl_adv$NET_RATING),max(gl_adv$NET_RATING)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(10, domain = c(min(gl_adv$AST_PCT),median(gl_adv$AST_PCT),max(gl_adv$AST_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(11, domain = c(min(gl_adv$AST_TO),median(gl_adv$AST_TO),max(gl_adv$AST_TO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(12, domain = c(min(gl_adv$AST_RATIO),median(gl_adv$AST_RATIO),max(gl_adv$AST_RATIO)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(13, domain = c(min(gl_adv$OREB_PCT),median(gl_adv$OREB_PCT),max(gl_adv$OREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(14, domain = c(min(gl_adv$DREB_PCT),median(gl_adv$DREB_PCT),max(gl_adv$DREB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(15, domain = c(min(gl_adv$REB_PCT),median(gl_adv$REB_PCT),max(gl_adv$REB_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(16, domain = c(min(gl_adv$TM_TOV_PCT),median(gl_adv$TM_TOV_PCT),max(gl_adv$TM_TOV_PCT)),
                         palette = pal_hex_rev, suffix = "%", accuracy = 0.1) %>%
            gt_color_box(17, domain = c(min(gl_adv$EFG_PCT),median(gl_adv$EFG_PCT),max(gl_adv$EFG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(18, domain = c(min(gl_adv$TS_PCT),median(gl_adv$TS_PCT),max(gl_adv$TS_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(19, domain = c(min(gl_adv$USG_PCT),median(gl_adv$USG_PCT),max(gl_adv$USG_PCT)),
                         palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
            gt_color_box(20, domain = c(min(gl_adv$PACE),median(gl_adv$PACE),max(gl_adv$PACE)),
                         palette = pal_hex, accuracy = 0.1) %>%
            gt_color_box(21, domain = c(min(gl_adv$PIE),median(gl_adv$PIE),max(gl_adv$PIE)),
                         palette = pal_hex, scale = 100, accuracy = 0.1)
        
    } else {
        
        print("error")
        
    }
    
    
}


team_stats("Advanced") # Base - Advanced - Scoring - Four Factors - Opponent - Defense
player_stats("Advanced") # Base - Advanced - Scoring - Usage - Opponent - Defense
team_logs("Advanced") # Base - Advanced
player_logs("Advanced") # Base - Advanced
team_logs_game("Advanced", "0022200512") # Base - Advanced
player_logs_game("Advanced", "0022200512") # Base - Advanced
team_logs_team("Advanced", "Dallas Mavericks") # Base - Advanced
player_logs_team("Advanced", "Dallas Mavericks") # Base - Advanced
