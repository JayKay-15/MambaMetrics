### Live Stats -----------------------------------------------------

library(tidyverse)
library(gt)
library(gtExtras)


lg_avg <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db"), "league_avg_current") %>%
    collect() %>%
    mutate(Date = as_date(Date, origin ="1970-01-01"))
    
sched <- nbastatR::current_schedule() %>% filter(dateGame == Sys.Date())
sched[,c(29,24,16,8)]

scoreboard <- function(d) {
    
    sched <- nbastatR::current_schedule() %>% filter(dateGame == d)
    sched[,c(29,24,16,8)]
    sched_lst <- as.character(sched$slugGame)
    
    j <- 1
    for (j in j:length(sched_lst)) {
        
        gm <- as.character(sched_lst[[j]])
        
        headers = c(
            `Accept` = '*/*',
            `Origin` = 'https://www.nba.com',
            `Accept-Encoding` = 'gzip, deflate, br',
            `If-None-Match` = '"07c79fa4b5cae550c567ceb98d8248ab"',
            `Host` = 'cdn.nba.com',
            `If-Modified-Since` = 'Thu, 27 Oct 2022 00:37:04 GMT',
            `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
            `Referer` = 'https://www.nba.com/',
            `Accept-Language` = 'en-US,en;q=0.9',
            `Connection` = 'keep-alive'
        )
        
        res <- httr::GET(url = paste0('https://cdn.nba.com/static/json/liveData/boxscore/boxscore_',gm,'.json'), httr::add_headers(.headers=headers))
        data <- httr::content(res) %>% .[['game']]
        
        
        ### base ----
        ht_base <- as.data.frame(data$homeTeam[1:7])
        at_base <- as.data.frame(data$awayTeam[1:7])
        
        
        ### periods ----
        ht_periods <- data.frame()
        
        i <- 1
        for (i in i:length(data$homeTeam$periods)) {
            
            ht_periods_df <- as.data.frame(data$homeTeam$periods[[i]])
            ht_periods <- rbind(ht_periods, ht_periods_df)
            
        }
        rm(ht_periods_df)
        
        ht_periods <- ht_periods %>%
            mutate(period = ifelse(periodType == "REGULAR", paste0("q",as.character(period)), paste0("ot",as.character(period-4))))
        
        ht_periods_wide <- ht_periods %>%
            select(-periodType) %>%
            pivot_wider(names_from = period, values_from = score) %>%
            mutate(teamName = ht_base$teamName) %>%
            select(teamName, 1:ncol(.)) %>%
            left_join(., ht_base[,c(2,5,6,7)], by = "teamName") %>%
            mutate(clock = data$gameStatusText,
                   inBonus = ifelse(inBonus == "1", "yes", "no"),
                   gameID = data$gameId) %>%
            rename(timeouts = timeoutsRemaining) %>%
            select(gameID, teamName:ncol(.))
        
        
        at_periods <- data.frame()
        
        i <- 1
        for (i in i:length(data$awayTeam$periods)) {
            
            at_periods_df <- as.data.frame(data$awayTeam$periods[[i]])
            at_periods <- rbind(at_periods, at_periods_df)
            
        }
        rm(at_periods_df)
        
        at_periods <- at_periods %>%
            mutate(period = ifelse(periodType == "REGULAR", paste0("q",as.character(period)), paste0("ot",as.character(period-4))))
        
        at_periods_wide <- at_periods %>%
            select(-periodType) %>%
            pivot_wider(names_from = period, values_from = score) %>%
            mutate(teamName = at_base$teamName) %>%
            select(teamName, 1:ncol(.)) %>%
            left_join(., at_base[,c(2,5,6,7)], by = "teamName") %>%
            mutate(clock = data$gameStatusText,
                   inBonus = ifelse(inBonus == "1", "yes", "no"),
                   gameID = data$gameId) %>%
            rename(timeouts = timeoutsRemaining) %>%
            select(gameID, teamName:ncol(.))
        
        scoreboard <- as.data.frame(rbind(at_periods_wide,ht_periods_wide))
        
        print(paste0("game: ",j))
        print(scoreboard)
        
        
    }
    
}
team_stats <- function(gm) {
    
    headers = c(
        `Accept` = '*/*',
        `Origin` = 'https://www.nba.com',
        `Accept-Encoding` = 'gzip, deflate, br',
        `If-None-Match` = '"07c79fa4b5cae550c567ceb98d8248ab"',
        `Host` = 'cdn.nba.com',
        `If-Modified-Since` = 'Thu, 27 Oct 2022 00:37:04 GMT',
        `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
        `Referer` = 'https://www.nba.com/',
        `Accept-Language` = 'en-US,en;q=0.9',
        `Connection` = 'keep-alive'
    )
    
    res <- httr::GET(url = paste0('https://cdn.nba.com/static/json/liveData/boxscore/boxscore_',paste0('00',as.character(gm)),'.json'), httr::add_headers(.headers=headers))
    data <- httr::content(res) %>% .[['game']]
    
    
    ### arena ----
    arena <- as.data.frame(data$arena) %>% pivot_longer(cols = !arenaId)
    
    
    ### base ----
    ht_base <- as.data.frame(data$homeTeam[1:7])
    at_base <- as.data.frame(data$awayTeam[1:7])
    
    
    ### periods ----
    ht_periods <- data.frame()
    
    i <- 1
    for (i in i:length(data$homeTeam$periods)) {
        
        ht_periods_df <- as.data.frame(data$homeTeam$periods[[i]])
        ht_periods <- rbind(ht_periods, ht_periods_df)
        
    }
    rm(ht_periods_df)
    
    ht_periods <- ht_periods %>%
        mutate(period = ifelse(periodType == "REGULAR", paste0("q",as.character(period)), paste0("ot",as.character(period-4))))
    
    ht_periods_wide <- ht_periods %>%
        select(-periodType) %>%
        pivot_wider(names_from = period, values_from = score) %>%
        mutate(teamName = ht_base$teamName) %>%
        select(teamName, 1:ncol(.)) %>%
        left_join(., ht_base[,c(2,5,6,7)], by = "teamName") %>%
        mutate(clock = data$gameStatusText,
               inBonus = ifelse(inBonus == "1", "yes", "no")) %>%
        rename(timeouts = timeoutsRemaining)
    
    
    at_periods <- data.frame()
    
    i <- 1
    for (i in i:length(data$awayTeam$periods)) {
        
        at_periods_df <- as.data.frame(data$awayTeam$periods[[i]])
        at_periods <- rbind(at_periods, at_periods_df)
        
    }
    rm(at_periods_df)
    
    at_periods <- at_periods %>%
        mutate(period = ifelse(periodType == "REGULAR", paste0("q",as.character(period)), paste0("ot",as.character(period-4))))
    
    at_periods_wide <- at_periods %>%
        select(-periodType) %>%
        pivot_wider(names_from = period, values_from = score) %>%
        mutate(teamName = at_base$teamName) %>%
        select(teamName, 1:ncol(.)) %>%
        left_join(., at_base[,c(2,5,6,7)], by = "teamName") %>%
        mutate(clock = data$gameStatusText,
               inBonus = ifelse(inBonus == "1", "yes", "no")) %>%
        rename(timeouts = timeoutsRemaining)
    
    
    ### team stats ----
    ht_stats <- as.data.frame(data$homeTeam$statistics) %>% mutate(teamName = ht_base$teamName)
    
    at_stats <- as.data.frame(data$awayTeam$statistics) %>% mutate(teamName = at_base$teamName)
    
    
    ht_linescores <- ht_stats %>%
        select(teamName,pointsInThePaint,pointsFastBreak,biggestLead,reboundsTeam,turnovers,turnoversTeam) %>%
        mutate(pointsOffTurnovers = at_stats$pointsFromTurnovers)
    
    at_linescores <- at_stats %>%
        select(teamName,pointsInThePaint,pointsFastBreak,biggestLead,reboundsTeam,turnovers,turnoversTeam) %>%
        mutate(pointsOffTurnovers = ht_stats$pointsFromTurnovers)
    
    
    ht_box <- ht_stats %>%
        select(teamName,fieldGoalsMade,fieldGoalsAttempted,fieldGoalsPercentage,
               threePointersMade,threePointersAttempted,threePointersPercentage,
               freeThrowsMade,freeThrowsAttempted,freeThrowsPercentage,
               reboundsOffensive,reboundsDefensive,reboundsPersonal,assists,steals,blocks,turnovers,
               foulsPersonal,points) %>%
        mutate(across(c(fieldGoalsPercentage,threePointersPercentage,freeThrowsPercentage), round, 3))
    
    colnames(ht_box) <- c("team","fgm","fga","fg","fg3m","fg3a","fg3",
                          "ftm","fta","ft","oreb","dreb","reb",
                          "ast","stl","blk","tov","pf","pts")
    
    at_box <- at_stats %>%
        select(teamName,fieldGoalsMade,fieldGoalsAttempted,fieldGoalsPercentage,
               threePointersMade,threePointersAttempted,threePointersPercentage,
               freeThrowsMade,freeThrowsAttempted,freeThrowsPercentage,
               reboundsOffensive,reboundsDefensive,reboundsPersonal,assists,steals,blocks,turnovers,
               foulsPersonal,points) %>%
        mutate(across(c(fieldGoalsPercentage,threePointersPercentage,freeThrowsPercentage), round, 3))
    
    colnames(at_box) <- c("team","fgm","fga","fg","fg3m","fg3a","fg3",
                          "ftm","fta","ft","oreb","dreb","reb",
                          "ast","stl","blk","tov","pf","pts")
    
    
    ht_box_adv <- ht_stats %>%
        mutate(poss =  fieldGoalsAttempted + 0.44 * freeThrowsAttempted - reboundsOffensive + turnovers,
               oposs = at_stats$fieldGoalsAttempted + 0.44 * at_stats$freeThrowsAttempted - at_stats$reboundsOffensive + at_stats$turnovers,
               min = as.numeric(sapply(gsub("^PT|M$","", ht_stats$minutesCalculated), function(x)
                   ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x))),
               pace = 48 * ((poss + oposs) / (2 * (min/5))),
               ortg = (points/poss)*100,
               drtg = (at_stats$points/oposs)*100,
               nrtg = ortg - drtg,
               ast = assists/fieldGoalsMade,
               ast_tov = assistsTurnoverRatio,
               ast_ratio = (assists*100) / (fieldGoalsAttempted + (0.44 * freeThrowsAttempted) + assists + turnovers),
               oreb = reboundsOffensive / (reboundsOffensive + at_stats$reboundsDefensive),
               dreb = reboundsDefensive / (reboundsDefensive + at_stats$reboundsOffensive),
               treb = reboundsPersonal / (reboundsPersonal + at_stats$reboundsPersonal),
               tov = turnovers / poss,
               efg = fieldGoalsEffectiveAdjusted,
               ts = fieldGoalsEffectiveAdjusted
        ) %>%
        select(teamName,points,ortg,drtg,nrtg,ast,ast_tov,ast_ratio,oreb,dreb,treb,tov,efg,ts,pace) %>%
        mutate(across(c(ortg:nrtg,ast_tov:ast_ratio,pace), round, 1)) %>%
        mutate(across(c(oreb:ts,ast), round, 3))
    
    
    at_box_adv <- at_stats %>%
        mutate(poss =  fieldGoalsAttempted + 0.44 * freeThrowsAttempted - reboundsOffensive + turnovers,
               oposs = ht_stats$fieldGoalsAttempted + 0.44 * ht_stats$freeThrowsAttempted - ht_stats$reboundsOffensive + ht_stats$turnovers,
               min = as.numeric(sapply(gsub("^PT|M$","", ht_stats$minutesCalculated), function(x)
                   ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x))),
               pace = 48 * ((poss + oposs)/ (2 * (min/5))),
               ortg = (points/poss)*100,
               drtg = (ht_stats$points/oposs)*100,
               nrtg = ortg - drtg,
               ast = assists/fieldGoalsMade,
               ast_tov = assistsTurnoverRatio,
               ast_ratio = (assists*100) / (fieldGoalsAttempted + (0.44 * freeThrowsAttempted) + assists + turnovers),
               oreb = reboundsOffensive  / (reboundsOffensive + ht_stats$reboundsDefensive),
               dreb = reboundsDefensive / (reboundsDefensive + ht_stats$reboundsOffensive),
               treb = reboundsPersonal / (reboundsPersonal + ht_stats$reboundsPersonal),
               tov = turnoversTotal / poss,
               efg = fieldGoalsEffectiveAdjusted,
               ts = fieldGoalsEffectiveAdjusted
        ) %>%
        select(teamName,points,ortg,drtg,nrtg,ast,ast_tov,ast_ratio,oreb,dreb,treb,tov,efg,ts,pace) %>%
        mutate(across(c(ortg:nrtg,ast_tov:ast_ratio,pace), round, 1)) %>%
        mutate(across(c(oreb:ts,ast), round, 3))
    
    
    ht_stack <- ht_box %>%
        mutate(fg2m = fgm - fg3m,
               fg2a = fga - fg3a,
               fg2 = fg2m / fg2a) %>%
        select(1:7,20:22,8:10)
    
    ht_tbl <- data.frame(mapply(c, ht_stack[,1:4], ht_stack[,c(1,5:7)], ht_stack[,c(1,8:10)], ht_stack[,c(1,11:13)], SIMPLIFY=F))
    ht_stack <- ht_tbl %>%
        mutate(rate = fga / ht_tbl[1,3],
               loc = c("fg","three","two","ft")) %>%
        mutate(across(fg:rate, round, 3))
    colnames(ht_stack) <- c("team","made","attempt","percent","rate","loc")
    
    
    at_stack <- at_box %>%
        mutate(fg2m = fgm - fg3m,
               fg2a = fga - fg3a,
               fg2 = fg2m / fg2a) %>%
        select(1:7,20:22,8:10)
    
    at_tbl <- data.frame(mapply(c, at_stack[,1:4], at_stack[,c(1,5:7)], at_stack[,c(1,8:10)], at_stack[,c(1,11:13)], SIMPLIFY=F))
    at_stack <- at_tbl %>%
        mutate(rate = fga / at_tbl[1,3],
               loc = c("fg","three","two","ft")) %>%
        mutate(across(fg:rate, round, 3))
    colnames(at_stack) <- c("team","made","attempt","percent","rate","loc")
    
    
    ht_ff <- ht_box_adv %>%
        mutate(ftr = ht_box$fta/ht_box$fga) %>%
        select(teamName,points,efg,ftr,oreb,tov)
    
    at_ff <- at_box_adv %>%
        mutate(ftr = at_box$fta/at_box$fga) %>%
        select(teamName,points,efg,ftr,oreb,tov)
    

    print('scoreboard')
    print(as.data.frame(rbind(at_periods_wide,ht_periods_wide)))
    
    print('box score')
    print(rbind(at_box,ht_box))
    
    print('advanced box score')
    print(rbind(at_box_adv,ht_box_adv))
    
    print('shot rates')
    print(at_stack)
    print(ht_stack)
    
    pal_hex <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
                 "#d9f0d3", "#7fbf7b", "#1b7837")
    
    pal_hex_rev <- c("#1b7837", "#7fbf7b", "#d9f0d3", "#f7f7f7",
                     "#e7d4e8", "#af8dc3", "#762a83")
    
    adv_box_viz <- rbind(at_box_adv,ht_box_adv) %>%
    gt() %>%
    gt_theme_dark() %>%
    gt_color_box(ortg, domain = c(min(lg_avg$ORtg),
                                  median(lg_avg$ORtg),
                                  max(lg_avg$ORtg)),
                 palette = pal_hex, accuracy = 0.1) %>%
    gt_color_box(drtg, domain = c(min(lg_avg$DRtg),
                                  median(lg_avg$DRtg),
                                  max(lg_avg$DRtg)),
                 palette = pal_hex_rev, accuracy = 0.1) %>%
    gt_color_box(nrtg, domain = c(min(lg_avg$NRtg),
                                  median(lg_avg$NRtg),
                                  max(lg_avg$NRtg)),
                 palette = pal_hex, accuracy = 0.1) %>%
    gt_color_box(ast, domain = c(min(lg_avg$AST),
                                 median(lg_avg$AST),
                                 max(lg_avg$AST)),
                 palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(ast_tov, domain = c(min(lg_avg$AST_TOV),
                                     median(lg_avg$AST_TOV),
                                     max(lg_avg$AST_TOV)),
                 palette = pal_hex, accuracy = 0.1) %>%
    gt_color_box(ast_ratio, domain = c(min(lg_avg$AST_RATIO),
                                       median(lg_avg$AST_RATIO),
                                       max(lg_avg$AST_RATIO)),
                 palette = pal_hex, accuracy = 0.1) %>%
    gt_color_box(oreb, domain = c(min(lg_avg$ORB),
                                  median(lg_avg$ORB),
                                  max(lg_avg$ORB)),
                 palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(dreb, domain = c(min(lg_avg$DRB),
                                  median(lg_avg$DRB),
                                  max(lg_avg$DRB)),
                 palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(treb, domain = c(min(lg_avg$TRB),
                                  median(lg_avg$TRB),
                                  max(lg_avg$TRB)),
                 palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(tov, domain = c(min(lg_avg$TOV),
                                 median(lg_avg$TOV),
                                 max(lg_avg$TOV)),
                 palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(efg, domain = c(min(lg_avg$eFG),
                                 median(lg_avg$eFG),
                                 max(lg_avg$eFG)),
                 palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(ts, domain = c(min(lg_avg$TS),
                                median(lg_avg$TS),
                                max(lg_avg$TS)),
                 palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
    gt_color_box(pace, domain = c(min(lg_avg$Pace),
                                  median(lg_avg$Pace),
                                  max(lg_avg$Pace)),
                 palette = pal_hex, accuracy = 0.1)
    
    
    ff_viz <- rbind(at_ff,ht_ff) %>%
        gt() %>%
        gt_theme_dark() %>%
        gt_color_box(efg, domain = c(min(lg_avg$eFG),
                                     median(lg_avg$eFG),
                                     max(lg_avg$eFG)),
                     palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
        gt_color_box(ftr, domain = c(min(lg_avg$FTR),
                                     median(lg_avg$FTR),
                                     max(lg_avg$FTR)),
                     palette = pal_hex, scale = 100, accuracy = 0.1) %>%
        gt_color_box(oreb, domain = c(min(lg_avg$ORB),
                                      median(lg_avg$ORB),
                                      max(lg_avg$ORB)),
                     palette = pal_hex, suffix = "%", scale = 100, accuracy = 0.1) %>%
        gt_color_box(tov, domain = c(min(lg_avg$TOV),
                                     median(lg_avg$TOV),
                                     max(lg_avg$TOV)),
                     palette = pal_hex_rev, suffix = "%", scale = 100, accuracy = 0.1)
    
    print(adv_box_viz)
    print(ff_viz)

    
}
player_stats <- function(gm) {
    
    headers = c(
        `Accept` = '*/*',
        `Origin` = 'https://www.nba.com',
        `Accept-Encoding` = 'gzip, deflate, br',
        `If-None-Match` = '"07c79fa4b5cae550c567ceb98d8248ab"',
        `Host` = 'cdn.nba.com',
        `If-Modified-Since` = 'Thu, 27 Oct 2022 00:37:04 GMT',
        `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
        `Referer` = 'https://www.nba.com/',
        `Accept-Language` = 'en-US,en;q=0.9',
        `Connection` = 'keep-alive'
    )
    
    res <- httr::GET(url = paste0('https://cdn.nba.com/static/json/liveData/boxscore/boxscore_',paste0('00',as.character(gm)),'.json'), httr::add_headers(.headers=headers))
    data <- httr::content(res) %>% .[['game']]
    
    
    ### arena ----
    arena <- as.data.frame(data$arena) %>% pivot_longer(cols = !arenaId)
    
    
    ### base ----
    ht_base <- as.data.frame(data$homeTeam[1:7])
    at_base <- as.data.frame(data$awayTeam[1:7])
    
    ht_stats <- as.data.frame(data$homeTeam$statistics) %>% mutate(teamName = ht_base$teamName)
    at_stats <- as.data.frame(data$awayTeam$statistics) %>% mutate(teamName = at_base$teamName)

    
    ht_box <- ht_stats %>%
        select(teamName,fieldGoalsMade,fieldGoalsAttempted,fieldGoalsPercentage,
               threePointersMade,threePointersAttempted,threePointersPercentage,
               freeThrowsMade,freeThrowsAttempted,freeThrowsPercentage,
               reboundsOffensive,reboundsDefensive,reboundsPersonal,assists,steals,blocks,turnovers,
               foulsPersonal,points) %>%
        mutate(across(c(fieldGoalsPercentage,threePointersPercentage,freeThrowsPercentage), round, 3))
    
    colnames(ht_box) <- c("team","fgm","fga","fg","fg3m","fg3a","fg3",
                          "ftm","fta","ft","oreb","dreb","reb",
                          "ast","stl","blk","tov","pf","pts")
    
    at_box <- at_stats %>%
        select(teamName,fieldGoalsMade,fieldGoalsAttempted,fieldGoalsPercentage,
               threePointersMade,threePointersAttempted,threePointersPercentage,
               freeThrowsMade,freeThrowsAttempted,freeThrowsPercentage,
               reboundsOffensive,reboundsDefensive,reboundsPersonal,assists,steals,blocks,turnovers,
               foulsPersonal,points) %>%
        mutate(across(c(fieldGoalsPercentage,threePointersPercentage,freeThrowsPercentage), round, 3))
    
    colnames(at_box) <- c("team","fgm","fga","fg","fg3m","fg3a","fg3",
                          "ftm","fta","ft","oreb","dreb","reb",
                          "ast","stl","blk","tov","pf","pts")
    
    
    ### player stats ----
    
    ht_players <- data.frame()
    i <- 1
    
    for (i in i:length(data$homeTeam$players)) {
        
        df <- as.data.frame(unlist(data$homeTeam$players[[i]]))
        colnames(df) <- "val"
        df <- tibble::rownames_to_column(df,var = "rnames")
        df <- df %>%
            pivot_wider(names_from = rnames, values_from = val)
        df <- df %>%
            select(personId, jerseyNum, starter, oncourt, played, name,
                   starts_with("statistics.")) %>%
            mutate(team = ht_base$teamName,
                   team_full = paste0(ht_base$teamCity," ",ht_base$teamName))
        
        ht_players <- bind_rows(ht_players, df)
        
    }
    
    at_players <- data.frame()
    i <- 1
    
    for (i in i:length(data$awayTeam$players)) {
        
        df <- as.data.frame(unlist(data$awayTeam$players[[i]]))
        colnames(df) <- "val"
        df <- tibble::rownames_to_column(df,var = "rnames")
        df <- df %>%
            pivot_wider(names_from = rnames, values_from = val)
        df <- df %>%
            select(personId, jerseyNum, starter, oncourt, played, name,
                   starts_with("statistics.")) %>%
            mutate(team = at_base$teamName,
                   team_full = paste0(at_base$teamCity," ",at_base$teamName))
        
        at_players <- bind_rows(at_players, df)
        
    }
    
    ht_players <- ht_players %>%
        mutate(across(statistics.assists:statistics.minus, as.numeric),
               across(statistics.plus:statistics.twoPointersPercentage, as.numeric))
    
    at_players <- at_players %>%
        mutate(across(statistics.assists:statistics.minus, as.numeric),
               across(statistics.plus:statistics.twoPointersPercentage, as.numeric))
    
    
    ht_players_trad <- ht_players %>%
        select(starter,oncourt,played,jerseyNum,name,statistics.minutesCalculated,
               statistics.fieldGoalsMade,statistics.fieldGoalsAttempted,statistics.fieldGoalsPercentage,
               statistics.threePointersMade,statistics.threePointersAttempted,statistics.threePointersPercentage,
               statistics.freeThrowsMade,statistics.freeThrowsAttempted,statistics.freeThrowsPercentage,
               statistics.reboundsOffensive,statistics.reboundsDefensive,statistics.reboundsTotal,
               statistics.assists,statistics.steals,statistics.blocks,statistics.turnovers,statistics.foulsPersonal,
               statistics.points,statistics.plusMinusPoints) %>%
        mutate(statistics.minutesCalculated = as.numeric(sapply(gsub("^PT|M$","", statistics.minutesCalculated), function(x)
            ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x)))) %>%
        mutate(across(c(statistics.fieldGoalsPercentage,statistics.threePointersPercentage,statistics.freeThrowsPercentage), 
                      round, 3))
    
    colnames(ht_players_trad) <- c("starter","oncourt","played","jerseyNum","name","min",
                                   "fgm","fga","fg","fg3m","fg3a","fg3","ftm","fta","ft",
                                   "oreb","dreb","reb","ast","stl","blk","tov","pf","pts","pm")
    
    at_players_trad <- at_players %>%
        select(starter,oncourt,played,jerseyNum,name,statistics.minutesCalculated,
               statistics.fieldGoalsMade,statistics.fieldGoalsAttempted,statistics.fieldGoalsPercentage,
               statistics.threePointersMade,statistics.threePointersAttempted,statistics.threePointersPercentage,
               statistics.freeThrowsMade,statistics.freeThrowsAttempted,statistics.freeThrowsPercentage,
               statistics.reboundsOffensive,statistics.reboundsDefensive,statistics.reboundsTotal,
               statistics.assists,statistics.steals,statistics.blocks,statistics.turnovers,statistics.foulsPersonal,
               statistics.points,statistics.plusMinusPoints) %>%
        mutate(statistics.minutesCalculated = as.numeric(sapply(gsub("^PT|M$","", statistics.minutesCalculated), function(x)
            ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x)))) %>%
        mutate(across(c(statistics.fieldGoalsPercentage,statistics.threePointersPercentage,statistics.freeThrowsPercentage), 
                      round, 3))
    
    colnames(at_players_trad) <- c("starter","oncourt","played","jerseyNum","name","min",
                                   "fgm","fga","fg","fg3m","fg3a","fg3","ftm","fta","ft",
                                   "oreb","dreb","reb","ast","stl","blk","tov","pf","pts","pm")
    
    
    
    ht_players_adv <- ht_players %>%
        mutate(min = as.numeric(sapply(gsub("^PT|M$","", statistics.minutesCalculated), function(x)
            ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x))),
            efg = (statistics.fieldGoalsMade + 0.5 * statistics.threePointersMade) / statistics.fieldGoalsAttempted,
            ts = statistics.points / (2 * statistics.fieldGoalsAttempted + .44 * statistics.freeThrowsAttempted),
            sr3 = statistics.threePointersAttempted / statistics.fieldGoalsAttempted,
            ftr = statistics.freeThrowsAttempted / statistics.fieldGoalsAttempted,
            ast_tov = statistics.assists / ifelse(statistics.turnovers == 0 & played == 1, 1, statistics.turnovers),
            fb = statistics.pointsFastBreak,
            pitp = statistics.pointsInThePaint,
            sc = statistics.pointsSecondChance,
            fd = statistics.foulsDrawn,
            usg = ((statistics.fieldGoalsAttempted + 0.44 * statistics.freeThrowsAttempted + statistics.turnovers) * 
                       (as.numeric(sapply(gsub("^PT|M$","", ht_stats$minutesCalculated), function(x)
                           ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x))) / 5)) / (min * (ht_box$fga + 0.44 * ht_box$fta + ht_box$tov)),
            pts = statistics.points,
            pm = statistics.plusMinusPoints
        ) %>%
        mutate(across(everything(), ~ ifelse(is.nan(.x), 0, .x))) %>%
        select(starter,oncourt,played,jerseyNum,name,min,
               efg:pm) %>%
        mutate(across(c(efg:ftr,usg),round,3)) %>%
        mutate(across(ast_tov,round,1))
    
    at_players_adv <- at_players %>%
        mutate(min = as.numeric(sapply(gsub("^PT|M$","", statistics.minutesCalculated), function(x)
            ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x))),
            efg = (statistics.fieldGoalsMade + 0.5 * statistics.threePointersMade) / statistics.fieldGoalsAttempted,
            ts = statistics.points / (2 * statistics.fieldGoalsAttempted + .44 * statistics.freeThrowsAttempted),
            sr3 = statistics.threePointersAttempted / statistics.fieldGoalsAttempted,
            ftr = statistics.freeThrowsAttempted / statistics.fieldGoalsAttempted,
            ast_tov = statistics.assists / ifelse(statistics.turnovers == 0 & played == 1, 1, statistics.turnovers),
            fb = statistics.pointsFastBreak,
            pitp = statistics.pointsInThePaint,
            sc = statistics.pointsSecondChance,
            fd = statistics.foulsDrawn,
            usg = ((statistics.fieldGoalsAttempted + 0.44 * statistics.freeThrowsAttempted + statistics.turnovers) * 
                       (as.numeric(sapply(gsub("^PT|M$","", ht_stats$minutesCalculated), function(x)
                           ifelse(length(x)==2, as.numeric(x[1])*60+as.numeric(x[2]), x))) / 5)) / (min * (at_box$fga + 0.44 * at_box$fta + at_box$tov)),
            pts = statistics.points,
            pm = statistics.plusMinusPoints
        ) %>%
        mutate(across(everything(), ~ ifelse(is.nan(.x), 0, .x))) %>%
        select(starter,oncourt,played,jerseyNum,name,min,
               efg:pm) %>%
        mutate(across(c(efg:ftr,usg),round,3)) %>%
        mutate(across(ast_tov,round,1))

    print("traditional box score")
    print(at_players_trad)
    print(ht_players_trad)
    
    print("advanced box score")
    print(at_players_adv)
    print(ht_players_adv)
    
    
}

### run live stats ----
scoreboard('2022-12-31')
team_stats(0022200542)
player_stats(0022200542)



# add dynamic league average
# review all code for improvements

