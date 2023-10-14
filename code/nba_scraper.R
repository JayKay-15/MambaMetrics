library(tidyverse)
library(data.table)
library(magrittr)
library(lubridate)


### Team Tracking
headers = c(
    `Accept` = '*/*',
    `Origin` = 'https://www.nba.com',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
    `Accept-Language` = 'en-US,en;q=0.9',
    `Referer` = 'https://www.nba.com/',
    `Connection` = 'keep-alive'
)

# headers = c(
#     `Origin` = 'https://www.nba.com',
#     `Referer` = 'https://www.nba.com/',
#     `Accept` = '*/*',
#     `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15'
# )


sched <- nbastatR::game_logs(seasons = 2023,
                             result_types = "team",
                             season_types = "Regular Season") %>%
    select(5) %>%
    distinct()

# test <- seq(as.Date("2021-10-18"), as.Date("2022-10-19"), by="days")
gm_df <- format(sched$dateGame, "%m/%d/%Y")


df <- data.frame()

i <- 1
h <- length(gm_df)

for (i in i:h) {
    
    g <- gm_df[i]
    
    params = list(
        `College` = '',
        `Conference` = '',
        `Country` = '',
        `DateFrom` = g,
        `DateTo` = g,
        `Division` = '',
        `DraftPick` = '',
        `DraftYear` = '',
        `GameScope` = '',
        `Height` = '',
        `LastNGames` = '0',
        `LeagueID` = '00',
        `Location` = '',
        `Month` = '0',
        `OpponentTeamID` = '0',
        `Outcome` = '',
        `PORound` = '0',
        `PerMode` = 'PerGame',
        `PlayerExperience` = '',
        `PlayerOrTeam` = 'Team',
        `PlayerPosition` = '',
        `PtMeasureType` = 'Rebounding',
        `Season` = '2022-23',
        `SeasonSegment` = '',
        `SeasonType` = 'Regular Season',
        `StarterBench` = '',
        `TeamID` = '0',
        `VsConference` = '',
        `VsDivision` = '',
        `Weight` = ''
    )
    
    res <- httr::GET(url = 'https://stats.nba.com/stats/leaguedashptstats', httr::add_headers(.headers=headers), query = params)
    data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
    column_names <- data$headers %>% as.character()  
    dt <- rbindlist(data$rowSet) %>% setnames(column_names) %>% mutate(date = as_date(params[["DateFrom"]], format = "%m/%d/%Y"))
    print(params[["DateFrom"]])
    
    df <- bind_rows(df, dt)
    
}

openxlsx::write.xlsx(df, file = "./output/rebounding.xlsx")

#####

### Team Playtype
headers = c(
    `Accept` = '*/*',
    `Origin` = 'https://www.nba.com',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
    `Accept-Language` = 'en-US,en;q=0.9',
    `Referer` = 'https://www.nba.com/',
    `Connection` = 'keep-alive'
)

params = list(
        `LeagueID` = '00',
        `PerMode` = 'PerGame',
        `PlayType` = 'Isolation',
        `PlayerOrTeam` = 'T',
        `SeasonType` = 'Regular Season',
        `SeasonYear` = '2021-22',
        `TypeGrouping` = 'offensive'
)

res <- httr::GET(url = 'https://stats.nba.com/stats/synergyplaytypes', httr::add_headers(.headers=headers), query = params)
data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
column_names <- data$headers %>% as.character()  
dt <- rbindlist(data$rowSet) %>% setnames(column_names)

openxlsx::write.xlsx(dt, file = "./output/isolation_off.xlsx")

#####

### Player Tracking
headers = c(
    `Accept` = '*/*',
    `Origin` = 'https://www.nba.com',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
    `Accept-Language` = 'en-US,en;q=0.9',
    `Referer` = 'https://www.nba.com/',
    `Connection` = 'keep-alive'
)

params = list(
    `LeagueID` = '00',
    `PerMode` = 'PerGame',
    `PlayType` = 'Isolation',
    `PlayerOrTeam` = 'T',
    `SeasonType` = 'Regular Season',
    `SeasonYear` = '2021-22',
    `TypeGrouping` = 'offensive'
)

res <- httr::GET(url = 'https://stats.nba.com/stats/synergyplaytypes', httr::add_headers(.headers=headers), query = params)
data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
column_names <- data$headers %>% as.character()  
dt <- rbindlist(data$rowSet) %>% setnames(column_names)

openxlsx::write.xlsx(dt, file = "./output/isolation_off.xlsx")

#####

### Player Play Type
headers = c(
    `Accept` = '*/*',
    `Origin` = 'https://www.nba.com',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15',
    `Accept-Language` = 'en-US,en;q=0.9',
    `Referer` = 'https://www.nba.com/',
    `Connection` = 'keep-alive'
)

params = list(
    `LeagueID` = '00',
    `PerMode` = 'PerGame',
    `PlayType` = 'Isolation',
    `PlayerOrTeam` = 'T',
    `SeasonType` = 'Regular Season',
    `SeasonYear` = '2021-22',
    `TypeGrouping` = 'offensive'
)

res <- httr::GET(url = 'https://stats.nba.com/stats/synergyplaytypes', httr::add_headers(.headers=headers), query = params)
data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
column_names <- data$headers %>% as.character()  
dt <- rbindlist(data$rowSet) %>% setnames(column_names)

openxlsx::write.xlsx(dt, file = "./output/isolation_off.xlsx")





# json <-
#     res$content %>%
#     rawToChar() %>%
#     jsonlite::fromJSON(simplifyVector = T)




# pie

headers = c(
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

params = list(
    `DateFrom` = "",
    `DateTo` = "",
    `GameSegment` = "",
    `ISTRound` = "",
    `LastNGames` = "0",
    `LeagueID` = "00",
    `Location` = "",
    `MeasureType` = "Advanced",
    `Month` = "0",
    `OpponentTeamID` = "0",
    `Outcome` = "",
    `PORound` = "0",
    `PaceAdjust` = "N",
    `PerMode` = "Totals",
    `Period` = "0",
    `PlusMinus` = "N",
    `Rank` = "N",
    `Season` = "2022-23",
    `SeasonSegment` = "",
    `SeasonType` = "Regular Season",
    `ShotClockRange` = "",
    `VsConference` = "",
    `VsDivision` = ""
)

res <- httr::GET(url = "https://stats.nba.com/stats/playergamelogs", httr::add_headers(.headers=headers), query = params)
data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
column_names <- data$headers %>% as.character()  
dt <- rbindlist(data$rowSet) %>% setnames(column_names)



# pts off to, 2nd pts, fb pts, pitp

headers = c(
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

params = list(
    `DateFrom` = "",
    `DateTo` = "",
    `GameSegment` = "",
    `ISTRound` = "",
    `LastNGames` = "0",
    `LeagueID` = "00",
    `Location` = "",
    `MeasureType` = "Misc",
    `Month` = "0",
    `OpponentTeamID` = "0",
    `Outcome` = "",
    `PORound` = "0",
    `PaceAdjust` = "N",
    `PerMode` = "Totals",
    `Period` = "0",
    `PlusMinus` = "N",
    `Rank` = "N",
    `Season` = "2022-23",
    `SeasonSegment` = "",
    `SeasonType` = "Regular Season",
    `ShotClockRange` = "",
    `VsConference` = "",
    `VsDivision` = ""
)

res <- httr::GET(url = "https://stats.nba.com/stats/playergamelogs", httr::add_headers(.headers=headers), query = params)
data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
column_names <- data$headers %>% as.character()  
dt <- rbindlist(data$rowSet) %>% setnames(column_names)








