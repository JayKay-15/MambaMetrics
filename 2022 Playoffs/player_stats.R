library(RSQLite)
library(DBI)
library(dplyr)
library(tidyr)
library(stringr)
library(gt)


NBAdb <- DBI::dbConnect(RSQLite::SQLite(), 
                        "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite")
DBI::dbListTables(NBAdb)

player_advanced <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                             "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                              "PlayerAdvanced")
player_advanced <- player_advanced %>%
    collect()

player_game <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                             "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                              "PlayerPerGame")
player_game <- player_game %>%
    collect()

team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(),
                                         "/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb.sqlite"),
                          "TeamDictionary")
team_dict <- team_dict %>%
    collect()

DBI::dbDisconnect(NBAdb)

pg <- player_game %>%
    filter(yearSeason==2021 & countGames>=10) %>%
    select(6,7,18,12,13,16,36:40,42)

pa <- player_advanced %>%
    filter(yearSeason==2021) %>%
    select(6,7,12,21,31,36:38)

viz <- pg %>%
    left_join(pa) %>%
    separate(slugTeamsBREF, into = c("team3","team2","team"), extra = "drop", remove = FALSE, fill = "left") %>%
    select(1,2,6,15,10:14,7,8,9,16:20)

viz <- viz %>%
    mutate(across(where(is.character), str_replace_all,
                  pattern = "PHO", replacement = "PHX")) %>%
    mutate(across(where(is.character), str_replace_all,
                  pattern = "BRK", replacement = "BKN")) %>%
    mutate(across(where(is.character), str_replace_all,
                  pattern = "CHO", replacement = "CHA"))
viz <- viz %>%
    left_join(team_dict, by=c("team"="slugTeam")) %>%
    mutate(team = nameTeam) %>%
    select(1:17)

colnames(viz) <- c("Player","Position","team","Points","Rebounds","Assists","Steals","Blocks","Turnovers",
                   "FG%","3FG%","FT%","TS%","Usage","Off BPM","Def BPM","BPM")

tab <- viz %>%
    filter(team == "Memphis Grizzlies") %>%
    arrange(desc(Points))

tab %>%
    select(-3) %>%
    head(10) %>%
    gt() %>%
    tab_spanner(
        label = "Player Stats",
        columns = c(Rebounds:Usage)
    ) %>%
    fmt_percent(
        columns = c(`FG%`:Usage),
        decimals = 1
    ) %>%
    tab_options(
        table.border.top.color = "white",
        row.striping.include_table_body = FALSE
    ) %>%
    tab_source_note(
        source_note = "@MambaMetrics"
    )



