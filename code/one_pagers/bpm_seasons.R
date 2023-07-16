#  BPM Last Four Season -----------------------------------
library(RSQLite)
library(DBI)
library(dplyr)
library(tidyr)
library(stringr)
library(ggimage)
library(ggrepel)
library(gridExtra)
library(readxl)
# library(nbastatR)
# library(teamcolors)

# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite")
DBI::dbListTables(NBAdb)

player_advanced <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "/../NBAdb/nba_db.sqlite"), "PlayerAdvanced")
player_advanced <- player_advanced %>%
    collect()

lineup <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "LineupStats22")
lineup <- lineup %>%
    collect()

team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "TeamDictionary")
team_dict <- team_dict %>%
    collect()


DBI::dbDisconnect(NBAdb)


pop <- lineup %>%
    group_by(lineup, slugTeam) %>%
    summarize(poss = sum(totalPossTeam),
              min = sum(totalTime)) %>%
    group_by(slugTeam) %>%
    filter(poss == max(poss)) %>%
    arrange(slugTeam)
pop <- pop[-23,]
    
pop <- pop %>%
    left_join(team_dict, by = "slugTeam") %>%
    select(1,2,6,10) %>%
    separate(lineup, into = c("one","two","three","four","five"), sep = ",") %>%
    pivot_longer(cols = c("one","two","three","four","five"), values_to = "player") %>%
    mutate(player = str_trim(player, side = "both")) %>%
    select(nameTeam, player, slugTeam, primary)


pa <- player_advanced %>%
    select(2,6,7,38) %>%
    filter(yearSeason %in% c(2018,2019,2020)) %>%
    left_join(., pop, by = c("namePlayer" = "player")) %>%
    filter(!is.na(nameTeam)) %>% 
    mutate(yearSeason = yearSeason + 1) %>%
    select(1,5,2,3,4,7)

pa$yearSeason <- lubridate::ymd(pa$yearSeason, truncated = 2L)

tm <- pa %>%
    filter(nameTeam == "Phoenix Suns")

tm %>%
    ggplot() +
    geom_line(aes(yearSeason, ratioBPM), size=3, color=tm$primary, alpha=.7, lineend="round") +
    geom_point(aes(yearSeason, ratioBPM), data = filter(tm, yearSeason == "2021-01-01"), size=5, alpha=.9) + 
    geom_hline(yintercept=0, linetype = "dashed", alpha=.3) +
    labs(x="Season", 
         y="BPM", 
         title = "BPM - Last Four Seasons",
         caption = "@MambaMetrics") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_bw() +
    theme(aspect.ratio = 9 / 16,
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) +
    facet_wrap(vars(namePlayer))

############## VORP -------------------------------------------------------------


NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite")
DBI::dbListTables(NBAdb)

player_advanced <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "AdvancedBoxScoreBREF")
player_advanced <- player_advanced %>%
    collect()

team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "TeamDictionary")
team_dict <- team_dict %>%
    collect()

pos <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "PlayerAdvanced")
pos <- pos %>%
    collect()

DBI::dbDisconnect(NBAdb)


pos <- pos %>%
    filter(yearSeason==2021) %>%
    select(6,7)

pos[ , 3:4] <- str_split_fixed(pos$slugPosition, "-", 2)

pos <- pos %>%
    select(1,3) %>%
    rename("pos_new" = "V1")

pos$pos_new <- factor(pos$pos_new, levels=c("PG", "SG", "SF", "PF", "C"))

pop <- read_xlsx("./playoff_lineups.xlsx")

player_advanced$date_game <- as.Date(player_advanced$date_game, origin ="1970-01-01")

player_advanced <- player_advanced %>%
    filter(date_game > '2021-10-15') %>%
    select(2,4,5,22)

pa <- player_advanced %>%
    left_join(pop, by = c("player" = "player")) %>%
    filter(!is.na(nameTeam)) %>%
    select(1,3:7) %>%
    rename("team" = "nameTeam") %>%
    left_join(.,pos, by = c("player" = "namePlayer")) %>%
    arrange(date_game, team, pos_new) %>%
    group_by(month = lubridate::floor_date(date_game, "month"), player, team, primary, pos_new) %>%
    summarize(VORP = round(mean(VORP, na.rm=T),1))


tm <- pa %>%
    filter(team == "Memphis Grizzlies")
    # mutate(pos_new = replace(pos_new, player == "Jalen Brunson", "SG"))
    

tm %>%
    ggplot() +
    geom_line(aes(month, VORP), size=2, color=tm$primary, alpha=.7, lineend="round") +
    geom_point(aes(month, VORP), data = filter(tm, month == month), size=3, alpha=.7) +
    geom_hline(yintercept=0, linetype = "dashed", alpha=.3) +
    labs(x="", 
         y="VORP", 
         title = "VORP 2021-2022 Season",
         subtitle = "VORP: Value Over Replacement Player", 
         caption = "@MambaMetrics") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    theme_bw() +
    theme(aspect.ratio = 9 / 16,
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")) +
    facet_wrap(pos_new~player)



# player_advanced <- player_advanced %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Bogdan Bogdanović", replacement = "Bogdan Bogdanovic"))
# 
# player_advanced <- player_advanced %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Luka Dončić", replacement = "Luka Doncic"))
# 
# player_advanced <- player_advanced %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Nikola Jokić", replacement = "Nikola Jokic"))
# 
# player_advanced <- player_advanced %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Bojan Bogdanović", replacement = "Bojan Bogdanovic"))
# 
# player_advanced <- player_advanced %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Nikola Vučević", replacement = "Nikola Vucevic"))
# pos <- pos %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Robert Williams III", replacement = "Robert Williams"))
# player_advanced <- player_advanced %>%
#     mutate(across(where(is.character), str_replace_all,
#                   pattern = "Jonas Valančiūnas", replacement = "Jonas Valanciunas"))






