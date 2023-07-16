library(dplyr)
library(nbastatR)
library(gt)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

standings_raw <- standings(seasons = 2022, season_types = c("Regular Season"))

standings <- standings_raw %>%
    select("nameTeam","nameConference","nameDivison","recordOverall","pctWinTeam","recordLast10","slugStreakCurrent",
           "recordAway","recordAwayWinPct","recordLast10Away","slugStreakAwayCurrent",
           "recordHome","recordHomeWinPct","recordLast10Home","slugStreakHomeCurrent",
           "rankPlayoffs")

colnames(standings) <- c("Team","Conference","Division","Record","Win%","L10","Streak",
                         "Away Record","Away Win%","Away L10","Away Streak",
                         "Home Record", "Home Win%","Home L10","Home Streak",
                         "Seed")

standings <- standings %>%
    select(16,1,2,4,8,12) %>%
    mutate(Seed = replace(Seed, Team == "New Orleans Pelicans", 8))



NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite")
box_score_team <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "BoxScoreTeam")
box_score_team <- box_score_team %>%
    collect()

team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../NBAdb/nba_db.sqlite"), "TeamDictionary")
team_dict <- team_dict %>%
    collect()

DBI::dbDisconnect(NBAdb)


tms <- box_score_team %>%
    filter(substr(idGame, 1,3) == 221 &
               teamName %in% c("Suns", "Grizzlies", "Warriors", "Mavericks",
                               "Jazz", "Nuggets", "Timberwolves","Pelicans",
                               "Heat", "Bucks", "Celtics", "76ers",
                               "Raptors", "Bulls", "Nets", "Hawks")) %>%
    mutate(full_name = paste(cityTeam, teamName, sep = " ")) %>%
    group_by(full_name) %>%
    summarize(round(across(c(ortg,drtg),mean),1))

standings %>%
    left_join(tms, by=c("Team" = "full_name")) %>%
    filter(!is.na(ortg)) %>%
    mutate(Net = round(ortg-drtg,1)) %>%
    filter(Conference=="West" & Seed <= 10) %>%
    arrange(Seed) %>%
    select(-3) %>%
    gt() %>%
    tab_spanner(
        label = "Western Conference",
        columns = c(Team, Record, `Away Record`, `Home Record`, ortg, drtg)
    ) %>%
    tab_options(
        table.border.top.color = "white",
        row.striping.include_table_body = FALSE
    ) %>%
    tab_style(
        style = list(
            cell_fill(color = "#005083", alpha = .7)
            # cell_text(color="white")
        ),
        locations = cells_body(
            #columns = vars(V1, V2), # not needed if coloring all columns
            rows = 7)
    ) %>%
    tab_source_note(
        source_note = "@MambaMetrics"
    ) %>%
    cols_label(
        ortg = "Off Rating",
        drtg = "Def Rating",
        Net = "Net Rating"
    )
    






