# Playoff Teams by ORtg & DRtg -----------------------------------
library(RSQLite)
library(DBI)
library(dplyr)
library(ggimage)
library(ggrepel)
library(teamcolors)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# logos <- teamcolors %>%
#     filter(league == "nba") %>%
#     select(1,3,11) %>%
#     mutate(across(where(is.character), str_replace_all, 
#                   pattern = "Los Angeles Clippers", replacement = "LA Clippers"))

bound.label <- 115
df.text <- data.frame(lab.text = c("+Off, +Def", "+Off, -Def", "-Off, -Def", "-Off, +Def"), 
                      x = c(bound.label-1, bound.label-1, bound.label-7, bound.label-7), 
                      y = c(bound.label-10, bound.label, bound.label, bound.label-10))

NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db")

box_score_team <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db"), "BoxScoreTeam") %>%
    collect()

team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db"), "TeamDictionary") %>%
    collect()
DBI::dbDisconnect(NBAdb)


tms <- box_score_team %>%
    filter(substr(idGame, 1,3) == 221 &
               teamName %in% c("Suns", "Grizzlies", "Warriors", "Mavericks",
                               "Jazz", "Nuggets", "Timberwolves", "Clippers",
                               "Pelicans", "Spurs",
                               "Heat", "Bucks", "Celtics", "76ers",
                               "Raptors", "Bulls", "Cavaliers", "Nets",
                               "Hawks", "Hornets")) %>%
    mutate(full_name = paste(cityTeam, teamName, sep = " ")) %>%
    group_by(full_name) %>%
    summarize(across(c(ortg,drtg),mean))


viz <- tms %>%
    left_join(team_dict, by = c("full_name" = "nameTeam"))

viz$nameConference <- factor(viz$nameConference,      
                         levels = c("West", "East"))

viz %>%
    ggplot(aes(ortg, drtg)) +
    geom_hline(yintercept=median(tms$drtg), colour = "red", linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(tms$ortg), colour = "red", linetype = "dashed", alpha=.5) + 
    geom_abline(slope = -1.5, intercept = c(70, 65, 60, 55, 50, 45, 40, 35), alpha = .15) +
    geom_point(color = viz$primary, cex=10, alpha = .6) +
    geom_text_repel(aes(label=full_name)) +
    labs(x="Offensive Rating (points scored per 100 possessions)", 
         y="Defensive Rating (points allowed per 100 possessions)", 
         title = "Play-In Teams by Offensive & Defensive Rating",
         caption = "@MambaMetrics") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=4, fontface=2) +
    theme_bw() +
    theme(aspect.ratio = 9 / 16,
        plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) +
    facet_wrap(vars(nameConference)) +
    scale_y_reverse()






