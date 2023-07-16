# Playoff Teams Movement -----------------------------------
library(RSQLite)
library(DBI)
library(dplyr)
library(stringr)
library(ggimage)
library(ggrepel)
library(gridExtra)
library(teamcolors)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

# logos <- teamcolors %>%
#     filter(league == "nba") %>%
#     select(1,3,11) %>%
#     mutate(across(where(is.character), str_replace_all, 
#                   pattern = "Los Angeles Clippers", replacement = "LA Clippers"))

bound.label <- 115
df.text <- data.frame(lab.text = c("+Off, +Def", "+Off, -Def", "-Off, -Def", "-Off, +Def"), 
                      x = c(bound.label, bound.label, bound.label-8, bound.label-8), 
                      y = c(bound.label-15, bound.label, bound.label, bound.label-15))

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
                               "Jazz", "Nuggets", "Timberwolves", "Pelicans", 
                               "Heat", "Bucks", "Celtics", "76ers",
                               "Raptors", "Bulls", "Nets", "Hawks")) %>%
    mutate(full_name = paste(cityTeam, teamName, sep = " ")) %>%
    group_by(full_name) %>%
    mutate(gmCount = row_number())


tms_q1 <- tms %>%
    filter(gmCount <= 21) %>%
    group_by(full_name) %>%
    summarize(across(c(ortg,drtg),mean))

tms_q2 <- tms %>%
    filter(gmCount <= 41) %>%
    group_by(full_name) %>%
    summarize(across(c(ortg,drtg),mean))

tms_q3 <- tms %>%
    filter(gmCount <= 61) %>%
    group_by(full_name) %>%
    summarize(across(c(ortg,drtg),mean))

tms_q4 <- tms %>%
    group_by(full_name) %>%
    summarize(across(c(ortg,drtg),mean))

tms <- tms_q1 %>%
    left_join(tms_q2, by = "full_name") %>%
    left_join(., tms_q3, by = "full_name") %>%
    left_join(., tms_q4, by = "full_name")

colnames(tms) <- c("team", "ortg_q1", "drtg_q1", "ortg_q2", "drtg_q2", 
                   "ortg_q3", "drtg_q3", "ortg_q4", "drtg_q4")

viz <- tms %>%
    left_join(team_dict , by = c("team" = "nameTeam"))

viz$nameConference <- factor(viz$nameConference,
                             levels = c("West", "East"))

viz %>%
    ggplot(aes(ortg_q1, drtg_q1)) +
    geom_hline(yintercept=median(viz$drtg_q4), linetype = "dashed", alpha=.5) +
    geom_vline(xintercept=median(viz$ortg_q4), linetype = "dashed", alpha=.5) +
    geom_abline(slope = -1.5, intercept = c(70, 65, 60, 55, 50, 45, 40, 35), alpha = .2) +
    geom_point(color = viz$primary, cex=5, alpha = .6) +
    geom_point(aes(x = ortg_q2, y = drtg_q2), color = viz$primary, cex=6, alpha = .6) +
    geom_point(aes(x = ortg_q3, y = drtg_q3), color = viz$primary, cex=7, alpha = .6) +
    geom_point(aes(x = ortg_q4, y = drtg_q4), color = viz$primary, cex=8, alpha = .6) +
    geom_segment(aes(x = ortg_q1, y=drtg_q1, xend = ortg_q2, yend = drtg_q2), 
                     color=viz$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_segment(aes(x = ortg_q2, y=drtg_q2, xend = ortg_q3, yend = drtg_q3), 
                     color=viz$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_segment(aes(x = ortg_q3, y=drtg_q3, xend = ortg_q4, yend = drtg_q4), 
                     color=viz$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_text_repel(aes(x = ortg_q4, y = drtg_q4),label=viz$team) +
    labs(x="Offensive Rating (points scored per 100 possessions)", 
         y="Defensive Rating (points allowed per 100 possessions)", 
         title = "Rating Movement - Playoff Teams",
         caption = "@MambaMetrics") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=4, fontface=2) +
    theme_bw() +
    theme(aspect.ratio = 9 / 16,
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) +
    facet_wrap(vars(nameConference)) +
    scale_y_reverse()

viz_e <- viz %>%
    filter(nameConference=="East")

viz_e %>%
    ggplot(aes(ortg_q1, drtg_q1)) +
    geom_hline(yintercept=median(viz_e$drtg_q4), linetype = "dashed", alpha=.5) +
    geom_vline(xintercept=median(viz_e$ortg_q4), linetype = "dashed", alpha=.5) +
    geom_abline(slope = -1.5, intercept = c(70, 65, 60, 55, 50, 45, 40, 35), alpha = .2) +
    geom_point(color = viz_e$primary, cex=5, alpha = .6) +
    geom_point(aes(x = ortg_q2, y = drtg_q2), color = viz_e$primary, cex=6, alpha = .6) +
    geom_point(aes(x = ortg_q3, y = drtg_q3), color = viz_e$primary, cex=7, alpha = .6) +
    geom_point(aes(x = ortg_q4, y = drtg_q4), color = viz_e$primary, cex=8, alpha = .6) +
    geom_segment(aes(x = ortg_q1, y=drtg_q1, xend = ortg_q2, yend = drtg_q2), 
                 color=viz_e$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_segment(aes(x = ortg_q2, y=drtg_q2, xend = ortg_q3, yend = drtg_q3), 
                 color=viz_e$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_segment(aes(x = ortg_q3, y=drtg_q3, xend = ortg_q4, yend = drtg_q4), 
                 color=viz_e$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_text_repel(aes(x = ortg_q4, y = drtg_q4),label=viz_e$team) +
    labs(x="Offensive Rating (points scored per 100 possessions)", 
         y="Defensive Rating (points allowed per 100 possessions)", 
         title = "Rating Movement - Playoff Teams",
         subtitle = "Eastern Conference",
         caption = "@MambaMetrics") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=4, fontface=2) +
    theme_bw() +
    theme(aspect.ratio = 9 / 16,
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 10, hjust = 0.5)) +
    scale_y_reverse()

viz_w <- viz %>%
    filter(nameConference=="West")

viz_w %>%
    ggplot(aes(ortg_q1, drtg_q1)) +
    geom_hline(yintercept=median(viz_w$drtg_q4), linetype = "dashed", alpha=.5) +
    geom_vline(xintercept=median(viz_w$ortg_q4), linetype = "dashed", alpha=.5) +
    geom_abline(slope = -1.5, intercept = c(70, 65, 60, 55, 50, 45, 40, 35), alpha = .2) +
    geom_point(color = viz_w$primary, cex=5, alpha = .6) +
    geom_point(aes(x = ortg_q2, y = drtg_q2), color = viz_w$primary, cex=6, alpha = .6) +
    geom_point(aes(x = ortg_q3, y = drtg_q3), color = viz_w$primary, cex=7, alpha = .6) +
    geom_point(aes(x = ortg_q4, y = drtg_q4), color = viz_w$primary, cex=8, alpha = .6) +
    geom_segment(aes(x = ortg_q1, y=drtg_q1, xend = ortg_q2, yend = drtg_q2), 
                 color=viz_w$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_segment(aes(x = ortg_q2, y=drtg_q2, xend = ortg_q3, yend = drtg_q3), 
                 color=viz_w$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_segment(aes(x = ortg_q3, y=drtg_q3, xend = ortg_q4, yend = drtg_q4), 
                 color=viz_w$primary, arrow=arrow(type="closed",angle=30, unit(0.2, "inches")), size=3, alpha=.4) +
    geom_text_repel(aes(x = ortg_q4, y = drtg_q4),label=viz_w$team) +
    labs(x="Offensive Rating (points scored per 100 possessions)", 
         y="Defensive Rating (points allowed per 100 possessions)", 
         title = "Rating Movement - Playoff Teams",
         subtitle = "Western Conference",
         caption = "@MambaMetrics") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=4, fontface=2) +
    theme_bw() +
    theme(aspect.ratio = 9 / 16,
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 10, hjust = 0.5)) +
    scale_y_reverse()







