# Team Radar Charts -----------------------------------
library(RSQLite)
library(DBI)
library(dplyr)
library(fmsb)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
# connect to db
NBAdb <- DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db")
# read in team box scores
box_score_team <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db"), "BoxScoreTeam")
box_score_team <- box_score_team %>%
    collect()
# read in team dictionary
team_dict <- dplyr::tbl(DBI::dbConnect(RSQLite::SQLite(), "../nba_sql_db/nba_db"), "TeamDictionary")
team_dict <- team_dict %>%
    collect()
# disconnect from db
DBI::dbDisconnect(NBAdb)

# self join for easy opponent stats
cm <- box_score_team %>%
    left_join(box_score_team, by = "idGame")
# create team ff stats
tm <- cm %>%
    filter(substr(idGame, 1,3) == 221) %>%
    select(2,4:13,16:50,134:143,146:180) %>%
    group_by(slugTeam.x) %>%
    summarise(across(c(fgm.x:possessions.y), sum)) %>%
    mutate(efg = (fgm.x + .5 * fg3m.x) / fga.x) %>%
    mutate(tov = tov.x / possessions.x) %>%
    mutate(orb = oreb.x / (oreb.x+dreb.y)) %>%
    mutate(ftr = ftm.x / fga.x) %>%
    mutate(oefg = (fgm.y + .5 * fg3m.y) / fga.y) %>%
    mutate(otov = tov.y / possessions.y) %>%
    mutate(drb = dreb.x / (oreb.y+dreb.x)) %>%
    mutate(oftr = ftm.y / fga.y) %>%
    select(1,92:99) %>%
    rename("team" = "slugTeam.x")
    

# max_min <- data.frame(
#     efg = c(max(tm$efg), min(tm$efg)), tov = c(max(tm$tov), min(tm$tov)), orb = c(max(tm$orb), min(tm$orb)), ftr = c(max(tm$ftr), min(tm$ftr)), 
#     oefg = c(max(tm$oefg), min(tm$oefg)), otov = c(max(tm$otov), min(tm$otov)), drb = c(max(tm$drb), min(tm$drb)), oftr = c(max(tm$oftr), min(tm$oftr))
# )
# 
# rownames(max_min) <- c("Max", "Min")
# tm <- tibble::column_to_rownames(tm, var = "team")
# tm <- rbind(max_min, tm)
# 
# viz <- tm[c(1,2,19), ]
# radarchart(viz,
#            vlcex = 1.5,
#            pcol = "#00471b",
#            pfcol = scales::alpha("#00471b", .7),
#            plwd = 2,
#            plty = 1,
#            cglcol = "grey", cglty = 1, cglwd = 0.8,
#            axislabcol = "grey",
#            title = "Milwaukee Bucks")

# create team ff ranked
tr <- tm

a_list <- list("tov","oefg","oftr")
tr_a <- tr %>%
    mutate_if(grepl(paste(a_list, collapse = "|"), names(.)), list(rank=~rank(-.)))

d_list <- list("efg","orb","ftr","otov","drb")
tr_d <- tr %>%
    mutate_if(grepl(paste(d_list, collapse = "|"), names(.)), list(rank=~rank(.)))

tr_rank <- tr %>%
    left_join(tr_d[,c(1,10:12,14,15)], by = "team") %>%
    left_join(.,tr_a[,c(1,10,11,13)], by = "team") %>%
    select(1,10,11,12,15,16,14,17,13)
    
max_min <- data.frame(
    eFG = c(30,0), ORB = c(30,0), FTR = c(30,0), TOV = c(30,0), 
    oeFG = c(30,0), DRB = c(30,0), oFTR = c(30,0), oTOV = c(30,0)
)

rownames(max_min) <- c("Max", "Min")
tm <- tibble::column_to_rownames(tr_rank, var = "team")
colnames(tm) <- c("eFG","ORB","FTR","TOV",
                  "oeFG","DRB","oFTR","oTOV")
tm <- rbind(max_min, tm)

viz <- tm[c("Max", "Min", "MIN"), ]
radarchart(viz,
           vlcex = 1.5,
           pcol = "#005083",
           pfcol = scales::alpha("#005083", .7),
           plwd = 2,
           plty = 1,
           cglcol = "grey", cglty = 1, cglwd = 0.8,
           axislabcol = "grey",
           title = "Minnesota Timberwolves")





