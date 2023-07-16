Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

### Animated Shot Charts ----

library(nbastatR)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(ggalt)

tm_shots <- teams_shots(teams = "Los Angeles Lakers",
                        seasons = 2023)

shots <- tm_shots

shots <- shots %>% 
    mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>% 
    mutate(time_chron=case_when(
        numberPeriod==1~ms("12:00")-gtime,
        numberPeriod==2~ms("24:00")-gtime,
        numberPeriod==3~ms("36:00")-gtime,
        numberPeriod==4~ms("48:00")-gtime,
        numberPeriod==5~ms("52:00")-gtime)) %>%
    mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))


shots_anim_line <- shots %>%
    ggplot() +
    geom_hline(yintercept = 23.9,linetype=2,color="gray") +
    annotate("text",label="from downtown!",x=700,26.5,size=5,alpha=0.5,color="grey70") +
    geom_vline(xintercept = as.numeric(ms("48:00")),linetype=3,color="red") +
    geom_lollipop(aes(x=time_chron,y=distTrans,
                      color=isShotMade)) +
    labs(y="shot distance (feet)",
         x="time (minutes)", title = "Lakers Shot Chart") +
    scale_x_time(breaks = ms(c("12:00","24:00","36:00","48:00"))) +
    scale_color_manual(values = c("#00529b","#cc4b4b"),labels=c("made","missed")) +
    theme_bw() +
    theme(text = element_text(size = 19),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=19)) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    transition_states(idEvent) +
    shadow_mark()

# animate and export
shots_anim_line <- animate(shots_anim_line,height=800,width=800)
shots_anim_line
#anim_save(path = here::here(),animation = scoringAnim,filename = "bballjh.gif")  


source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")
plot_court() 
court_points <- court_points %>% mutate_if(is.numeric,~.*10)


shots_anim_court <- shots %>%
    ggplot(aes(x=locationX, y=locationY+45)) + 
    scale_fill_manual(values = c("#00529b","#cc4b4b"),guide=FALSE) +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = "black") +
    coord_equal()+
    geom_point(aes(fill=isShotMade),pch=21,size=4,color="white") +
    xlim(-260, 260) +
    theme_bw() +
    labs(title="Shot location",x="",
         y="",
         caption = "by @LuisDVerde\nNBA data accessed with nbastatr") +
    theme(text = element_text(size = 19),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          plot.caption = element_text(color="white")) +
    transition_states(idEvent) +
    shadow_mark()

shots_anim_court <- animate(shots_anim_court,height=800,width=800,bg="#272822")
shots_anim_court





# Combine plots side by side, by Patrick Toche
combine_gifs <- function(plot1, plot2) {
    # read the plots and store them
    plot1 <- image_read(plot1) 
    plot2 <- image_read(plot2) 
    # sync the number of frames in each plot
    n1 = length(plot1)
    n2 = length(plot2)
    # match number of frames of the two plots
    if (!(n1 == n2)) plot1 <- rep(plot1, n2) 
    if (!(n1 == n2)) plot2 <- rep(plot2, n1)
    # initialize the combined plot
    p <- image_append(c(plot1[1], plot2[1]))
    # grow the combined plot frame by frame
    n = ifelse(n1 == n2, n1, n1 * n2) 
    n = min(1000, n)  # set max to 1000
    for (i in 2:(n-1)) {
        tmp <- image_append(c(plot1[i], plot2[i]))
        p <- c(p, tmp)
    }
    return(p) 
}

library(magick)
gifs <- combine_gifs(shots_anim_line, shots_anim_court)
# gifs %>% image_write("lal.gif")






# team shot chart for current season
hou <- teams_shots(teams = "Houston Rockets",
                   seasons = 2019)


last3games <- hou %>% select(idGame) %>% 
    unique() %>% top_n(3) %>% pull(idGame)
JHshots <- 
    hou %>% filter(namePlayer=="James Harden") %>% 
    filter(idGame %in% last3games) 

library(ggplot2)
library(gganimate)
library(lubridate)
library(ggalt)
library(artyfarty)

# chronological game time
JHshots <- JHshots %>% mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>% 
    mutate(time_chron=case_when(
        numberPeriod==1~ms("12:00")-gtime,
        numberPeriod==2~ms("24:00")-gtime,
        numberPeriod==3~ms("36:00")-gtime,
        numberPeriod==4~ms("48:00")-gtime,
        numberPeriod==5~ms("52:00")-gtime))

# for better plotting
JHshots <- JHshots %>% mutate(opponent=if_else(slugTeamAway!="HOU",slugTeamAway,slugTeamHome)) %>% 
    mutate(opponent=paste("vs",opponent,dateGame))
JHshots <- JHshots %>% 
    mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))

# plot
scoringD <- 
    ggplot(JHshots) +
    geom_hline(yintercept = 23.9,linetype=2,color="gray") +
    annotate("text",label="from downtown!",x=800,26.5,size=5,alpha=0.5) +
    geom_vline(xintercept = as.numeric(ms("48:00")),linetype=3,color="red") +
    geom_lollipop(aes(x=time_chron,y=distTrans,
                      color=isShotMade)) +
    labs(y="shot distance (feet) \n *excludes dunks and free throws",
         x="time (minutes)", title = "James Harden")+
    scale_x_time(breaks = ms(c("12:00","24:00","36:00","48:00"))) +
    scale_color_manual(values = c("#00529b","#cc4b4b"),labels=c("made","missed")) +
    facet_wrap(~opponent,ncol = 1) +
    theme_farty() +
    theme(text = element_text(size = 19),
          strip.background = element_blank(),
          strip.text = element_text(family = "sans"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=19)) +
    transition_states(idEvent)+shadow_mark()

# animate and export
scoringAnim <- animate(scoringD,height=900, width=800)
scoringAnim
#anim_save(path = here::here(),animation = scoringAnim,filename = "bballjh.gif")  

















# team shots
TeamShots <- teams_shots(teams = c("Milwaukee Bucks"),
                         seasons = 2022, season_types = "Playoffs")

TeamShots$zoneBasic <- factor(TeamShots$zoneBasic, levels=c("Restricted Area", "In The Paint (Non-RA)", "Mid-Range",
                                                            "Left Corner 3", "Right Corner 3", "Above the Break 3",
                                                            "Backcourt"))

ts_team <- TeamShots %>%
    filter(idGame==42100213) %>%
    group_by(nameTeam, zoneBasic) %>%
    summarise(across(c(isShotAttempted,isShotMade),sum)) %>%
    mutate(pct = round(isShotMade/isShotAttempted,3))

ts_player <- TeamShots %>%
    filter(idGame==42100213) %>%
    group_by(nameTeam, namePlayer, zoneBasic) %>%
    summarise(across(c(isShotAttempted,isShotMade),sum)) %>%
    mutate(pct = round(isShotMade/isShotAttempted,3))



# animated shot chart

# shots vs Bos
shots <- TeamShots %>%
    filter(namePlayer == "Giannis Antetokounmpo" & idGame == 42100213)

library(ggplot2)
library(gganimate)
library(lubridate)
library(ggalt)
library(artyfarty) #devtools::install_github('datarootsio/artyfarty')


# chronological game time
shots <- shots %>% mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>%
    mutate(time_chron=case_when(
        numberPeriod==1~ms("12:00")-gtime,
        numberPeriod==2~ms("24:00")-gtime,
        numberPeriod==3~ms("36:00")-gtime,
        numberPeriod==4~ms("48:00")-gtime,
        numberPeriod==5~ms("52:00")-gtime))

# make short shots visible
shots <- shots %>%
    mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))

# plot
scoring <-
    ggplot(shots) +
    geom_hline(yintercept = 23.9,linetype=2,color="gray") +
    annotate("text",label="For Three!!!",x=700,26.5,size=5,alpha=0.5,color="grey70") +
    geom_vline(xintercept = as.numeric(ms("48:00")),linetype=3,color="red") +
    geom_lollipop(aes(x=time_chron,y=distTrans,
                      color=isShotMade)) +
    labs(y="shot distance (feet) \n *excludes dunks and free throws",
         x="time (minutes)", title = "Devin Booker vs. Boston - March 24 2017") +
    scale_x_time(breaks = ms(c("12:00","24:00","36:00","48:00")))+
    scale_color_manual(values = c("#00529b","#cc4b4b"),labels=c("made","missed")) +
    theme_monokai_full() +
    theme(text = element_text(size = 19),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=19)) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    transition_states(idEvent) +
    shadow_mark()

# animate and export
scoringAnim <- animate(scoring,height=800,width=800)
scoringAnim
#anim_save(path = here::here(),animation = scoringAnim,filename = "bballjh.gif")


# to plot a half court, using data from the ballR shiny app
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")
plot_court() # created the court_points object we need
court_points <- court_points %>% mutate_if(is.numeric,~.*10)

# y coordinates are shifted to account for the backboard+rim in the ballr data
court <-
    ggplot(shots, aes(x=locationX, y=locationY+45)) +
    scale_fill_manual(values = c("red","green"),guide=FALSE) +
    # scale_fill_manual(values = c("#00529b","#cc4b4b"),guide=FALSE) +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc),
              color = "white") +
    coord_equal() +
    geom_point(aes(fill=isShotMade),pch=21,size=6,color="white") +
    xlim(-260, 260) +
    theme_monokai_full() +
    labs(title="Giannis Antetokoumnpo Shot Chart - Game 3", x="",
         y="",
         caption = "@MambaMetrics | Data Courtesy of nbastatR") +
    theme(text = element_text(size = 19),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          plot.caption = element_text(color="white")) +
    transition_states(idEvent) +
    shadow_mark() +
    ease_aes('cubic-in-out')

DBcourtanim <- animate(court,height=800,width=800,bg="#272822",nframes = 200, fps=10)
DBcourtanim

anim_save(path = "./output/",
          animation = DBcourtanim, filename = "shots.gif")