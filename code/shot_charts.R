### All Shot Charts ----
library(tidyverse)
library(httr)
library(hexbin)
library(jsonlite)
library(scales)

library(prismatic)
library(extrafont)
library(cowplot)

library(RSQLite)
library(DBI)

options(dplyr.summarise.inform = FALSE)
# options(tidyverse.quiet = TRUE)

NBAdb <- dbConnect(SQLite(), "../nba_sql_db/nba_db")

shots_processed <- tbl(NBAdb, "all_shots_processed") %>%
    collect() %>%
    filter(season_year == 2023) %>%
    mutate(game_date = as_date(game_date, origin ="1970-01-01"))

league_avg_processed <- tbl(NBAdb, "league_avg_processed") %>%
    collect() %>%
    filter(season_year == 2023)

nba_schedule <- tbl(NBAdb, "all_nba_scores") %>%
    collect()

dbDisconnect(NBAdb)

# Court themes -----------------------------------------------------------------
court_themes <- list(
    light = list(
        court = 'floralwhite',
        lines = '#999999',
        text = '#222222',
        made = '#1b7837',
        missed = '#762a83',
        hex_border_size = 1,
        hex_border_color = "#000000"
    ),
    dark = list(
        court = '#000004',
        lines = '#999999',
        text = '#f0f0f0',
        made = '#1b7837',
        missed = '#762a83',
        hex_border_size = 0,
        hex_border_color = "#000000"
    )
)

# Court drawing & chart functions ----------------------------------------------

plot_court <- function(court_theme = court_themes$light, use_short_three = FALSE) {
    
    # Court dimensions & themes
    width <- 50
    height <- 94 / 2
    key_height <- 19
    inner_key_width <- 12
    outer_key_width <- 16
    backboard_width <- 6
    backboard_offset <- 4
    neck_length <- 0.5
    hoop_radius <- 0.75
    hoop_center_y <- backboard_offset + neck_length + hoop_radius
    three_point_radius <- 23.75
    three_point_side_radius <- 22
    three_point_side_height <- 14
    
    circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
        angles <- seq(0, 2 * pi, length.out = npoints)
        return(tibble(x = center[1] + radius * cos(angles),
                      y = center[2] + radius * sin(angles)))
    }
    
    if (use_short_three) {
        three_point_radius = 22
        three_point_side_height = 0
    }
    
    court_points <- tibble(
        x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
        y = c(height, 0, 0, height, height),
        desc = "perimeter"
    )
    
    court_points <- bind_rows(court_points , tibble(
        x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
        y = c(0, key_height, key_height, 0),
        desc = "outer_key"
    ))
    
    court_points <- bind_rows(court_points , tibble(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset),
        desc = "backboard"
    ))
    
    court_points <- bind_rows(court_points , tibble(
        x = c(0, 0),
        y = c(backboard_offset, backboard_offset + neck_length),
        desc = "neck"
    ))
    
    foul_circle <- circle_points(center = c(0, key_height),
                                 radius = inner_key_width / 2)
    
    foul_circle_top <- foul_circle %>% 
        filter(y > key_height) %>%
        mutate(desc = "foul_circle_top")
    
    foul_circle_bottom <- foul_circle %>% 
        filter(y < key_height) %>%
        mutate(
            angle = atan((y - key_height) / x) * 180 / pi,
            angle_group = floor((angle - 5.625) / 11.25),
            desc = paste0("foul_circle_bottom_", angle_group)
        ) %>%
        filter(angle_group %% 2 == 0) %>%
        select(x, y, desc)
    
    hoop <- circle_points(center = c(0, hoop_center_y),
                          radius = hoop_radius) %>%
        mutate(desc = "hoop")
    
    restricted <- circle_points(center = c(0, hoop_center_y), radius = 4) %>%
        filter(y >= hoop_center_y) %>%
        mutate(desc = "restricted")
    
    three_point_circle <- circle_points(center = c(0, hoop_center_y),
                                        radius = three_point_radius) %>%
        filter(y >= three_point_side_height, y >= hoop_center_y)
    
    three_point_line <- tibble(
        x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
        y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
        desc = "three_point_line"
    )
    
    court_points <- bind_rows(
        court_points,
        foul_circle_top,
        foul_circle_bottom,
        hoop,
        restricted,
        three_point_line
    )
    
    court_points <<- court_points
    
    ggplot() +
        geom_path(
            data = court_points,
            aes(x = x, y = y, group = desc),
            color = court_theme$lines
        ) +
        coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
        theme_minimal(base_size = 22) +
        theme(
            text = element_text(color = court_theme$text),
            # plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite'),
            panel.background = element_rect(fill = court_theme$court,
                                            color = court_theme$court),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_rect(fill = court_theme$court,
                                             color = court_theme$court),
            legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
            legend.position = "bottom",
            legend.key = element_blank(),
            legend.text = element_text(size = rel(1.0))
        )
}
# plot_court(court_themes$light)

generate_shot_chart <- function(shots_chart, court_theme = court_themes$dark) {
    
    # Convert shot location data to binned-hexagon data  ----------------------
    
    xbnds <- c(plyr::round_any(min(shots_chart$loc_x), 1.5, floor) - 1e-6, 
               plyr::round_any(max(shots_chart$loc_x), 1.5, ceiling) + 1e-6)
    xbins <- diff(xbnds) / 1.5
    ybnds <- c(plyr::round_any(min(shots_chart$loc_y), 1.5, floor) - 1e-6, 
               plyr::round_any(max(shots_chart$loc_y), 1.5, ceiling) + 1e-6)
    ybins <- diff(ybnds) / 1.5
    
    
    hb <- hexbin(
        x = shots_chart$loc_x,
        y = shots_chart$loc_y,
        xbins = xbins,
        xbnds = xbnds,
        ybnds = ybnds,
        shape = ybins / xbins,
        IDs = TRUE
    )
    
    shots_chart <- shots_chart %>% mutate(hexbin_id = hb@cID)
    
    hexbin_stats <- shots_chart %>%
        group_by(hexbin_id) %>%
        summarize(
            hex_attempts = n(),
            hex_pct = mean(shot_made_numeric),
            hex_points_scored = sum(shot_made_numeric * shot_value),
            hex_points_per_shot = mean(shot_made_numeric * shot_value)
        ) 
    
    hexbin_ids_to_zones <- shots_chart %>%
        group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
        summarize(attempts = n()) %>%
        ungroup() %>%
        arrange(hexbin_id, desc(attempts)) %>%
        group_by(hexbin_id) %>%
        filter(row_number() == 1) %>%
        select(hexbin_id, shot_zone_range, shot_zone_area)
    
    hexbin_stats <- inner_join(hexbin_stats, hexbin_ids_to_zones,
                               by = "hexbin_id")
    
    # from hexbin package, see: https://github.com/edzer/hexbin
    sx <- hb@xbins / diff(hb@xbnds)
    sy <- (hb@xbins * hb@shape) / diff(hb@ybnds)
    dx <- 1 / (2 * sx)
    dy <- 1 / (2 * sqrt(3) * sy)
    origin_coords <- hexcoords(dx, dy)
    
    hex_centers <- hcell2xy(hb)
    
    hexbin_coords <- bind_rows(lapply(1:hb@ncells, function(i) {
        data.frame(
            x = origin_coords$x + hex_centers$x[i],
            y = origin_coords$y + hex_centers$y[i],
            center_x = hex_centers$x[i],
            center_y = hex_centers$y[i],
            hexbin_id = hb@cell[i]
        )
    }))
    
    hex_data <- inner_join(hexbin_coords, hexbin_stats,
                           by = "hexbin_id")
    
    zone_stats <- shots_chart %>%
        group_by(shot_zone_range, shot_zone_area) %>%
        reframe(
            zone_attempts = n(),
            zone_pct = mean(shot_made_numeric),
            zone_points_scored = sum(shot_made_numeric * shot_value),
            zone_points_per_shot = mean(shot_made_numeric * shot_value),
            team_name
        ) %>%
        distinct()
    
    
    league_zone_stats <- league_avg_processed %>%
        group_by(shot_zone_range, shot_zone_area) %>%
        summarize(league_pct = sum(fgm) / sum(fga))
    
    hex_data <- hex_data %>%
        inner_join(zone_stats,
                   by = c("shot_zone_area", "shot_zone_range")) %>%
        inner_join(league_zone_stats,
                   by = c("shot_zone_area", "shot_zone_range")) %>%
        mutate(radius_factor = 0.25 + (1 - 0.25) * log(hex_attempts + 1) / log(max(hex_attempts) + 1),
               adj_x = center_x + radius_factor * (x - center_x),
               adj_y = center_y + radius_factor * (y - center_y),
               bounded_fg_diff = pmin(pmax(zone_pct - league_pct, -0.15), 0.15),
               bounded_fg_pct = pmin(pmax(zone_pct, 0.2), 0.7),
               bounded_points_per_shot = pmin(pmax(zone_points_per_shot, 0.5), 1.5)
        )
    
    return(hex_data)
    
}

generate_heatmap_chart <- function(shots, base_court, court_theme = court_themes$dark) {
    
    base_court +
        stat_density_2d(
            data = shots,
            aes(x = loc_x, y = loc_y, fill = stat(density / max(density))),
            geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
        ) +
        geom_path(
            data = court_points,
            aes(x = x, y = y, group = desc),
            color = court_theme$lines
        ) +
        scale_fill_viridis_c(
            "Shot Frequency    ",
            limits = c(0, 1),
            breaks = c(0, 1),
            labels = c("lower", "higher"),
            option = "inferno",
            guide = guide_colorbar(barwidth = 15)
        ) +
        theme(text=element_text(size=14,  family="Gill Sans MT"), 
              legend.spacing.x = unit(0, 'cm'), 
              legend.title=element_text(size=12), 
              legend.text = element_text(size = rel(0.6)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0),
              legend.key = element_blank(),
              legend.background = element_rect(fill = court_theme$court,
                                               color = court_theme$court),
              plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = -.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(0, -5, 0, -5, "cm"))
    
}

generate_scatter_chart <- function(shots, base_court, court_theme = court_themes$light, alpha = 0.8, size = 5) {
    
    base_court +
        geom_point(
            data = shots,
            aes(x = loc_x, y = loc_y, color = shot_made_flag),
            alpha = alpha, size = size
        ) +
        scale_color_manual(
            "",
            values = c(made = court_theme$made, missed = court_theme$missed)
        ) + 
        theme(text=element_text(size=14,  family="Gill Sans MT"), 
              legend.spacing.x = unit(0.24, 'cm'), 
              legend.title=element_text(size=12), 
              legend.text = element_text(size = rel(0.6)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0),
              legend.key = element_blank(),
              legend.background = element_rect(fill = court_theme$court,
                                               color = court_theme$court),
              plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = -.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(0, -5, 0, -5, "cm"))
}



# Game shot chart function players  --------------------------------------------
shot_chart_player_gm <- function(player, game) {
    
    shots_chart <- shots_processed %>%
        filter(player_name == player & game_id == game)
    
    game_info <- nba_schedule %>%
        filter(game_id == game)
    
    hd <- generate_shot_chart(shots_chart, plot_court(court_themes$dark))
    
    p <- plot_court(court_themes$light) +
        geom_polygon(
            data = hd,
            aes(
                x = adj_x,
                y = adj_y,
                group = hexbin_id, 
                fill = bounded_fg_diff, 
                color = after_scale(clr_darken(fill, .333))),
            size = .25) + 
        scale_x_continuous(limits = c(-27.5, 27.5)) + 
        scale_y_continuous(limits = c(0, 45)) +
        scale_fill_distiller(direction = -1, 
                             palette = "RdBu", 
                             limits = c(-.15, .15), 
                             breaks = seq(-.15, .15, .03),
                             labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                             "FG Percentage Points vs. League Average") +
        guides(fill=guide_legend(
            label.position = 'bottom', 
            title.position = 'top', 
            keywidth=.45,
            keyheight=.15, 
            default.unit="inch", 
            title.hjust = .5,
            title.vjust = 0,
            label.vjust = 3,
            nrow = 1))  +
        theme(text=element_text(size=14,  family="Gill Sans MT"), 
              legend.spacing.x = unit(0, 'cm'), 
              legend.title=element_text(size=12), 
              legend.text = element_text(size = rel(0.6)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0), 
              plot.title = element_text(size = 18, hjust = 0.5, vjust = -1, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = -0.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(1, -5, 1, -5, "cm")) +
        labs(title = player,
             subtitle = paste0(game_info$away_team_name, " vs ", game_info$home_team_name, "  ",
                               game_info$game_date))
    
    ggdraw(p) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
}

shot_heatmap_player_gm <- function(player, game) {
    
    shots_heatmap <- shots_processed %>%
        filter(player_name == player & game_id == game) %>%
        filter(shot_zone_basic != "Restricted Area")
    
    game_info <- nba_schedule %>%
        filter(game_id == game)
    
    hm <- generate_heatmap_chart(shots_heatmap, plot_court(court_themes$dark))
    
    p <- ggdraw(hm) + 
        theme(plot.background = element_rect(fill="black", color = NA))
    
    title <- ggdraw() + 
        draw_label(player, fontface='bold', size = 18, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(paste0(game_info$away_team_name, " vs ", game_info$home_team_name, "  ",
                          game_info$game_date), size = 14, color = "white", fontfamily = "Gill Sans MT", ) +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}

shot_scatter_player_gm <- function(player, game) {
    
    shots_scatter <- shots_processed %>%
        filter(player_name == player & game_id == game)
    
    game_info <- nba_schedule %>%
        filter(game_id == game)
    
    sc <- generate_scatter_chart(shots_scatter, plot_court(court_themes$light))
    
    p <- ggdraw(sc) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    title <- ggdraw() + 
        draw_label(player, fontface='bold', size = 18, color = "black", fontfamily = "Gill Sans MT", ) +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    subtitle <- ggdraw() +
        draw_label(paste0(game_info$away_team_name, " vs ", game_info$home_team_name, "  ",
                          game_info$game_date), size = 14, color = "black", fontfamily = "Gill Sans MT", ) +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}


# Game shot chart function teams   ---------------------------------------------
shot_chart_team_gm <- function(team, game) {
    
    shots_chart <- shots_processed %>%
        filter(team_name == team & game_id == game)
    
    game_info <- nba_schedule %>%
        filter(game_id == game)
    
    hd <- generate_shot_chart(shots_chart, plot_court(court_themes$dark))
    
    p <- plot_court(court_themes$light) +
        geom_polygon(
            data = hd,
            aes(
                x = adj_x,
                y = adj_y,
                group = hexbin_id, 
                fill = bounded_fg_diff, 
                color = after_scale(clr_darken(fill, .333))),
            size = .25) + 
        scale_x_continuous(limits = c(-27.5, 27.5)) + 
        scale_y_continuous(limits = c(0, 45)) +
        scale_fill_distiller(direction = -1, 
                             palette = "RdBu", 
                             limits = c(-.15, .15), 
                             breaks = seq(-.15, .15, .03),
                             labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                             "FG Percentage Points vs. League Average") +
        guides(fill=guide_legend(
            label.position = 'bottom', 
            title.position = 'top', 
            keywidth=.45,
            keyheight=.15, 
            default.unit="inch", 
            title.hjust = .5,
            title.vjust = 0,
            label.vjust = 3,
            nrow = 1))  +
        theme(text=element_text(size=14,  family="Gill Sans MT"), 
              legend.spacing.x = unit(0, 'cm'), 
              legend.title=element_text(size=12), 
              legend.text = element_text(size = rel(0.6)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0), 
              plot.title = element_text(size = 18, hjust = 0.5, vjust = -1, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = -.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(1, -5, 1, -5, "cm")) +
        labs(title = team,
             subtitle = paste0("vs ",if_else(game_info$away_team_name == team, 
                                             game_info$home_team_name, 
                                             game_info$away_team_name), "  ", game_info$game_date))
    
    ggdraw(p) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
}

shot_heatmap_team_gm <- function(team, game) {
    
    shots_heatmap <- shots_processed %>%
        filter(team_name == team & game_id == game) %>%
        filter(shot_zone_basic != "Restricted Area")
    
    game_info <- nba_schedule %>%
        filter(game_id == game)
    
    hm <- generate_heatmap_chart(shots_heatmap, plot_court(court_themes$dark))
    
    p <- ggdraw(hm) + 
        theme(plot.background = element_rect(fill="black", color = NA))
    
    title <- ggdraw() + 
        draw_label(team, fontface='bold', size = 18, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(paste0("vs ",if_else(game_info$away_team_name == team, 
                                        game_info$home_team_name, 
                                        game_info$away_team_name), "  ", game_info$game_date),
                   size = 14, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}

shot_scatter_team_gm <- function(team, game) {
    
    shots_scatter <- shots_processed %>%
        filter(team_name == team & game_id == game)
    
    game_info <- nba_schedule %>%
        filter(game_id == game)
    
    sc <- generate_scatter_chart(shots_scatter, plot_court(court_themes$light))
    
    p <- ggdraw(sc) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    title <- ggdraw() + 
        draw_label(team, fontface='bold', size = 18, color = "black", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(paste0("vs ",if_else(game_info$away_team_name == team, 
                                        game_info$home_team_name, 
                                        game_info$away_team_name), "  ", game_info$game_date), 
                   size = 14, color = "black", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}


# Season shot chart function players   -----------------------------------------
shot_chart_player <- function(player, season) {
    
    shots_chart <- shots_processed %>%
        filter(player_name == player & season_year == season) %>%
        mutate(season = sprintf("%d-%02d", (season_year - 1),
                                ((season_year - 1)  + 1) %% 100))
    
    hd <- generate_shot_chart(shots_chart, plot_court(court_themes$dark))
    
    p <- plot_court(court_themes$light) +
        geom_polygon(
            data = hd,
            aes(
                x = adj_x,
                y = adj_y,
                group = hexbin_id, 
                fill = bounded_fg_diff, 
                color = after_scale(clr_darken(fill, .333))),
            size = .25) + 
        scale_x_continuous(limits = c(-27.5, 27.5)) + 
        scale_y_continuous(limits = c(0, 45)) +
        scale_fill_distiller(direction = -1, 
                             palette = "RdBu", 
                             limits = c(-.15, .15), 
                             breaks = seq(-.15, .15, .03),
                             labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                             "FG Percentage Points vs. League Average") +
        guides(fill=guide_legend(
            label.position = 'bottom', 
            title.position = 'top', 
            keywidth=.45,
            keyheight=.15, 
            default.unit="inch", 
            title.hjust = .5,
            title.vjust = 0,
            label.vjust = 3,
            nrow = 1))  +
        theme(text=element_text(size=14,  family="Gill Sans MT"), 
              legend.spacing.x = unit(0, 'cm'), 
              legend.title=element_text(size=12), 
              legend.text = element_text(size = rel(0.6)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0), 
              plot.title = element_text(size = 18, hjust = 0.5, vjust = -1, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = -.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(1, -5, 1, -5, "cm")) +
        labs(title = player,
             subtitle = shots_chart$season)
    
    ggdraw(p) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
}

shot_heatmap_player <- function(player, season) {
    
    shots_heatmap <- shots_processed %>%
        filter(player_name == player & season_year == season
               & shot_zone_basic != "Restricted Area") %>%
        mutate(season = sprintf("%d-%02d", (season_year - 1),
                                ((season_year - 1)  + 1) %% 100))
    
    hm <- generate_heatmap_chart(shots_heatmap, plot_court(court_themes$dark))
    
    p <- ggdraw(hm) + 
        theme(plot.background = element_rect(fill="black", color = NA))
    
    title <- ggdraw() + 
        draw_label(player, fontface='bold', size = 18, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(shots_heatmap$season, size = 14, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}

shot_scatter_player <- function(player, season) {
    
    shots_scatter <- shots_processed %>%
        filter(player_name == player & season_year == season) %>%
        mutate(season = sprintf("%d-%02d", (season_year - 1),
                                ((season_year - 1)  + 1) %% 100))
    
    sc <- generate_scatter_chart(shots_scatter, plot_court(court_themes$light))
    
    p <- ggdraw(sc) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    title <- ggdraw() + 
        draw_label(player, fontface='bold', size = 18, color = "black", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(shots_scatter$season, size = 14, color = "black", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}


# Season shot chart function teams   -------------------------------------------
shot_chart_team <- function(team, season) {
    
    shots_chart <- shots_processed %>%
        filter(team_name == team & season_year == season) %>%
        mutate(season = sprintf("%d-%02d", (season_year - 1),
                                ((season_year - 1)  + 1) %% 100))
    
    hd <- generate_shot_chart(shots_chart, plot_court(court_themes$dark))
    
    p <- plot_court(court_themes$light) +
        geom_polygon(
            data = hd,
            aes(
                x = adj_x,
                y = adj_y,
                group = hexbin_id, 
                fill = bounded_fg_diff, 
                color = after_scale(clr_darken(fill, .333))),
            linewidth = .25) + 
        scale_x_continuous(limits = c(-27.5, 27.5)) + 
        scale_y_continuous(limits = c(0, 45)) +
        scale_fill_distiller(direction = -1, 
                             palette = "RdBu", 
                             limits = c(-.15, .15), 
                             breaks = seq(-.15, .15, .03),
                             labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                             "FG Percentage Points vs. League Average") +
        guides(fill=guide_legend(
            label.position = 'bottom', 
            title.position = 'top', 
            keywidth=.45,
            keyheight=.15, 
            default.unit="inch", 
            title.hjust = .5,
            title.vjust = 0,
            label.vjust = 3,
            nrow = 1))  +
        theme(text=element_text(size=14,  family="Gill Sans MT"), 
              legend.spacing.x = unit(0, 'cm'), 
              legend.title=element_text(size=12), 
              legend.text = element_text(size = rel(0.6)), 
              legend.margin=margin(-10,0,-1,0),
              legend.position = 'bottom',
              legend.box.margin=margin(-30,0,15,0), 
              plot.title = element_text(size = 18, hjust = 0.5, vjust = -1, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 14, vjust = -.5), 
              plot.caption = element_text(face = "italic", size = 8), 
              plot.margin = margin(1, -5, 1, -5, "cm")) +
        labs(title = team,
             subtitle = shots_chart$season)
    
    ggdraw(p) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
}

shot_heatmap_team <- function(team, season) {
    
    shots_heatmap <- shots_processed %>%
        filter(team_name == team & season_year == season
               & shot_zone_basic != "Restricted Area") %>%
        mutate(season = sprintf("%d-%02d", (season_year - 1),
                                ((season_year - 1)  + 1) %% 100))
    
    hm <- generate_heatmap_chart(shots_heatmap, plot_court(court_themes$dark))
    
    p <- ggdraw(hm) + 
        theme(plot.background = element_rect(fill="black", color = NA))
    
    title <- ggdraw() + 
        draw_label(team, fontface='bold', size = 18, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(shots_heatmap$season, size = 14, color = "white", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="black", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}

shot_scatter_team <- function(team, season) {
    
    shots_scatter <- shots_processed %>%
        filter(team_name == team & season_year == season) %>%
        mutate(season = sprintf("%d-%02d", (season_year - 1),
                                ((season_year - 1)  + 1) %% 100))
    
    sc <- generate_scatter_chart(shots_scatter, plot_court(court_themes$light))
    
    p <- ggdraw(sc) + 
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    title <- ggdraw() + 
        draw_label(team, fontface='bold', size = 18, color = "black", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    subtitle <- ggdraw() + 
        draw_label(shots_scatter$season, size = 14, color = "black", fontfamily = "Gill Sans MT") +
        theme(plot.background = element_rect(fill="floralwhite", color = NA))
    
    plot_grid(title, subtitle, p, ncol = 1, rel_heights = c(0.1, 0.1, 1))
    
}


### Run functions for game ----
shot_chart_player_gm("Luka Doncic","0022200013")
shot_heatmap_player_gm("Luka Doncic","0022200013")
shot_scatter_player_gm("Luka Doncic","0022200013")

shot_chart_team_gm("Dallas Mavericks","0022200013")
shot_heatmap_team_gm("Dallas Mavericks","0022200013")
shot_scatter_team_gm("Dallas Mavericks","0022200013")


### Run functions for season ----
shot_chart_player("Luka Doncic", 2023)
shot_heatmap_player("Luka Doncic", 2023)
shot_scatter_player("Luka Doncic", 2023)

shot_chart_team("Dallas Mavericks", 2023)
shot_heatmap_team("Dallas Mavericks", 2023)
shot_scatter_team("Dallas Mavericks", 2023)




#### fix league average for selected year & game info

