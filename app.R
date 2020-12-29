# HARRISON BASKETBALL ANALYTICS 2020-21
# updated through 12/20/2020

library(tidyverse)
library(xml2)
library(rvest)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(rsconnect)
library(knitr)
library(docstring)
library(DT)
library(kableExtra)
library(reactable)

# a constructor function for the "opponent" class
opponent <- function(school, venue, url, status = "approved", box = NULL) {
  #' school: name of opponent school
  #' venue: Home or Away
  #' status: status of opponent stats on MaxPreps (approved/empty/incorrect)
  #' url: url of MaxPreps box score for game
  #' box: opponent box score for game if they do not publish their stats
  
  value <- list(school = school, venue = venue, url = url, status = status, box = box)
  attr(value, "class") <- "opponent"
  value
}

#implement a printing method for the "opponent" class
print.opponent <- function(obj) {
  cat("School: ", obj$school, "\n")
  cat("MaxPreps status: ", obj$status, "\n")
  cat("MaxPreps url: ", obj$url)
}


# Create object of "opponent" class for each game
# assign opponent school names, home/away status, MaxPreps urls, and opponent stat line if opponent has not published stats
# stat line categories:
# Pts, FG, FGA, FG%, 3FG, 3FGA, 3FG%, 2FG, 2FGA, 2FG%, FT, FTA, FT%, Off, Def, Tot Reb, Ast, Stls, Blk, TO, PF
university <- opponent("University",
                       "Away",
                       "https://www.maxpreps.com/games/12-8-2020/basketball-winter-20-21/harrison-vs-university.htm?c=BwIWIEJuv0KEbgjimxD-sQ#tab=box-score&schoolid=",
                       "incorrect",
                       data.frame('Pts' = 48,  'FG' = 20,  'FGA' = 43, 'FG%' = 46, '3FG' = 5, '3FGA' = 13, '3FG%' = 38, 
                                  '2FG' = 15, '2FGA' = 30, '2FG%' = 50, 'FT' = 3, 'FTA' = 6, 'FT%' = 50, 
                                  'Off' = 7, 'Def' = 12, 'Tot Reb' = 19, 'Ast' = 8, 'Stls' = 0, 'Blk' = 0, 'TO' = 18, 'PF' = 8,
                                  check.names = FALSE))

jeff <- opponent("Lafayette Jeff",
                 "Away",
                 "https://www.maxpreps.com/games/12-11-2020/basketball-winter-20-21/harrison-vs-lafayette-jefferson.htm?c=xNHaqbzXpkOtd_9jnupqcg#tab=box-score&schoolid=")

north_mont <- opponent("North Montgomery",
                       "Away",
                       "https://www.maxpreps.com/games/12-12-2020/basketball-winter-20-21/harrison-vs-north-montgomery.htm?c=ZkpVLhzYbkGpt0OQx42z3g#tab=box-score&schoolid=")

rossville <- opponent("Rossville",
                      "Home",
                      "https://www.maxpreps.com/games/12-18-2020/basketball-winter-20-21/harrison-vs-rossville.htm?c=GkNCRqwr902HafASFz4PJg#tab=box-score&schoolid=")

lebanon <- opponent("Lebanon",
                    "Home",
                    "https://www.maxpreps.com/games/12-19-2020/basketball-winter-20-21/harrison-vs-lebanon.htm?c=GNr75k3hIEmChJDJQuUIaw#tab=box-score&schoolid=",
                    "incorrect",
                    data.frame('Pts' = 35,  'FG' = 13,  'FGA' = 36, 'FG%' = 36, '3FG' = 4, '3FGA' = 15, '3FG%' = 27, 
                               '2FG' = 9, '2FGA' = 21, '2FG%' = 43, 'FT' = 5, 'FTA' = 11, 'FT%' = 45, 
                               'Off' = 5, 'Def' = 23, 'Tot Reb' = 28, 'Ast' = 9, 'Stls' = 1, 'Blk' = 1, 'TO' = 19, 'PF' = 17,
                               check.names = FALSE))

tri_west <- opponent("Tri-West",
                     "Away",
                     "https://www.maxpreps.com/games/12-23-2020/basketball-winter-20-21/harrison-vs-tri-west-hendricks.htm?c=AG6rQYDA8kO-EG_c-wIfDA#tab=box-score&schoolid=")


# Add each opponent to list
opponent_list <- list(university,
                      jeff,
                      north_mont,
                      rossville,
                      lebanon,
                      tri_west)


# Functions to be used...
scrape_box_score <- function(url, node) {
  #' scrapes box score tables in MaxPreps
  #' @param url the url of MaxPreps page that is to be scraped
  #' @param node the html node that is to be scraped
  
  scraped_data <- read_html(url) %>%                     
    html_nodes(node) %>%
    html_table()
}


mutate_player_box_columns <- function(df) {
  #' renames and creates common player box score categories
  #' @param df the dataframe that is to be modified
  
  df %>%
    mutate(MIN = Min,
           OREB = OReb,
           DREB = DReb,
           REB = Reb,
           AST = Ast,
           STL = Stl,
           BLK = Blk,
           CHRG = Chr,
           PTS = Pts,
           `FG%` = ifelse(FGA == 0, NA, round(FGM / FGA * 100, 2)),
           `2FG%` = ifelse(`2FGA` == 0, NA, round(`2FGM` / `2FGA` * 100, 2)),
           `3P%` = ifelse(`3PA` == 0, NA, round(`3PM` / `3PA` * 100, 2)),
           `FT%` = ifelse(`FTA` == 0, NA, round(FTM / FTA * 100, 2)))
}


mutate_team_box_columns <- function(df) {
  #' renames and creates common team box score categories
  #' @param df the dataframe that is to be modified
  
  df %>%
    mutate(FGM = FG,
           `2FGM` = `2FG`,
           `3PM` = `3FG`,
           `3PA` = `3FGA`,
           `3P%` = `3FG%`,
           FTM = FT,
           OREB = Off,
           DREB = Def,
           REB = `Tot Reb`,
           AST = Ast,
           STL = Stls,
           BLK = Blk,
           PTS = Pts,
           CHRG = Chrg)
}


convert_scraped_data_to_game_totals <- function(scraped_data, school) {
  #' converts raw box score tables to team totals for a game
  #' @param scraped_data data returned from scrape_box_Score function
  #' @param school name of opposing school
  
  box_score <- scraped_data[[1]]
  for (table in scraped_data[2:6]) {
    box_score <- full_join(box_score, table)
  }
  team_single_game_totals <- box_score[1, ]
  team_single_game_totals <- team_single_game_totals %>%
    mutate(Opponent = `Athlete Name`, .after = `#`) %>%
    select(-`Athlete Name`)
  team_single_game_totals$Opponent <- school
  
  invisible(team_single_game_totals)
}


compute_team_game_totals <- function(opponent_list){
  #' returns a table of Harrison's team stat totals for each game of the season
  #' @param opponent_list list of Harrison's opponents
  
  all_games <- data.frame()
  
  for (opponent in opponent_list) {
    
    if (opponent$venue == "Away" & opponent$status != "empty" ) {
      harrison_data <- scrape_box_score(opponent$url, ".team-list__team:nth-child(1) .stats-grid")
      
    } else if (opponent$venue == "Home" & opponent$status != "empty") {
      harrison_data <- scrape_box_score(opponent$url, ".team-list__team+ .team-list__team .stats-grid")
      
    } else {
      harrison_data <- scrape_box_score(opponent$url, ".stats-grid")
    }
    
    game_totals <- convert_scraped_data_to_game_totals(harrison_data, opponent$school)
    all_games <- rbind(all_games, game_totals)
  }
  invisible(all_games)
}


compute_opponent_game_totals <- function(opponent_list){
  #' returns a table of Harrison's opponents' team stat totals for each game of the season
  #' @param opponent_list list of Harrison's opponents
  
  all_games <- data.frame()
  
  for (opponent in opponent_list) {
    
    if (opponent$venue == "Away" & opponent$status == "approved") {
      opponent_data <- scrape_box_score(opponent$url, ".team-list__team+ .team-list__team .stats-grid")
      game_totals <- convert_scraped_data_to_game_totals(opponent_data, opponent$school)
      
    } else if (opponent$venue == "Home" & opponent$status == "approved") {
      opponent_data <- scrape_box_score(opponent$url, ".team-list__team:nth-child(1) .stats-grid")
      game_totals <- convert_scraped_data_to_game_totals(opponent_data, opponent$school)
      
    } else {
      game_totals1 <- opponent$box
      #need to cbind these in order to rbind with same number of columns
      game_totals2 <- data.frame('#' = NA,
                                 'Opponent' = opponent$school,
                                 'Defl' = NA,
                                 'Chrg' = NA,
                                 'TF' = NA,
                                 check.names = FALSE)
      game_totals <- cbind(game_totals1, game_totals2)
      
    }
    all_games <- rbind(all_games, game_totals)
  }
  invisible(all_games)
}


# Create a season box score for Harrison's players
player_season_stats_url <- "https://www.maxpreps.com/high-schools/harrison-raiders-(west-lafayette,in)/basketball/stats.htm"

player_maxpreps_data <- scrape_box_score(player_season_stats_url, ".stats-grid")
player_stats_raw <- player_maxpreps_data[[2]]
for (table in player_maxpreps_data[3:5]) {
  player_stats_raw <- full_join(player_stats_raw, table)
}

player_stats_raw[ , 3:length(player_stats_raw)] <- sapply(player_stats_raw[ , 3:length(player_stats_raw)], as.numeric)
player_box <- player_stats_raw[-c(1, 2), ]

player_box <- player_box %>%
  separate(`Athlete Name`, c('Player', 'Class'), sep = "[\\(]") %>%
  mutate(Class = str_remove(Class, '[\\)]')) %>%
  mutate_player_box_columns() %>%
  mutate(across(.cols = -c('Player', '#', 'Class', 'FG%', '2FG%', '3P%', 'FT%'), .fns = ~replace_na(.x, 0))) %>%
  filter(Player != 'T. Stat') %>%
  select(Player, `#`, Class, GP, MIN, 
         FGM, FGA, `FG%`, 
         `2FGM`, `2FGA`, `2FG%`, 
         `3PM`, `3PA`, `3P%`, 
         FTM, FTA, `FT%`, 
         OREB, DREB, REB, 
         AST, STL, BLK, TO, CHRG, PF, PTS) %>%
  arrange(desc(MIN))


# Create a table with Harrison's team stat totals for each game of the season
team_game_totals <- compute_team_game_totals(opponent_list) %>%
  mutate_team_box_columns() %>%
  select(Opponent, PTS, FGM, FGA, `FG%`, 
         `2FGM`, `2FGA`, `2FG%`, 
         `3PM`, `3PA`, `3P%`, 
         FTM, FTA, `FT%`, 
         OREB, DREB, REB, 
         AST, STL, BLK, TO, PF, CHRG)


# Create a table with Harrison's opponents' team stat totals for each game of the season
opponent_game_totals <- compute_opponent_game_totals(opponent_list) %>%
  mutate_team_box_columns() %>%
  select(Opponent, PTS, 
         FGM, FGA, `FG%`, 
         `2FGM`, `2FGA`, `2FG%`, 
         `3PM`, `3PA`, `3P%`, 
         FTM, FTA, `FT%`, 
         OREB, DREB, REB, 
         AST, STL, BLK, TO, PF)


# Assign opponent inputs for advanced stat formulas
opp_OREB <- sum(opponent_game_totals$OREB)
opp_DREB <- sum(opponent_game_totals$DREB)
opp_FGA <- sum(opponent_game_totals$FGA)
opp_3FGA <- sum(opponent_game_totals$`3PM`)
opp_2FGA <- sum(opponent_game_totals$`2FGA`)
opp_FTA <- sum(opponent_game_totals$FTA)
opp_TO <- sum(opponent_game_totals$TO)


# Assign Harrison inputs for advanced stat formulas
team_season_totals <- player_stats_raw[1, ] %>%
  mutate_player_box_columns() %>%
  select(GP, MIN, PTS, FGM, FGA, `FG%`, 
         `2FGM`, `2FGA`, `2FG%`, 
         `3PM`, `3PA`, `3P%`, 
         FTM, FTA, `FT%`, 
         OREB, DREB, REB, 
         AST, STL, BLK, TO, PF, CHRG)

team_possessions <- (0.5 * ( team_season_totals$FGA + 0.475 * team_season_totals$FTA - team_season_totals$OREB + team_season_totals$TO )) + (0.5 * (opp_FGA + (0.475 * opp_FTA) - opp_OREB + opp_TO))
team_minutes <- team_season_totals$MIN
team_PTS <- team_season_totals$PTS
team_AST <- team_season_totals$AST
team_TO <- team_season_totals$TO
team_FGM <- team_season_totals$FGM
team_FGA <- team_season_totals$FGA
team_FTM <- team_season_totals$FTM
team_FTA <- team_season_totals$FTA
team_3PM <- team_season_totals$`3PM`
team_3PA <- team_season_totals$`3PA`
team_OREB <- team_season_totals$OREB
team_DREB <- team_season_totals$DREB


#Create advanced box score
player_adv <- player_box %>%
  mutate(
    #formula inputs
    qAST = ((MIN / (team_minutes / 5)) * (1.14 * ((team_AST - AST) / team_FGM))) + ((((team_AST / team_minutes) * MIN * 5 - AST) / ((team_FGM / team_minutes) * MIN * 5 - FGM)) * (1 - (MIN / (team_minutes / 5)))),
    FG_Part = ifelse(FGA == 0, 0, FGM * (1 - 0.5 * ((PTS - FTM) / (2 * FGA)) * qAST)),
    AST_Part = 0.5 * (((team_PTS - team_FTM) - (PTS - FTM)) / (2 * (team_FGA - FGA))) * AST,
    FT_Part = ifelse(FTA == 0, 0, (1 - ( 1- (FTM / FTA))^2)*0.475*FTA),
    team_Scoring_Poss = team_FGM + (1 - (1 - (team_FTM / team_FTA))^2) * team_FTA * 0.475,
    team_Play_pct = team_Scoring_Poss / (team_FGA + team_FTA * 0.475 + team_TO),
    team_OREB_pct = team_OREB / (team_OREB + (opp_DREB)),
    team_OREB_Weight = ((1 - team_OREB_pct ) * team_Play_pct) / ((1 - team_OREB_pct) * team_Play_pct + team_OREB_pct * (1 - team_Play_pct)),
    OREB_Part = OREB * team_OREB_Weight * team_Play_pct,
    missed_FG_possessions = (FGA - FGM) * (1 - 1.07 * team_OREB_pct),
    missed_FT_possessions = ifelse(FTA == 0, 0, ((1 - (FTM / FTA))^2) * 0.475 * FTA),
    scoring_possessions = (FG_Part + AST_Part + FT_Part) * (1 - (team_OREB / team_Scoring_Poss) * team_OREB_Weight * team_Play_pct) + OREB_Part,
    player_possessions = scoring_possessions + missed_FG_possessions + missed_FT_possessions + TO,
    PProd_FG_Part = ifelse(FGA == 0, 0, 2 * (FGM + 0.5 * `3PM`) * (1 - 0.5 * ((PTS - FTM) / (2 * FGA)) * qAST)),
    PProd_AST_Part = 2 * ((team_FGM - FGM + 0.5 * (team_3PM - `3PM`)) / (team_FGM - FGM)) * 0.5 * (((team_PTS - team_FTM) - (PTS - FTM)) / (2 * (team_FGA - FGA))) * AST,
    PProd_OREB_Part = OREB * team_OREB_Weight * team_Play_pct * (team_PTS / (team_FGM + (1 - (1 - (team_FTM / team_FTA))^2) * 0.475 * team_FTA)),
    individual_points_prod = (PProd_FG_Part + PProd_AST_Part + FTM) * (1 - (team_OREB / team_Scoring_Poss) * team_OREB_Weight * team_Play_pct) + PProd_OREB_Part,
    #stat categories
    ORtg = 100 * (individual_points_prod / player_possessions),
    VPS = (PTS + REB + (2 * (AST + CHRG + STL + BLK))) / ((FTA - FTM) + (2 * ((FGA - FGM) + PF + TO))),
    `Floor%` = scoring_possessions * 100 / player_possessions, 
    `%Min` = MIN / (team_minutes / 5), 
    `%Poss` = player_possessions * 100 / (`%Min` * team_possessions), 
    `Pts/Shot` = PTS / FGA,
    `Reb/Min` = REB * 100 / MIN,
    `Fouls/32` = PF * 32 / MIN, 
    `Attempts/Min` = FGA * 100 / MIN, 
    FTRate = FTA * 100 / FGA,
    `AST/TO` = AST / TO,
    `Blk/Min` = BLK / MIN * 100,
    `eFG%` = (FGM + 0.5 * `3PM`) * 100 / FGA,
    `TS%` = PTS * 100 / (2 * (FGA + 0.475 * FTA)),
    Possessions = player_possessions,
    `%Shots` = FGA * 100 / (`%Min` * team_FGA),
    `OREB%` = OREB * 100 / ( (`%Min`) * (team_OREB + opp_DREB) ),
    `DREB%` = DREB * 100 / ( (`%Min`) * (team_DREB + opp_OREB) ),
    ARate = (AST * 100) / ( (`%Min` * team_FGM) - FGM),
    `BLK%` = BLK * 100 / (`%Min` * opp_2FGA), 
    `STL%` = STL * 100 / (`%Min` * team_possessions), 
    `Usage%` = 100 * (FGA + 0.475 * FTA + TO) / (`%Min` * (team_FGA + 0.475 * team_FTA + team_TO)),
    `TO%` = TO * 100 / (FGA + (0.475 * FTA) + TO), 
    TORate = TO * 100 / player_possessions, 
    `FT` = paste(FTM, FTA, sep = "-"),
    `2P` = paste(FGM - `3PM`, FGA - `3PA`, sep = "-"),
    `2P%` = (FGM - `3PM`) * 100 / (FGA - `3PA`),
    `3P` = paste(`3PM`, `3PA`, sep = "-"),
    Share3PA = `3PA` * 100 / team_3PA,
    `3PRate` = `3PA` * 100 / FGA,
    `%Min` = `%Min` * 100) %>%
  select(Player, `#`, Class, GP, `%Min`, 
         ORtg, VPS, `Floor%`, Possessions, `%Poss`, `%Shots`, 
         `eFG%`, `TS%`, `Pts/Shot`, 
         `OREB%`, `DREB%`, ARate, TORate, `AST/TO`, 
         `BLK%`, `STL%`, `Fouls/32`, FTRate, `3PRate`, 
         `FT`, `FT%`, `2P`, `2P%`, `3P`, `3P%`, Share3PA) %>%
  arrange(desc(Possessions))

player_adv <- player_adv %>%
  mutate(across(.cols = c('VPS', 'Pts/Shot', 'AST/TO', 'BLK%', 'STL%'), .fns = ~ round(.x, digits = 2)),
         across(.cols = c('%Min', 'ORtg', 'Floor%', 'Possessions', 
                          '%Poss', '%Shots', 'eFG%', 'TS%', 'OREB%', 
                          'DREB%', 'ARate', 'TORate', 'Fouls/32', 
                          'FTRate', '3PRate', 'FT%', '2P%', '3P%', 'Share3PA'), 
                .fns = ~ round(.x, digits = 1)))


multiply_by_100 <- function(x) {
  x * 100
}


# Offense
experience_table <- player_adv %>%
  mutate(
    years = ifelse(Class == 'Fr', 0,
                   ifelse(Class == 'So', 1,
                          ifelse(Class == "Jr", 2,
                                 ifelse(Class == "Sr", 3, NA)))),
    exp_weight = (`%Min` / 100) * years
  ) %>%
  select(Player, Class, years, `%Min`, exp_weight)
experience <- round(sum(experience_table$exp_weight) / 5, 2)

team_adv <- team_season_totals %>%
  mutate(
    possessions = FGA - OREB + TO + 0.475*FTA,
    Efficiency = PTS / possessions,
    Tempo = possessions * 32 / (MIN / 5),
    `eFG%` = (FGM + 0.5 * `3PM`) / FGA,
    `TO%` = TO / possessions,
    `OREB%` = OREB / (OREB + opp_DREB),
    FTRate = FTA / FGA,
    `3PRate` = `3PA` / FGA,
    ARate = AST / FGM,
    #`Fouls/32` = PF * 32 / (MIN / 5), 
    `3P%` = `3PM` / `3PA`,
    `2P%` = `2FGM` / `2FGA`,
    `FT%` = FTM / FTA,
    `3P_distr` = (`3PM` * 3) / PTS,
    `2P_distr` = (`2FGM` * 2) / PTS,
    FT_distr = FTM / PTS,
    Experience = experience,
    .keep = "none"
  ) %>%
  mutate(across(.cols = -c(Tempo, Experience), .fns = multiply_by_100),
         #across(.cols = Experience, .fns = ~ round(.x, digits = 2)),
         across(.cols = , .fns = ~ round(.x, digits = 1))) %>%
  select(-possessions)
names(team_adv)[names(team_adv) == '3P_distr'] <- "3-pointers"
names(team_adv)[names(team_adv) == '2P_distr'] <- "2-pointers"
names(team_adv)[names(team_adv) == 'FT_distr'] <- "Free Throws"

tempo <- team_adv$Tempo

offense <- team_adv[1,]
offense <- t(offense)


# Defense
opponent_season_totals <- opponent_game_totals %>%
  mutate(across(.cols = -1, .fns = sum),
         `FG%` = ifelse(FGA == 0, NA, round(FGM / FGA * 100, 2)),
         `2FG%` = ifelse(`2FGA` == 0, NA, round(`2FGM` / `2FGA` * 100, 2)),
         `3P%` = ifelse(`3PA` == 0, NA, round(`3PM` / `3PA` * 100, 2)),
         `FT%` = ifelse(`FTA` == 0, NA, round(FTM / FTA * 100, 2)),
         GP = nrow(.), .before = `PTS`)
opponent_season_totals <- opponent_season_totals[1, -1]

opponent_adv <- opponent_season_totals %>%
  mutate(
    possessions = FGA - OREB + TO + 0.475*FTA,
    Efficiency = PTS / possessions,
    Tempo = tempo,
    `eFG%` = (FGM + 0.5 * `3PM`) / FGA,
    `TO%` = TO / possessions,
    `OREB%` = OREB / (OREB + team_DREB),
    FTRate = FTA / FGA,
    `3PRate` = `3PA` / FGA,
    ARate = AST / FGM,
    `3P%` = `3PM` / `3PA`,
    `2P%` = `2FGM` / `2FGA`,
    `FT%` = FTM / FTA,
    `3P_distr` = (`3PM` * 3) / PTS,
    `2P_distr` = (`2FGM` * 2) / PTS,
    FT_distr = FTM / PTS,
    Experience = experience,
    .keep = "none"
  ) %>%
  mutate(across(.cols = -c(Tempo, Experience), .fns = multiply_by_100),
         #across(.cols = Experience, .fns = ~ round(.x, digits = 2)),
         across(.cols = , .fns = ~ round(.x, digits = 1))) %>%
  select(-possessions)
names(opponent_adv)[names(opponent_adv) == '3P_distr'] <- "3-pointers"
names(opponent_adv)[names(opponent_adv) == '2P_distr'] <- "2-pointers"
names(opponent_adv)[names(opponent_adv) == 'FT_distr'] <- "Free Throws"

defense <- opponent_adv[1,]
defense <- t(defense)

kenpom <- data.frame(offense)
kenpom <- cbind(kenpom, defense)
names(kenpom) <- c("Offense", "Defense")

kenpom <- kenpom %>%
  kable(format = "html", escape = F, align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover"), fixed_thead = T) %>%
  row_spec(row = 0, background = "#E6E6E6", color = "black") %>%
  column_spec(column = 1, width = "2cm") %>%
  column_spec(column = c(2, 3), width = "1.5cm") %>%
  column_spec(column = 2, extra_css = "border-right: 2px solid") %>%
  column_spec(column = 1, background = "#E6E6E6", color = "black") %>%
  pack_rows(index = c(" " = 2,
                      "Four Factors" = 4,
                      "Playing Style" = 2,
                      "Shooting Percentages" = 3,
                      "Point Distribution" = 3,
                      "Personnel" = 1))


opponent_season_totals$MIN <- team_minutes
opponent_season_totals$CHRG <- NA

combined_season_totals <- rbind(team_season_totals, opponent_season_totals)
combined_season_totals$`Off/Def` <- c("Offense", "Defense")
combined_season_totals <- combined_season_totals %>%
  select(`Off/Def`, everything(), -MIN)


# Create visualizations for advanced stats
plot1 <- player_adv %>%
  ggplot(aes(x = `%Min`, y = ORtg, size = `%Poss`)) +
  geom_point() +
  geom_text(aes(label = Player, vjust = -1.5), size = 8 / .pt) +
  theme_grey() %>%
  coord_cartesian(xlim = c(0, max(player_adv$`%Min`)),
                  ylim = c(0, max(player_adv$ORtg)),
                  expand = TRUE)

plot2 <- player_adv %>%
  ggplot(aes(x = `%Poss`, y = ORtg, alpha = `%Min`)) +
  geom_point(size = 5) +
  geom_text(aes(label = Player, vjust = -1.5), size = 8 / .pt) +
  theme_grey() %>%
  coord_cartesian(xlim = c(0, max(player_adv$`%Poss`)),
                  ylim = c(0, max(player_adv$ORtg)),
                  expand = TRUE)

plot3 <- player_adv %>%
  ggplot(aes(x = `%Min`, y = `%Poss`, size = ORtg)) +
  geom_point() +
  geom_text(aes(label = Player, vjust = -1.5), size = 8 / .pt) +
  theme_grey() %>%
  coord_cartesian(xlim = c(0, max(player_adv$`%Min`)),
                  ylim = c(0, max(player_adv$`%Poss`)),
                  expand = TRUE)

plot4 <- player_adv %>%
  ggplot(aes(x = `%Shots`, y = `eFG%`, alpha = `%Min`)) +
  geom_point(size = 5) +
  geom_text(aes(label = Player, vjust = -1.5), size = 8 / .pt) +
  theme_grey() %>%
  coord_cartesian(xlim = c(0, max(player_adv$`%Shots`)),
                  ylim = c(0, max(player_adv$`eFG%`)),
                  expand = TRUE)


# Create Harrison Basketball Analytics web page with Shiny
ui <- dashboardPage(
  
  skin = "yellow",
  
  dashboardHeader(title = 'HARRISON BASKETBALL ANALYTICS 2020-21',
                  titleWidth = 600),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Player Advanced Stats', tabName = 'player_advanced'),
      menuItem('Player Standard Stats', tabName = 'player_standard'),
      menuItem('Player Offensive Visualizations', tabName = 'graph'),
      menuItem('Team Advanced Stats', tabName = 'team_advanced'),
      menuItem('Team Standard Stats', tabName = 'team_standard'),
      menuItem('COMING SOON', tabName = 'coming')
    )
  ),
  
  dashboardBody(
    setBackgroundColor(
      color = "navy",
      gradient = c("linear", "radial"),
      direction = c("bottom", "top", "right", "left"),
      shinydashboard = TRUE
    ),
    tabItems(
      tabItem(tabName = 'player_advanced',
              fluidRow(
                box(titlePanel('Advanced Box Score (Season)'),
                    reactableOutput(outputId = 'player_advanced'),
                    solidHeader = TRUE,
                    width = 12),
                fluidPage(
                  box(includeMarkdown("PlayerGlossary.Rmd"),
                      width = 12
                  )
                )
              )
      ),
      tabItem(tabName = 'player_standard',
              fluidRow(
                box(titlePanel('Standard Box Score (Season)'),
                    reactableOutput(outputId = 'player_standard'),
                    solidHeader = TRUE,
                    width = 12)
              )
      ),
      tabItem(tabName = 'team_advanced',
              fluidPage(
                box(titlePanel('Team Advanced Stats'),
                    htmlOutput(outputId = 'team_advanced'),
                    solidHeader = TRUE,
                    width = 5),
                box(includeMarkdown("TeamGlossary.Rmd"),
                    width = 7)
              )
      ),
      tabItem(tabName = 'team_standard',
              fluidRow(
                box(titlePanel('Team Season Totals'),
                    reactableOutput(outputId = 'team_season_totals'),
                    solidHeader = TRUE,
                    width = 12),
                box(titlePanel('Offensive Game Log'),
                    reactableOutput(outputId = 'offensive_log'),
                    solidHeader = TRUE,
                    width = 12),
                box(titlePanel('Defensive Game Log'),
                    reactableOutput(outputId = 'defensive_log'),
                    solidHeader = TRUE,
                    width = 12)
              )
      ),
      tabItem(tabName = 'graph',
              fluidRow(
                box(titlePanel('%Min vs ORtg'),
                    plotOutput(outputId = 'graph1'),
                    width = 12
                ),
                box(titlePanel('%Poss vs ORtg'),
                    plotOutput(outputId = 'graph2'),
                    width = 12
                ),
                box(titlePanel('%Min vs %Poss'),
                    plotOutput(outputId = 'graph3'),
                    width = 12
                ),
                box(titlePanel('%Shots vs eFG%'),
                    plotOutput(outputId = 'graph4'),
                    width = 12
                )
              )
      ),
      tabItem(tabName = 'coming',
              fluidPage(
                box(includeMarkdown("ComingSoon.Rmd"),
                    width = 12
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  
  output$player_advanced <- renderReactable({
    reactable(
      player_adv,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        Player = colDef(style = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1),
                        headerStyle = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1)),
        `#` = colDef(align = "center", width = 40),
        Class = colDef(align = "left", width = 50),
        GP = colDef(align = "left", width = 50),
        FT = colDef(align = "right"),
        `2P` = colDef(align = "right"),
        `3P` = colDef(align = "right")
        )
      )},
  )
  
  output$player_standard <- renderReactable({
    reactable(
      player_box,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        Player = colDef(style = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1),
                        headerStyle = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1)),
        `#` = colDef(align = "center", width = 40),
        Class = colDef(align = "left", width = 50),
        GP = colDef(align = "left", width = 50)
      )
    )},
  )
  
  output$team_advanced <- renderText({
    kenpom}
  )
  
  output$team_season_totals <- renderReactable({
    reactable(
      combined_season_totals,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        `Off/Def` = colDef(style = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1),
                        headerStyle = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1))
      )
    )},
  )
  
  output$offensive_log <- renderReactable({
    reactable(
      team_game_totals,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        Opponent = colDef(style = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1),
                           headerStyle = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1))
      )
    )},
  )
  
  output$defensive_log <- renderReactable({
    reactable(
      opponent_game_totals,
      pagination = FALSE,
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      columns = list(
        Opponent = colDef(style = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1),
                          headerStyle = list(position = "sticky", left = 0, background = "#E6E6E6", zIndex = 1))
      )
    )},
  )
  
  output$graph1 <- renderPlot({
    plot1
  })
  
  output$graph2 <- renderPlot({
    plot2
  })
  
  output$graph3 <- renderPlot({
    plot3
  })
  
  output$graph4 <- renderPlot({
    plot4
  })
  
}

shinyApp(ui = ui, server = server)

#deployApp('/Users/cal/Desktop/BasketballAnalytics/HarrisonHoops/')
