##
## using R's nflscrapR API to pull player stats from 2019 season
##

# install the nflscrapR package:
# https://rdrr.io/github/maksimhorowitz/nflscrapR/api/
# devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(nflscrapR)
library(dplyr)
library(devtools)

# Pull game data from 2019
games_2019 <- scrape_game_ids(2019)

# empty datafame to player game stats
PlayerGameData_2019 <- c()

# loop through all games to put player stats into a df
for (i in 1:nrow(games_2019)){
  currentGame <- player_game(games_2019[i,"game_id"])
  PlayerGameData_2019 <- rbind(PlayerGameData_2019, currentGame)
}

# convert factors to strings
PlayerGameData_2019$game.id <- as.character(PlayerGameData_2019$game.id)
PlayerGameData_2019$playerID <- as.character(PlayerGameData_2019$playerID)
PlayerGameData_2019$name <- as.character(PlayerGameData_2019$name)
PlayerGameData_2019$Team <- as.character(PlayerGameData_2019$Team)

# add opponent to dataframe
PlyrGmDat_2019 <- left_join(PlayerGameData_2019,
                             games_2019[,c("game_id","week","season","home_team","away_team")],
                             by = c("game.id" = "game_id"))
PlyrGmDat_2019$Opp <- ifelse(PlyrGmDat_2019$Team == PlyrGmDat_2019$home_team,
                              PlyrGmDat_2019$away_team, PlyrGmDat_2019$home_team)

# function to calculate fantasy points
# https://www.draftkings.com/help/rules/1/1
calcFanPts <- function( stats ){
  fan_pts <- 0
  # Passing Yards: 1 point per 25 yards
  fan_pts <- fan_pts + stats$passyds / 25
  # 300+ Yard Passing Game: 3 points
  fan_pts <- ifelse(stats$passyds >= 300 , fan_pts + 3 , fan_pts)
  # Passing Touchdowns: 4 points
  fan_pts <- fan_pts + stats$pass.tds * 4
  # Passing Interceptions: -1 points
  fan_pts <- fan_pts + stats$pass.ints * -1
  # Rushing Yards: 1 point per 10 yards
  fan_pts <- fan_pts + stats$rushyds / 10
  # 100+ Yard Rushing Game: 3 points
  fan_pts <- ifelse(stats$rushyds >= 100 , fan_pts + 3 , fan_pts)
  # Rushing Touchdowns: 6 points
  fan_pts <- fan_pts + stats$rushtds * 6
  # Receptions: 1 point
  fan_pts <- fan_pts + stats$recept * 1
  # Receiving Yards: 1 point per 10 yards
  fan_pts <- fan_pts + stats$recyds / 10
  # 100+ Yard Rushing Game: 3 points
  fan_pts <- ifelse(stats$recyds >= 100 , fan_pts + 3 , fan_pts)
  # Receiving Touchdowns: 6 points
  fan_pts <- fan_pts + stats$rec.tds * 6
  # 2-Point Conversions: 2 points
  fan_pts <- fan_pts + (stats$rush.twoptm + stats$rec.twoptm) * 2
  # Fumbles Lost: -1 points
  fan_pts <- fan_pts + stats$fumbslost * -1
  # Any return touchdown: 6 points
  fan_pts <- fan_pts + (stats$kickret.tds + stats$puntret.tds) * 6

  return(fan_pts)
}

# Use function to calculate fantasy points
PlyrGmDat_2019$fan_pts <- calcFanPts(PlyrGmDat_2019)

# extract nfl rosters with player positions
teams <- nflteams
full_rosters <- season_rosters(2019, teams$abbr, positions = c("QUARTERBACK", "RUNNING_BACK",
                                                               "WIDE_RECEIVER", "TIGHT_END"))
# join player positions to dataframe
PlyrGmDat_2019_pos <- left_join(PlyrGmDat_2019,
                                 full_rosters[,c("GSIS_ID","Pos","Player")],
                                 by = c("playerID" = "GSIS_ID"))

# check which players are positionless
na_plyrs <- PlyrGmDat_2019_pos[PlyrGmDat_2019_pos$fan_pts > 0 & is.na(PlyrGmDat_2019_pos$Pos),]
head(na_plyrs[order(-na_plyrs$fan_pts),c("name","week","fan_pts")])

# save dataset
write_csv(PlyrGmDat_2019_pos, "2019_nfl_player_stats.csv")
