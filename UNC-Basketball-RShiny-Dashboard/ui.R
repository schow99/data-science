library(shiny)
library(shinydashboard)
library(bigballR)
library(DT)
library(data.table)
library(tidyverse)
library(conflicted)
library(shinythemes)
library(shinyWidgets)



conflict_prefer("renderDataTable", "DT")
conflict_prefer("box", "shinydashboard")
conflict_prefer("filter", "dplyr")


#setwd("C:/Users/samue/OneDrive/Personal Projects/Data Science/Shiny/UNC Basketball")

#schedule = read_csv("unc_schedule_2019_20.csv")
#play_by_play = read_csv("unc_play_by_play_2019_20.csv")
#lineups = read_csv("unc_lineups_2019_20.csv")
#player_stats = read_csv("unc_player_stats_2019_20.csv")

lineups = read_csv("unc_lineups_2020_21.csv")
#lineups_by_game = read_csv("unc_lineups_by_game_2020_21.csv")
player_stats = read_csv("unc_player_stats_2020_21.csv")

lineups$PACE = 40 * lineups$ePOSS / lineups$Mins

player_columns = c("P1", "P2", "P3", "P4", "P5")
player_variables = c("player1", "player2", "player3", "player4", "player5")

max_minutes = max(lineups$Mins)

ui = dashboardPage(
  dashboardHeader(title = "UNC Basketball 2020-21", titleWidth = 275), 
  dashboardSidebar(
    width = 275, 
    sidebarMenu(
      menuItem("Lineup Statistics", tabName = "dashboard", icon = icon("users")), 
      menuItem("Lineup Table", tabName = "table", icon = icon("table")), 
      menuItem("Player Statistics", tabName = "player", icon = icon("user-alt"))
      
    )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(
                  width = 3,
                  shinydashboard::box(
                    title = "Players",
                    width = 12,
                    uiOutput("player1"), 
                    uiOutput("player2"), 
                    uiOutput("player3"), 
                    uiOutput("player4"), 
                    uiOutput("player5")
                  ), 
                  shinydashboard::box (
                    width = 12,
                    sliderInput("lineupMinutes", "Minutes Lineup Has Played", min = 0, max = max_minutes, value = c(0, max_minutes))
                  ), 
                  # shinydashboard::box(
                  #   width = 12, 
                  #   pickerInput(
                  #     inputId = "opponents", 
                  #     label = "Opponents", 
                  #     choices = unique(lineups_by_game$Opponent), 
                  #     options = list(
                  #       `actions-box` = TRUE, 
                  #       size = 10,
                  #       `selected-text-format` = "count > 3"
                  #     ), 
                  #     multiple = TRUE
                  #   )
                  # )
                ), 
                column(
                  width = 9, 
                  shinydashboard::box(
                    title = "General",
                    width = 12, 
                    shinydashboard::box(
                      title = "POSS",
                      textOutput("poss"), 
                      width = 3
                    ), 
                    shinydashboard::box(
                      title = "MINS", 
                      textOutput("mins"), 
                      width = 3
                    ), 
                    shinydashboard::box(
                      title = "PACE", 
                      textOutput("pace"), 
                      width = 3
                    ),
                    shinydashboard::box(
                      title = "Net RTG", 
                      textOutput("netrtg"), 
                      width = 3
                    )
                  )
                ), 
                column(
                  width = 9,
                  shinydashboard::box(
                    title = "Offense",
                    width = 12,
                    shinydashboard::box(
                      title = "ORtg",
                      width = 4,
                      textOutput("ortg")
                    ), 
                    shinydashboard::box(
                      title = "FG%",
                      width = 4,
                      textOutput("fgpct")
                    ), 
                    shinydashboard::box(
                      title = "eFG%",
                      width = 4,
                      textOutput("efgpct")
                    )
                  )
                ), 
                column(
                  width = 9, 
                  shinydashboard::box(
                    title = "Defense", 
                    width = 12, 
                    shinydashboard::box(
                      title = "DRtg", 
                      width = 4, 
                      textOutput("drtg")
                    ), 
                    shinydashboard::box(
                      title = "oFG%", 
                      width = 4, 
                      textOutput("ofg")
                    ), 
                    shinydashboard::box(
                      title = "oEFG%", 
                      width = 4, 
                      textOutput("oefg")
                    )
                  )
                ), 
                column(
                  width = 9, 
                  shinydashboard::box(
                    title = "Others", 
                    width = 12, 
                    shinydashboard::box(
                      title = "O Rebound %", 
                      width = 4, 
                      textOutput("oReboundpct")
                    ), 
                    shinydashboard::box(
                      title = "D Rebound %", 
                      width = 4, 
                      textOutput("dReboundpct")
                    )
                  )
                )
              )
      ), 
      tabItem(
        tabName = "table",
        DT::dataTableOutput("table_results")#, 
        #shinydashboard::box(
        #   title = "Results", 
        #  width = 12)
      ), 
      
      tabItem(
        tabName = "player", 
        fluidRow(
          column(
            width = 4,
            shinydashboard::box(
              title = "Player Statistics Per Game",
              width = 12,
              uiOutput("individual_player")
            )
          ), 
          column(
            width = 8,
            shinydashboard::box(
              title = "General",
              width = 12,
              shinydashboard::box(
                title = "POSS",
                textOutput("iposs"), 
                width = 6
              ), 
              shinydashboard::box(
                title = "MPG",
                textOutput("mpg"), 
                width = 6
              )
            )
          ),
          column(
            width = 12, 
            shinydashboard::box(
              title = "Offense",
              width = 12, 
              shinydashboard::box(
                title = "PPG",
                textOutput("ppg"), 
                width = 2
              ), 
              shinydashboard::box(
                title = "ORPG", 
                textOutput("orpg"), 
                width = 2
              ), 
              shinydashboard::box(
                title = "APG", 
                textOutput("apg"), 
                width = 2
              ), 
              shinydashboard::box(
                title = "TOV", 
                textOutput("tov"), 
                width = 2
              ), 
              shinydashboard::box(
                title = "FG%", 
                textOutput("ifg"), 
                width = 2
              ), 
              shinydashboard::box(
                title = "eFG%", 
                textOutput("iefg"), 
                width = 2
              )
            ), 
            column(
              width = 12, 
              shinydashboard::box(
                title = "Shooting",
                width = 12, 
                shinydashboard::box(
                  title = "FTA", 
                  textOutput("ifta"), 
                  width = 2
                ),
                shinydashboard::box(
                  title = "3PA", 
                  textOutput("i3pa"), 
                  width = 2 
                ),
                shinydashboard::box(
                  title = "PaintA", 
                  textOutput("painta"), 
                  width = 2
                ),
                shinydashboard::box(
                  title = "MidrangeA", 
                  textOutput("mida"), 
                  width = 2
                ),
                shinydashboard::box(
                  title = "PutbackA", 
                  textOutput("pbacka"), 
                  width = 2
                ),
                shinydashboard::box(
                  title = "FGA", 
                  textOutput("ifga"), 
                  width = 2
                ),
                shinydashboard::box(
                  title = "FT%", 
                  textOutput("iftpct"), 
                  width = 2
                ), 
                
                shinydashboard::box(
                  title = "3P%",
                  textOutput("i3ppct"), 
                  width = 2
                ), 
                
                shinydashboard::box(
                  title = "Paint%", 
                  textOutput("paintpct"), 
                  width = 2
                ), 
                
                shinydashboard::box(
                  title = "Midrange%", 
                  textOutput("midpct"), 
                  width = 2
                ), 
                
                shinydashboard::box(
                  title = "Putback%", 
                  textOutput("pbackpct"), 
                  width = 2
                ), 
                shinydashboard::box(
                  title = "TS%", 
                  textOutput("tspct"), 
                  width = 2
                )
              )
            ), 
            
            column(
              width = 12, 
              shinydashboard::box(
                title = "Defense",
                width = 12, 
                shinydashboard::box(
                  title = "DRPG",
                  textOutput("drb"), 
                  width = 3
                ),
                shinydashboard::box(
                  title = "STL",
                  textOutput("stl"), 
                  width = 3
                ), 
                shinydashboard::box(
                  title = "BLK", 
                  textOutput("blk"), 
                  width = 3
                ), 
                shinydashboard::box(
                  title = "PF", 
                  textOutput("pf"), 
                  width = 3
                )
              )
            )
          )
        )
      )
    )
    
  )
)
