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


server = function(input, output){
  # filter_player = reactive({
  #   if(length(input) == 0){
  #     return (NULL)
  #   } else if(input$player1 == "--" & input$player2 == "--" & input$player3 == "--" & input$player4 == "--" & input$player5 == "--"){
  #     return(NULL)
  #   }
  #   lineup_initial = lineups_by_game
  #   opponents = input$opponents
  #   lineup_initial = lineup_initial[lineup_initial$Opponent %in% opponents,]
  #   lineup_selection = create_lineup(lineup_initial)
  #   
  # 
  #   for(i in 1:length(player_variables)) {
  #     if(input[[player_variables[i]]] != "--") {
  #       lineup_selection = lineup_selection %>%
  #         filter_at(vars(player_columns), any_vars(. %in% input[[player_variables[i]]]))
  #     }
  #   }
  #   lineup_selection = lineup_selection %>%
  #     filter(
  #       Mins >= input$lineupMinutes[1], 
  #       Mins <= input$lineupMinutes[2]
  #     )
  #   return(lineup_selection)
  # })
  
  filter_player = reactive({
    if(length(input) == 0){
      return (NULL)
    } else if(input$player1 == "--" & input$player2 == "--" & input$player3 == "--" & input$player4 == "--" & input$player5 == "--"){
      return(NULL)
    }
    lineup_selection = lineups
    for(i in 1:length(player_variables)) {
      if(input[[player_variables[i]]] != "--") {
        lineup_selection = lineup_selection %>%
          filter_at(vars(player_columns), any_vars(. %in% input[[player_variables[i]]]))
      }
    }
    lineup_selection = lineup_selection %>%
      filter(
        Mins >= input$lineupMinutes[1],
        Mins <= input$lineupMinutes[2]
      )
    return(lineup_selection)
  })
  
  all_results = reactive({
    if(input$player1 == "--" & input$player2 == "--" & input$player3 == "--" & input$player4 == "--" & input$player5 == "--"){
      return(lineups %>% filter(
        Mins >= input$lineupMinutes[1], 
        Mins <= input$lineupMinutes[2]
      ))
    }
    filter_player()
  })
  
  
  
  
  filter_individual = reactive({
    if (length(input) == 0){
      return (NULL)
    }
    if (input$individual_player == "--"){
      return (NULL)
    }
    individual_stats = player_stats
    individual_stats = individual_stats %>%
      filter(Player == input$individual_player)
    
    return(individual_stats)
  })
  
  output$player1 = renderUI({
    selectInput(
      "player1", 
      "Player 1", 
      c("--", sort(unique(player_stats$Player))), 
      selected = "--"
    )
  })
  output$player2 = renderUI({
    selectInput(
      "player2", 
      "Player 2", 
      c("--", sort(unique(player_stats$Player))), 
      selected = "--"
    )
  })
  output$player3 = renderUI({
    selectInput(
      "player3", 
      "Player 3", 
      c("--", sort(unique(player_stats$Player))), 
      selected = "--"
    )
  })
  output$player4 = renderUI({
    selectInput(
      "player4", 
      "Player 4", 
      c("--", sort(unique(player_stats$Player))), 
      selected = "--"
    )
  })
  output$player5 = renderUI({
    selectInput(
      "player5", 
      "Player 5", 
      c("--", sort(unique(player_stats$Player))), 
      selected = "--"
    )
  })
  
  output$poss = renderText({
    sum(filter_player()$POSS)
  })
  
  output$mins = renderText({
    sum(filter_player()$Mins)
  })
  
  output$pace = renderText({
    40 * sum(filter_player()$ePOSS) / sum(filter_player()$Mins)
  })
  
  output$netrtg = renderText({
    100 * sum(filter_player()$PTS) / sum(filter_player()$POSS) - 100* sum(filter_player()$oPTS) / sum(filter_player()$POSS)
  })
  
  output$ortg = renderText({
    100 * sum(filter_player()$PTS) / sum(filter_player()$POSS)
  })
  
  output$fgpct = renderText({
    sum(filter_player()$FGM) / sum(filter_player()$FGA)
  })
  
  output$efgpct = renderText({
    sum(filter_player()$FGM + 0.5 * filter_player()$TPM) / sum(filter_player()$FGA)
  })
  
  output$drtg = renderText({
    100* sum(filter_player()$oPTS) / sum(filter_player()$POSS)
  })
  
  output$ofg = renderText({
    sum(filter_player()$oFGM) / sum(filter_player()$oFGA)
  })
  
  output$oefg = renderText({
    sum(filter_player()$oFGM + 0.5 * filter_player()$oTPM) / sum(filter_player()$oFGA)
  })
  
  output$oReboundpct = renderText({
    sum(filter_player()$ORB) / (sum(filter_player()$ORB) + sum(filter_player()$oDRB))
  })
  
  output$dReboundpct = renderText({
    sum(filter_player()$DRB) / (sum(filter_player()$DRB) + sum(filter_player()$oORB))
  })
  
  output$table_results = DT::renderDataTable({
    DT::datatable(all_results() %>%
                    arrange(-Mins), 
                  extensions = "FixedColumns",  
                  options = list(
                    #     dom = 't',
                    scrollX = TRUE,
                    fixedColumns = TRUE
                  ))
  })
  
  output$individual_player = renderUI({
    selectInput(
      "individual_player", 
      "Player", 
      c("--", sort(unique(player_stats$Player))), 
      selected = "--"
    )
  })
  
  output$iposs = renderText({
    sum(filter_individual()$POSS)/nrow(filter_individual()) 
  })
  
  output$mpg = renderText({
    sum(filter_individual()$MINS)/nrow(filter_individual()) 
  })
  
  
  output$ppg = renderText({
    sum(filter_individual()$PTS)/nrow(filter_individual()) 
  })
  
  output$orpg = renderText({
    sum(filter_individual()$ORB)/nrow(filter_individual()) 
  })
  
  output$apg = renderText({
    sum(filter_individual()$AST) / nrow(filter_individual()) 
  })
  
  output$tov = renderText({
    sum(filter_individual()$TOV) / nrow(filter_individual()) 
  })
  
  output$ifg = renderText({
    sum(filter_individual()$FGM) / sum(filter_individual()$FGA) 
  })
  
  output$iefg = renderText({
    (sum(filter_individual()$FGM) + 0.5 * sum(filter_individual()$TPM))/ sum(filter_individual()$FGA) 
  })
  
  output$ifta = renderText({
    sum(filter_individual()$FTA) / nrow(filter_individual()) 
  })
  
  output$i3pa = renderText({
    sum(filter_individual()$TPA) / nrow(filter_individual()) 
  })
  
  output$painta = renderText({
    sum(filter_individual()$RIMA) / nrow(filter_individual()) 
  })
  
  output$mida = renderText({
    sum(filter_individual()$MIDA) / nrow(filter_individual()) 
  })
  
  output$pbacka = renderText({
    sum(filter_individual()$PBACKA) / nrow(filter_individual()) 
  })
  
  output$ifga = renderText({
    sum(filter_individual()$FGA) / nrow(filter_individual()) 
  })
  
  output$iftpct = renderText({
    sum(filter_individual()$FTM) / sum(filter_individual()$FTA) 
  })
  
  output$i3ppct = renderText({
    sum(filter_individual()$TPM) / sum(filter_individual()$TPA) 
  })
  
  output$paintpct = renderText({
    sum(filter_individual()$RIMM) / sum(filter_individual()$RIMA) 
  })
  
  output$midpct = renderText({
    sum(filter_individual()$MIDM) / sum(filter_individual()$MIDA) 
  })
  
  output$pbackpct = renderText({
    sum(filter_individual()$PBACKM) / sum(filter_individual()$PBACKA) 
  })
  
  output$tspct = renderText({
    sum(filter_individual()$PTS) / (2 * (sum(filter_individual()$FGA) + 0.44 * sum(filter_individual()$FTA))) 
  })
  
  output$drb = renderText({
    sum(filter_individual()$DRB) / nrow(filter_individual()) 
  })
  
  output$stl = renderText({
    sum(filter_individual()$STL) / nrow(filter_individual()) 
  })
  
  output$blk = renderText({
    sum(filter_individual()$BLK) / nrow(filter_individual()) 
  })
  
  output$pf = renderText({
    sum(filter_individual()$PF) / nrow(filter_individual()) 
  })
  
  
  create_lineup = function(lineup_whole){
    final_lineup = data.frame()
    
    final_lineup$P1 = lineup_whole$P1
    final_lineup$P2 = lineup_whole$P2
    final_lineup$P3 = lineup_whole$P3
    final_lineup$P4 = lineup_whole$P4
    final_lineup$P5 = lineup_whole$P5
    
    final_lineup$Mins = lineup_whole %>%
      group_by(P1, P2, P3, P4, P5) %>%
      summarise(Mins = sum(Mins))
    
    return (final_lineup)
    
  }
  
  
  
  
  
  
}
