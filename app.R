library(tidyverse)
library(ggtext)
library(shiny)
library(shinydashboard)
library(shinyjs)

full_stats <- read.csv("https://raw.githubusercontent.com/alittlefitness/PlayerSimilarity/main/AFL%20Stats%202012-2023.csv", stringsAsFactors = F) %>%
  mutate(Season = str_sub(match_date, 1, 4))

player_ids <- full_stats %>%
  select(Name, player_id) %>%
  distinct()

normalised_stats <- read.csv("https://raw.githubusercontent.com/alittlefitness/PlayerSimilarity/main/Normalised%20Stats.csv", stringsAsFactors = F) %>%
  select(-Name) %>%
  left_join(., player_ids)

teams <- full_stats %>%
  select(player_team) %>%
  distinct() %>%
  arrange(player_team)

stat_selections <- full_stats %>%
  select(7:51) %>%
  colnames(.)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "AFL Player Similarity"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar", height = 100,
                menuItem("Similarity & Projections", tabName = "sim", icon = icon("project-diagram")),
                menuItem("Instructions & Suggestions", tabName = "instructions", icon = icon("clipboard")),
                tags$style(type="text/css", # Suppress error messages if no data available
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sim",
              fluidRow(box(tags$a(href = 'http://alittleanalysis.com/shiny/PlayerSimilarity',
                              img(src = 'ALittleAnalysisLogo2.png',
                                  title = "A Little Analysis", height = "185", width = "300")), width = 12, background = "black")),
              fluidRow(box(selectInput("team", "Select Team:",
                                       choices = teams$player_team), width = 3, background = "black"),
                       box(uiOutput("player"), width = 3, background = "black"),
                       box(selectInput("n_years", "Number of years to project forward:", choices = c(1:5)), 
                           width = 3, background = "black"),
                       box(selectizeInput("selected_stats", "Select Stats:", choices = stat_selections, multiple = T), 
                           width = 3, background = "black")),
              fluidRow(box(title = "10 most similar players", tableOutput("sim_tab"), width = 2),
                       box(title = "Ouput at same age", plotOutput("recent"), width = 5, height = 670),
                       box(title = "Ouput of similar players in future seasons", plotOutput("future"), width = 5, height = 670)),
              fluidRow(box(textOutput("source"), width = 12))
      ),
      tabItem(tabName = "instructions",
              fluidRow(box(title = "Instructions", uiOutput("instruct"), width = 12)),
              fluidRow(box(title = "Suggestions", uiOutput("suggestions"), width = 12))
      )
    )
  )
)

server <- function(input, output) {
  
  output$player <- renderUI({
    selectInput("player", "Select Player:", choices = filter(full_stats, player_team == input$team & Season == 2023)$Name)
  })
  
  player_age <- reactive({
    player_age <- full_stats %>%
      filter(Name == input$player & Season == 2023) %>%
      select(Age) %>%
      distinct()
  })
  
  player_age_past <- reactive({
    player_age_past <- full_stats %>%
      filter(Name == input$player & Season %in% c(2020:2023)) %>%
      select(Age) %>%
      distinct()
  })
  
  calc_distance <- reactive({
    calculated_distance <- normalised_stats %>%
      select(Name, Age, all_of(input$selected_stats)) %>%
      filter(between(Age, min(player_age_past()$Age), max(player_age_past()$Age) + as.numeric(input$n_years))) %>%
      group_by(Name) %>%
      mutate(Keep = if_else(n() == nrow(player_age_past()) + as.numeric(input$n_years), 1, 0)) %>%
      filter(Age %in% player_age_past()$Age & Keep == 1) %>%
      ungroup() %>%
      select(-Keep)
    
    player_distance <- normalised_stats %>%
      select(Name, Age, all_of(input$selected_stats)) %>%
      filter(Age %in% player_age_past()$Age & Name == input$player) %>%
      ungroup() 
    
    calc_distance <- bind_rows(calculated_distance, player_distance)
    return(calc_distance)
  })
  
  player_sim <- reactive({
    player_sim_new <- tibble(Name = "")
    
    for(i in player_age_past()$Age) {
    player_age_sim <- calc_distance() %>%
      filter(Age == i) %>%
      select(-Age) %>%
      mutate_at(vars(all_of(input$selected_stats)), ~(. - .[[which(Name == input$player)]])^2) %>%
      mutate(Distance = rowSums(.[-1], na.rm = T))
    
    player_sim_new <- bind_rows(player_sim_new, player_age_sim)
    }
    
    player_sim <- player_sim_new %>%
      group_by(Name) %>%
      filter(Name != "") %>%
      summarise(Distance = sum(Distance, na.rm = T)) %>%
      arrange(Distance) %>%
      slice(2:11)
    return(player_sim)
  })
  
  output$sim_tab <- renderTable({
    req(input$selected_stats)
    sim_tab <- player_sim() %>%
      select(Name)
  })
  
  recent_stats <- reactive({
    recent_stats <- full_stats %>%
      filter((Name %in% player_sim()$Name & Age <= player_age()$Age) | (Name == input$player & Age <= player_age()$Age)) %>%
      filter(Age >= max(slice(full_stats, max(which(Name == input$player)))$Age - 2, 
                        slice(full_stats, min(which(Name == input$player)))$Age)) %>%
      select(Name, Age, tog, all_of(input$selected_stats)) %>%
      group_by(Name, Age) %>%
      summarise_at(vars(tog, all_of(input$selected_stats)), ~sum(., na.rm = T)) %>%
      mutate_at(vars(all_of(input$selected_stats)), ~(. / tog) * 100) %>%
      pivot_longer(4:ncol(.), names_to = "Stats", values_to = "Values")
    
    return(recent_stats)
  })
 
  future_stats <- reactive({ 
    n_years_stats <- full_stats %>%
      filter(Name %in% player_sim()$Name & Age > player_age()$Age & Age <= player_age()$Age + as.numeric(input$n_years)) %>%
      select(Name, Age, tog, all_of(input$selected_stats)) %>%
      group_by(Name, Age) %>%
      summarise_at(vars(tog, all_of(input$selected_stats)), ~sum(., na.rm = T)) %>%
      mutate_at(vars(all_of(input$selected_stats)), ~(. / tog) * 100)
    
    stat_means <- n_years_stats %>%
      group_by(Age) %>%
      summarise_at(vars(all_of(input$selected_stats)), ~mean(.))
    
    future_stats <- bind_rows(n_years_stats, stat_means) %>%
      pivot_longer(4:ncol(.), names_to = "Stats", values_to = "Values")
    
    return(future_stats)
  })
  
  y_values <- reactive({
    y_values <- bind_rows(recent_stats(), future_stats()) %>%
      group_by(Age, Stats) %>%
      summarise(min_y = min(Values),
                max_y = max(Values)) %>%
      mutate(Type = if_else(Age %in% recent_stats()$Age, "Recent", "Future"))
  })
  
  output$recent <- renderPlot({
    req(input$selected_stats)
    
    recent_plot <- ggplot(recent_stats(), aes(as.factor(Age), Values)) +
      geom_point(aes(col = "individual")) +
      geom_point(data = filter(recent_stats(), Name == input$player), aes(col = "selected player"), size = 5) +
      geom_blank(data = y_values(), aes(as.factor(Age), y = min_y)) +
      geom_blank(data = y_values(), aes(as.factor(Age), y = max_y)) +
      facet_wrap(~Stats, ncol = 1, scales = "free") +
      theme_minimal() +
      theme(strip.placement = "outside",
            panel.border = element_rect(color = "black", fill = "#ffffff00"),
            legend.title = element_blank(),
            legend.position = "top",
            strip.text = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12), 
            axis.title = element_text(size = 12)) +
      scale_color_manual(values = c("selected player" = "red", "individual" = "black")) +
      labs(y = "Value per 100 minutes of time on ground",
           x = "Age") +
      scale_x_discrete(limits = factor(c(min(y_values()$Age[which(y_values()$Type == "Recent")]):
                                           max(y_values()$Age[which(y_values()$Type == "Recent")]))))
    
    return(recent_plot)
  }, height = 600)
  
  output$future <- renderPlot({
    req(input$selected_stats)
    
    future_plot <- ggplot(future_stats(), aes(as.factor(Age), Values)) +
      geom_point(aes(col = "individual")) +
      geom_point(data = filter(future_stats(), is.na(Name)), aes(col = "average"), size = 5) +
      geom_blank(data = y_values(), aes(as.factor(Age), y = min_y)) +
      geom_blank(data = y_values(), aes(as.factor(Age), y = max_y)) +
      facet_wrap(~Stats, ncol = 1, scales = "free") +
      theme_minimal() +
      theme(strip.placement = "outside",
            panel.border = element_rect(color = "black", fill = "#ffffff00"),
            legend.title = element_blank(),
            legend.position = "top",
            strip.text = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12), 
            axis.title = element_text(size = 12)) +
      scale_color_manual(values = c("average" = "red", "individual" = "black")) +
      labs(y = "Value per 100 minutes of time on ground",
           x = "Age") +
      scale_x_discrete(limits = factor(c(min(y_values()$Age[which(y_values()$Type == "Future")]):
                                           max(y_values()$Age[which(y_values()$Type == "Future")]))))
    
    return(future_plot)
  }, height = 600)
  
  output$source <- renderText({
    paste("Note: All data sourced via the fitzRoy R package.")
  })
  
  output$instruct <- renderUI({
    HTML("The Player Similarity app uses a k Nearest Neighbours (kNN) approach to determine the 10 most similar players to the selected player.  
          All stats have been normalised prior to implementing kNN.  Using the selected player's age in 2022 and the list of selected stats, the algorithm 
          looks for players with similar statistics per 100 minutes of time on ground at the same age.  The similarity period is a maximum of 3 seasons 
          providing the selected player has played at least 3 seasons. 
          <br><br> The menus at the top of the page allow you to:
          <br>\u25cf  Select the Team
          <br>\u25cf	Select the Player
          <br>\u25cf	Determine the number of years to project forward (1-5). This can be used to account for different contract lengths or windows of opportunity.
          <br>\u25cf	Select Stats. This is a multi selection box with a number of different stats available.
          <br><br>The 10 most similar players are displayed on the left hand side table.  The output at same age plot shows how the selected player 
         and the 10 most similar players performed at the same age for the selected stats.  The output of similar players in future seasons provides 
         the output of the 10 most similar players for the selected stats, at the age the selected player will be in the next 1-5 seasons.")
  })
  
  output$suggestions <- renderUI({
    HTML("The selected stats should be limited to 8 or less.  Due to the nature of the kNN algorithm, higher dimensions will lead to
         non-meaningful results as the distance between the points becomes large.  In most cases 5 or less stats will provide the best
         results with regard to similarity.  Stats should be chosen that reflect the nature of the position the selected player currently
         plays.  For a Key Forward this might include goals, shots_at_goal, marks_inside_fifty, score_involvements and inside_fifties, 
         however you have the ability to choose any combination you wish.  There are 2 single number overall ratings included; ratings_points,
         which is the AFL Player Ratings and afl_fantasy_score, which is an older metric.  If using these numbers it is still useful to 
         select 2-3 other stats that help define the selected players position.
         <br><br>In projecting forward, as the search is limited to players who have played at least the extra number of years past the age of the 
         selected player, the projections become more optimistic the more years projected forward.   There will be slightly different lists of 10 most 
         similar players when projecting forward 2 years vs 5 years.  If we consider the case of a 27 year old free agent, the pool of players who
         play until they are 29 in the case of projecting forward 2 years, is much bigger than the list of players who have played until they are 32
         in the case of projecting forward 5 years.  To get an accurate representation it is worthwhile trialling several different future years of projections.
")
  })
  
}

shinyApp(ui, server)