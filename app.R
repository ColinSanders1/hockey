library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(ggimage)
library(hockeyR)
library(glue)
library(magick)
library(sportyR)
library(DT)

db_path <- "hockeydat.sqlite"

ui <- fluidPage(
  titlePanel("NHL Game Recap"),
  sidebarLayout(
    sidebarPanel(
      dateInput("game_date", "Select Game Date:", value = Sys.Date() - 1),
      uiOutput("matchup_selector"), 
      actionButton("load", "Load Game Data")
    ),
    mainPanel(
      plotOutput("shotPlot"),
      uiOutput("goalHighlights"),
      h3("xG Leaders"),
      DTOutput("xgLeaders") 
    
    )
  )
)

server <- function(input, output, session) {
  db <- dbConnect(SQLite(), dbname = db_path)
  
  matchups <- reactive({
    query <- glue::glue(
      "SELECT home_name, away_name FROM hockey_data WHERE game_date = '{input$game_date}'"
    )
    games <- dbGetQuery(db, query)
    if (nrow(games) > 0) {
      games %>%
        mutate(matchup = glue("{away_name} @ {home_name}")) %>%
        pull(matchup)
    } else {
      NULL
    }
  })
  
  output$matchup_selector <- renderUI({
    available_matchups <- matchups()
    if (!is.null(available_matchups)) {
      selectInput("selected_matchup", "Choose a Matchup:", choices = available_matchups)
    } else {
      HTML("<p>No games available for the selected date.</p>")
    }
  })
  
  game_data <- eventReactive(input$load, {
    req(input$selected_matchup)
    selected_matchup <- input$selected_matchup
    home_team <- strsplit(selected_matchup, " @ ")[[1]][2]
    away_team <- strsplit(selected_matchup, " @ ")[[1]][1]
    
    query <- glue::glue(
      "SELECT * FROM hockey_data WHERE game_date = '{input$game_date}' 
       AND home_name = '{home_team}' AND away_name = '{away_team}'"
    )
    dbGetQuery(db, query)
  })
  
  output$shotPlot <- renderPlot({
    game <- game_data()
    req(nrow(game) > 0)
    
    team_logos <- hockeyR::team_logos_colors %>%
      filter(team_abbr %in% unique(c(game$home_abbr, game$away_abbr))) %>%
      mutate(
        x = ifelse(full_team_name == unique(game$home_name), 50, -50),
        y = 0
      )
    
    transparent <- function(img) {
      magick::image_fx(img, expression = "0.3*a", channel = "alpha")
    }
    
    fenwick_events <- c("MISSED_SHOT", "SHOT", "GOAL")
    
    shots <- game %>%
      filter(event_type %in% fenwick_events) %>%
      left_join(team_logos, by = c("event_team_abbr" = "team_abbr")) %>%
      distinct(event_idx, .keep_all = TRUE)
    
    geom_hockey("nhl") +
      ggimage::geom_image(
        data = team_logos,
        aes(x = x, y = y, image = team_logo_espn),
        image_fun = transparent, size = 0.22, asp = 2.35
      ) +
      geom_point(
        data = shots,
        aes(x_fixed, y_fixed),
        size = 6,
        color = shots$team_color1,
        shape = ifelse(shots$event_type == "GOAL", 19, 1)
      ) +
      labs(
        title = glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
        subtitle = glue(
          "{unique(game$game_date)}\n
          {unique(shots$away_name)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_name)}"
        ),
        caption = "data from hockeyR | plot made with sportyR"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = .9)
      )
  })
  
  output$goalHighlights <- renderUI({
    game <- game_data()
    req(nrow(game) > 0)
    
    goalhighlights <- game %>%
      filter(event_type == "GOAL") %>%
      distinct(event_idx, .keep_all = TRUE) %>%
      mutate(
        scoring_team = event_team,
        goal_scorer = event_player_1_name,
        assisters = case_when(
          !is.na(event_player_2_name) & !is.na(event_player_3_name) ~ paste(event_player_2_name, event_player_3_name, sep = ", "),
          !is.na(event_player_2_name) ~ event_player_2_name,
          !is.na(event_player_3_name) ~ event_player_3_name,
          TRUE ~ NA_character_
        ),
        goalie = event_goalie_name
      )
    
    if (nrow(goalhighlights) > 0) {
      highlights_html <- paste0(
        "<table style='width:100%; border-collapse: collapse;'>",
        "<tr>
        <th style='border: 1px solid black; padding: 5px;'>Scoring Team</th>
        <th style='border: 1px solid black; padding: 5px;'>Goal Scorer</th>
        <th style='border: 1px solid black; padding: 5px;'>Assisters</th>
        <th style='border: 1px solid black; padding: 5px;'>Goalie</th>
        <th style='border: 1px solid black; padding: 5px;'>Highlight Clip</th>
      </tr>",
        paste0(
          apply(goalhighlights, 1, function(row) {
            paste0(
              "<tr>",
              "<td style='border: 1px solid black; padding: 5px;'>", row["scoring_team"], "</td>",
              "<td style='border: 1px solid black; padding: 5px;'>", row["goal_scorer"], "</td>",
              "<td style='border: 1px solid black; padding: 5px;'>", ifelse(is.na(row["assisters"]), "None", row["assisters"]), "</td>",
              "<td style='border: 1px solid black; padding: 5px;'>", row["goalie"], "</td>",
              "<td style='border: 1px solid black; padding: 5px;'><a href='", 
              row["highlightClipSharingUrl"], 
              "' target='_blank'>Watch</a></td>",
              "</tr>"
            )
          }),
          collapse = ""
        ),
        "</table>"
      )
      HTML(highlights_html)
    } else {
      HTML("<p>No goal highlights available for this game.</p>")
    }
  })
  
  output$xgLeaders <- renderDT({
    game <- game_data()
    if (nrow(game) == 0) {
      return(datatable(data.frame(Message = "No data available for xG Leaders"), 
                       options = list(dom = "t"), 
                       rownames = FALSE))
    }
    
    game <- calculate_xg(game)  # Ensure this function works correctly
    if (!"xg" %in% colnames(game)) {
      stop("The column 'xg' does not exist. Ensure 'calculate_xg()' is correctly defined.")
    }
    
    xg_leaders <- game %>%
      group_by(event_player_1_name, event_team) %>%  # Group by player and team
      summarize(
        shots = sum(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT"), na.rm = TRUE),
        goals = sum(event_type == "GOAL", na.rm = TRUE),
        total_xg = round(sum(xg, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(total_xg)) %>%
      head(5)
    
    datatable(
      xg_leaders,
      options = list(pageLength = 5, dom = "t", order = list(list(4, "desc"))),
      rownames = FALSE
    )
  })
  
  onStop(function() {
    dbDisconnect(db)
  })
}

shinyApp(ui = ui, server = server)