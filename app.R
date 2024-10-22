library(shiny)
library(shinydashboard)
library(gt)
library(gtExtras)
library(tidyverse)
library(mlbplotR)
library(shinythemes)

#setwd("~/Downloads/2025 Reds Analytics Trainee Take Home")
predictions <- read_csv("predictions.csv")
stats <- read_csv("all_years_stats.csv")

# create plot showing previous year pitch mix
prev_year_mix_plot <- function(player, year){
  player_name <- sub("(.*),\\s*(.*)", "\\2 \\1", player)
  mix <- stats |> 
    filter(player_name == player & game_year == as.numeric(year)-1) |> 
    select(Fastballs = pitch_type_fb, `Breaking Balls` = pitch_type_bb, 
           `Off-Speed Pitches` = pitch_type_os) |> 
    pivot_longer(cols = c("Fastballs", "Breaking Balls", "Off-Speed Pitches"), 
                 names_to = "Pitch Type", values_to = "percentage")
  if (nrow(mix) == 0) {
    return(ggplot() + ggtitle("No data available for this player/year"))
  }
  mix |> 
    ggplot(aes(`Pitch Type`, percentage, fill = `Pitch Type`)) +
    geom_col() +
    ggthemes::theme_clean() +
    labs(title = paste(player_name, "Pitch Mix in", as.numeric(year)-1)) +
    geom_label(aes(label = paste(round(percentage, 2)*100, "%")), fill = "white")
}
prev_year_mix_plot("Alonso, Pete", 2022)

# create plot projecting next year pitch mix
projected_mix_plot <- function(player, year){
  player_name <- sub("(.*),\\s*(.*)", "\\2 \\1", player)
  mix <- stats |> 
    filter(player_name == player & game_year == as.numeric(year)-1) |> 
    select(Fastballs = predicted_fb, `Breaking Balls` = predicted_bb, 
           `Off-Speed Pitches` = predicted_os) |> 
    pivot_longer(cols = c("Fastballs", "Breaking Balls", "Off-Speed Pitches"), 
                 names_to = "Pitch Type", values_to = "percentage")
  if (nrow(mix) == 0) {
    return(ggplot() + ggtitle("No data available for this player/year"))
  }
  mix |> 
    ggplot(aes(`Pitch Type`, percentage, fill = `Pitch Type`)) +
    geom_col() +
    ggthemes::theme_clean() +
    labs(title = paste(player_name, "Projected Pitch Mix in", year)) +
    geom_label(aes(label = paste(round(percentage, 2)*100, "%")), fill = "white")
}
projected_mix_plot("Alonso, Pete", 2022)

# create table with previous years stats
prev_year_stats_table <- function(player, year){
  player_name <- sub("(.*),\\s*(.*)", "\\2 \\1", player)
  player_stats <- stats |> filter(player_name == player & game_year == as.numeric(year)-1) |> 
    select(batter_id, at_bats, strikeouts, home_runs, walks, wOBA, BABIP, ISO)
  if (nrow(player_stats) == 0) {
    return()
  }
  id <- player_stats |> pull(batter_id)
  player_stats |> 
    select(-batter_id) |> 
    pivot_longer(cols = c("at_bats", "strikeouts", "home_runs", "walks", "wOBA",
                          "BABIP", "ISO"), names_to = "Stat", values_to = "Value") |> 
    gt() |> 
    gt_theme_538() |> 
    fmt_number(rows = 1:4, decimals = 0) |> 
    fmt_number(rows = 5:7, decimals = 3) |> 
    tab_header(title = paste(player_name, "Stats in", as.numeric(year)-1)) |> 
    data_color(columns = "Value", rows = 3, palette = "RdYlGn", domain = c(0, 62)) |> 
    data_color(columns = "Value", rows = 4, palette = "RdYlGn", domain = c(0, 129)) |> 
    data_color(columns = "Value", rows = 5, palette = "RdYlGn", domain = c(0, 0.4586411)) |> 
    data_color(columns = "Value", rows = 6, palette = "RdYlGn", domain = c(0, 0.3179572)) |> 
    data_color(columns = "Value", rows = 7, palette = "RdYlGn", domain = c(0, 0.3161004))
}
prev_year_stats_table("Alonso, Pete", 2022)

# add player headshot
player_headshot <- function(player){
  id <- stats |> filter(player_name == player) |> pull(batter_id)
  ggplot(mapping = aes(x = 0, y = 1)) +
    theme_void() +
    mlbplotR::geom_mlb_headshots(player_id = id[1])
}
player_headshot("Alonso, Pete")


# Define UI for application
ui <- fluidPage(
    theme = shinytheme("yeti"),
    # Application title
    titlePanel("Pitch Mix Predictor"),
    # Sidebar with a dropdown input for player selection and another for year selection
    sidebarLayout(
        sidebarPanel(
            selectInput("Player",
                        "Player",
                        choices = stats$player_name),
            selectInput("Year",
                        "Prediction Year",
                        choices = c(2022, 2023, 2024))
        ),

        mainPanel(
          fluidRow(
            column(6, plotOutput("headshot")),
            column(6, gt_output("prevyearstats"))
          ),
          fluidRow(
           column(6, plotOutput("pitchmixprior")),
           column(6, plotOutput("pitchmixpreds")))
        )
    )
)

# Define server logic
server <- function(input, output) {
    output$pitchmixprior <- renderPlot({
      prev_year_mix_plot(input$Player, input$Year)
    })
    output$pitchmixpreds <- renderPlot({
      projected_mix_plot(input$Player, input$Year)
    })
    output$prevyearstats <- render_gt({
      prev_year_stats_table(input$Player, input$Year)
    })
    output$headshot <- renderPlot({
      player_headshot(input$Player)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
