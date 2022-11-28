library(shiny)
library(tidyverse)
library(worldfootballR)
library(janitor)

WC_results <-
  fb_match_results(
    country = "",
    gender = "M",
    season_end_year = 2022,
    tier = "",
    non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons"
  )

wc_resclean <- WC_results %>%
  mutate(
    Home = str_sub(Home, end = -4) %>% trimws(),
    Away = str_sub(Away, start = 4) %>% trimws()
  ) %>%
  select(Date, Home, Away, HomeGoals, AwayGoals) %>%
  mutate(
    HomeResult = ifelse(HomeGoals == AwayGoals, 1, ifelse(HomeGoals > AwayGoals, 3, 0)),
    AwayResult = ifelse(HomeResult == 1, 1, ifelse(HomeResult == 3, 0, 3))
  )


team_names <- read_csv('https://raw.githubusercontent.com/mattbarger/WC_results_pool/main/team_name.csv')
team_rankings <- read_csv('https://raw.githubusercontent.com/mattbarger/WC_results_pool/main/team_rankings.csv')

tr_clean <- team_rankings %>%
  pivot_longer(-Coeff, names_to = 'Name', values_to = 'Country') %>%
  mutate(Country = ifelse(
    Country == 'Korea',
    'Korea Republic',
    ifelse(
      Country == 'Iran',
      'IR Iran',
      ifelse(Country == 'USA', 
             'United States',
             Country)
    )
  )) %>%
  left_join(team_names) %>%
  select(Name, Team, Coeff, Country)

results_home <- wc_resclean %>%
  filter(!is.na(HomeGoals)) %>%
  mutate(GD = HomeGoals - AwayGoals) %>%
  select(Country = Home,
         Points = HomeResult)

results_all <- wc_resclean %>%
  filter(!is.na(HomeGoals)) %>%
  mutate(GD = AwayGoals - HomeGoals) %>%
  select(Country = Away,
         Points = AwayResult) %>%
  bind_rows(results_home) %>%
  group_by(Country) %>%
  summarize(GP = n(),
            Points = sum(Points))

totalstandings <- tr_clean %>% 
  left_join(results_all) %>% 
  mutate(Points = replace_na(Points, 0), 
         TotalPoints = Coeff * Points %>% as.integer()) %>% 
  arrange(Name, Team, Coeff, Points, TotalPoints) %>% 
  arrange(Name,-TotalPoints,-GP, -Coeff) %>% 
  group_by(Name, Team) %>%
  summarize(GP = sum(GP, na.rm = T),
            Points = sum(TotalPoints)) %>%
  mutate(Points = as.integer(Points)) %>%
  arrange(-Points)

Participants <- totalstandings %>% select(Name) %>% arrange(Name) %>% unlist() %>% as.vector()


teambyteam <- tr_clean %>% 
  left_join(results_all) %>% 
  mutate(Points = replace_na(Points, 0), TotalPoints = Coeff * Points) %>% 
  arrange(Name, Team, Coeff, Points, TotalPoints) %>% 
  arrange(Name,-TotalPoints,-GP, -Coeff)

ui <- fluidPage(
  ### App Title
  titlePanel('Data Strategy World Cup Pool'),
  sidebarPanel(
    selectInput(
      inputId = 'TeamSelector',
      label = 'Select a Team',
      choices = Participants
    )
  ),
  mainPanel(
    h2(p('Standings')),
    tableOutput(outputId = 'StandingsTable'),
    h3(textOutput(outputId = 'TeamName')),
    h4(textOutput(outputId = 'OwnerName')),
    tableOutput(outputId = 'TeambyTeam')
  )
)

server <- function(input, output) {
  output$StandingsTable <- renderTable(totalstandings)
  output$TeambyTeam <- renderTable(
    teambyteam %>% 
      filter(Name == input$TeamSelector) %>%
      select(Coeff, Country, GP, Points, TotalPoints) %>%
      mutate(Coeff = as.integer(Coeff),
             Points = as.integer(Points),
             TotalPoints = as.integer(TotalPoints)) %>%
      arrange(-Coeff) %>%
      adorn_totals('row')
  )
  output$TeamName <- renderText(
    team_names %>% 
      filter(Name == input$TeamSelector) %>%
      select(Team) %>% unlist() %>% as.vector()
    )
  output$OwnerName <- renderText(
    team_names %>% 
      filter(Name == input$TeamSelector) %>% 
      select(Name) %>% unlist() %>% as.vector()
  )
}

shinyApp(ui = ui, server = server)