library(dplyr)
#install.packages('rvest')
library(rvest)
#install.packages('xml2')
library(xml2)
#install.packages('janitor')
library(janitor)
#install.packages('stringr')
library(stringr)
library(purrr)
library(gtExtras)
library(gt)

recruiting_data_pull <- function(year) {
  #set URL and read html
  url <- paste0('https://247sports.com/season/', year,'-football/recruitrankings/')
  page <- read_html(url)
  
  #get player info
  player_names <- page %>%
    html_nodes(".rankings-page__name-link") %>%
    html_text() %>%
    as.data.frame()
  
  player_rankings <- page %>%
    html_nodes(".score") %>%
    html_text() %>%
    as.data.frame()
  
  player_rankings <- player_rankings[-1,] %>%
    as.data.frame()
  
  player_position <- page %>%
    html_nodes(".position") %>%
    html_text() %>%
    as.data.frame()
  
  player_committed_schools <- page %>%
    html_nodes("a.img-link") %>%
    html_nodes("img") %>%
    html_attr("alt") %>%
    as.data.frame()
  
  #Remove duplicates from committed schools
  player_committed_schools_w_index <- cbind(player_committed_schools, 1:nrow(player_committed_schools))
  colnames(player_committed_schools_w_index) <- c('school', 'index')
  player_committed_schools_no_dups <- player_committed_schools_w_index[player_committed_schools_w_index$index %% 2 != 0, ]
  
  
  
  higher_selector <- ".rankings-page__list-item"  # Replace with the CSS selector for higher hierarchy nodes
  lower_selector <- ".cb-block"  # Replace with the CSS selector for lower hierarchy nodes
  
  # Identify nodes of a higher hierarchy that do not contain nodes of a lower hierarchy
  higher_hierarchy_nodes <- page %>%
    html_nodes(higher_selector)
  
  lower_hierarchy_nodes <- page %>%
    html_nodes(lower_selector)
  
  # Filter the higher hierarchy nodes that do not contain lower hierarchy nodes
  filtered_nodes <- Filter(function(node) {
    lower_nodes <- html_nodes(node, lower_selector)
    length(lower_nodes) == 0
  }, higher_hierarchy_nodes)
  
  player_committed <- filtered_nodes %>%
    html_nodes(".rankings-page__name-link") %>%
    html_text() %>%
    as.data.frame()
  
  commits <- cbind(player_committed, player_committed_schools_no_dups)
  
  #Find NA Commits
  na_commits <- page %>%
    html_nodes(higher_selector) %>%
    .[sapply(., function(node) any(html_nodes(node, "div.cb-block span") %>% html_text() == "N/A"))] %>%
    html_nodes(".rankings-page__name-link") %>%
    html_text() %>%
    as.data.frame()
  
  #Find player that aren't committed but have schools leading the race
  non_commits <- page %>%
    html_nodes(higher_selector) %>%
    .[sapply(., function(node) {
      span_nodes <- html_nodes(node, xpath = ".//span[contains(@class, 'percentage')]")
      length(span_nodes) > 0
    })] %>%
    html_nodes(".rankings-page__name-link") %>%
    html_text() %>%
    as.data.frame()
  
  #uncommitted school
  uncommitted_school <- page %>%
    html_nodes("div.cb-block:not(.cb-block--bottom)")%>%
    html_nodes("img") %>%
    html_attr("alt") %>%
    as.data.frame()
  
  uncommitted_school_pct <- page %>%
    html_nodes("div.cb-block:not(.cb-block--bottom)") %>%
    html_nodes(".percentage") %>%
    html_text() %>%
    as.data.frame()
  
  
  
  #Remove duplicates from uncommitted schools
  if (nrow(uncommitted_school) > 0) {
    # Proceed with the operations if there are records
    player_uncommitted_schools_w_index <- cbind(uncommitted_school, 1:nrow(uncommitted_school))
    colnames(player_uncommitted_schools_w_index) <- c('school', 'index')
    player_uncommitted_schools_no_dups <- player_uncommitted_schools_w_index[player_uncommitted_schools_w_index$index %% 2 != 0, ]
  } else {
    # Handle the case where there are no records
    player_uncommitted_schools_w_index <- data.frame(school = character(), index = integer())
    player_uncommitted_schools_no_dups <- player_uncommitted_schools_w_index
  }
  
  
  non_commits_w_data <- cbind(non_commits, player_uncommitted_schools_no_dups, uncommitted_school_pct)
  colnames(non_commits_w_data) <- c('player', 'leading school', 'index', 'crystal_ball_pct')
  
  
  recruiting_data <- cbind(player_names, 1:nrow(player_names), player_rankings,
                           player_position)
  colnames(recruiting_data) <- c('player', 'rank', 'score', 'position')
  
  committed_players <- player_committed %>%
    pull()
  
  final_recruiting_data <- recruiting_data %>%
    mutate(commitment_status = ifelse(player %in% committed_players, 'committed', 'not committed')) %>%
    left_join(select(commits, player = ., committed_school = school), by = 'player') %>%
    left_join(select(non_commits_w_data, player, leading_school = `leading school`, crystal_ball_pct), by = 'player') %>%
    mutate(position = trimws(position)) %>%
    mutate(position = ifelse(position == 'ATH', 'WR', position))
  
  return(final_recruiting_data)
}

data_2024 <- recruiting_data_pull(year = '2024')
