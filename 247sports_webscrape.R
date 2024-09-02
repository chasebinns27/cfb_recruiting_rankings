#load packages
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

#set URL and read html
url <- 'https://247sports.com/season/2024-football/recruitrankings/'
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
player_uncommitted_schools_w_index <- cbind(uncommitted_school, 1:nrow(uncommitted_school))
colnames(player_uncommitted_schools_w_index) <- c('school', 'index')
player_uncommitted_schools_no_dups <- player_uncommitted_schools_w_index[player_uncommitted_schools_w_index$index %% 2 != 0, ]
  

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


#Bring in salary data
base_url <- "http://www.spotrac.com/nfl/"
read.base <- read_html(base_url)
team.URL <- read.base %>% html_nodes(".team-name") %>% html_attr('href')
team.URL

team.names <- gsub("/cap/","", gsub(base_url, "", team.URL))

data.creator <- function(link) {
  read_html(link) %>% html_nodes("table") %>% html_table(header=TRUE, fill=TRUE) -> res
  names(res) <- c("Active","Draft","Inactive","Team1")
  return(res)
}
team.names <- gsub("-", " ", team.names)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
team.names <- sapply(team.names, simpleCap)
NFL.scrape <- sapply(team.URL, function(x) {data.creator(x)})
names(NFL.scrape) <- team.names 
# This is a hack but it works
Actives <- lapply(NFL.scrape, function(x){x$Active})
rep.res <- sapply(seq(1,32), function(x) {dim(Actives[[x]])[[1]]})


#NFL Player data frame
nfl_player_names <- bind_rows(lapply(names(Actives), function(sublist_name) {
  data.frame(Sublist = sublist_name, Actives[[sublist_name]], row.names = NULL)})) %>%
    #mutate(name = ifelse(is.na(Active.Players..88.)==FALSE, Active.Players..88.,
                                       #ifelse(is.na(Active.Players..89.) == FALSE, Active.Players..89.,
                                              #ifelse(is.na(Active.Players..90.) == FALSE, Active.Players..90.,
                                                     #ifelse(is.na(Active.Players..91.) == FALSE, Active.Players..91., Active.Players..92.))))) %>%
  select(matches('Active')) %>%
  rowwise() %>%
  transmute(player_name = paste(na.omit(c_across()), collapse = ", ")) %>%
  mutate(player_name = gsub("^[^[:space:]]+\\s+", "", player_name))

nfl_player_data <- bind_rows(lapply(names(Actives), function(sublist_name) {
  data.frame(Sublist = sublist_name, Actives[[sublist_name]], row.names = NULL)})) %>%
  mutate(Cap.. = as.numeric(Cap..),
       Cap.Hit = as.numeric(gsub("[\\$,]", "", Cap.Hit)),
       team = str_match(Sublist, "/nfl/([A-Za-z ]+)")[, 2]) %>%
  bind_cols(nfl_player_names) %>%
  select(player_name, team, position = Pos., Cap.Hit, Base.Salary, Signing.Bonus, Roster.Bonus, Option.Bonus, Workout.Bonus, Restruc..Bonus, Misc.,
         Dead.Cap, cap_pct = Cap..) %>%
  arrange(desc(Cap.Hit))

# positions <- nfl_player_data %>%
#   select(position) %>%
#   distinct()

position_value <- nfl_player_data %>%
  group_by(position, team) %>%
  mutate(Rank = row_number()) %>%
  mutate(starter = case_when(
    position == 'QB' & Rank == 1 ~ 1,
    position == 'DE' & Rank <= 2 ~ 1,
    position == 'OLB' & Rank == 1 ~ 1,
    position == 'LT' & Rank == 1 ~ 1,
    position == 'DT' & Rank <= 2 ~ 1,
    position == 'WR' & Rank <= 2 ~ 1,
    position == 'G' & Rank <= 2 ~ 1,
    position == 'ILB' & Rank == 1 ~ 1,
    position == 'CB' & Rank <= 2 ~ 1,
    position == 'RT' & Rank == 1 ~ 1,
    position == 'FS' & Rank == 1 ~ 1,
    position == 'SS' & Rank == 1 ~ 1,
    position == 'TE' & Rank == 1 ~ 1,
    position == 'S' & Rank == 1 ~ 1,
    position == 'C' & Rank == 1 ~ 1,
    position == 'RB' & Rank == 1 ~ 1,
    position == 'LB' & Rank == 1 ~ 1,
    position == 'T' & Rank == 1 ~ 1,
    position == 'FB' & Rank == 1 ~ 1,
    position == 'K' & Rank == 1 ~ 1,
    position == 'P' & Rank == 1 ~ 1,
    position == 'LS' & Rank == 1 ~ 1,
    position == 'OL' & Rank == 1 ~ 1,
    position == 'WLB' & Rank == 1 ~ 1
  )) %>%
  filter(starter == 1) %>%
  mutate(Cap.Hit = ifelse(is.na(Cap.Hit) == TRUE, 750000, Cap.Hit)) %>%
  mutate(position = case_when(
    position == 'OLB' | position == 'DE' ~ 'Edge',
    position == 'DT' ~ 'DL',
    position == 'LT' | position == 'RT' | position == 'T' ~ 'OT',
    position == 'LB' | position == 'ILB' | position == 'WLB' ~ 'LB',
    position == 'SS' | position == 'S' | position == 'FS' ~ 'S',
    position == 'C' | position == 'G' | position == 'OL' ~ 'IOL',
    position == 'QB' ~ 'QB',
    position == 'WR' ~ 'WR',
    position == 'CB' ~ 'CB',
    position == 'TE' ~ 'TE',
    position == 'RB' ~ 'RB',
    position == 'FB' ~ 'FB',
    position == 'K' ~ 'K',
    position == 'P' ~ 'P',
    position == 'LS' ~ 'LS'
  ))

starter_team_total <- position_value %>%
  group_by(team) %>%
  summarize(total_starter_cap_hit = sum(Cap.Hit))

starter_hit_pcts <- position_value %>%
  inner_join(starter_team_total, by = 'team') %>%
  mutate(hit_pct = Cap.Hit / total_starter_cap_hit)

position_pct <- starter_hit_pcts %>%
  group_by(position) %>%
  summarize(mean_cap_pct = mean(hit_pct)) %>%
  arrange(desc(mean_cap_pct))






###Combine data
recruiting_power_rankings <- final_recruiting_data %>%
  left_join(position_pct, by = "position") %>%
  mutate(value = mean_cap_pct * as.numeric(score)) %>%
  group_by(committed_school) %>%
  summarise(class_total = sum(value)) %>%
  arrange(desc(class_total)) %>%
  filter(is.na(committed_school) == FALSE)

power_remaining <- final_recruiting_data %>%
  left_join(position_pct, by = "position") %>%
  mutate(value = mean_cap_pct * as.numeric(score)) %>%
  group_by(committed_school) %>%
  summarise(class_total = sum(value)) %>%
  arrange(desc(class_total)) %>%
  filter(is.na(committed_school) == TRUE) %>%
  select(power_remaining = class_total)

recruiting_details <- final_recruiting_data %>%
  left_join(position_pct, by = "position") %>%
  mutate(value = mean_cap_pct * as.numeric(score))




###save image
setwd("C:/Users/chase/OneDrive/PostGrad 3/College Football Recruiting")
save.image('recruiting_power_rankings')
