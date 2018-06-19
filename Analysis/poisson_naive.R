library(tidyverse)
library(lubridate)
library(broom)

options(scipen = 99)

set.seed(1234)

results <- read_csv("data/results.csv")
matches <- read_csv("data/wc_matches.csv")
world_cup <- read_csv("data/WorldCupMatches.csv")

team_results <- bind_rows(
  results %>%
    select(date, neutral, team = home_team, goals = home_score, opponent_goals = away_score, opponent = away_team) %>%
    mutate(
      field = case_when(
        neutral == TRUE ~ 0.5,
        TRUE ~ 0
      )
    ),
  results %>%
    select(date, neutral, team = away_team, goals = away_score, opponent_goals = home_score, opponent = home_team) %>%
    mutate(
      field = case_when(
        neutral == T ~ -0.5,
        TRUE ~ 0
      )
    )
) %>%
  filter(year(date) >= 2013)

team_results

crossing(team1_score = 0:5, team2_score = 0:5)

test_sim <- tibble(
  team1 = rpois(1e6, 1.5),
  team2 = rpois(1e6, 1.8)
)

test_sim %>% 
  count(team1, team2) %>% 
  mutate(prob = n/sum(n)) %>% 
  select(-n) %>% 
  spread(team2, prob, fill = 0)

test_sim %>% 
  count(team1, team2) %>% 
  mutate(prob = n/sum(n)) %>% 
  select(-n) %>% filter(prob == max(prob))

test_sim %>%
  count(team1, team2) %>% 
  mutate(prob = n/sum(n)) %>% 
  select(-n) %>%
  filter(team1 > team2) %>%
  summarize_at(vars(prob), funs(sum))

team_stats <- team_results %>%
  mutate(
    team = case_when(
      team == "Korea Republic" ~ "South Korea",
      TRUE ~ team
    ),
    opponent = case_when(
      opponent == "Korea Republic" ~ "South Korea",
      TRUE ~ opponent
    )
  ) %>%
  group_by(team) %>%
  summarize(
    goals = mean(goals),
    conceded = mean(opponent_goals)
  )

team_stats %>%
  filter(team == "Russia" | team == "Saudi Arabia") %>%
  gather(goals, conceded, key = "stat", value = "value") %>%
  spread(team, value)

library(jsonlite)

json <- fromJSON("https://raw.githubusercontent.com/lsv/fifa-worldcup-2018/master/data.json")
teams <- json$teams %>%
  select(id, team = name, fifaCode)

group_stage <- map_df(json$groups, function(x) return(x$matches)) %>%
  as_tibble() %>%
  filter(finished == TRUE)

group_results <- bind_rows(
  group_stage %>%
    select(name, team_id = home_team, goals = home_result, opponent_id = away_team, opponent_goals = away_result),
  group_stage %>%
    select(name, team_id = away_team, goals = away_result, opponent_id = home_team, opponent_goals = home_result)
) %>%
  inner_join(teams, by = c(team_id = "id")) %>%
  inner_join(teams, by = c(opponent_id = "id")) %>%
  rename(team = "team.x", opponent = "team.y")

group_stats <- group_results %>%
  group_by(team) %>%
  summarize(
    goals = mean(goals),
    conceded = mean(opponent_goals)
  )

library(rvest)

rankings <- read_html("https://www.fifa.com/fifa-world-ranking/ranking-table/men/index.html") %>%
  html_node(xpath = '//*[@id="profile"]/div[2]/table') %>%
  html_table()

rankings <- rankings[,1:6]

colnames(rankings) <- c("id", "Rank", "Team", "x", "fifaCode", "points")

fifa_rankings <- as.tibble(rankings) %>%
  select(Rank, fifaCode, points) %>%
  mutate(points = as.numeric(str_extract(points, "(?<=\\().+?(?=\\))"))) %>%
  inner_join(teams)

fifa_rankings

augment_stats <- function(team1, team2) {
  
  team1_current <- group_stats %>%
    filter(team == team1)
  
  team2_current <- group_stats %>%
    filter(team == team2)
  
  team1_stats <- team_stats %>%
    filter(team == team1)
  
  team2_stats <- team_stats %>%
    filter(team == team2)
  
  team1_goals <- 0.9*team1_stats$goals + 0.1*team1_current$goals
  team2_goals <- 0.9*team2_stats$goals + 0.1*team2_current$goals
  
  if(team1_current$conceded == 0) {
    team1_conceded <- team1_stats$conceded
  } else {
    team1_conceded <- 0.9*team1_stats$conceded + 0.1*team1_current$conceded
  }
  
  if(team2_current$conceded == 0) {
    team2_conceded <- team2_stats$conceded
  } else {
    team2_conceded <- 0.9*team2_stats$conceded + 0.1*team2_current$conceded
  }
  
  all_combinations <- tibble(
    team1_goals = rpois(1e6, team1_goals * team2_conceded),
    team2_goals = rpois(1e6, team2_goals * team1_conceded)
  ) %>%
    count(team1_goals, team2_goals) %>%
    mutate(prob = n/sum(n)) %>%
    select(-n)
  
  likely_score <- all_combinations %>%
    filter(prob == max(prob))
  
  team1_win <- all_combinations %>%
    filter(team1_goals > team2_goals) %>%
    summarize_at(vars(prob), funs(team1_win = sum))
  
  team2_win <- all_combinations %>%
    filter(team1_goals < team2_goals) %>%
    summarize_at(vars(prob), funs(team2_win = sum))
  
  draw <- all_combinations %>%
    filter(team1_goals == team2_goals) %>%
    summarise_at(vars(prob), funs(draw = sum))
  
  # likely_score <- str_c(likely_score$team1_goals, likely_score$team2_goals, sep = " - ")
  return(bind_cols(likely_score, team1_win, team2_win, draw))
}

goals_distribution <- function(team1, team2) {
  
  team1_current <- group_stats %>%
    filter(team == team1)
  
  team2_current <- group_stats %>%
    filter(team == team2)
  
  team1_stats <- team_stats %>%
    filter(team == team1)
  
  team2_stats <- team_stats %>%
    filter(team == team2)
  
  team1_goals <- 0.8*team1_stats$goals + 0.2*team1_current$goals
  team1_conceded <- 0.8*team1_stats$conceded + 0.2*team1_current$conceded
  team2_goals <- 0.8*team2_stats$goals + 0.2*team2_current$goals
  team2_conceded <- 0.8*team2_stats$conceded + 0.2*team2_current$conceded
  
  all_combinations <- tibble(
    team1_goals = rpois(1e6, team1_goals * team2_conceded),
    team2_goals = rpois(1e6, team2_goals * team1_conceded)
  )
  
  colnames(all_combinations) <- c(team1, team2)
  
  return(all_combinations %>% gather(1:2, key = "team", value = "goals"))
}

goals_distribution("Russia", "Egypt") %>%
  ggplot(aes(x = goals, fill = team, color = team)) +
  geom_density() +
  facet_wrap(~team, ncol = 1) +
  scale_x_continuous(breaks = 0:15) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.8), family = "Merriweather"),
    plot.subtitle = element_text(size = rel(1.2), family = "Merriweather Light", margin = margin(0,0,20,0)),
    text = element_text(family = "Noto Sans CJK JP Light"),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    # axis.text = element_text(size = rel(0.9)),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

group_stages <- matches %>%
  select(team1, team2) %>%
  mutate(
    stats = map2(team1, team2, augment_stats)
  ) %>%
  unnest()

group_stages %>%
  write_csv("data/forecasts.csv")

group_stages

team_stats
matches %>%
  select(team1) %>%
  anti_join(team_stats, by = c(team1 = "team"))

poisson_model <- glm(goals ~ team + opponent, data = team_results, family = poisson(link=log))

augment_stats2 <- function(team1, team2) {
  team1_goals <- predict(poisson_model, tibble(team = team1, opponent = team2), type = "response")
  team2_goals <- predict(poisson_model, tibble(team = team2, opponent = team1), type = "response")
  
  all_combinations <- dpois(0:10, team1_goals) %o% dpois(0:10, team2_goals) %>%
    as.tibble() %>%
    rownames_to_column("team1_goals") %>%
    gather(V1:V11, key = "team2_goals", value = "prob") %>%
    mutate(
      team1_goals = as.numeric(team1_goals) - 1,
      team2_goals = as.numeric(str_replace(team2_goals, "V", "")) - 1
    ) 
  
  likely_score <- all_combinations %>%
    filter(prob == max(prob))
  
  team1_win <- all_combinations %>%
    filter(team1_goals > team2_goals) %>%
    summarize_at(vars(prob), funs(team1_win = sum))
  
  team2_win <- all_combinations %>%
    filter(team1_goals < team2_goals) %>%
    summarize_at(vars(prob), funs(team2_win = sum))
  
  draw <- all_combinations %>%
    filter(team1_goals == team2_goals) %>%
    summarise_at(vars(prob), funs(draw = sum))
  
  # likely_score <- str_c(likely_score$team1_goals, likely_score$team2_goals, sep = " - ")
  return(bind_cols(likely_score, team1_win, team2_win, draw))
}

augment_stats2("Brazil", "Switzerland")

