### Setup R session ------------------------------------------------------------
needed_packages <- c("rvest", "dplyr", "purrr", "lubridate", "rstan", "ggplot2",
  "tidyr", "portableParallelSeeds", "parallel", "methods")
load_packages <- function(x) {
  if(!(x %in% rownames(installed.packages()))) {
    install.packages(x, repos = c("https://cran.rstudio.com/",
      "http://rweb.crmda.ku.edu/kran"))
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)

# Set options
rstan_options(auto_write = TRUE)
options(mc.cores = (parallel::detectCores() - 1))

rm(list = ls())


### gather-data ----------------------------------------------------------------
scrape_league <- function(x) {
  cont <- TRUE
  while(cont) {
    url_data <- safe_read_html(x)
    
    if(is.null(url_data[[1]])) {
      closeAllConnections()
      Sys.sleep(5)
    } else {
      url_data <- url_data[[1]]
      cont <- FALSE
    }
  }
  
  league_table <- url_data %>%
    html_nodes(css = "table") %>%
    html_table()
  league_table <- league_table[[1]]
  colnames(league_table) <- as.character(league_table[1,])
  colnames(league_table) <- make.names(colnames(league_table), unique = TRUE)
  league_table <- league_table[-1,]
  
  league_table <- league_table %>%
    select(club = TEAM, goals_for = `F`, goals_against = A, points = PTS) %>%
    mutate(club = trimws(club, which = "both"))
  
  teams <- url_data %>%
    html_nodes("td a") %>%
    html_text() %>%
    as.character() %>%
    trimws(which = "both")
  team_urls <- url_data %>%
    html_nodes("td a") %>%
    html_attr("href") %>%
    as.character()
  
  league_table <- league_table %>%
    left_join(data_frame(club = teams, club_url = team_urls), by = "club") %>%
    as_data_frame()
  
  return(league_table)
}
scrape_major_cup <- function(x) {
  cont <- TRUE
  while(cont) {
    url_data <- safe_read_html(x)
    
    if(is.null(url_data[[1]])) {
      closeAllConnections()
      Sys.sleep(5)
    } else {
      url_data <- url_data[[1]]
      cont <- FALSE
    }
  }
  
  league_table <- url_data %>%
    html_nodes(css = "table") %>%
    html_table()
  
  league_table <- map_df(.x = league_table, .f = function(x) {
    colnames(x) <- as.character(x[1,])
    colnames(x) <- make.names(colnames(x), unique = TRUE)
    x <- x[-1,]
    
    x <- x %>%
      select(club = TEAM)
    return(x)
  }) %>%
    bind_rows() %>%
    mutate(club = trimws(club, which = "both"))
  
  teams <- url_data %>%
    html_nodes("td a") %>%
    html_text() %>%
    as.character() %>%
    trimws(which = "both")
  team_urls <- url_data %>%
    html_nodes("td a") %>%
    html_attr("href") %>%
    as.character()
  
  league_table <- league_table %>%
    left_join(data_frame(club = teams, club_url = team_urls), by = "club") %>%
    as_data_frame()
  
  return(league_table)
}
scrape_dom_cup <- function(x) {
  cont <- TRUE
  while(cont) {
    url_data <- safe_read_html(x)
    
    if(is.null(url_data[[1]])) {
      closeAllConnections()
      Sys.sleep(5)
    } else {
      url_data <- url_data[[1]]
      cont <- FALSE
    }
  }

  teams <- url_data %>%
    html_nodes("#stats-fair-play a") %>%
    html_text() %>%
    as.character() %>%
    trimws(which = "both")
  team_urls <- url_data %>%
    html_nodes("#stats-fair-play a") %>%
    html_attr("href") %>%
    as.character()
  
  data_frame(
    club = teams,
    club_url = team_urls
  )
}

leagues <- list(
  belgium = "http://www.espnfc.us/belgian-jupiler-league/6/table",
  denmark = "http://www.espnfc.us/danish-sas-ligaen/7/table",
  england = "http://www.espnfc.us/english-premier-league/23/table",
  england2 = "http://www.espnfc.us/english-league-championship/24/table",
  france = "http://www.espnfc.us/french-ligue-1/9/table",
  france2 = "http://www.espnfc.us/french-ligue-2/96/table",
  germany = "http://www.espnfc.us/german-bundesliga/10/table",
  germany2 = "http://www.espnfc.us/german-2-bundesliga/97/table",
  italy = "http://www.espnfc.us/italian-serie-a/12/table",
  italy2 = "http://www.espnfc.us/italian-serie-b/99/table",
  netherlands = "http://www.espnfc.us/dutch-eredivisie/11/table",
  portugal = "http://www.espnfc.us/portuguese-liga/14/table",
  russia = "http://www.espnfc.us/russian-premier-league/106/table",
  scotland = "http://www.espnfc.us/scottish-premiership/45/table",
  spain = "http://www.espnfc.us/spanish-primera-division/15/table",
  switzerland = "http://www.espnfc.us/swiss-super-league/17/table",
  turkey = "http://www.espnfc.us/turkish-super-lig/18/table"
)

major_cups <- list(
  champions_league = "http://www.espnfc.us/uefa-champions-league/2/table",
  europa_league = "http://www.espnfc.us/uefa-europa-league/2310/table",
  icc = "http://www.espnfc.us/international-champions-cup/2326/table"
)

domestic_cups <- list(
  copa_del_rey = "http://www.espnfc.us/spanish-copa-del-rey/80/statistics/fairplay",
  coppa_italia = "http://www.espnfc.us/italian-coppa-italia/2192/statistics/fairplay",
  coupe_de_france = "http://www.espnfc.us/french-coupe-de-france/182/statistics/fairplay",
  coupe_de_la_ligue = "http://www.espnfc.us/french-coupe-de-la-ligue/159/statistics/fairplay",
  dfb_pokal = "http://www.espnfc.us/german-dfb-pokal/2061/statistics/fairplay",
  efl_cup = "http://www.espnfc.us/efl-cup/41/statistics/fairplay",
  fa_cup = "http://www.espnfc.us/english-fa-cup/40/statistics/fairplay",
  spanish_super_cup = "http://www.espnfc.us/spanish-super-cup/431/statistics/fairplay"
)

safe_read_html <- safely(read_html)
league_urls <- map_df(.x = leagues, .f = scrape_league)
major_cup_urls <- map_df(.x = major_cups, .f = scrape_major_cup)
domestic_cup_urls <- map_df(.x = domestic_cups, .f = scrape_dom_cup)

url_lookup <- list(
  select(league_urls, club, club_url),
  major_cup_urls,
  domestic_cup_urls
) %>%
  bind_rows() %>%
  filter(!is.na(club_url)) %>%
  unique()

full_urls <- league_urls %>%
  select(-club_url) %>%
  full_join(url_lookup, by = "club") %>%
  filter(!is.na(club_url))

safe_read_html <- safely(read_html)
scrape_team <- function(x, y) {
  x <- gsub("/index", "/fixtures", x, fixed = TRUE)
  
  cont <- TRUE
  while(cont) {
    url_data <- safe_read_html(x)
    
    if(is.null(url_data[[1]])) {
      closeAllConnections()
      Sys.sleep(5)
    } else {
      url_data <- url_data[[1]]
      cont <- FALSE
    }
  }
  date <- url_data %>%
    html_nodes(".headline") %>%
    html_text() %>%
    as.character()
  if ("LIVE" %in% date) {
    date[which(date == "LIVE")] <- format(Sys.Date(), "%b %d, %Y")
  }
  date <- mdy(date)
  home_team <- url_data %>%
    html_nodes(".score-home-team .team-name") %>%
    html_text() %>%
    as.character()
  away_team <- url_data %>%
    html_nodes(".score-away-team .team-name") %>%
    html_text() %>%
    as.character()
  home_score <- url_data %>%
    html_nodes(".home-score") %>%
    html_text() %>%
    as.character() %>%
    gsub(" ", "", x = .) %>%
    gsub( " *\\(.*?\\) *", "", x = .) %>%
    as.numeric()
  away_score <- url_data %>%
    html_nodes(".away-score") %>%
    html_text() %>%
    as.character() %>%
    gsub(" ", "", x = .) %>%
    gsub( " *\\(.*?\\) *", "", x = .) %>%
    as.numeric()
  competition <- url_data %>%
    html_nodes(".score-column.score-competition") %>%
    html_text() %>%
    as.character()
  
  team_data <- data_frame(
    date = date,
    home = home_team,
    away = away_team,
    home_goals = home_score,
    away_goals = away_score,
    competition = competition
  ) %>%
    arrange(date) %>%
    unique()
  
  abbrev <- as_data_frame(table(c(team_data$home, team_data$away))) %>%
    top_n(n = 1, wt = n) %>%
    select(Var1) %>%
    flatten_chr()
  
  if (nrow(team_data) < 3) {
    ret_data <- data_frame(
      club = y,
      abbrev = y,
      team_data = NA
    )
  } else {
    if (abbrev == "Sporting") {
      team_data$home[which(team_data$home == "Sporting")] <- y
      team_data$away[which(team_data$away == "Sporting")] <- y
      ret_data <- data_frame(
        club = y,
        abbrev = y,
        team_data = list(team_data)
      )
    } else {
      team_data <- filter(team_data, home != "Sporting", away != "Sporting")
      ret_data <- data_frame(
        club = y,
        abbrev = abbrev,
        team_data = list(team_data)
      )
    }
  }
  
  return(ret_data)
}

full_data <- map2_df(.x = full_urls$club_url, .y = full_urls$club,
  .f = scrape_team)

team_lookup <- select(full_data, -team_data)

full_data <- bind_rows(full_data$team_data) %>%
  unique() %>%
  arrange(date, home) %>%
  left_join(select(full_data, -team_data), by = c("home" = "abbrev")) %>%
  rename(home_club = club) %>%
  left_join(select(full_data, -team_data), by = c("away" = "abbrev")) %>%
  rename(away_club = club) %>%
  mutate(
    real_home = ifelse(is.na(home_club), home, home_club),
    real_away = ifelse(is.na(away_club), away, away_club),
    home = real_home,
    away = real_away
  ) %>%
  select(-(home_club:real_away)) %>%
  mutate(home_game = ifelse(competition %in% c("Champions Cup"), 0, 1)) %>%
  filter(!(date < Sys.Date() & is.na(home_goals))) %>%
  filter(date > ymd("2016-03-01")) %>%
  rename(h_goals = home_goals, a_goals = away_goals)

rm(list = setdiff(ls(), c("full_urls", "full_data"))); gc()
save(full_urls, file = "_data/full_urls.rda")
save(full_data, file = "_data/full_data.rda")


### Fit Stan Model -------------------------------------------------------------

load("_data/full_data.rda")
fit_data <-  full_data %>%
  filter(!is.na(h_goals),
    competition %in% c("Bund", "Prem", "Ligue 1", "La Liga", "Serie A"))

filter_games <- TRUE
while(filter_games) {
  team_counts <- table(c(fit_data$home, fit_data$away)) %>% as_data_frame() %>%
    arrange(desc(n)) %>%
    filter(n >= 5) %>%
    select(team = Var1) %>%
    arrange(team) %>%
    mutate(code = seq_len(nrow(.)))

  fit_data <- fit_data %>%
    select(-competition) %>%
    left_join(team_counts, by = c("home" = "team")) %>%
    rename(home_code = code) %>%
    left_join(team_counts, by = c("away" = "team")) %>%
    rename(away_code = code) %>%
    filter(!is.na(home_code), !is.na(away_code))
  
  new_min <- table(c(fit_data$home, fit_data$away)) %>% as.numeric() %>% min()
  if (new_min >= 5) {
    filter_games <- FALSE
  }
}

stan_data <- list(
  num_clubs = nrow(team_counts),
  num_games = nrow(fit_data),
  home = fit_data$home_code,
  away = fit_data$away_code,
  h_goals = fit_data$h_goals,
  a_goals = fit_data$a_goals,
  homeg = fit_data$home_game
)

bivpois <- stan(file = "_data/stan-models/biv_pois.stan", data = stan_data,
  chains = 2, iter = 15000, warmup = 5000, init = "random", thin = 1,
  cores = 2, control = list(adapt_delta = 0.99))
mixeffc <- stan(file = "_data/stan-models/mix_effc.stan", data = stan_data,
  chains = 2, iter = 15000, warmup = 5000, init = "random", thin = 1,
  cores = 2, control = list(adapt_delta = 0.99))

summary(bivpois, pars = c("mu", "eta", "sigma_a", "sigma_d", "sigma_r"),
  probs = c(0.025, 0.25, 0.75, 0.975))$summary
summary(mixeffc, pars = c("mu", "eta", "sigma_a", "sigma_d", "sigma_g"),
  probs = c(0.025, 0.25, 0.75, 0.975))$summary

### Examine convergence --------------------------------------------------------
as.data.frame(summary(int_fit)[[1]]) %>%
  mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>%
  ggplot(aes(x = Parameter, y = Rhat, color = Parameter)) +
  geom_jitter(height = 0, width = 0.4, show.legend = FALSE) +
  geom_hline(aes(yintercept = 1.1), linetype = "dashed") +
  labs(y = expression(hat(italic(R))),
    title = "Convergence for Random Intercept Model") +
  theme_bw() -> rnd_int_rhat
ggsave(filename = "rnd_int_rhat.png", plot = rnd_int_rhat,
  path = "model_output/", width = 10, height = 7, units = "in", dpi = 300)

as.data.frame(summary(lng_fit)[[1]]) %>%
  mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>%
  ggplot(aes(x = Parameter, y = Rhat, color = Parameter)) +
  geom_jitter(height = 0, width = 0.4, show.legend = FALSE) +
  geom_hline(aes(yintercept = 1.1), linetype = "dashed") +
  labs(y = expression(hat(italic(R))),
    title = "Convergence for Long Form Model") +
  theme_bw() -> lng_rnd_rhat
ggsave(filename = "lng_rnd_rhat.png", plot = rnd_int_rhat,
  path = "model_output/", width = 10, height = 7, units = "in", dpi = 300)

as.data.frame(summary(biv_fit)[[1]]) %>%
  mutate(Parameter = as.factor(gsub("\\[.*]", "", rownames(.)))) %>%
  ggplot(aes(x = Parameter, y = Rhat, color = Parameter)) +
  geom_jitter(height = 0, width = 0.4, show.legend = FALSE) +
  geom_hline(aes(yintercept = 1.1), linetype = "dashed") +
  labs(y = expression(hat(italic(R))),
    title = "Convergence for Bivariate Poisson Model") +
  theme_bw() -> biv_poi_rhat
ggsave(filename = "biv_poi_rhat.png", plot = rnd_int_rhat,
  path = "model_output/", width = 10, height = 7, units = "in", dpi = 300)


### Analyze results ------------------------------------------------------------
params_summary <- summary(int_fit, pars = c("mu", "H", "alpha", "delta"),
  probs = c(0.025, 0.25, 0.75, 0.975))$summary

params <- extract(int_fit, pars = c("mu", "H", "alpha", "delta"))
alpha <- colMeans(params$alpha)
delta <- colMeans(params$delta)
mu <- mean(params$mu)
H <- mean(params$H)


results <- data_frame(
  club = team_counts$team,
  attacking = alpha,
  defense = delta
) %>%
  mutate(
    home_goals = exp(mu + alpha),
    away_goals = exp(mu + delta),
    exp_margin = home_goals - away_goals
  ) %>%
  arrange(desc(exp_margin))
