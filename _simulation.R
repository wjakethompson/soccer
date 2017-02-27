### Setup R session ------------------------------------------------------------
needed_packages <- c("dplyr", "purrr", "rstan", "tidyr", "parallel",
  "portableParallelSeeds", "parallel")
load_packages <- function(x) {
  if(!(x %in% rownames(installed.packages()))) {
    install.packages(x, repos = "https://cran.rstudio.com/")
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)

# Set options
rstan_options(auto_write = TRUE)
options(mc.cores = (parallel::detectCores() - 1))

rm(list = ls())


### define functions -----------------------------------------------------------
generate_bivpois <- function(run, seeds, num_club) {
  setSeeds(seeds, run = run)
  
  teams <- data_frame(
    club = paste0("club", sprintf("%02d", seq_len(num_club))),
    attack = rnorm(n = num_club, mean = 0, sd = 0.35),
    defend = rnorm(n = num_club, mean = 0, sd = 0.35),
    cov = rnorm(n = num_club, mean = 0, sd = 0.1)
  )
  
  games <- teams %>%
    select(club) %>%
    flatten_chr() %>%
    crossing(., .)
  colnames(games) <- c("home", "away")
  
  games <- games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend, h_cov = cov) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend, a_cov = cov) %>%
    mutate(
      lambda1 = exp(0 + 0.5 + h_att + a_def),
      lambda2 = exp(0 + a_att + h_def),
      lambda3 = exp(h_cov + a_cov),
      h_goals = rpois(n = nrow(.), lambda = (lambda1 + lambda3)),
      a_goals = rpois(n = nrow(.), lambda = (lambda2 + lambda3)),
      home_game = 1
    ) %>%
    select(home, away, h_goals, a_goals, home_game)
  
  list(
    method = "bivpois",
    teams = teams,
    games = games
  )
}
generate_gri <- function(run, seeds, num_club) {
  setSeeds(seeds, run = run)
  
  teams <- data_frame(
    club = paste0("club", sprintf("%02d", seq_len(num_club))),
    attack = rnorm(n = num_club, mean = 0, sd = 0.35),
    defend = rnorm(n = num_club, mean = 0, sd = 0.35)
  )
  
  games <- teams %>%
    select(club) %>%
    flatten_chr() %>%
    crossing(., .)
  colnames(games) <- c("home", "away")
  
  games <- games %>%
    filter(home != away) %>%
    left_join(teams, by = c("home" = "club")) %>%
    rename(h_att = attack, h_def = defend) %>%
    left_join(teams, by = c("away" = "club")) %>%
    rename(a_att = attack, a_def = defend) %>%
    mutate(
      gamma = rnorm(n = nrow(.), mean = 0, sd = 0.1),
      lambda1 = exp(0 + 0.5 + h_att + a_def + gamma),
      lambda2 = exp(0 + a_att + h_def + gamma),
      h_goals = rpois(n = nrow(.), lambda = (lambda1)),
      a_goals = rpois(n = nrow(.), lambda = (lambda2)),
      home_game = 1
    ) %>%
    select(home, away, h_goals, a_goals, home_game)
  
  list(
    method = "gri",
    teams = teams,
    games = games
  )
}
simulation_fun <- function(x) {
  team_codes <- data_frame(
    club = sort(unique(c(x$games$home, x$games$away)))
  ) %>%
    mutate(code = seq_len(nrow(.)))
  
  fit_data <- left_join(x$games, team_codes, by = c("home" = "club")) %>%
    rename(home_code = code) %>%
    left_join(team_codes, by = c("away" = "club")) %>%
    rename(away_code = code)
  
  stan_data <- list(
    num_clubs = nrow(team_codes),
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
  gri <- stan(file = "_data/stan-models/gri.stan", data = stan_data,
    chains = 2, iter = 15000, warmup = 5000, init = "random", thin = 1,
    cores = 2, control = list(adapt_delta = 0.99))
  
  bivpois_maxrhat <- as.data.frame(summary(bivpois)[[1]]) %>%
    select(Rhat) %>%
    flatten_dbl() %>%
    max()
  gri_maxrhat <- as.data.frame(summary(gri)[[1]]) %>%
    select(Rhat) %>%
    flatten_dbl() %>%
    max()
  
  bivpois_params <- rstan::extract(bivpois, pars = c("mu", "eta", "alpha",
    "delta", "rho"))
  gri_params <- rstan::extract(gri, pars = c("mu", "eta", "alpha",
    "delta"))
  
  bivpois_alpha <- colMeans(bivpois_params$alpha)
  bivpois_delta <- colMeans(bivpois_params$delta)
  gri_alpha <- colMeans(gri_params$alpha)
  gri_delta <- colMeans(gri_params$delta)
  
  bivpois_alpha_bias <- mean(bivpois_alpha - x$teams$attack)
  bivpois_delta_bias <- mean(bivpois_delta - x$teams$defend)
  bivpois_alpha_mse <- mean((bivpois_alpha - x$teams$attack)^2)
  bivpois_delta_mse <- mean((bivpois_delta - x$teams$defend)^2)
  
  gri_alpha_bias <- mean(gri_alpha - x$teams$attack)
  gri_delta_bias <- mean(gri_delta - x$teams$defend)
  gri_alpha_mse <- mean((gri_alpha - x$teams$attack)^2)
  gri_delta_mse <- mean((gri_delta - x$teams$defend)^2)
  
  data_frame(
    generator = x$method,
    bivpois_rhat = bivpois_maxrhat,
    bivpois_params = list(list(bivpois_alpha = bivpois_alpha,
      bivpois_delta = bivpois_delta)),
    bivpois_alpha_bia = bivpois_alpha_bias,
    bivpois_delta_bias = bivpois_delta_bias,
    bivpois_alpha_mse = bivpois_alpha_mse,
    bivpois_delta_mse = bivpois_delta_mse,
    gri_rhat = gri_maxrhat,
    gri_params = list(list(gri_alpha = gri_alpha, gri_delta = gri_delta)),
    gri_alpha_bias,
    gri_delta_bias,
    gri_alpha_mse,
    gri_delta_mse
  )
}


### run simulation -------------------------------------------------------------
# Define parameters for the simulation
n_reps <- 200
streams_per_rep <- 1

# Create the seed warehouse
project_seeds <- seedCreator(n_reps, streams_per_rep, seed = 9416)
save(project_seeds, file = "_data/simulation_seeds.rda")

# Create data sets
bivpois_data <- lapply(X = 1:(n_reps / 2), FUN = generate_bivpois,
  seeds = project_seeds, num_club = 20)
gri_data <- lapply(X = ((n_reps / 2) + 1):n_reps, FUN = generate_gri,
  seeds = project_seeds, num_club = 20)

simulation_data <- c(
  bivpois_data,
  gri_data
)

simulation <- map2(.x = game_data, .y = team_data, .f = simulation_fun)
save(simulation, file = "_data/simulation.rda")

rm(list = ls())
