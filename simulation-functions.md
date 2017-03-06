
# (APPENDIX) Appendix {-}

# Simulation Functions {#simulation-functions}

## Generate bivariate Poisson {#bivpois-generate}


```r
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
```

## Generate game random intercept {#gri-generate}


```r
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
```

## Fit models to simualted data {#estimate-fun}


```r
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
    true_params = list(x$teams),
    bivpois_rhat = bivpois_maxrhat,
    bivpois_params = list(list(bivpois_alpha = bivpois_alpha,
      bivpois_delta = bivpois_delta)),
    bivpois_alpha_bias = bivpois_alpha_bias,
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
```

## Plot correlation matrices {#ggally-cor}

### Plot components {#ggally-comp}


```r
lowerFn <- function(data, mapping, ..., alpha = 0.5, lim = 2) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(..., alpha = alpha) +
    geom_abline(intercept = 0, slope = 1, color = "black") +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 1)) +
    scale_y_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 1)) +
    theme_bw()
  p
}

diagFn <- function(data, mapping, ..., alpha = 0.5, lim = 2) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_density(..., alpha = alpha) +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 1)) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  p
}

upperFn <- function(data, mapping, ..., size = 3, lim = 2) {
  xCol <- deparse(mapping$x)
  yCol <- deparse(mapping$y)
  colorCol <- deparse(mapping$colour)
  xVal <- data[[xCol]]
  yVal <- data[[yCol]]
  cVal <- data[[colorCol]]
  
  plot_data <- data_frame(xVal, yVal, cVal) %>%
    group_by(cVal) %>%
    summarize(corr = cor(xVal, yVal, method = "pearson")) %>%
    mutate(
      cVal = factor(cVal, levels = c("gri", "bivpois"),
        labels =c("GRI", "Biv Poisson")),
      lab = paste0(cVal, ": ", sprintf("%.3f", corr)),
      y_pos = 1:2,
      x_pos = 0
    )
  
  p <- ggplot(data = plot_data, aes(x = x_pos, y = y_pos, fill = cVal,
    label = lab)) +
    geom_label(color = "white", fontface = "bold", size = size) +
    scale_x_continuous(limits = c(-lim, lim), breaks = seq(-lim, lim, 1)) +
    scale_y_continuous(limits = c(0, 3)) +
    scale_fill_manual(values = c(`Biv Poisson` = "#F8766D", GRI = "#00BFC4")) +
    theme_bw() +
    theme(
      legend.position = "none"
    )
  p
}

get_lim <- function(data) {
  data %>%
    as.list() %>%
    flatten_dbl() %>%
    abs() %>%
    max() %>%
    round_any(accuracy = 0.5, f = ceiling)
}
```

### Plot creation {#ggally-plot}


```r
lim <- get_lim(select(plot_sim, -generator))

ggpairs(
  title = "Alpha Recovery", data = plot_sim,
  columns = c("true_alpha", "bivpois_alpha", "gri_alpha"),
  mapping = aes(color = generator, fill = generator),
  columnLabels = c("True", "Bivariate Poisson", "Game Random Intercept"),
  upper = list(continuous = wrap(upperFn, size = 3, lim = lim)),
  lower = list(continuous = wrap(lowerFn, alpha = 0.1, lim = lim)),
  diag = list(continuous = wrap(diagFn, alpha = 0.5, lim = lim))
)

ggpairs(
  title = "Delta Recovery", data = plot_sim,
  columns = c("true_delta", "bivpois_delta", "gri_delta"),
  mapping = aes(color = generator, fill = generator),
  columnLabels = c("True", "Bivariate Poisson", "Game Random Intercept"),
  upper = list(continuous = wrap(upperFn, size = 3, lim = lim)),
  lower = list(continuous = wrap(lowerFn, alpha = 0.1, lim = lim)),
  diag = list(continuous = wrap(diagFn, alpha = 0.5, lim = lim))
)
```

