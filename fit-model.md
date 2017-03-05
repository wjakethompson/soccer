
# Fitting the Model {#fit-model}


```r
library(dplyr)
library(rstan)
load("_data/full_data.rda")

fit_data <-  full_data %>%
  filter(!is.na(h_goals))

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

gri <- stan(file = "_data/stan-models/gri.stan", data = stan_data,
  chains = 3, iter = 15000, warmup = 5000, init = "random", thin = 1,
  cores = 3, control = list(adapt_delta = 0.99))


sp <- get_sampler_params(gri, inc_warmup = FALSE)
E <- as.matrix(sapply(sp, FUN = function(x) x[,"energy__"]))
EBFMI <- get_num_upars(gri) / apply(E, 2, var)
```


## MCMC diagnostics {#diagnostics}



### Convergence {#convergence}

Rhat
geweke (ggmcmc)
running means
autocorrelation

### Efficiency {#efficiency}

mean accept stat
max tree depth (should be < 10, the default value)
BFMI

## Model fit {#model-fit}

posterior predictive checks
* distribution of scores
* distribution of MOV
* correct predictions

## Results {#results}
