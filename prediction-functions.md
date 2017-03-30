
# Prediction Functions {#prediction-functions}

## Predict individual games {#predict-game}


```r
predict_game <- function(home, away, neutral = FALSE, visualize = TRUE,
  team_codes = team_counts, chains = model_params) {
  home_code <- team_codes$code[which(team_codes$team == home)]
  away_code <- team_codes$code[which(team_codes$team == away)]
  
  mu <- chains$mu %>% as.vector()
  eta <- ifelse(neutral, 0, chains$eta %>% as.vector())
  home_off <- chains$alpha[, home_code]
  home_def <- chains$delta[, home_code]
  away_off <- chains$alpha[, away_code]
  away_def <- chains$delta[, away_code]
  sigma_g <- chains$sigma_g %>% as.vector()
  game_int <- pmap_dbl(.l = list(sd = sigma_g), rnorm, n = 1, mean = 0)
  
  home_goals <- exp(mu + eta + home_off + away_def + game_int) %>%
    map_dbl(rpois, n = 1)
  away_goals <- exp(mu + away_off + home_def + game_int) %>%
    map_dbl(rpois, n = 1)
  
  outcomes <- data_frame(
    club = c(home, away),
    expected_goals = c(mean(home_goals), mean(away_goals)),
    prob_win = c(length(which(home_goals > away_goals)),
      length(which(away_goals > home_goals))) / length(home_goals),
    prob_tie = rep(length(which(home_goals == away_goals)) / length(home_goals),
      2),
    prob_loss = c(length(which(away_goals > home_goals)),
      length(which(home_goals > away_goals))) / length(home_goals)
  )
  
  if (visualize) {
    heatmap <- data_frame(home_goals, away_goals) %>%
      group_by(home_goals, away_goals) %>%
      summarize(probability = n() / nrow(.)) %>%
      mutate(plot = "Goal Distribution")
    histogram <- data_frame(home_goals, away_goals) %>%
      mutate(home_mov = home_goals - away_goals) %>%
      select(home_mov) %>%
      group_by(home_mov) %>%
      summarize(probability = n() / nrow(.)) %>%
      mutate(plot = "Margin of Victory", winner = ifelse(home_mov > 0, home,
        ifelse(home_mov < 0, away, "Tie"))) %>%
      mutate(winner = factor(winner, levels = c(home, "Tie", away)))
    
    score_dist <- ggplot(heatmap, aes(x = home_goals, y = away_goals)) +
      geom_tile(aes(fill = probability)) +
      scale_fill_gradient(name = "Probability") +
      scale_x_continuous(breaks = seq(0, 100, 1)) +
      scale_y_continuous(breaks = seq(0, 100, 1)) +
      labs(x = paste0(home, " Score"), y = paste0(away, " Score")) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
    
    mov_dist <- ggplot(histogram, aes(x = home_mov, y = probability)) +
      geom_col(aes(fill = winner)) +
      scale_fill_brewer(type = "qual", palette = 3, name = "Winner") +
      scale_x_continuous(breaks = seq(-100, 100, 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.05), labels = scales::percent) +
      labs(x = paste0(home, " Margin of Victory"), y = "Probability") +
      theme_minimal() +
      theme(
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
      )
    
    plot_list <- list(score_dist, mov_dist)
  }
  
  if (visualize) {
    list(
      predictions = outcomes,
      plots = plot_list
    )
  } else {
    list(
      predictions = outcomes
    )
  }
}
```

## Plot multiple plots {#multi-plot}


```r
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
      ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col))
    }
  }
}
```

## Predict leagues {#predict-league}


```r
predict_league <- function(league, games = full_data, chains = model_params,
  team_codes = team_counts) {
  lookup <- data_frame(
    league = c("Premier League", "Bundesliga", "La Liga", "Ligue 1", "Serie A"),
    abbrev = c("Prem", "Bund", "La Liga", "Ligue 1", "Serie A"),
    website = c("http://www.espnfc.us/english-premier-league/23/table",
      "http://www.espnfc.us/german-bundesliga/10/table",
      "http://www.espnfc.us/spanish-primera-division/15/table",
      "http://www.espnfc.us/french-ligue-1/9/table",
      "http://www.espnfc.us/italian-serie-a/12/table")
  )
  abbrev <- lookup$abbrev[which(lookup$league == league)]
  website <- lookup$website[which(lookup$league == league)]
  
  safe_read_html <- safely(read_html)
  cont <- TRUE
  while(cont) {
    url_data <- safe_read_html(website)
    
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
    mutate(club = trimws(club, which = "both"), points = as.numeric(points),
      goals_for = as.numeric(goals_for),
      goals_against = as.numeric(goals_against))
  club_names <- league_table$club

  future_games <- games %>%
    filter(competition == abbrev, date >= ymd(Sys.Date()), home %in% club_names,
      away %in% club_names)
  
  data_list <- list(
    mu = chains$mu %>% as.vector(),
    eta = chains$eta %>% as.vector(),
    alpha = t(chains$alpha) %>% as_data_frame() %>% as.list(),
    delta = t(chains$delta) %>% as_data_frame() %>% as.list(),
    sigma_g = chains$sigma_g %>% as.vector()
  )
  
  league_sim <- pmap_df(.l = data_list, .f = function(mu, eta, alpha, delta,
    sigma_g, sim_games, sim_league, team_codes) {
    for (g in seq_len(nrow(sim_games))) {
      home_code <- team_codes$code[which(team_codes$team == sim_games$home[g])]
      away_code <- team_codes$code[which(team_codes$team == sim_games$away[g])]
      
      home_off <- alpha[home_code]
      home_def <- delta[home_code]
      away_off <- alpha[away_code]
      away_def <- delta[away_code]
      game_int <- rnorm(1, mean = 0, sd = sigma_g)
      
      sim_games$h_goals[g] <- rpois(1, lambda = exp(mu + eta + home_off +
          away_def + game_int))
      sim_games$a_goals[g] <- rpois(1, lambda = exp(mu + away_off + home_def +
          game_int))
    }
    sim_games <- sim_games %>%
      mutate(
        home_pts = ifelse(h_goals > a_goals, 3, ifelse(h_goals < a_goals, 0, 1)),
        away_pts = ifelse(h_goals > a_goals, 0, ifelse(h_goals < a_goals, 3, 1))
      )
    pts_total <- bind_rows(
      select(sim_games, club = home, sim_pts = home_pts,
        sim_goals_for = h_goals, sim_goals_against = a_goals),
      select(sim_games, club = away, sim_pts = away_pts,
        sim_goals_for = a_goals, sim_goals_against = h_goals)
    ) %>%
      group_by(club) %>%
      summarize(sim_pts = sum(sim_pts), sim_goals_for = sum(sim_goals_for),
        sim_goals_against = sum(sim_goals_against))
    
    final_league <- left_join(sim_league, pts_total, by = "club") %>%
      mutate(
        tot_goals_for = goals_for + sim_goals_for,
        tot_goals_against = goals_against + sim_goals_against,
        goal_diff = tot_goals_for - tot_goals_against,
        tot_points = points + sim_pts
      ) %>%
      arrange(desc(tot_points), desc(goal_diff), desc(tot_goals_for))
    champ <- final_league$club[1]
    
    final_league %>%
      select(club, tot_points) %>%
      arrange(club) %>%
      spread(key = club, value = tot_points) %>%
      mutate(Champion = champ)
    }, sim_games = future_games,
    sim_league = league_table, team_codes = team_codes)
  
  champions <- data_frame(club = league_sim$Champion) %>%
    group_by(club) %>%
    summarize(champ_pct = n() / nrow(league_sim)) %>%
    arrange(desc(champ_pct))
  
  sim_results <- league_sim %>%
    select(-Champion) %>%
    gather(key = club, value = points, everything()) %>%
    group_by(club) %>%
    summarize(sim_points = mean(points)) %>%
    arrange(desc(sim_points)) %>%
    left_join(champions, by = "club")
  
  league_table %>%
    left_join(sim_results, by = "club")
}
```
