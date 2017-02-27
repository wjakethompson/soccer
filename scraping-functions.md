
# Web Scraping Functions {#scrape-functions}

## Scrape league pages {#league-scrape}


```r
scrape_league <- function(x) {
  url_data <- read_html(x)
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
```

## Scrape international cups {#cup-scrape}


```r
scrape_major_cup <- function(x) {
  url_data <- read_html(x)
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
```

## Scrape domestic cups {#dom-scrape}


```r
scrape_dom_cup <- function(x) {
    url_data <- read_html(x)
  
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
```

## Scrape games {#game-scrape}


```r
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
```
