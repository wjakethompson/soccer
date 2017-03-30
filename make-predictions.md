
# Making Predictions {#predict}

To make predictions, I will use the retained iterations from the `gri_stanfit` object. Alternatively, I could use the means of the posterior distributions to calculate a single lambda for each score within each game. These lambda values could then be used to generate a sample of scores which could be used to predict the outcome of the game. However, this approach would ignore the uncertainty in the parameter estimates. A better solution would be to calculate lambda values at each iteration of the chain, using the current estimates of the parameters. Thus, at each iteration, it is possible to simulate a score for each team at each iteration, creating a posterior distribution for the score of each game.

## Predict individual games

First, I will extract the parameters we need from the fitted model, and load in the team codes associated with each team.


```r
model_params <- rstan::extract(gri_stanfit, pars = c("mu", "eta", "alpha",
  "delta", "sigma_g"))
load("_data/team_counts.rda")
```



I then use the `predict_games` function (see Appendix \@ref(predict-game)) to predict the outcome of a game between any two teams included in the model. For example, we can predict the winner of a game between Barcelona and Real Madrid played in Barcelona.




```r
library(dplyr)
library(ggplot2)
library(purrr)
prediction <- predict_game(home = "Barcelona", away = "Real Madrid",
  neutral = FALSE, visualize = TRUE, team_codes = team_counts,
  chains = model_params)

prediction$predictions %>%
  select(Club = club, `Expected Goals` = expected_goals,
    `Win Probability` = prob_win, `Tie Probability` = prob_tie) %>%
  knitr::kable(caption = "Prediction for Real Madrid at Barcelona",
    align = "c", digits = 3)
```



Table: (\#tab:exp-predict)Prediction for Real Madrid at Barcelona

    Club        Expected Goals    Win Probability    Tie Probability 
-------------  ----------------  -----------------  -----------------
  Barcelona          2.65              0.607              0.166      
 Real Madrid         1.60              0.227              0.166      

Because I specified `visualize = TRUE` in the call to `predict_game` we can use the `multiplot` function (Appendix \@ref(multi-plot)) to visualize the range of possible outcomes from the posteriors.


```r
library(grid)
multiplot(plotlist = prediction$plots, cols = 2)
```

<div class="figure" style="text-align: center">
<img src="make-predictions_files/figure-html/exp-vis-1.png" alt="Visualizations for Real Madrid at Barcelona" width="80%" />
<p class="caption">(\#fig:exp-vis)Visualizations for Real Madrid at Barcelona</p>
</div>

## Predict domestic leagues

To predict entire leagues, I follow the same general process, simulating an outcome for each retained iteration of the chain. The difference for leagues is that instead of simulating a single game at each iteration, we simulate the remainder of the league season, and calculate the league winner. This is all done by the `predict_league` function (Appendix \@ref(predict-league)).



In order to simulate these outcome, I'll first need to load in the full data set that includes future games


```r
library(lubridate)
library(rvest)
library(tidyr)
library(scales)

load("_data/full_data.rda")
load("_data/club_rankings.rda")
```

### English Premier League {#epl}


```r
predict_league(league = "Premier League", games = full_data,
  chains = model_params, team_codes = team_counts) %>%
  left_join(select(club_rankings, club, exp_offense, exp_defense),
    by = "club") %>%
  arrange(desc(champ_pct)) %>%
  mutate(champ_pct = percent(ifelse(is.na(champ_pct), 0, champ_pct))) %>%
  select(Club = club, Offense = exp_offense, Defense = exp_defense,
    `Expected Points` = sim_points, `Championship Probability` = champ_pct) %>%
  knitr::kable(caption = "Premier League Championship Probabilities",
    align = "c", digits = 2)
```



Table: (\#tab:epl-sim)Premier League Championship Probabilities

         Club            Offense    Defense    Expected Points    Championship Probability 
----------------------  ---------  ---------  -----------------  --------------------------
       Chelsea            1.83       0.77           86.5                   89.6%           
  Tottenham Hotspur       1.71       0.75           78.4                    7.9%           
   Manchester City        1.77       0.82           75.1                    1.8%           
      Liverpool           1.74       0.82           74.0                    0.7%           
  Manchester United       1.56       0.74           69.0                    0.0%           
       Everton            1.38       0.87           62.9                    0.0%           
       Arsenal            1.95       0.91           69.0                    0.0%           
 West Bromwich Albion     1.18       1.01           53.5                    0.0%           
      Stoke City          1.07       1.04           46.6                    0.0%           
     Southampton          1.09       0.88           44.9                    0.0%           
   AFC Bournemouth        1.22       1.21           43.0                    0.0%           
   West Ham United        1.18       1.24           42.5                    0.0%           
       Burnley            1.02       0.96           43.0                    0.0%           
       Watford            1.04       1.11           40.7                    0.0%           
    Leicester City        1.20       1.11           42.1                    0.0%           
    Crystal Palace        1.11       1.12           37.5                    0.0%           
     Swansea City         1.29       1.25           38.1                    0.0%           
      Hull City           1.07       1.12           35.0                    0.0%           
    Middlesbrough         1.01       0.88           34.2                    0.0%           
      Sunderland          0.87       1.13           29.4                    0.0%           

### French Ligue 1 {#ligue1}


```r
predict_league(league = "Ligue 1", games = full_data,
  chains = model_params, team_codes = team_counts) %>%
  left_join(select(club_rankings, club, exp_offense, exp_defense),
    by = "club") %>%
  arrange(desc(champ_pct)) %>%
  mutate(champ_pct = percent(ifelse(is.na(champ_pct), 0, champ_pct))) %>%
  select(Club = club, Offense = exp_offense, Defense = exp_defense,
    `Expected Points` = sim_points, `Championship Probability` = champ_pct) %>%
  knitr::kable(caption = "Ligue 1 Championship Probabilities",
    align = "c", digits = 2)
```



Table: (\#tab:ligue1-sim)Ligue 1 Championship Probabilities

        Club            Offense    Defense    Expected Points    Championship Probability 
---------------------  ---------  ---------  -----------------  --------------------------
      AS Monaco          2.12       0.89           88.2                   71.2%           
 Paris Saint-Germain     1.90       0.75           86.1                   28.6%           
        Nice             1.20       0.93           75.2                    0.1%           
        Lyon             1.65       0.96           67.2                    0.0%           
      Marseille          1.25       1.00           58.9                    0.0%           
      Bordeaux           1.16       1.08           57.8                    0.0%           
     St Etienne          0.92       0.83           54.1                    0.0%           
    Stade Rennes         0.86       1.06           48.2                    0.0%           
       Angers            1.00       1.05           48.0                    0.0%           
      Guingamp           1.11       1.05           49.7                    0.0%           
       Nantes            0.88       1.12           47.5                    0.0%           
      Toulouse           1.00       0.93           48.1                    0.0%           
        Metz             0.87       1.25           44.3                    0.0%           
        Lille            0.88       1.01           44.1                    0.0%           
     Montpellier         1.19       1.36           42.8                    0.0%           
        Caen             0.97       1.32           41.1                    0.0%           
      Dijon FCO          1.08       1.19           37.7                    0.0%           
  AS Nancy Lorraine      0.79       0.99           36.9                    0.0%           
       Bastia            0.80       1.21           32.8                    0.0%           
       Lorient           1.02       1.42           34.5                    0.0%           

### German Bundesliga {#bund}


```r
predict_league(league = "Bundesliga", games = full_data,
  chains = model_params, team_codes = team_counts) %>%
  left_join(select(club_rankings, club, exp_offense, exp_defense),
    by = "club") %>%
  arrange(desc(champ_pct)) %>%
  mutate(champ_pct = percent(ifelse(is.na(champ_pct), 0, champ_pct))) %>%
  select(Club = club, Offense = exp_offense, Defense = exp_defense,
    `Expected Points` = sim_points, `Championship Probability` = champ_pct) %>%
  knitr::kable(caption = "Bundesliga Championship Probabilities",
    align = "c", digits = 2)
```



Table: (\#tab:bund-sim)Bundesliga Championship Probabilities

           Club              Offense    Defense    Expected Points    Championship Probability 
--------------------------  ---------  ---------  -----------------  --------------------------
      Bayern Munich           2.09       0.68           82.2                    100%           
        RB Leipzig            1.39       0.93           63.5                     0%            
    Borussia Dortmund         1.81       0.90           62.4                     0%            
      TSG Hoffenheim          1.41       0.91           58.7                     0%            
      Hertha Berlin           1.18       0.94           53.8                     0%            
        FC Cologne            1.22       0.96           50.4                     0%            
   Eintracht Frankfurt        0.96       0.88           47.6                     0%            
       SC Freiburg            1.13       1.18           46.4                     0%            
        Schalke 04            1.32       0.87           48.0                     0%            
 Borussia Monchengladbach     1.25       0.97           45.0                     0%            
     Bayer Leverkusen         1.28       1.03           43.3                     0%            
          Mainz               1.16       1.16           39.7                     0%            
      Werder Bremen           1.16       1.23           39.3                     0%            
       FC Augsburg            0.93       1.05           38.1                     0%            
      VfL Wolfsburg           0.90       0.98           39.0                     0%            
        Hamburg SV            0.97       1.22           36.9                     0%            
     FC Ingolstadt 04         0.90       1.10           29.9                     0%            
     SV Darmstadt 98          0.79       1.27           22.4                     0%            

### Italian Serie A {#seriea}


```r
predict_league(league = "Serie A", games = full_data,
  chains = model_params, team_codes = team_counts) %>%
  left_join(select(club_rankings, club, exp_offense, exp_defense),
    by = "club") %>%
  arrange(desc(champ_pct)) %>%
  mutate(champ_pct = percent(ifelse(is.na(champ_pct), 0, champ_pct))) %>%
  select(Club = club, Offense = exp_offense, Defense = exp_defense,
    `Expected Points` = sim_points, `Championship Probability` = champ_pct) %>%
  knitr::kable(caption = "Serie A Championship Probabilities",
    align = "c", digits = 2)
```



Table: (\#tab:seriea-sim)Serie A Championship Probabilities

      Club         Offense    Defense    Expected Points    Championship Probability 
----------------  ---------  ---------  -----------------  --------------------------
    Juventus        1.61       0.71           90.7                   90.6%           
    AS Roma         1.71       0.87           82.4                    8.2%           
     Napoli         1.70       0.98           78.1                    1.3%           
     Lazio          1.39       0.88           72.1                    0.0%           
 Internazionale     1.43       1.08           69.2                    0.0%           
    Atalanta        1.28       0.95           69.1                    0.0%           
    AC Milan        1.22       0.95           67.6                    0.0%           
   Fiorentina       1.34       1.02           63.0                    0.0%           
   Sampdoria        1.03       0.99           52.3                    0.0%           
     Torino         1.49       1.14           54.6                    0.0%           
 Chievo Verona      1.04       1.04           49.3                    0.0%           
    Udinese         1.07       1.06           47.5                    0.0%           
    Bologna         0.98       1.08           44.6                    0.0%           
    Cagliari        1.13       1.35           43.5                    0.0%           
    Sassuolo        1.11       1.18           41.7                    0.0%           
     Genoa          1.01       1.18           38.5                    0.0%           
     Empoli         0.72       1.14           30.7                    0.0%           
    Palermo         0.82       1.31           24.4                    0.0%           
    Crotone         0.77       1.21           21.6                    0.0%           
   US Pescara       0.93       1.46           21.1                    0.0%           

### Spanish La Liga {#laliga}


```r
predict_league(league = "La Liga", games = full_data,
  chains = model_params, team_codes = team_counts) %>%
  left_join(select(club_rankings, club, exp_offense, exp_defense),
    by = "club") %>%
  arrange(desc(champ_pct)) %>%
  mutate(champ_pct = percent(ifelse(is.na(champ_pct), 0, champ_pct))) %>%
  select(Club = club, Offense = exp_offense, Defense = exp_defense,
    `Expected Points` = sim_points, `Championship Probability` = champ_pct) %>%
  knitr::kable(caption = "La Liga Championship Probabilities",
    align = "c", digits = 2)
```



Table: (\#tab:laliga-sim)La Liga Championship Probabilities

        Club            Offense    Defense    Expected Points    Championship Probability 
---------------------  ---------  ---------  -----------------  --------------------------
      Barcelona          2.28       0.84           85.2                   51.6%           
     Real Madrid         2.03       0.95           85.5                   48.0%           
     Sevilla FC          1.36       0.94           74.0                    0.3%           
   Atletico Madrid       1.45       0.73           73.7                    0.1%           
     Villarreal          1.08       0.81           63.0                    0.0%           
    Real Sociedad        1.27       1.06           62.6                    0.0%           
   Athletic Bilbao       1.10       0.96           57.8                    0.0%           
        Eibar            1.26       1.08           54.2                    0.0%           
      Espanyol           1.15       0.99           54.5                    0.0%           
       Alavés            0.99       0.87           53.9                    0.0%           
     Celta Vigo          1.28       0.99           54.0                    0.0%           
     Las Palmas          1.28       1.17           47.7                    0.0%           
     Real Betis          0.94       1.14           41.6                    0.0%           
      Valencia           1.19       1.28           42.9                    0.0%           
       Málaga            1.04       1.23           37.8                    0.0%           
 Deportivo La Coruña     1.03       1.11           39.6                    0.0%           
       Leganes           0.79       1.10           36.1                    0.0%           
   Sporting Gijón        0.97       1.35           31.0                    0.0%           
       Granada           0.85       1.37           27.6                    0.0%           
       Osasuna           0.87       1.47           20.0                    0.0%           

## UEFA Champions League


```r
matchups <- list(
  c("Barcelona", "Juventus"),
  c("AS Monaco", "Borussia Dortmund"),
  c("Bayern Munich", "Real Madrid"),
  c("Leicester City", "Atletico Madrid")
)
```



