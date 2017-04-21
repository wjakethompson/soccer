
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
  Barcelona          2.62              0.574              0.187      
 Real Madrid         1.70              0.239              0.187      

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

Then, I can use the `predict_league` function to get championship probabilities for each league.

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
       Chelsea            1.81       0.77           87.5                   84.2%           
  Tottenham Hotspur       1.81       0.71           82.5                   15.6%           
   Manchester City        1.82       0.81           76.2                    0.1%           
      Liverpool           1.75       0.81           76.3                    0.1%           
  Manchester United       1.55       0.71           70.9                    0.0%           
       Arsenal            1.94       0.92           69.7                    0.0%           
       Everton            1.45       0.89           64.5                    0.0%           
 West Bromwich Albion     1.11       0.98           49.6                    0.0%           
     Southampton          1.08       0.86           48.4                    0.0%           
       Watford            1.03       1.09           44.9                    0.0%           
      Stoke City          1.06       1.04           44.9                    0.0%           
    Leicester City        1.25       1.09           43.8                    0.0%           
   West Ham United        1.17       1.24           41.8                    0.0%           
       Burnley            0.99       0.95           42.5                    0.0%           
    Crystal Palace        1.18       1.10           40.9                    0.0%           
   AFC Bournemouth        1.20       1.21           42.0                    0.0%           
      Hull City           1.13       1.16           36.3                    0.0%           
     Swansea City         1.22       1.23           34.2                    0.0%           
    Middlesbrough         0.99       0.89           30.9                    0.0%           
      Sunderland          0.84       1.15           26.1                    0.0%           

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
 Paris Saint-Germain     1.98       0.73           88.5                   54.9%           
      AS Monaco          2.11       0.89           87.4                   45.0%           
        Nice             1.26       0.93           79.2                    0.1%           
        Lyon             1.63       0.96           63.0                    0.0%           
      Marseille          1.27       0.97           60.2                    0.0%           
      Bordeaux           1.18       1.06           59.5                    0.0%           
     St Etienne          0.90       0.86           51.6                    0.0%           
      Guingamp           1.12       1.06           52.0                    0.0%           
    Stade Rennes         0.86       1.07           48.6                    0.0%           
       Nantes            0.88       1.12           47.8                    0.0%           
      Toulouse           0.98       0.91           48.0                    0.0%           
     Montpellier         1.18       1.30           44.4                    0.0%           
       Angers            0.98       1.04           45.1                    0.0%           
        Lille            0.87       1.00           43.4                    0.0%           
        Metz             0.87       1.34           41.2                    0.0%           
        Caen             0.94       1.33           38.1                    0.0%           
  AS Nancy Lorraine      0.81       0.99           37.6                    0.0%           
       Lorient           1.05       1.37           37.7                    0.0%           
      Dijon FCO          1.08       1.17           35.8                    0.0%           
       Bastia            0.78       1.17           32.6                    0.0%           

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
      Bayern Munich           2.08       0.68           80.8                    100%           
        RB Leipzig            1.47       0.89           68.3                     0%            
      TSG Hoffenheim          1.49       0.91           63.0                     0%            
    Borussia Dortmund         1.82       0.92           62.9                     0%            
      Hertha Berlin           1.13       0.92           50.7                     0%            
        FC Cologne            1.21       0.99           47.7                     0%            
       SC Freiburg            1.10       1.23           46.9                     0%            
      Werder Bremen           1.29       1.20           44.6                     0%            
 Borussia Monchengladbach     1.28       0.98           47.3                     0%            
   Eintracht Frankfurt        0.93       0.88           44.7                     0%            
        Schalke 04            1.33       0.89           45.1                     0%            
     Bayer Leverkusen         1.27       1.00           43.3                     0%            
      VfL Wolfsburg           0.95       1.02           38.2                     0%            
        Hamburg SV            0.98       1.20           39.5                     0%            
          Mainz               1.14       1.15           37.5                     0%            
       FC Augsburg            0.93       1.13           36.8                     0%            
     FC Ingolstadt 04         0.96       1.15           33.4                     0%            
     SV Darmstadt 98          0.79       1.32           21.9                     0%            

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
    Juventus        1.61       0.66           92.0                   94.8%           
    AS Roma         1.73       0.85           83.4                    4.5%           
     Napoli         1.77       0.94           80.9                    0.6%           
     Lazio          1.40       0.91           71.0                    0.0%           
    Atalanta        1.33       0.92           70.0                    0.0%           
    AC Milan        1.27       0.94           67.6                    0.0%           
 Internazionale     1.42       1.11           64.7                    0.0%           
   Fiorentina       1.34       1.02           61.6                    0.0%           
     Torino         1.50       1.15           53.7                    0.0%           
   Sampdoria        1.06       0.99           52.7                    0.0%           
    Udinese         1.10       1.05           48.3                    0.0%           
    Cagliari        1.23       1.34           45.7                    0.0%           
 Chievo Verona      0.99       1.08           45.2                    0.0%           
    Bologna         0.93       1.06           42.0                    0.0%           
    Sassuolo        1.11       1.16           42.1                    0.0%           
     Genoa          0.99       1.25           36.0                    0.0%           
     Empoli         0.72       1.12           32.2                    0.0%           
    Crotone         0.81       1.17           26.6                    0.0%           
    Palermo         0.79       1.34           22.0                    0.0%           
   US Pescara       0.91       1.43           20.1                    0.0%           

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
     Real Madrid         2.13       0.94           87.7                   70.9%           
      Barcelona          2.23       0.84           84.6                   29.0%           
     Sevilla FC          1.32       0.93           75.5                    0.1%           
   Atletico Madrid       1.43       0.68           76.3                    0.0%           
     Villarreal          1.12       0.83           62.8                    0.0%           
   Athletic Bilbao       1.18       0.97           61.0                    0.0%           
    Real Sociedad        1.28       1.05           61.5                    0.0%           
        Eibar            1.31       1.06           58.3                    0.0%           
      Espanyol           1.14       0.97           57.5                    0.0%           
     Celta Vigo          1.33       1.00           53.3                    0.0%           
       Alavés            0.95       0.88           51.3                    0.0%           
      Valencia           1.24       1.24           47.5                    0.0%           
     Las Palmas          1.32       1.25           45.6                    0.0%           
     Real Betis          0.94       1.14           40.3                    0.0%           
       Málaga            1.01       1.17           40.0                    0.0%           
 Deportivo La Coruña     1.00       1.14           37.7                    0.0%           
       Leganes           0.80       1.12           33.2                    0.0%           
   Sporting Gijón        0.95       1.33           28.5                    0.0%           
       Granada           0.81       1.40           24.8                    0.0%           
       Osasuna           0.88       1.44           22.7                    0.0%           

## UEFA Champions League

Simulating the UEFA Champions League is very similar to the process used for simulating the domestic leagues. At each retained iteration of the MCMC chain, I simulate the remainder of the Champions League matches. Because there isn't a true bracket, and the opponents are drawn randomly before each round, I first define the current match-ups.


```r
matchups <- list(
  c("Real Madrid", "Atletico Madrid"),
  c("AS Monaco", "Juventus")
)
```

I can then use the `predict_ucl` function (Appendix \@ref(predict-ucl)) to calculate the probability of each team advancing to each subsequent round.




```r
predict_ucl(matchups = matchups, games = full_data, chains = model_params,
  team_codes = team_counts) %>%
  left_join(select(club_rankings, club, exp_offense, exp_defense),
    by = c("Club" = "club")) %>%
  arrange(desc(Champion)) %>%
  mutate(Quarterfinals = percent(Quarterfinals),
    Semifinals = percent(Semifinals), Final = percent(Final),
    Champion = percent(Champion)) %>%
  select(Club, Offense = exp_offense, Defense = exp_defense, Quarterfinals,
    Semifinals, Final, Champion) %>%
  knitr::kable(caption = "UEFA Champions League Probabilities",
    align = "c", digits = 2)
```



Table: (\#tab:ucl-sim)UEFA Champions League Probabilities

      Club          Offense    Defense    Quarterfinals    Semifinals    Final    Champion 
-----------------  ---------  ---------  ---------------  ------------  -------  ----------
    Juventus         1.61       0.66          100%            100%       51.9%     27.7%   
   Real Madrid       2.13       0.94          100%            100%       54.3%     26.2%   
    AS Monaco        2.11       0.89          100%            100%       48.1%     25.0%   
 Atletico Madrid     1.43       0.68          100%            100%       45.7%     21.1%   

