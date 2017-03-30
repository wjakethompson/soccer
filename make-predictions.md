
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
  Barcelona          2.64              0.594              0.181      
 Real Madrid         1.61              0.225              0.181      

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
       Chelsea            1.83       0.77           86.5                   89.1%           
  Tottenham Hotspur       1.71       0.75           78.5                    8.1%           
   Manchester City        1.77       0.82           75.2                    2.2%           
      Liverpool           1.74       0.82           74.0                    0.5%           
       Arsenal            1.95       0.91           68.9                    0.1%           
  Manchester United       1.56       0.74           69.1                    0.0%           
       Everton            1.38       0.87           62.8                    0.0%           
 West Bromwich Albion     1.18       1.01           53.4                    0.0%           
      Stoke City          1.07       1.04           46.6                    0.0%           
     Southampton          1.09       0.88           44.8                    0.0%           
   AFC Bournemouth        1.22       1.21           42.9                    0.0%           
   West Ham United        1.18       1.24           42.4                    0.0%           
       Burnley            1.02       0.96           43.0                    0.0%           
       Watford            1.04       1.11           40.8                    0.0%           
    Leicester City        1.20       1.11           42.0                    0.0%           
    Crystal Palace        1.11       1.12           37.7                    0.0%           
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
      AS Monaco          2.12       0.89           88.3                   73.1%           
 Paris Saint-Germain     1.90       0.75           85.9                   26.7%           
        Nice             1.20       0.93           75.4                    0.2%           
        Lyon             1.65       0.96           67.3                    0.0%           
      Marseille          1.25       1.00           59.0                    0.0%           
      Bordeaux           1.16       1.08           57.8                    0.0%           
     St Etienne          0.92       0.83           54.2                    0.0%           
    Stade Rennes         0.86       1.06           48.2                    0.0%           
       Angers            1.00       1.05           48.1                    0.0%           
      Guingamp           1.11       1.05           49.8                    0.0%           
       Nantes            0.88       1.12           47.4                    0.0%           
      Toulouse           1.00       0.93           47.9                    0.0%           
        Metz             0.87       1.25           44.3                    0.0%           
        Lille            0.88       1.01           44.1                    0.0%           
     Montpellier         1.19       1.36           42.8                    0.0%           
        Caen             0.97       1.32           41.0                    0.0%           
      Dijon FCO          1.08       1.19           37.8                    0.0%           
  AS Nancy Lorraine      0.79       0.99           36.6                    0.0%           
       Bastia            0.80       1.21           32.7                    0.0%           
       Lorient           1.02       1.42           34.6                    0.0%           

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
      Bayern Munich           2.09       0.68           82.2                   99.9%           
        RB Leipzig            1.39       0.93           63.5                    0.1%           
    Borussia Dortmund         1.81       0.90           62.4                    0.0%           
      TSG Hoffenheim          1.41       0.91           58.6                    0.0%           
      Hertha Berlin           1.18       0.94           53.8                    0.0%           
        FC Cologne            1.22       0.96           50.4                    0.0%           
   Eintracht Frankfurt        0.96       0.88           47.5                    0.0%           
       SC Freiburg            1.13       1.18           46.3                    0.0%           
        Schalke 04            1.32       0.87           48.1                    0.0%           
 Borussia Monchengladbach     1.25       0.97           45.1                    0.0%           
     Bayer Leverkusen         1.28       1.03           43.4                    0.0%           
          Mainz               1.16       1.16           39.8                    0.0%           
      Werder Bremen           1.16       1.23           39.2                    0.0%           
       FC Augsburg            0.93       1.05           38.3                    0.0%           
      VfL Wolfsburg           0.90       0.98           38.9                    0.0%           
        Hamburg SV            0.97       1.22           37.1                    0.0%           
     FC Ingolstadt 04         0.90       1.10           29.9                    0.0%           
     SV Darmstadt 98          0.79       1.27           22.3                    0.0%           

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
    Juventus        1.61       0.71           90.8                   90.8%           
    AS Roma         1.71       0.87           82.4                    8.1%           
     Napoli         1.70       0.98           78.1                    1.0%           
     Lazio          1.39       0.88           72.2                    0.1%           
 Internazionale     1.43       1.08           69.2                    0.0%           
    Atalanta        1.28       0.95           68.9                    0.0%           
    AC Milan        1.22       0.95           67.4                    0.0%           
   Fiorentina       1.34       1.02           63.0                    0.0%           
   Sampdoria        1.03       0.99           52.3                    0.0%           
     Torino         1.49       1.14           54.3                    0.0%           
 Chievo Verona      1.04       1.04           49.4                    0.0%           
    Udinese         1.07       1.06           47.6                    0.0%           
    Bologna         0.98       1.08           44.6                    0.0%           
    Cagliari        1.13       1.35           43.5                    0.0%           
    Sassuolo        1.11       1.18           41.6                    0.0%           
     Genoa          1.01       1.18           38.6                    0.0%           
     Empoli         0.72       1.14           30.9                    0.0%           
    Palermo         0.82       1.31           24.4                    0.0%           
    Crotone         0.77       1.21           21.8                    0.0%           
   US Pescara       0.93       1.46           20.9                    0.0%           

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
      Barcelona          2.28       0.84           85.3                   51.8%           
     Real Madrid         2.03       0.95           85.5                   47.4%           
     Sevilla FC          1.36       0.94           74.0                    0.6%           
   Atletico Madrid       1.45       0.73           73.6                    0.2%           
     Villarreal          1.08       0.81           63.0                    0.0%           
    Real Sociedad        1.27       1.06           62.8                    0.0%           
   Athletic Bilbao       1.10       0.96           57.8                    0.0%           
        Eibar            1.26       1.08           54.2                    0.0%           
      Espanyol           1.15       0.99           54.5                    0.0%           
       Alavés            0.99       0.87           53.9                    0.0%           
     Celta Vigo          1.28       0.99           53.9                    0.0%           
     Las Palmas          1.28       1.17           47.8                    0.0%           
     Real Betis          0.94       1.14           41.7                    0.0%           
      Valencia           1.19       1.28           43.0                    0.0%           
       Málaga            1.04       1.23           37.7                    0.0%           
 Deportivo La Coruña     1.03       1.11           39.6                    0.0%           
       Leganes           0.79       1.10           36.1                    0.0%           
   Sporting Gijón        0.97       1.35           31.0                    0.0%           
       Granada           0.85       1.37           27.6                    0.0%           
       Osasuna           0.87       1.47           20.1                    0.0%           

## UEFA Champions League

Simulating the UEFA Champions League is very similar to the process used for simulating the domestic leagues. At each retained iteration of the MCMC chain, I simulate the remainder of the Champions League matches. Because there isn't a true bracket, and the opponents are drawn randomly before each round, I first define the current match-ups.


```r
matchups <- list(
  c("Barcelona", "Juventus"),
  c("AS Monaco", "Borussia Dortmund"),
  c("Bayern Munich", "Real Madrid"),
  c("Leicester City", "Atletico Madrid")
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

       Club           Offense    Defense    Quarterfinals    Semifinals    Final    Champion 
-------------------  ---------  ---------  ---------------  ------------  -------  ----------
   Bayern Munich       2.09       0.68          100%           67.0%       43.4%     25.5%   
     Barcelona         2.28       0.84          100%           58.1%       35.8%     19.7%   
     AS Monaco         2.12       0.89          100%           59.0%       29.9%     14.4%   
  Atletico Madrid      1.45       0.73          100%           74.3%       30.4%     13.4%   
     Juventus          1.61       0.71          100%           41.9%       22.0%     10.5%   
 Borussia Dortmund     1.81       0.90          100%           41.0%       17.8%      7.8%   
    Real Madrid        2.03       0.95          100%           33.0%       16.6%      7.3%   
  Leicester City       1.20       1.11          100%           25.7%       4.1%       1.3%   

