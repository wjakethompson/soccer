
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
  Barcelona          2.66              0.612              0.166      
 Real Madrid         1.60              0.222              0.166      

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
       Chelsea            1.84       0.76           86.5                   82.8%           
  Tottenham Hotspur       1.78       0.72           81.7                   17.0%           
   Manchester City        1.80       0.82           74.6                    0.2%           
      Liverpool           1.76       0.82           75.0                    0.1%           
  Manchester United       1.55       0.72           68.0                    0.0%           
       Arsenal            1.93       0.92           66.8                    0.0%           
       Everton            1.42       0.89           63.4                    0.0%           
 West Bromwich Albion     1.13       0.99           50.6                    0.0%           
     Southampton          1.09       0.85           47.4                    0.0%           
       Watford            1.04       1.11           43.0                    0.0%           
    Leicester City        1.26       1.10           43.9                    0.0%           
       Burnley            0.99       0.94           43.3                    0.0%           
      Stoke City          1.03       1.04           43.5                    0.0%           
   West Ham United        1.15       1.23           42.2                    0.0%           
   AFC Bournemouth        1.22       1.19           42.7                    0.0%           
    Crystal Palace        1.18       1.09           41.4                    0.0%           
      Hull City           1.13       1.14           37.4                    0.0%           
     Swansea City         1.24       1.23           35.5                    0.0%           
    Middlesbrough         0.99       0.89           31.8                    0.0%           
      Sunderland          0.83       1.15           26.3                    0.0%           

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
      AS Monaco          2.09       0.90           86.8                   53.6%           
 Paris Saint-Germain     1.97       0.73           86.8                   45.8%           
        Nice             1.23       0.93           78.2                    0.6%           
        Lyon             1.67       0.98           65.0                    0.0%           
      Bordeaux           1.19       1.08           57.9                    0.0%           
      Marseille          1.22       0.98           57.6                    0.0%           
     St Etienne          0.91       0.83           52.8                    0.0%           
       Nantes            0.90       1.13           49.2                    0.0%           
      Toulouse           0.98       0.89           49.4                    0.0%           
      Guingamp           1.10       1.07           50.4                    0.0%           
    Stade Rennes         0.85       1.08           47.0                    0.0%           
       Angers            0.99       1.05           46.0                    0.0%           
        Lille            0.89       1.00           44.7                    0.0%           
     Montpellier         1.18       1.33           43.0                    0.0%           
        Metz             0.84       1.31           42.2                    0.0%           
        Caen             0.93       1.32           38.3                    0.0%           
  AS Nancy Lorraine      0.81       0.97           37.4                    0.0%           
       Lorient           1.07       1.38           38.9                    0.0%           
      Dijon FCO          1.08       1.19           36.1                    0.0%           
       Bastia            0.80       1.20           33.4                    0.0%           

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
      Bayern Munich           2.16       0.67           82.1                   99.9%           
        RB Leipzig            1.44       0.91           67.0                    0.1%           
      TSG Hoffenheim          1.42       0.87           60.7                    0.0%           
    Borussia Dortmund         1.80       0.90           61.8                    0.0%           
      Hertha Berlin           1.16       0.92           52.2                    0.0%           
       SC Freiburg            1.13       1.20           47.8                    0.0%           
        FC Cologne            1.23       0.98           48.4                    0.0%           
 Borussia Monchengladbach     1.25       0.94           48.3                    0.0%           
   Eintracht Frankfurt        0.94       0.87           45.5                    0.0%           
        Schalke 04            1.34       0.87           47.3                    0.0%           
      Werder Bremen           1.30       1.21           43.4                    0.0%           
     Bayer Leverkusen         1.30       1.03           42.8                    0.0%           
        Hamburg SV            0.99       1.20           40.6                    0.0%           
      VfL Wolfsburg           0.91       1.03           36.5                    0.0%           
          Mainz               1.16       1.17           35.8                    0.0%           
       FC Augsburg            0.92       1.13           34.9                    0.0%           
     FC Ingolstadt 04         0.98       1.12           34.7                    0.0%           
     SV Darmstadt 98          0.78       1.34           19.4                    0.0%           

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
    Juventus        1.62       0.70           91.2                   88.9%           
    AS Roma         1.75       0.86           84.4                   10.5%           
     Napoli         1.75       0.95           79.9                    0.6%           
     Lazio          1.39       0.91           71.8                    0.0%           
    Atalanta        1.33       0.93           69.8                    0.0%           
    AC Milan        1.25       0.94           67.9                    0.0%           
 Internazionale     1.42       1.10           65.4                    0.0%           
   Fiorentina       1.35       1.00           63.9                    0.0%           
   Sampdoria        1.06       0.98           53.9                    0.0%           
     Torino         1.52       1.15           55.2                    0.0%           
    Udinese         1.11       1.05           49.1                    0.0%           
 Chievo Verona      1.02       1.05           46.7                    0.0%           
    Cagliari        1.17       1.37           44.0                    0.0%           
    Bologna         0.95       1.08           42.6                    0.0%           
    Sassuolo        1.10       1.17           40.7                    0.0%           
     Genoa          0.97       1.25           35.8                    0.0%           
     Empoli         0.71       1.13           29.8                    0.0%           
    Crotone         0.80       1.19           26.0                    0.0%           
    Palermo         0.80       1.37           22.3                    0.0%           
   US Pescara       0.93       1.44           20.8                    0.0%           

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
     Real Madrid         2.08       0.95           86.5                   63.8%           
      Barcelona          2.28       0.83           84.3                   36.2%           
   Atletico Madrid       1.44       0.69           75.9                    0.0%           
     Sevilla FC          1.35       0.94           73.1                    0.0%           
     Villarreal          1.12       0.82           64.3                    0.0%           
    Real Sociedad        1.27       1.05           61.8                    0.0%           
        Eibar            1.34       1.05           60.0                    0.0%           
   Athletic Bilbao       1.13       0.97           59.5                    0.0%           
      Espanyol           1.14       0.99           55.7                    0.0%           
     Celta Vigo          1.29       1.01           52.0                    0.0%           
       Alavés            0.93       0.88           49.5                    0.0%           
      Valencia           1.26       1.27           47.8                    0.0%           
     Las Palmas          1.32       1.20           46.9                    0.0%           
       Málaga            1.03       1.16           41.3                    0.0%           
     Real Betis          0.92       1.17           38.3                    0.0%           
 Deportivo La Coruña     0.99       1.15           36.2                    0.0%           
       Leganes           0.82       1.13           34.4                    0.0%           
   Sporting Gijón        0.94       1.32           29.1                    0.0%           
       Granada           0.83       1.38           25.9                    0.0%           
       Osasuna           0.89       1.44           23.1                    0.0%           

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
   Bayern Munich       2.16       0.67          100%           69.7%       46.8%     29.2%   
     Barcelona         2.28       0.83          100%           57.8%       33.2%     17.9%   
     AS Monaco         2.09       0.90          100%           58.3%       29.0%     13.4%   
  Atletico Madrid      1.44       0.69          100%           74.0%       31.7%     13.3%   
     Juventus          1.62       0.70          100%           42.2%       21.9%     10.4%   
 Borussia Dortmund     1.80       0.90          100%           41.7%       17.7%      7.9%   
    Real Madrid        2.08       0.95          100%           30.3%       15.5%      6.8%   
  Leicester City       1.26       1.10          100%           26.0%       4.3%       1.1%   

