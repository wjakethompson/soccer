
# Fitting the Model {#fit-model}

With the data gathered, the next step is to estimate the model to get ability estimates for each team. Following the findings of the simulation study in Section \@ref(simulation), the game random intercept model will be used for the estimation.


## Model estimation {#fit-real-data}

The first step is to load in the data set and do some filtering using the **dplyr** package [@R-dplyr].


```r
library(dplyr)
load("_data/full_data.rda")
```

To filter, I first remove any game that hasn't been played (or was canceled) by removing any games that don't have a score for either team. I also remove the competition field, as it is not necessary for the model estimation.


```r
fit_data <-  full_data %>%
  filter(!is.na(h_goals), !is.na(a_goals)) %>%
  select(-competition)
```

Next, I filter the data to only include teams with a least 5 games played. This is to ensure that all teams included have a sufficient number of games to get reasonable estimates. This process is done iteratively to ensure that after removing teams with less than five games, teams that orginally had more than 5 games, but now no longer meet the criteria are also excluded.


```r
filter_games <- TRUE
while(filter_games) {
  team_counts <- table(c(fit_data$home, fit_data$away)) %>% as_data_frame() %>%
    arrange(desc(n)) %>%
    filter(n >= 5) %>%
    select(team = Var1, games = n) %>%
    arrange(team) %>%
    mutate(code = seq_len(nrow(.)))
  
  fit_data <- fit_data %>%
    left_join(select(team_counts, -games), by = c("home" = "team")) %>%
    rename(home_code = code) %>%
    left_join(select(team_counts, -games), by = c("away" = "team")) %>%
    rename(away_code = code) %>%
    filter(!is.na(home_code), !is.na(away_code))
  
  new_min <- table(c(fit_data$home, fit_data$away)) %>% as.numeric() %>% min()
  if (new_min >= 5) {
    filter_games <- FALSE
  } else {
    fit_data <- fit_data %>%
      select(-home_code, -away_code)
  }
}
```

This filtering process leaves a total of 6608 games between 432 teams. The number of games for each team ranges from 5 to 55.

To estimate the model, the data has to be supplied to Stan in a list format. The names of the elements of the list should match the names of the input data specified in the Stan model (see Section \@ref(imp-gri) for Stan definition of the game random intercept model).


```r
stan_data <- list(
  num_clubs = nrow(team_counts),
  num_games = nrow(fit_data),
  home = fit_data$home_code,
  away = fit_data$away_code,
  h_goals = fit_data$h_goals,
  a_goals = fit_data$a_goals,
  homeg = fit_data$home_game
)
```

In order to later assess model fit with posterior predictive model checks, a `generated quantities` section was added to the Stan model specification.


```stan
generated quantities {
  int home_rep[num_games];
  int away_rep[num_games];
  
  for (g in 1:num_games) {
    home_rep[g] = poisson_rng(exp(mu + (eta * homeg[g]) + alpha[home[g]] +
      delta[away[g]] + gamma[g]));
    away_rep[g] = poisson_rng(exp(mu + alpha[away[g]] + delta[home[g]] +
      gamma[g]));
  }
}
```

This section attempt to replicated the original data. At each each iteration of the chain, the $\lambda$ for each score within each game is calculated using the current values of the parameters. A random Poisson is then drawn for each team using the calculated $\lambda$ values. Thus, the model is able to produce a posterior distribution for the scores of each game. These distribution can then be compared to the observed scores to see how well the model fits the data.

Finally, the model can be estimated using the Stan interface **rstan** [@R-rstan]. The model is estimated with 3 chains, each with 4000 iterations. The first 2000 iterations from each chain are discarded as burn in. Additionally, a thinning interval of 2 was used. These decisions were made to decrease computation time, and to limit the size of the result `stanfit` objects, which become quite large. This leaves 1000 iterations from each chain, for a total of 3000 iterations that will make up the final posterior distributions. As in the simulation study, the target proposal acceptance rate during the adaptation period was set to 0.99 [see Section \@ref(model-est); @stan; @stanwarn].


```r
library(rstan)

gri_stanfit <- stan(file = "_data/stan-models/gri_ppmc.stan", data = stan_data,
  chains = 3, iter = 7000, warmup = 2000, init = "random", thin = 5,
  cores = 3, algorithm = "NUTS", seed = 9416,
  control = list(adapt_delta = 0.99, max_treedepth = 15))
```


## MCMC diagnostics {#diagnostics}



After estimating the model, before the parameters can be analyzed and inferences can be made, the model has to be checked to make sure it estimated correctly and completely. This diagnostic information is critical to Markov Chain Monte Carlo estimation, as with proper estimation, no valid inferences can be made. The code used to create the plot and tables used to display the diagnostic information can be seen in Appendix \@ref(diagnostic-output).

### Convergence {#convergence}

The first check is convergence. Convergence means that the MCMC chain found the high density area of the posterior distribution, and stayed there. When multiple chains are estimated, this can be done by verifying that the estimates from each chain end up in the same place. For a single chain, this means verifying that the chain is sampling roughly the same area at the beginning of the chain (after burn in) as in the end of the chain.

One way to assess convergence is through the $\hat{R}$ statistic [@bda3; @brooks1997]. The $\hat{R}$ statistic is also known as the potential scale reduction, and is a measure of how much variance there is between chains compared to the amount of variation within chains. Gelman and Rubin [-@gelman1992] suggest that in order to conclude the model has successfully converged, all $\hat{R}$ values should be less than 1.1. 

<div class="figure" style="text-align: center">
<img src="fit-model_files/figure-html/rhat-plot-1.png" alt="Rhat statistics for the estimated game random intercept model." width="80%" />
<p class="caption">(\#fig:rhat-plot)Rhat statistics for the estimated game random intercept model.</p>
</div>

Figure \@ref(fig:rhat-plot) shows that all of the $\hat{R}$ values are below the suggested cutoff of 1.1, indicating that the model has converged.

### Efficiency {#efficiency}

The second important check for MCMC estimation is the efficiency of the sampler. In other words, it is important to check that the algorithm adequately sampled the full posterior. There are several ways this can be examined. The first is by examing the effective sample size. This diagnostic takes into account the autocorrelation in the chain to determine the 'effective' number of independent draws from the posterior. If the chain is slow moving, the draws will be highly autocorrelated, and effective sample size will be well below the total number of iterations in the chain. However, low autocorrelations would indicate that the sampler is moving around the posterior fairly quickly, and the effictive sample size will be at or near the true sample size of the posterior. The effective sample size for all parameters can been seen in Figure \@ref(fig:neff-plot).

<div class="figure" style="text-align: center">
<img src="fit-model_files/figure-html/neff-plot-1.png" alt="Effective sample size for the estimated game random intercept model parameters." width="80%" />
<p class="caption">(\#fig:neff-plot)Effective sample size for the estimated game random intercept model parameters.</p>
</div>

There are also measure of efficiency that are exclusive the No U-Turn Sampler [NUTS; @nuts]. For example the Bayesian Factor of Missing Information (BFMI) gives an estimate of how well the sampler adpated and explored the posterior distribution. The BFMI ranges from 0 to 1, with 0 and 1 representing poor and excellent estimation respectively. This is calculated for the chain overall [@betancourt2016].


Table: (\#tab:diag-tab)Diagnostic statistics for the NUTS algorithm.

  Chain     BFMI     Mean Acceptance Rate    Max Treedepth 
---------  -------  ----------------------  ---------------
 Chain 1    0.966           0.991                  7       
 Chain 2    0.844           0.982                  6       
 Chain 3    0.890           0.982                  6       

The BFMI values in Table \@ref(tab:diag-tab) indicate that the sampler was able to adequately visit the posterior distributions. Additionally, Table \@ref(tab:diag-tab) shows the mean acceptance rate for each chain. As expected, these values are very close to the 0.99 that was specified when the model was estimated (`control = list(adapt_delta = 0.99)`; Section \@ref(fit-real-data)). As noted in Sections \@ref(model-est) and \@ref(fit-real-data), a target acceptance rate this high is needed to prevent divergent transitions. This occurs due to the small variances of $\alpha$, $\delta$, and $\gamma$ that are estimated. The high target acceptance rate forces the sampler to take smaller steps, keeping the variances within reasonable ranges.

The concern with setting the target acceptance this high is that for parameters with wider posteriors, the sampler will not be able to move fast enough. In the NUTS algorithm, at each iteration, the sampler looks for a place to "U-Turn" in a series of possible branches. If the sampler is terminating before the maximum possible tree depth (set to 15; see Section \@ref(fit-real-data)), then the algorithm is able to adequately find good values for the next iteration of the chain. Bumping up against the maximum allowed tree depth, or going beyond it, indicates that stepsize is too small [@stanwarn; @stanintro]. Because the Max Treedepth values in Table \@ref(tab:diag-tab) are all below the maximum specified, and the BFMI values are close to 1, there is strong evidence that the sampler was indeed able to adequately sample the posteriors.

## Model fit {#model-fit}

To assess model fit, I will use posterior predictive model checks. Posterior predictive checks involve simulating replications of the data using the values of the Markov chain, and then comparing the replicated data to the observed data [@bda3]. This means that replicated data sets take into account the uncertainty in the parameter estimates, as a new replicated data set is created at each iteration of the Markov chain. These data sets can then be used to look for systematic differences in the characterics of the observed and simulated data, often through visualizations [@gelmanhill2006].

The first step is to extract the replicated data sets from the `stanfit` object using the **purrr** [@R-purrr] and **tidyr** [@R-tidyr] packages.


```r
library(purrr)
library(tidyr)

home_rep <- rstan::extract(gri_stanfit, pars = "home_rep",
  permuted = TRUE)$home_rep
away_rep <- rstan::extract(gri_stanfit, pars = "away_rep",
  permuted = TRUE)$away_rep

home_rep <- t(home_rep) %>%
  as_data_frame() %>% as.list()
away_rep <- t(away_rep) %>%
  as_data_frame() %>% as.list()
counter <- seq_along(home_rep)

rep_data <- pmap_df(.l = list(h = home_rep, a = away_rep, c = counter),
  .f = function(h, a, c) {
    data_frame(
      replication = c,
      game = seq_len(length(h)),
      home_score = h,
      away_score = a
    )
  })
```



### Score distributions {#ppmc-score-dist}

The first posterior predictive check to be examined is the distribution of home and away scores. For each replicated data set there is a distribution of goals scored by the home and away teams. To compare to the observed data, we can plot each of these distributions, and then overlay the distribution from the observed data using **ggplot2** [@R-ggplot2]. The code for Figure \@ref(fig:plot-score-dist) can be see in Appendix \@ref(ppmc-sdp).

<div class="figure" style="text-align: center">
<img src="fit-model_files/figure-html/plot-score-dist-1.png" alt="Recovery of observed score distributions." width="80%" />
<p class="caption">(\#fig:plot-score-dist)Recovery of observed score distributions.</p>
</div>

Figure \@ref(fig:plot-score-dist) shows that the observed score distributions for both the home and away scores are very similar to what is seen in the replicated data sets. Thus, this provides evidence that the model is able to recover the observed distributions.

### Margin of victory intervals {#ppmc-mov-int}

It is also possible to examine the margin of victory for each game. In each of the replicated data sets, the home team's margin of victory for every game can be calculated as replicated home score minus the replicated away score. Doing this for every replication creates a posterior distribution for the home team's margin of victory in every game. From the posterior we can create credible intervals (Appendix \@ref(ppmc-mov)) and determine how often the observed margin of victory falls outside the credible interval.

<div class="figure" style="text-align: center">
<img src="fit-model_files/figure-html/plot-mov-int-1.png" alt="Example credible interval for game margin of victories." width="80%" />
<p class="caption">(\#fig:plot-mov-int)Example credible interval for game margin of victories.</p>
</div>

Figure \@ref(fig:plot-mov-int) shows examples of games where the observed margin of victory fell inside the 50% credible interval, inside the 95% credible interval, and outside both intervals. Overall, for the 6608 games included in the estimation, the observed margin of victory fell within the 50% credible interval 70.6 percent of the time and within the 95% credible 98.0 percent of the time.

### Prediction error {#ppmc-prediction}

Another posterior check that can be looked at is prediction accuracy. For each replication, whether the home team won, lost, or tied can be determined for each game. Across replications, it is then possible to look at the probability of the home teaming experiencing a given outcome in each game. The probabilities can then be compared to the observed outcome to determine how accurate the predictions were. The accuracy can be determined by using a binary loss function (was the most likely outcome the observed outcome), or a log loss function (how far from the observed outcome was the probability of that outcome).

The first step is to get the probability of the home team winning, tieing, and losing each game.


```r
outcomes <- rep_data %>%
  mutate(mov = home_score - away_score) %>%
  group_by(game) %>%
  summarize(
    win_prob = length(which(mov > 0)) / n(),
    tie_prob = length(which(mov == 0)) / n(),
    loss_prob = length(which(mov < 0)) / n()
  ) %>%
  mutate(
    most_likely = ifelse(win_prob > tie_prob & win_prob > loss_prob, "win",
      ifelse(tie_prob > win_prob & tie_prob > loss_prob, "tie", "loss"))
  ) %>%
  mutate(
    obs_mov = fit_data$h_goals - fit_data$a_goals,
    outcome = ifelse(obs_mov > 0, "win", ifelse(obs_mov == 0, "tie", "loss"))
  )
```

Using the predictions from the replicated data sets, the model gave the observed outcome the highest probability in 53.0 percent of the games.

One problem with this approach is that it doesn't take into the actual values of the probabilities. For example, take two games where the home team won. In the first game, the home team had a 60 percent chance of winning. In the second game, the home teams had a 90 percent chance of winning. In both cases, the most likely outcome matches the observed outcome, so both instances are assigned a one in the binary loss function as a correct prediction. Alternatively, we could use the log loss function to look at how far the probability was from the observed event [@altun2003]. In this example, the second game would be a better prediction, and have a lower log loss, because a probability of 0.9 is closer to the observed outcome (1) than a probability of 0.6.

The log loss for multiple games is defined in equation \@ref(eq:logloss).

\begin{equation}
logloss = - \frac{1}{N}\sum_{i=1}^N\sum_{j=1}^My_{ij}\log(p_{ij})
(\#eq:logloss)
\end{equation}

In equation \@ref(eq:logloss), $N$ is number of observations (in this case the number of games), $M$ is the number of outcomes (for soccer games this is 3: win, loss, and tie), $y_{ij}$ is a binary indicator of whether outcome $M$ occured for observation $N$ (1) or not (0), and $p_{ij}$ is the probability of outcome $M$ for observation $N$. Thus, the log loss for a set of predictions is the log of the probability of the observed outcomes, summed over all observations. In the case of perfect predictions, the probability of the observed outcome would be 1, and the log probability would be 0. This means that the closer the log loss is to 0, the better the predictions are [@roy2001].

To calculate the log loss for the estimated game random intercept model, I define a function that takes in a matrix of predictions and a matrix of outcomes.


```r
logloss <- function(pred, obs){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  (-1 / nrow(obs)) * sum(obs * log(pred) + (1 - obs) * log(1 - pred)) 
}
```

Then, matrices of predictions and outcomes can be created, and the log loss can be calculated.


```r
predictions <- outcomes %>%
  select(win_prob, tie_prob, loss_prob) %>%
  as.matrix()
observations <- outcomes %>%
  mutate(
    obs_win = ifelse(outcome == "win", 1, 0),
    obs_tie = ifelse(outcome == "tie", 1, 0),
    obs_loss = ifelse(outcome == "loss", 1, 0)
  ) %>%
  select(obs_win, obs_tie, obs_loss) %>%
  as.matrix()
avg_logloss <- logloss(pred = predictions, obs = observations)
```

The log loss for predictions from the replicated data sets is 1.722. Converting back to a probability scale, on average, the probability of the observed outcome was off by 0.18. In isolation, the log loss can be hard to interpret. Instead, it is often useful to compare to baseline models.


Table: (\#tab:logloss-tab)Log loss comparison to baseline models.

         Model            Log Loss 
-----------------------  ----------
 Game Random Intercept      1.72   
     Data Average           1.86   
  Equal Probabilities       1.91   
       Home Win            37.67   

Table \@ref(tab:logloss-tab) shows the log loss for a variety of models. In the data average model, the probability of each outcome is set to the overall average for the entire data set. In the equal probabilities model, the probability for each outcome is set to 0.33. Finally, in the home win model, the probability of the home team winning is set to 1 and the probability of the other outcomes is set to 0. The posterior predictive probabilities from the game random intercept model out perform all of these baseline models.

### Posterior predictive check summary

Overall, the posterior predictive model checks indicate adequate model fit. The model is able to accurately recover the distributions of scores for both the home and away teams (Figure \@ref(fig:plot-score-dist)). Additionally, when looking at individual games, the credible intervals for the margin of victory are able to consistently capture the observed margin of victory. Finally, the prediction error shows that the model's predictions are able to more accurately pick game outcomes than a variety of baseline models.

Taken in totality, there is sufficient evidence of model fit for us to proceed with the analysis and examine the posterior distributions of the parameter estimates.


## Results {#results}

A ranking of the teams can be created by the goals they would be expected to score and concede against an average team at a neutral location. The larger the difference between these expected goals, the better team. First we pull out the parameters we need from the model.


```r
params <- rstan::extract(gri_stanfit, pars = c("mu", "alpha", "delta"))
alpha <- colMeans(params$alpha)
delta <- colMeans(params$delta)
mu <- mean(params$mu)
```



Then we can compute the expected offense, defense, and margin for each team to create the rankings.


```r
rankings <- data_frame(
  Club = team_counts$team,
  Attacking = alpha,
  Defense = delta
) %>%
  mutate(
    `Expected Offense` = exp(mu + alpha),
    `Expected Defense` = exp(mu + delta),
    `Expected Margin` = `Expected Offense` - `Expected Defense`
  ) %>%
  arrange(desc(`Expected Margin`)) %>%
  mutate(
    Attacking = formatC(Attacking, digits = 3, drop0trailing = FALSE,
      format = "f"),
    Defense = formatC(Defense, digits = 3, drop0trailing = FALSE,
      format = "f"),
    `Expected Offense` = formatC(`Expected Offense`, digits = 2,
      drop0trailing = FALSE, format = "f"),
    `Expected Defense` = formatC(`Expected Defense`, digits = 2,
      drop0trailing = FALSE, format = "f"),
    `Expected Margin` = formatC(`Expected Margin`, digits = 2,
      drop0trailing = FALSE, format = "f")
  )
```

As we might expect, the top of the list is dominated by team leading the best European leagues and having success in the Champions League. Barcelona comes in as the top team in rankings, followed by Bayern Munich, AS Monaco, Paris Saint-Germain, and Real Madrid to round out the top 5. The top offense belongs to Barcelona according to the model, while Bayern Munich boasts the best defense.


```r
DT::datatable(select(rankings, -Club), rownames = rankings$Club,
  options = list(pageLength = 10, scrollX = TRUE,
  columnDefs = list(list(className = 'dt-center', targets = 1:5))),
  caption = "Club rankings from game random intercept model")
```

<!--html_preserve--><div id="htmlwidget-27bd867acf4efb0774ff" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-27bd867acf4efb0774ff">{"x":{"filter":"none","caption":"<caption>Club rankings from game random intercept model<\/caption>","data":[["Barcelona","Bayern Munich","AS Monaco","Paris Saint-Germain","Real Madrid","Chelsea","Arsenal","Tottenham Hotspur","Manchester City","Liverpool","Borussia Dortmund","Juventus","Celtic","AS Roma","FC Porto","Feyenoord Rotterdam","Manchester United","Napoli","Anderlecht","Atletico Madrid","Newcastle United","Ajax Amsterdam","Shakhtar Donetsk","Benfica","Lyon","Zenit St Petersburg","FC Basel","PSV Eindhoven","Brighton &amp; Hove Albion","FC Salzburg","Besiktas","Lazio","TSG Hoffenheim","Everton","Lincoln City","Fenerbahce","Norwich City","Olympiakos","Fulham","RB Leipzig","Schalke 04","Istanbul Buyuksehir BSK","Aberdeen","Sevilla FC","Rostov","Levante","Club Brugge","Torino","Internazionale","Spartak Moscow","Galatasaray","Luton Town","Atalanta","Fiorentina","Young Boys","Hannover 96","Sporting CP","Sheff Utd","FC Union Berlin","Celta Vigo","KV Oostende","Spal","Bolton Wanderers","KAA Gent","Panathinaikos","Lens","Borussia Monchengladbach","Nice","Villarreal","FC Cologne","AC Milan","VfB Stuttgart","FC Copenhagen","Leeds United","Dover","Braga","SC Rheindorf","Bayer Leverkusen","Marseille","Hertha Berlin","CSKA Moscow","Rangers","Standard Liege","Gateshead","Racing Genk","Krasnodar","Barrow","TSV Eintracht Braunschweig","Real Sociedad","Plymouth Argyle","Zulte-Waregem","Lokomotiv Moscow","Blackpool","Southampton","Dynamo Kiev","Huddersfield Town","Preston North End","Aldershot","Perugia","Austria Vienna","Dagenham &amp; Redbridge","F Green","Tranmere","Guimaraes","Eibar","Heart of Midlothian","West Bromwich Albion","FC Utrecht","Dynamo Dresden","Vitesse Arnhem","SC Amiens","Hellas Verona","Girona","Espanyol","Halifax","Brentford","Heidenheimer SB","Heerenveen","Oxford United","Reading","Athletic Bilbao","Frosinone","FC Sion","Sheffield Wednesday","Doncaster","Barnsley","Nimes","Middlesbrough","Benevento","Scunthorpe","Huesca","Troyes","Alavés","Rapid Vienna","Portsmouth","Las Palmas","FK Rubin Kazan","PAOK Salonika","Bradford","Strasbourg","Derby County","Exeter City","Millwall","KV Mechelen","Leicester City","Elche","Stevenage","Apoel Nicosia","St Etienne","Fleetwood Town","Bristol City","Bordeaux","Lugo","Eintracht Frankfurt","Kasimpasa","Virtus Entel","Royal Charleroi SC","Toulouse","Hapoel Be'e","Burnley","Wycombe Wanderers","Cardiff City","Getafe","Cádiz","Guingamp","Real Valladolid","Wolverhampton Wanderers","Le Havre AC","Stade de Reims","Trabzonspor","Rochdale","Tenerife","Aston Villa","Sampdoria","Swansea City","OSM","SV Sandhausen","FK Amkar Perm","SK Sturm Gra","Stoke City","Macclesfield Town","R Zaragoza","Lucerne","Colchester","Sochaux","AFC Bournemouth","Udinese","Accrington Stanley","Brest","Boavista","Twente Enschede","Mainz","Chievo Verona","AFC Wimbledon","GFC Ajaccio","Mallorca","Cambridge United","St Johnstone","Crystal Palace","Spezia","Salernitana","AEK Athens","AZ Alkmaar","Ludogorets","Panionios","Cesena","Blackburn Rovers","Evian Thonon Gaillard","Genclerbirligi","Antalyaspor","Carlisle","Brondby","York","Maritimo","FC Ufa","Cittadella","GD Chaves","Queens Park Rangers","Almeria","Rayo","Nurnberg","Morton","Sevilla At.","Novara","Eastleigh","Bourg-Peronnas","Charlton Athletic","Mansfield","FC Xanthi","FC Groningen","SC Freiburg","Southend","Hull City","Terek Grozny","Karabükspor","West Ham United","Angers","Clermont Foot","Würzburger Kickers","Grimsby","Liberec","Vitoria Setubal","Maccabi Tel-Aviv","Sassuolo","Bari","Partick Thistle","Werder Bremen","FC Zürich","Boreham Wood","Watford","Valenciennes","Steaua Bucuresti","Deportivo La Coruña","Reus Deporti","Numancia","Ipswich Town","Fortuna Düsseldorf","Birmingham City","VfL Wolfsburg","FC Zorya Luh","Valencia","Rio Ave","Bologna","Niort","Nottingham Forest","FC Astana","UCAM Murcia","Carpi","Kaiserslautern","FK Astra Giu","Viktoria Plzen","TSV 1860 Munich","Sutton United","Ascoli","Alanyaspor K","Dijon FCO","VfL Bochum","Bristol Rovers","Lausanne Spo","Milton Keynes Dons","Lille","FC Augsburg","Sparta Prague","Sparta Rotterdam","Pisa","Barnet","Krylia Sovetov","SpVgg Greuther Furth","Walsall","St Gallen","FK Qarabag","Bursaspor","Tours","Latina","AD Alcorcon","Anzhi Makhachkala","Guiseley","Montpellier","Peterborough United","Kayserispor","Konyaspor","Genoa","Chester FC","AC Ajaccio","AJ Auxerre","Dundalk","Legia Warsaw","Yeovil Town","KSC Lokeren","St Pauli","FC Thun","Cordoba","Málaga","Burton Albion","Gimnastic de Tarragona","Orléans","Dundee","Real Betis","St. Truiden","Cork","AS Nancy Lorraine","FC Ingolstadt 04","Stade Rennes","Orenburg","Belenenses","Willem II Tilburg","Solihull Moors","Northampton Town","Iraklis","Wrexham","Cagliari","Red Star FC 93","FK Qabala","Stade Laval","Oldham","Wigan Athletic","St. Pölten","Arminia Bielefeld","Torquay","SC Heracles Almelo","Mattersburg","Paços de Ferreira","Nantes","Hamburg SV","Panetolikos","Waasland-Bev","Sunderland","RZ Pellets W","Viitorul Con","Pas Giannina","KV Kortrijk","Brescia","Pro Vercelli","Real Oviedo","Bury","Trapani","Cheltenham Town","Estoril","Asteras Trip","Dinamo Zagreb","Crawley Town","Admira Wacke","US Avellino","Leganes","Akhisar","FC Lugano","Gillingham","Shrewsbury Town","Crewe","Vicenza","Arouca","Grasshoppers","Ross County","Braintree Town","Caen","Maidstone United","Caykur Rizespor","Woking","Moreirense","Ural","Port Vale","Morecambe","Newport County","Ternana","Bromley","Feirense","FC Erzgebirge Aue","Levadiakos","Metz","Sporting Gijón","Notts County","L Orient","Mouscron-Per","Swindon Town","Eupen","Excelsior","Hartlepool","Gaziantepspor","NEC Nijmegen","Kilmarnock","PEC Zwolle","Lorient","FC Arsenal T","Adanaspor","Bastia","Empoli","Karlsruher SC","Mirandes","Roda JC Kerkrade","Crotone","Hamilton Academical","Coventry City","Inverness Caledonian Thistle","Go Ahead Eagles","SV Darmstadt 98","Veria","Palermo","Motherwell","FC Vaduz","Chesterfield","CD Nacional de Madeira","SV Josko Rie","Granada","ADO Den Haag","US Pescara","KVC Westerlo","Tondela","Southport","Osasuna","Rotherham United","FK Tom Tomsk","North Ferrib"],["0.756","0.671","0.685","0.573","0.641","0.537","0.600","0.472","0.507","0.486","0.526","0.409","0.495","0.470","0.372","0.413","0.376","0.464","0.443","0.302","0.357","0.345","0.404","0.350","0.432","0.374","0.352","0.285","0.296","0.274","0.331","0.265","0.280","0.254","0.303","0.246","0.400","0.181","0.329","0.264","0.208","0.220","0.176","0.241","0.136","0.234","0.185","0.334","0.292","0.164","0.283","0.218","0.182","0.228","0.280","0.250","0.193","0.187","0.142","0.183","0.213","0.197","0.098","0.157","0.112","0.127","0.156","0.118","0.012","0.136","0.135","0.160","-0.013","0.062","0.318","0.127","0.186","0.177","0.158","0.101","0.012","0.110","0.140","0.055","0.145","0.083","0.097","0.129","0.176","0.053","0.165","0.044","0.087","0.020","0.062","0.062","0.139","0.050","0.094","0.303","0.129","0.114","0.017","0.124","0.162","0.193","0.102","0.103","0.131","0.109","0.080","0.125","0.111","0.075","0.120","0.193","0.063","0.139","0.131","0.118","0.032","0.030","0.186","0.007","0.106","0.138","0.083","-0.057","0.012","0.140","0.028","0.082","-0.075","0.033","0.067","0.180","-0.008","-0.092","-0.038","0.137","-0.064","0.121","0.097","0.033","0.118","0.062","0.156","-0.019","-0.154","-0.018","0.037","0.082","0.090","-0.107","0.081","0.054","-0.102","-0.066","-0.026","-0.043","0.037","0.086","0.005","0.068","0.039","-0.022","0.005","-0.111","-0.064","-0.112","0.143","-0.042","-0.100","-0.036","0.185","0.001","0.045","-0.185","-0.026","-0.002","-0.048","-0.025","0.187","0.036","-0.185","0.130","-0.001","-0.038","-0.012","-0.102","0.037","0.085","-0.027","0.008","-0.090","-0.012","-0.042","-0.000","0.039","-0.144","-0.090","-0.129","0.141","0.022","-0.103","-0.008","0.017","-0.081","-0.161","-0.063","0.045","-0.058","0.069","-0.218","-0.269","0.042","-0.089","-0.030","-0.010","-0.126","0.110","-0.002","-0.118","-0.090","0.025","0.070","-0.104","-0.073","-0.142","0.016","0.057","-0.019","0.002","-0.063","-0.038","0.098","-0.070","-0.097","-0.160","-0.151","0.033","-0.153","-0.073","0.036","-0.124","-0.149","0.080","-0.080","-0.229","-0.029","-0.037","-0.091","-0.033","-0.149","-0.085","-0.193","-0.097","-0.106","-0.174","-0.173","0.104","-0.046","-0.082","-0.022","0.091","-0.073","-0.069","-0.196","-0.259","-0.134","-0.099","-0.080","-0.092","-0.055","0.156","0.007","-0.011","0.037","0.095","-0.020","-0.194","-0.141","-0.094","0.057","-0.413","-0.035","-0.155","-0.123","-0.077","-0.124","-0.070","-0.141","0.002","-0.178","-0.328","-0.251","-0.164","0.112","-0.007","0.035","-0.127","-0.056","0.080","-0.106","-0.204","-0.171","0.030","-0.074","-0.310","-0.204","0.047","-0.143","-0.027","-0.127","-0.095","-0.193","-0.129","-0.133","-0.072","-0.115","-0.308","-0.173","-0.217","-0.216","-0.279","-0.255","0.026","0.048","-0.148","-0.234","0.060","-0.184","-0.092","-0.289","-0.335","-0.320","-0.148","-0.037","-0.117","-0.031","-0.175","-0.163","-0.199","-0.098","-0.146","-0.201","-0.206","-0.137","-0.101","-0.224","-0.009","-0.115","-0.228","-0.066","0.056","-0.151","-0.191","-0.268","-0.050","-0.220","-0.088","-0.222","-0.175","-0.296","-0.317","-0.065","-0.016","-0.197","-0.100","-0.264","-0.180","-0.153","-0.163","-0.085","-0.093","-0.072","-0.156","0.014","-0.183","-0.287","-0.204","0.034","-0.136","-0.229","-0.234","-0.214","-0.174","-0.251","-0.201","-0.095","-0.140","-0.119","-0.163","-0.219","0.030","-0.090","-0.152","-0.187","-0.210","-0.248","-0.113","-0.043","-0.391","-0.229","-0.286","-0.395","-0.319","-0.195","-0.410","-0.333","-0.250","-0.236","-0.107","-0.206","-0.297","-0.138","-0.265","-0.115","-0.081","-0.167","-0.335","-0.204","-0.230","-0.295","-0.135","-0.100","-0.332","-0.115","-0.200","-0.177","-0.367","-0.448"],["-0.245","-0.446","-0.187","-0.350","-0.114","-0.334","-0.157","-0.357","-0.262","-0.265","-0.169","-0.415","-0.210","-0.208","-0.386","-0.290","-0.364","-0.089","-0.125","-0.387","-0.271","-0.290","-0.178","-0.265","-0.109","-0.150","-0.185","-0.291","-0.231","-0.242","-0.128","-0.199","-0.165","-0.200","-0.102","-0.187","0.050","-0.272","-0.040","-0.136","-0.210","-0.192","-0.247","-0.131","-0.255","-0.097","-0.144","0.063","0.008","-0.156","0.005","-0.078","-0.114","-0.048","0.027","-0.008","-0.080","-0.082","-0.132","-0.075","-0.036","-0.046","-0.171","-0.092","-0.149","-0.129","-0.092","-0.138","-0.274","-0.112","-0.113","-0.079","-0.301","-0.199","0.121","-0.110","-0.031","-0.041","-0.062","-0.130","-0.238","-0.108","-0.067","-0.171","-0.059","-0.134","-0.117","-0.069","-0.010","-0.159","-0.021","-0.168","-0.113","-0.195","-0.137","-0.132","-0.029","-0.136","-0.081","0.163","-0.038","-0.056","-0.170","-0.040","0.008","0.045","-0.061","-0.059","-0.026","-0.048","-0.079","-0.023","-0.039","-0.079","-0.024","0.069","-0.077","0.009","0.005","-0.008","-0.106","-0.104","0.074","-0.127","-0.012","0.027","-0.034","-0.191","-0.113","0.037","-0.086","-0.025","-0.200","-0.078","-0.039","0.088","-0.119","-0.213","-0.151","0.045","-0.175","0.031","0.008","-0.057","0.037","-0.023","0.079","-0.110","-0.252","-0.103","-0.040","0.010","0.021","-0.189","0.014","-0.013","-0.178","-0.138","-0.094","-0.108","-0.020","0.032","-0.053","0.016","-0.014","-0.076","-0.044","-0.165","-0.114","-0.162","0.106","-0.086","-0.146","-0.077","0.156","-0.033","0.018","-0.216","-0.050","-0.024","-0.069","-0.044","0.172","0.019","-0.200","0.121","-0.010","-0.048","-0.021","-0.112","0.029","0.081","-0.031","0.005","-0.089","-0.010","-0.038","0.005","0.045","-0.134","-0.080","-0.118","0.150","0.033","-0.085","0.011","0.035","-0.061","-0.135","-0.039","0.068","-0.032","0.093","-0.184","-0.232","0.071","-0.055","0.003","0.025","-0.086","0.144","0.036","-0.076","-0.048","0.065","0.109","-0.055","-0.026","-0.091","0.063","0.102","0.031","0.051","-0.007","0.016","0.146","-0.014","-0.039","-0.098","-0.087","0.086","-0.087","-0.006","0.096","-0.053","-0.076","0.140","-0.009","-0.147","0.040","0.033","-0.013","0.042","-0.065","-0.006","-0.105","-0.016","-0.023","-0.082","-0.078","0.179","0.040","0.007","0.063","0.168","0.024","0.030","-0.083","-0.138","-0.024","0.008","0.026","0.017","0.052","0.246","0.110","0.095","0.139","0.192","0.091","-0.059","-0.013","0.029","0.164","-0.244","0.085","-0.012","0.017","0.058","0.017","0.066","0.009","0.134","-0.021","-0.147","-0.076","-0.000","0.239","0.136","0.173","0.034","0.099","0.217","0.058","-0.023","0.004","0.179","0.090","-0.104","-0.009","0.202","0.042","0.140","0.057","0.084","0.005","0.061","0.061","0.112","0.077","-0.080","0.029","-0.005","-0.004","-0.054","-0.034","0.197","0.218","0.055","-0.009","0.235","0.037","0.111","-0.044","-0.074","-0.061","0.077","0.168","0.105","0.175","0.060","0.077","0.050","0.130","0.094","0.059","0.055","0.111","0.141","0.048","0.217","0.134","0.052","0.177","0.276","0.115","0.086","0.029","0.196","0.070","0.172","0.075","0.113","0.027","0.013","0.206","0.248","0.113","0.186","0.070","0.132","0.154","0.149","0.212","0.209","0.229","0.168","0.297","0.153","0.081","0.141","0.318","0.195","0.131","0.130","0.145","0.174","0.124","0.159","0.235","0.203","0.220","0.190","0.152","0.332","0.245","0.202","0.181","0.167","0.141","0.236","0.286","0.050","0.157","0.122","0.065","0.127","0.209","0.074","0.122","0.177","0.188","0.275","0.215","0.170","0.273","0.206","0.303","0.327","0.278","0.177","0.261","0.248","0.208","0.310","0.336","0.205","0.349","0.316","0.403","0.302","0.265"],["2.28","2.09","2.12","1.90","2.03","1.83","1.95","1.71","1.77","1.74","1.81","1.61","1.75","1.71","1.55","1.61","1.56","1.70","1.66","1.45","1.53","1.51","1.60","1.52","1.65","1.55","1.52","1.42","1.44","1.40","1.49","1.39","1.41","1.38","1.45","1.37","1.59","1.28","1.48","1.39","1.32","1.33","1.27","1.36","1.22","1.35","1.29","1.49","1.43","1.26","1.42","1.33","1.28","1.34","1.41","1.37","1.30","1.29","1.23","1.28","1.32","1.30","1.18","1.25","1.19","1.21","1.25","1.20","1.08","1.22","1.22","1.25","1.06","1.14","1.47","1.21","1.29","1.28","1.25","1.18","1.08","1.19","1.23","1.13","1.24","1.16","1.18","1.22","1.27","1.13","1.26","1.12","1.17","1.09","1.14","1.14","1.23","1.12","1.17","1.45","1.22","1.20","1.09","1.21","1.26","1.30","1.18","1.19","1.22","1.19","1.16","1.21","1.19","1.15","1.20","1.30","1.14","1.23","1.22","1.20","1.10","1.10","1.29","1.08","1.19","1.23","1.16","1.01","1.08","1.23","1.10","1.16","0.99","1.10","1.14","1.28","1.06","0.97","1.03","1.23","1.00","1.21","1.18","1.10","1.20","1.14","1.25","1.05","0.92","1.05","1.11","1.16","1.17","0.96","1.16","1.13","0.97","1.00","1.04","1.02","1.11","1.16","1.07","1.14","1.11","1.05","1.07","0.96","1.00","0.96","1.23","1.02","0.97","1.03","1.29","1.07","1.12","0.89","1.04","1.07","1.02","1.04","1.29","1.11","0.89","1.22","1.07","1.03","1.06","0.96","1.11","1.16","1.04","1.08","0.98","1.06","1.02","1.07","1.11","0.93","0.98","0.94","1.23","1.09","0.96","1.06","1.09","0.99","0.91","1.00","1.12","1.01","1.14","0.86","0.82","1.11","0.98","1.04","1.06","0.94","1.19","1.07","0.95","0.98","1.10","1.15","0.96","0.99","0.93","1.09","1.13","1.05","1.07","1.00","1.03","1.18","1.00","0.97","0.91","0.92","1.10","0.92","0.99","1.11","0.94","0.92","1.16","0.99","0.85","1.04","1.03","0.98","1.03","0.92","0.98","0.88","0.97","0.96","0.90","0.90","1.19","1.02","0.98","1.05","1.17","0.99","1.00","0.88","0.82","0.93","0.97","0.99","0.97","1.01","1.25","1.08","1.06","1.11","1.18","1.05","0.88","0.93","0.97","1.13","0.71","1.03","0.92","0.94","0.99","0.94","1.00","0.93","1.07","0.89","0.77","0.83","0.91","1.19","1.06","1.11","0.94","1.01","1.16","0.96","0.87","0.90","1.10","0.99","0.78","0.87","1.12","0.93","1.04","0.94","0.97","0.88","0.94","0.94","0.99","0.95","0.79","0.90","0.86","0.86","0.81","0.83","1.10","1.12","0.92","0.85","1.13","0.89","0.97","0.80","0.76","0.78","0.92","1.03","0.95","1.04","0.90","0.91","0.88","0.97","0.92","0.87","0.87","0.93","0.97","0.85","1.06","0.95","0.85","1.00","1.13","0.92","0.88","0.82","1.02","0.86","0.98","0.86","0.90","0.79","0.78","1.00","1.05","0.88","0.97","0.82","0.89","0.92","0.91","0.98","0.97","0.99","0.91","1.08","0.89","0.80","0.87","1.11","0.93","0.85","0.85","0.86","0.90","0.83","0.87","0.97","0.93","0.95","0.91","0.86","1.10","0.98","0.92","0.89","0.87","0.83","0.95","1.02","0.72","0.85","0.80","0.72","0.78","0.88","0.71","0.77","0.83","0.84","0.96","0.87","0.79","0.93","0.82","0.95","0.99","0.90","0.76","0.87","0.85","0.80","0.93","0.97","0.77","0.95","0.87","0.90","0.74","0.68"],["0.84","0.68","0.89","0.75","0.95","0.77","0.91","0.75","0.82","0.82","0.90","0.71","0.87","0.87","0.73","0.80","0.74","0.98","0.94","0.73","0.81","0.80","0.89","0.82","0.96","0.92","0.89","0.80","0.85","0.84","0.94","0.88","0.91","0.87","0.97","0.89","1.12","0.81","1.03","0.93","0.87","0.88","0.83","0.94","0.83","0.97","0.93","1.14","1.08","0.91","1.07","0.99","0.95","1.02","1.10","1.06","0.99","0.98","0.94","0.99","1.03","1.02","0.90","0.97","0.92","0.94","0.97","0.93","0.81","0.96","0.95","0.99","0.79","0.88","1.21","0.96","1.04","1.03","1.00","0.94","0.84","0.96","1.00","0.90","1.01","0.93","0.95","1.00","1.06","0.91","1.05","0.90","0.95","0.88","0.93","0.94","1.04","0.93","0.99","1.26","1.03","1.01","0.90","1.03","1.08","1.12","1.01","1.01","1.04","1.02","0.99","1.04","1.03","0.99","1.04","1.14","0.99","1.08","1.07","1.06","0.96","0.96","1.15","0.94","1.06","1.10","1.03","0.88","0.95","1.11","0.98","1.04","0.87","0.99","1.03","1.17","0.95","0.86","0.92","1.12","0.90","1.10","1.08","1.01","1.11","1.04","1.16","0.96","0.83","0.96","1.03","1.08","1.09","0.88","1.08","1.06","0.89","0.93","0.97","0.96","1.05","1.10","1.01","1.09","1.05","0.99","1.02","0.91","0.95","0.91","1.19","0.98","0.92","0.99","1.25","1.03","1.09","0.86","1.02","1.04","1.00","1.02","1.27","1.09","0.88","1.21","1.06","1.02","1.05","0.96","1.10","1.16","1.04","1.07","0.98","1.06","1.03","1.07","1.12","0.93","0.99","0.95","1.24","1.10","0.98","1.08","1.11","1.01","0.93","1.03","1.14","1.03","1.17","0.89","0.85","1.15","1.01","1.07","1.10","0.98","1.23","1.11","0.99","1.02","1.14","1.19","1.01","1.04","0.98","1.14","1.18","1.10","1.12","1.06","1.09","1.24","1.05","1.03","0.97","0.98","1.16","0.98","1.06","1.18","1.01","0.99","1.23","1.06","0.92","1.11","1.10","1.05","1.11","1.00","1.06","0.96","1.05","1.04","0.98","0.99","1.28","1.11","1.08","1.14","1.26","1.09","1.10","0.98","0.93","1.04","1.08","1.10","1.09","1.13","1.37","1.19","1.18","1.23","1.29","1.17","1.01","1.05","1.10","1.26","0.84","1.16","1.06","1.09","1.13","1.09","1.14","1.08","1.22","1.05","0.92","0.99","1.07","1.36","1.22","1.27","1.11","1.18","1.33","1.13","1.04","1.07","1.28","1.17","0.96","1.06","1.31","1.11","1.23","1.13","1.16","1.07","1.14","1.14","1.20","1.15","0.99","1.10","1.06","1.06","1.01","1.03","1.30","1.33","1.13","1.06","1.35","1.11","1.19","1.02","0.99","1.00","1.15","1.26","1.19","1.27","1.13","1.15","1.12","1.22","1.17","1.13","1.13","1.19","1.23","1.12","1.33","1.22","1.13","1.28","1.41","1.20","1.16","1.10","1.30","1.15","1.27","1.15","1.20","1.10","1.08","1.31","1.37","1.20","1.29","1.15","1.22","1.25","1.24","1.32","1.32","1.34","1.26","1.44","1.25","1.16","1.23","1.47","1.30","1.22","1.22","1.24","1.27","1.21","1.25","1.35","1.31","1.33","1.29","1.24","1.49","1.37","1.31","1.28","1.26","1.23","1.35","1.42","1.12","1.25","1.21","1.14","1.21","1.32","1.15","1.21","1.28","1.29","1.41","1.32","1.27","1.40","1.31","1.45","1.48","1.41","1.28","1.39","1.37","1.32","1.46","1.50","1.31","1.52","1.47","1.60","1.45","1.39"],["1.44","1.41","1.23","1.14","1.07","1.06","1.03","0.97","0.95","0.92","0.91","0.90","0.89","0.84","0.82","0.82","0.81","0.72","0.72","0.72","0.71","0.71","0.71","0.70","0.69","0.63","0.63","0.62","0.59","0.57","0.55","0.52","0.51","0.50","0.48","0.48","0.47","0.47","0.46","0.46","0.45","0.45","0.44","0.42","0.40","0.38","0.36","0.35","0.35","0.35","0.34","0.34","0.33","0.32","0.32","0.31","0.31","0.30","0.30","0.29","0.29","0.28","0.28","0.28","0.27","0.27","0.27","0.27","0.27","0.27","0.27","0.27","0.26","0.26","0.26","0.26","0.25","0.25","0.25","0.24","0.24","0.23","0.23","0.23","0.23","0.23","0.23","0.22","0.22","0.21","0.21","0.21","0.21","0.21","0.21","0.20","0.19","0.19","0.19","0.19","0.19","0.19","0.19","0.18","0.18","0.18","0.18","0.18","0.18","0.17","0.17","0.17","0.17","0.16","0.16","0.15","0.15","0.15","0.14","0.14","0.14","0.14","0.14","0.13","0.13","0.13","0.13","0.13","0.13","0.12","0.12","0.12","0.12","0.12","0.12","0.11","0.11","0.11","0.11","0.11","0.10","0.10","0.10","0.10","0.09","0.09","0.09","0.09","0.09","0.09","0.08","0.08","0.08","0.08","0.07","0.07","0.07","0.07","0.07","0.06","0.06","0.06","0.06","0.06","0.06","0.06","0.05","0.05","0.05","0.05","0.04","0.04","0.04","0.04","0.04","0.04","0.03","0.03","0.02","0.02","0.02","0.02","0.02","0.02","0.01","0.01","0.01","0.01","0.01","0.01","0.01","0.00","0.00","0.00","-0.00","-0.00","-0.00","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.02","-0.02","-0.02","-0.02","-0.02","-0.02","-0.03","-0.03","-0.03","-0.03","-0.03","-0.03","-0.03","-0.04","-0.04","-0.04","-0.04","-0.04","-0.04","-0.04","-0.04","-0.05","-0.05","-0.05","-0.05","-0.05","-0.05","-0.05","-0.05","-0.06","-0.06","-0.06","-0.06","-0.06","-0.06","-0.06","-0.06","-0.06","-0.07","-0.07","-0.07","-0.07","-0.07","-0.07","-0.07","-0.07","-0.07","-0.08","-0.08","-0.08","-0.08","-0.08","-0.08","-0.08","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.10","-0.10","-0.11","-0.11","-0.11","-0.11","-0.11","-0.11","-0.11","-0.12","-0.12","-0.12","-0.12","-0.12","-0.12","-0.13","-0.13","-0.13","-0.13","-0.13","-0.13","-0.14","-0.14","-0.14","-0.14","-0.15","-0.15","-0.15","-0.15","-0.15","-0.16","-0.16","-0.16","-0.16","-0.16","-0.16","-0.17","-0.17","-0.17","-0.17","-0.17","-0.18","-0.18","-0.18","-0.19","-0.19","-0.19","-0.19","-0.19","-0.19","-0.19","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.21","-0.21","-0.21","-0.21","-0.22","-0.22","-0.22","-0.22","-0.23","-0.23","-0.23","-0.23","-0.24","-0.24","-0.24","-0.25","-0.25","-0.25","-0.25","-0.26","-0.26","-0.26","-0.26","-0.27","-0.27","-0.27","-0.27","-0.28","-0.28","-0.28","-0.28","-0.28","-0.28","-0.29","-0.29","-0.30","-0.30","-0.30","-0.30","-0.31","-0.32","-0.32","-0.32","-0.33","-0.33","-0.33","-0.33","-0.34","-0.34","-0.35","-0.35","-0.36","-0.36","-0.36","-0.36","-0.36","-0.37","-0.37","-0.37","-0.37","-0.37","-0.38","-0.38","-0.38","-0.38","-0.38","-0.38","-0.39","-0.39","-0.39","-0.39","-0.39","-0.40","-0.40","-0.40","-0.40","-0.40","-0.40","-0.40","-0.42","-0.44","-0.44","-0.44","-0.44","-0.44","-0.45","-0.45","-0.45","-0.47","-0.47","-0.49","-0.49","-0.50","-0.51","-0.51","-0.52","-0.52","-0.52","-0.52","-0.53","-0.54","-0.56","-0.59","-0.70","-0.71","-0.71"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Attacking<\/th>\n      <th>Defense<\/th>\n      <th>Expected Offense<\/th>\n      <th>Expected Defense<\/th>\n      <th>Expected Margin<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"scrollX":true,"columnDefs":[{"className":"dt-center","targets":[1,2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
