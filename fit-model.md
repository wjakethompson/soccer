
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

This filtering process leaves a total of 7400 games between 449 teams. The number of games for each team ranges from 5 to 60.

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
  cores = 3, algorithm = "NUTS", seed = 71715,
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
 Chain 1    1.003           0.988                  7       
 Chain 2    0.930           0.977                  6       
 Chain 3    0.939           0.985                  7       

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

Figure \@ref(fig:plot-mov-int) shows examples of games where the observed margin of victory fell inside the 50% credible interval, inside the 95% credible interval, and outside both intervals. Overall, for the 7400 games included in the estimation, the observed margin of victory fell within the 50% credible interval 70.4 percent of the time and within the 95% credible 98.1 percent of the time.

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

Using the predictions from the replicated data sets, the model gave the observed outcome the highest probability in 53.4 percent of the games.

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

The log loss for predictions from the replicated data sets is 1.717. Converting back to a probability scale, on average, the probability of the observed outcome was off by 0.18. In isolation, the log loss can be hard to interpret. Instead, it is often useful to compare to baseline models.


Table: (\#tab:logloss-tab)Log loss comparison to baseline models.

         Model            Log Loss 
-----------------------  ----------
 Game Random Intercept      1.72   
     Data Average           1.86   
  Equal Probabilities       1.91   
       Home Win            37.62   

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

As we might expect, the top of the list is dominated by team leading the best European leagues and having success in the Champions League. Bayern Munich comes in as the top team in rankings, followed by Barcelona, Paris Saint-Germain, AS Monaco, and Real Madrid to round out the top 5. The top offense belongs to Barcelona according to the model, while FC Copenhagen boasts the best defense.


```r
DT::datatable(select(rankings, -Club), rownames = rankings$Club,
  options = list(pageLength = 10, scrollX = TRUE,
  columnDefs = list(list(className = 'dt-center', targets = 1:5))),
  caption = "Club rankings from game random intercept model")
```

<!--html_preserve--><div id="htmlwidget-27bd867acf4efb0774ff" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-27bd867acf4efb0774ff">{"x":{"filter":"none","caption":"<caption>Club rankings from game random intercept model<\/caption>","data":[["Bayern Munich","Barcelona","Paris Saint-Germain","AS Monaco","Real Madrid","Tottenham Hotspur","Chelsea","Arsenal","Manchester City","Juventus","Liverpool","Celtic","Borussia Dortmund","Feyenoord Rotterdam","FC Porto","AS Roma","Ajax Amsterdam","Manchester United","FC Copenhagen","Napoli","Atletico Madrid","Shakhtar Donetsk","Benfica","Anderlecht","FC Basel","PSV Eindhoven","Lyon","FC Salzburg","Newcastle United","Zenit St Petersburg","Brighton &amp; Hove Albion","Besiktas","TSG Hoffenheim","RB Leipzig","Everton","Fenerbahce","Lincoln City","Istanbul Buyuksehir BSK","Fulham","Brøndby","Aberdeen","Olympiakos","Norwich City","Lazio","Schalke 04","Sporting CP","Spartak Moscow","Atalanta","Sevilla FC","Tranmere","Rostov","Levante","Club Brugge","Sheff Utd","PAOK Salonika","Torino","Hannover 96","Galatasaray","Luton Town","Nice","AC Milan","Celta Vigo","Fiorentina","KAA Gent","Internazionale","Spal","Racing Genk","Marseille","VfB Stuttgart","Borussia Monchengladbach","Lokomotiv Moscow","Young Boys","Villarreal","CSKA Moscow","Panathinaikos","Bayer Leverkusen","Dagenham &amp; Redbridge","Dover","Braga","Krasnodar","FC Union Berlin","FC Midtjylland","Plymouth Argyle","KV Oostende","Rangers","Lens","FC Utrecht","Leeds United","Eibar","Dynamo Kiev","Brentford","Bolton Wanderers","Real Sociedad","Standard Liege","TSV Eintracht Braunschweig","Aldershot","Southampton","Hertha Berlin","Athletic Bilbao","FC Cologne","Zulte-Waregem","Hellas Verona","Guimaraes","Huddersfield Town","Hibernian","Sheffield Wednesday","Preston North End","Vitesse Arnhem","Nimes","Blackpool","Portsmouth","Dynamo Dresden","Strasbourg","Halifax","F Green","Barrow","Girona","Espanyol","Getafe","Leicester City","Bradford","SC Amiens","Austria Vienna","Reading","Apoel Nicosia","Millwall","Troyes","Benevento","West Bromwich Albion","Barnsley","FC Nordsjaelland","Huesca","Bordeaux","Derby County","Perugia","Scunthorpe","Rapid Vienna","Brest","Middlesbrough","Doncaster","Frosinone","Gateshead","Oxford United","OSM","FC Sion","Sevilla At.","Exeter City","Fleetwood Town","Werder Bremen","Crystal Palace","Wycombe Wanderers","Toulouse","Lugo","Sampdoria","Heidenheimer SB","Las Palmas","Cádiz","Alavés","Heerenveen","Colchester","Royal Charleroi SC","Cardiff City","Rochdale","Elche","Wolverhampton Wanderers","Guingamp","Trabzonspor","Hapoel Be'e","Tenerife","Le Havre AC","Eintracht Frankfurt","Virtus Entel","Bristol City","Udinese","FK Rubin Kazan","Heart of Midlothian","Aston Villa","Burnley","York","St Etienne","Ludogorets","Accrington Stanley","Kasimpasa","SK Sturm Gra","GFC Ajaccio","Cambridge United","FK Amkar Perm","Stoke City","Stevenage","Real Valladolid","FC Groningen","Stade de Reims","SC Rheindorf","Lucerne","R Zaragoza","St Johnstone","Steaua Bucuresti","Macclesfield Town","Valencia","Cittadella","AFC Bournemouth","Rayo","Evian Thonon Gaillard","Swansea City","AEK Athens","Cesena","Salernitana","Antalyaspor","Valenciennes","Mainz","Ipswich Town","Grimsby","FC Twente","Liberec","Panionios","Spezia","Carlisle","Hull City","Sochaux","Morton","Randers FC","SV Sandhausen","Terek Grozny","FC Xanthi","Almeria","Maritimo","AZ Alkmaar","Clermont Foot","Lyngby","Genclerbirligi","Mansfield","Alanyaspor K","Platanias","Sassuolo","AFC Wimbledon","Blackburn Rovers","Angers","Watford","FC Zürich","Novara","Maccabi Tel-Aviv","West Ham United","Charlton Athletic","VfL Wolfsburg","Partick Thistle","Bourg-Peronnas","GD Chaves","Queens Park Rangers","Mallorca","FC Zorya Luh","Carpi","Würzburger Kickers","Chievo Verona","Vitoria Setubal","Tours","Milton Keynes Dons","Sutton United","Boavista","Nottingham Forest","Dijon FCO","Nurnberg","Krylia Sovetov","Rio Ave","Sparta Prague","Numancia","FC Astana","St. Truiden","Eastleigh","Bari","Fortuna Düsseldorf","Karabükspor","Viktoria Plzen","Bristol Rovers","FC Ufa","Boreham Wood","Kaiserslautern","Atromitos","Southend","VfL Bochum","Cagliari","AaB","Montpellier","KV Mechelen","Konyaspor","Niort","FK Qarabag","SC Freiburg","FK Astra Giu","Lille","Reus Deporti","Bologna","Bursaspor","Deportivo La Coruña","AJ Auxerre","Ascoli","Walsall","TSV 1860 Munich","SpVgg Greuther Furth","Birmingham City","Burton Albion","Larissa FC","St Pauli","UCAM Murcia","Sonderjyske","Málaga","Legia Warsaw","Lausanne Spo","Peterborough United","Gimnastic de Tarragona","Pisa","SC Heracles Almelo","KV Kortrijk","Cordoba","Dundalk","AS Nancy Lorraine","Odense Boldk","Cork","Iraklis","Arminia Bielefeld","Orenburg","St. Pölten","FC Ingolstadt 04","Eupen","Stade Laval","Barnet","Oldham","Guiseley","FC Augsburg","AGF Aarhus","Stade Rennes","Anzhi Makhachkala","Torquay","AD Alcorcon","Real Betis","Yeovil Town","Orléans","Chester FC","Admira Wacke","AC Ajaccio","FK Qabala","Pro Vercelli","Real Oviedo","FC Lugano","Hamburg SV","Wigan Athletic","US Avellino","Kayserispor","Trapani","Panetolikos","St Gallen","Nantes","Pas Giannina","Estoril","Latina","FC Thun","Cheltenham Town","Sparta Rotterdam","Genoa","Paços de Ferreira","Northampton Town","Lokeren","Viitorul Con","Brescia","Kerkyra","Wrexham","Grasshoppers","Viborg FF","Asteras Trip","Willem II Tilburg","Dinamo Zagreb","FC Erzgebirge Aue","Akhisar","Belenenses","Maidstone United","Red Star FC 93","Woking","Arouca","RZ Pellets W","Bromley","Sunderland","Crawley Town","Ural","Waasland-Bev","Bury","Solihull Moors","Leganes","Ternana","Mattersburg","Lorient","Ross County","Esbjerg FB","Gillingham","Caykur Rizespor","Shrewsbury Town","Levadiakos","Vicenza","Notts County","Excelsior","Crotone","Morecambe","Moreirense","Braintree Town","Newport County","Swindon Town","Dundee","Sporting Gijón","Gaziantepspor","Bastia","Caen","Empoli","Hartlepool","Feirense","Kilmarnock","AC Horsens","Hamilton Academical","Crewe","ADO Den Haag","SV Josko Rie","Port Vale","Silkeborg IF","Roda JC Kerkrade","FC Arsenal T","PEC Zwolle","L Orient","Coventry City","Metz","Karlsruher SC","Veria","Mirandes","Adanaspor","Chesterfield","NEC Nijmegen","Westerlo","Motherwell","US Pescara","CD Nacional de Madeira","SV Darmstadt 98","Inverness Caledonian Thistle","Mouscron-Per","Palermo","FC Vaduz","Osasuna","Tondela","Southport","Granada","Go Ahead Eagles","Rotherham United","North Ferrib","FK Tom Tomsk"],["0.666","0.734","0.616","0.678","0.690","0.525","0.529","0.595","0.533","0.411","0.494","0.515","0.530","0.456","0.391","0.482","0.434","0.371","0.309","0.502","0.288","0.414","0.351","0.411","0.378","0.314","0.424","0.312","0.330","0.363","0.306","0.386","0.332","0.318","0.303","0.266","0.278","0.252","0.365","0.285","0.222","0.163","0.426","0.268","0.216","0.256","0.234","0.218","0.209","0.186","0.088","0.232","0.195","0.216","0.095","0.340","0.219","0.295","0.199","0.162","0.168","0.216","0.225","0.154","0.287","0.208","0.192","0.175","0.214","0.180","0.054","0.260","0.045","0.018","0.075","0.175","0.189","0.314","0.147","0.066","0.137","0.211","0.092","0.175","0.104","0.145","0.129","0.039","0.200","0.075","0.221","0.029","0.179","0.146","0.080","0.049","0.008","0.057","0.097","0.121","0.166","0.144","0.161","0.064","0.032","0.024","0.165","0.101","0.105","0.052","0.116","0.143","0.177","0.124","0.099","0.094","0.121","0.062","0.044","0.156","-0.036","0.037","0.280","0.148","0.002","0.114","0.122","0.014","0.033","0.125","0.210","0.003","0.100","-0.032","0.053","0.130","0.032","0.018","-0.076","0.088","0.026","-0.004","0.113","0.022","0.175","0.038","0.099","-0.017","0.186","0.103","0.049","-0.083","0.091","-0.008","-0.002","0.212","0.055","-0.120","0.152","0.079","-0.127","0.057","0.125","0.028","0.001","0.043","-0.085","-0.065","-0.048","-0.122","-0.141","0.052","0.035","0.026","-0.053","0.096","-0.100","-0.073","0.092","-0.169","0.039","-0.042","0.061","-0.043","-0.004","-0.024","-0.170","-0.011","0.103","0.002","0.070","-0.058","0.102","0.158","-0.040","-0.013","-0.041","-0.026","0.145","0.021","0.115","-0.126","-0.081","0.130","-0.085","-0.019","-0.116","-0.061","-0.036","0.065","-0.121","-0.090","0.031","0.053","-0.131","-0.159","0.036","0.055","-0.190","0.010","-0.035","-0.012","-0.019","-0.136","-0.050","-0.168","0.107","-0.039","-0.186","-0.189","-0.083","0.210","0.006","0.041","-0.058","-0.020","-0.087","-0.039","-0.086","-0.063","-0.107","0.089","-0.112","-0.122","-0.171","0.054","-0.086","-0.057","-0.106","-0.171","-0.202","-0.204","-0.074","-0.189","0.048","0.002","-0.049","-0.186","0.047","0.010","0.075","-0.077","-0.077","-0.069","-0.096","-0.073","-0.006","-0.008","-0.159","-0.098","-0.080","-0.098","0.034","-0.310","-0.198","-0.272","-0.042","-0.049","-0.080","0.136","-0.211","0.101","-0.030","-0.116","-0.038","-0.063","0.029","-0.149","-0.202","-0.205","-0.136","-0.130","-0.064","-0.229","-0.082","-0.096","-0.108","-0.154","-0.144","-0.130","-0.190","-0.225","-0.116","-0.080","-0.053","0.034","0.081","-0.024","-0.092","-0.445","0.011","0.122","-0.148","-0.169","-0.277","-0.204","-0.106","-0.131","0.026","-0.163","-0.124","-0.111","0.075","-0.234","-0.099","-0.337","-0.200","-0.138","-0.133","-0.212","-0.236","-0.093","-0.366","-0.134","-0.094","-0.199","0.052","-0.194","-0.052","-0.087","-0.247","-0.047","-0.001","-0.091","-0.268","-0.167","0.023","-0.131","-0.140","-0.172","-0.198","-0.209","-0.235","-0.236","0.045","-0.156","0.004","-0.076","-0.207","0.027","-0.383","-0.097","-0.097","-0.205","-0.260","-0.151","-0.214","-0.048","-0.289","-0.211","-0.137","-0.287","-0.300","-0.041","-0.190","0.041","-0.170","-0.176","-0.196","-0.246","-0.131","-0.232","-0.262","0.015","0.013","-0.288","-0.199","-0.228","-0.021","-0.137","-0.133","0.002","-0.151","-0.221","-0.230","-0.320","-0.092","-0.077","-0.284","0.024","-0.233","-0.103","-0.147","-0.241","-0.193","-0.119","-0.182","-0.311","-0.127","-0.389","-0.152","-0.234","-0.297","-0.143","-0.265","-0.093","-0.163","-0.129","-0.245","-0.099","-0.364","-0.356","-0.116","-0.140","-0.251","-0.201","-0.352","-0.134","-0.160","-0.261","-0.174","-0.254","-0.100","-0.114","-0.158","-0.365","-0.297","-0.142","-0.257","-0.307","-0.126","-0.192","-0.335","-0.109","-0.277","-0.231","-0.193","-0.390","-0.394"],["-0.447","-0.244","-0.378","-0.182","-0.129","-0.405","-0.328","-0.151","-0.279","-0.485","-0.281","-0.201","-0.154","-0.301","-0.427","-0.228","-0.302","-0.416","-0.541","-0.129","-0.452","-0.191","-0.302","-0.168","-0.228","-0.312","-0.108","-0.295","-0.237","-0.161","-0.257","-0.120","-0.166","-0.183","-0.182","-0.209","-0.170","-0.208","-0.034","-0.145","-0.238","-0.330","0.075","-0.158","-0.182","-0.118","-0.147","-0.155","-0.144","-0.172","-0.314","-0.101","-0.147","-0.108","-0.267","0.072","-0.077","0.025","-0.092","-0.139","-0.129","-0.065","-0.048","-0.139","0.036","-0.058","-0.076","-0.095","-0.038","-0.083","-0.241","0.029","-0.250","-0.283","-0.200","-0.069","-0.049","0.109","-0.101","-0.196","-0.101","-0.009","-0.158","-0.052","-0.143","-0.089","-0.101","-0.216","-0.010","-0.158","0.027","-0.210","-0.021","-0.057","-0.132","-0.166","-0.213","-0.152","-0.102","-0.072","-0.016","-0.042","-0.018","-0.131","-0.164","-0.172","0.004","-0.069","-0.061","-0.123","-0.046","-0.014","0.030","-0.032","-0.060","-0.060","-0.029","-0.096","-0.117","0.018","-0.197","-0.110","0.171","0.026","-0.133","-0.005","0.005","-0.117","-0.091","0.013","0.109","-0.121","-0.008","-0.154","-0.056","0.030","-0.071","-0.083","-0.187","-0.005","-0.073","-0.105","0.024","-0.074","0.100","-0.048","0.022","-0.101","0.119","0.029","-0.028","-0.167","0.023","-0.079","-0.069","0.159","-0.005","-0.190","0.100","0.025","-0.194","0.004","0.077","-0.024","-0.052","-0.007","-0.140","-0.118","-0.100","-0.178","-0.193","0.010","-0.007","-0.014","-0.097","0.061","-0.142","-0.114","0.058","-0.213","0.006","-0.076","0.030","-0.077","-0.036","-0.054","-0.190","-0.028","0.089","-0.014","0.057","-0.070","0.093","0.152","-0.044","-0.017","-0.042","-0.026","0.148","0.026","0.120","-0.119","-0.073","0.136","-0.077","-0.010","-0.106","-0.050","-0.024","0.076","-0.108","-0.075","0.047","0.070","-0.108","-0.136","0.058","0.078","-0.161","0.034","-0.010","0.013","0.009","-0.104","-0.017","-0.127","0.139","-0.001","-0.141","-0.143","-0.042","0.244","0.049","0.084","-0.010","0.027","-0.027","0.021","-0.023","-0.002","-0.039","0.146","-0.041","-0.047","-0.092","0.118","-0.012","0.015","-0.026","-0.084","-0.109","-0.110","0.010","-0.094","0.123","0.083","0.035","-0.086","0.128","0.094","0.155","0.016","0.017","0.024","0.001","0.022","0.083","0.083","-0.052","0.005","0.026","0.010","0.130","-0.177","-0.078","-0.141","0.064","0.059","0.032","0.227","-0.084","0.197","0.079","0.005","0.077","0.055","0.139","-0.018","-0.064","-0.065","-0.004","0.002","0.060","-0.079","0.050","0.045","0.035","-0.004","0.005","0.019","-0.030","-0.060","0.036","0.068","0.092","0.169","0.215","0.125","0.068","-0.223","0.158","0.255","0.024","0.009","-0.074","-0.012","0.070","0.051","0.184","0.026","0.059","0.073","0.231","-0.027","0.085","-0.104","0.006","0.057","0.062","-0.002","-0.018","0.099","-0.120","0.066","0.099","0.013","0.223","0.025","0.141","0.118","-0.010","0.152","0.190","0.120","-0.018","0.063","0.216","0.094","0.088","0.064","0.046","0.037","0.018","0.019","0.245","0.087","0.216","0.157","0.056","0.240","-0.072","0.145","0.146","0.063","0.021","0.107","0.063","0.195","0.013","0.074","0.132","0.020","0.014","0.211","0.097","0.278","0.122","0.120","0.107","0.073","0.158","0.085","0.064","0.271","0.271","0.049","0.113","0.094","0.251","0.166","0.169","0.272","0.157","0.109","0.106","0.050","0.216","0.231","0.088","0.314","0.129","0.223","0.194","0.130","0.164","0.217","0.173","0.086","0.215","0.044","0.204","0.149","0.109","0.217","0.135","0.254","0.210","0.236","0.161","0.263","0.092","0.098","0.270","0.254","0.181","0.223","0.136","0.279","0.264","0.202","0.260","0.215","0.323","0.315","0.293","0.167","0.209","0.316","0.248","0.227","0.341","0.301","0.217","0.361","0.270","0.306","0.405","0.310","0.331"],["2.08","2.23","1.98","2.11","2.13","1.81","1.81","1.94","1.82","1.61","1.75","1.79","1.82","1.69","1.58","1.73","1.65","1.55","1.46","1.77","1.43","1.62","1.52","1.61","1.56","1.46","1.63","1.46","1.49","1.54","1.45","1.57","1.49","1.47","1.45","1.39","1.41","1.38","1.54","1.42","1.33","1.26","1.64","1.40","1.33","1.38","1.35","1.33","1.32","1.29","1.17","1.35","1.30","1.33","1.18","1.50","1.33","1.44","1.30","1.26","1.27","1.33","1.34","1.25","1.42","1.32","1.30","1.27","1.32","1.28","1.13","1.39","1.12","1.09","1.15","1.27","1.29","1.46","1.24","1.14","1.23","1.32","1.17","1.27","1.19","1.24","1.22","1.11","1.31","1.15","1.33","1.10","1.28","1.24","1.16","1.12","1.08","1.13","1.18","1.21","1.26","1.23","1.26","1.14","1.10","1.10","1.26","1.18","1.19","1.13","1.20","1.23","1.28","1.21","1.18","1.17","1.21","1.14","1.12","1.25","1.03","1.11","1.41","1.24","1.07","1.20","1.21","1.08","1.11","1.21","1.32","1.07","1.18","1.04","1.13","1.22","1.10","1.09","0.99","1.17","1.10","1.06","1.20","1.09","1.27","1.11","1.18","1.05","1.29","1.18","1.12","0.98","1.17","1.06","1.07","1.32","1.13","0.95","1.25","1.16","0.94","1.13","1.21","1.10","1.07","1.12","0.98","1.00","1.02","0.95","0.93","1.13","1.11","1.10","1.01","1.18","0.97","0.99","1.17","0.90","1.11","1.03","1.14","1.02","1.06","1.04","0.90","1.06","1.19","1.07","1.15","1.01","1.18","1.25","1.03","1.06","1.03","1.04","1.24","1.09","1.20","0.94","0.99","1.22","0.98","1.05","0.95","1.01","1.03","1.14","0.95","0.98","1.10","1.13","0.94","0.91","1.11","1.13","0.88","1.08","1.03","1.06","1.05","0.93","1.02","0.90","1.19","1.03","0.89","0.89","0.98","1.32","1.08","1.11","1.01","1.05","0.98","1.03","0.98","1.00","0.96","1.17","0.96","0.95","0.90","1.13","0.98","1.01","0.96","0.90","0.87","0.87","0.99","0.88","1.12","1.07","1.02","0.89","1.12","1.08","1.15","0.99","0.99","1.00","0.97","0.99","1.06","1.06","0.91","0.97","0.99","0.97","1.11","0.78","0.88","0.81","1.03","1.02","0.99","1.23","0.87","1.18","1.04","0.95","1.03","1.00","1.10","0.92","0.87","0.87","0.93","0.94","1.00","0.85","0.98","0.97","0.96","0.92","0.93","0.94","0.88","0.85","0.95","0.99","1.01","1.11","1.16","1.04","0.97","0.68","1.08","1.21","0.92","0.90","0.81","0.87","0.96","0.94","1.10","0.91","0.94","0.96","1.15","0.85","0.97","0.76","0.88","0.93","0.94","0.86","0.84","0.97","0.74","0.94","0.97","0.88","1.13","0.88","1.02","0.98","0.84","1.02","1.07","0.98","0.82","0.90","1.09","0.94","0.93","0.90","0.88","0.87","0.85","0.84","1.12","0.91","1.07","0.99","0.87","1.10","0.73","0.97","0.97","0.87","0.82","0.92","0.86","1.02","0.80","0.87","0.93","0.80","0.79","1.03","0.88","1.11","0.90","0.90","0.88","0.84","0.94","0.85","0.82","1.08","1.08","0.80","0.88","0.85","1.05","0.93","0.94","1.07","0.92","0.86","0.85","0.78","0.98","0.99","0.81","1.10","0.85","0.96","0.92","0.84","0.88","0.95","0.89","0.78","0.94","0.72","0.92","0.85","0.79","0.93","0.82","0.97","0.91","0.94","0.84","0.97","0.74","0.75","0.95","0.93","0.83","0.87","0.75","0.93","0.91","0.82","0.90","0.83","0.97","0.95","0.91","0.74","0.79","0.93","0.83","0.79","0.94","0.88","0.77","0.96","0.81","0.85","0.88","0.72","0.72"],["0.68","0.84","0.73","0.89","0.94","0.71","0.77","0.92","0.81","0.66","0.81","0.87","0.92","0.79","0.70","0.85","0.79","0.71","0.62","0.94","0.68","0.88","0.79","0.90","0.85","0.78","0.96","0.80","0.84","0.91","0.83","0.95","0.91","0.89","0.89","0.87","0.90","0.87","1.03","0.92","0.84","0.77","1.15","0.91","0.89","0.95","0.92","0.92","0.93","0.90","0.78","0.97","0.92","0.96","0.82","1.15","0.99","1.10","0.98","0.93","0.94","1.00","1.02","0.93","1.11","1.01","0.99","0.97","1.03","0.98","0.84","1.10","0.83","0.81","0.88","1.00","1.02","1.19","0.97","0.88","0.97","1.06","0.91","1.01","0.93","0.98","0.97","0.86","1.06","0.91","1.10","0.87","1.05","1.01","0.94","0.91","0.86","0.92","0.97","0.99","1.05","1.03","1.05","0.94","0.91","0.90","1.07","1.00","1.01","0.95","1.02","1.05","1.10","1.04","1.01","1.01","1.04","0.97","0.95","1.09","0.88","0.96","1.27","1.10","0.94","1.06","1.07","0.95","0.98","1.08","1.19","0.95","1.06","0.92","1.01","1.10","1.00","0.98","0.89","1.06","0.99","0.96","1.10","0.99","1.18","1.02","1.09","0.97","1.20","1.10","1.04","0.91","1.09","0.99","1.00","1.25","1.06","0.88","1.18","1.10","0.88","1.07","1.16","1.04","1.02","1.06","0.93","0.95","0.97","0.90","0.88","1.08","1.06","1.05","0.97","1.14","0.93","0.95","1.13","0.86","1.08","0.99","1.10","0.99","1.03","1.01","0.88","1.04","1.17","1.05","1.13","1.00","1.17","1.24","1.02","1.05","1.02","1.04","1.24","1.10","1.21","0.95","0.99","1.23","0.99","1.06","0.96","1.02","1.04","1.15","0.96","0.99","1.12","1.15","0.96","0.93","1.13","1.16","0.91","1.11","1.06","1.08","1.08","0.96","1.05","0.94","1.23","1.07","0.93","0.93","1.03","1.37","1.12","1.16","1.06","1.10","1.04","1.09","1.04","1.07","1.03","1.24","1.03","1.02","0.98","1.20","1.06","1.09","1.04","0.98","0.96","0.96","1.08","0.97","1.21","1.16","1.11","0.98","1.21","1.17","1.25","1.09","1.09","1.09","1.07","1.09","1.16","1.16","1.01","1.07","1.10","1.08","1.22","0.90","0.99","0.93","1.14","1.13","1.10","1.34","0.98","1.30","1.16","1.07","1.16","1.13","1.23","1.05","1.00","1.00","1.06","1.07","1.14","0.99","1.12","1.12","1.11","1.06","1.07","1.09","1.04","1.01","1.11","1.14","1.17","1.27","1.33","1.21","1.14","0.86","1.25","1.38","1.09","1.08","0.99","1.06","1.15","1.13","1.29","1.10","1.13","1.15","1.35","1.04","1.16","0.96","1.08","1.13","1.14","1.07","1.05","1.18","0.95","1.14","1.18","1.08","1.34","1.10","1.23","1.20","1.06","1.24","1.29","1.20","1.05","1.14","1.33","1.17","1.17","1.14","1.12","1.11","1.09","1.09","1.37","1.17","1.33","1.25","1.13","1.36","0.99","1.24","1.24","1.14","1.09","1.19","1.14","1.30","1.08","1.15","1.22","1.09","1.08","1.32","1.18","1.41","1.21","1.21","1.19","1.15","1.25","1.16","1.14","1.40","1.40","1.12","1.20","1.17","1.37","1.26","1.27","1.40","1.25","1.19","1.19","1.12","1.33","1.35","1.17","1.46","1.22","1.34","1.30","1.22","1.26","1.33","1.27","1.17","1.33","1.12","1.31","1.24","1.19","1.33","1.22","1.38","1.32","1.35","1.26","1.39","1.17","1.18","1.40","1.38","1.28","1.34","1.22","1.41","1.39","1.31","1.39","1.33","1.48","1.46","1.43","1.26","1.32","1.47","1.37","1.34","1.50","1.44","1.33","1.53","1.40","1.45","1.60","1.46","1.49"],["1.40","1.39","1.25","1.22","1.19","1.09","1.04","1.02","1.01","0.95","0.94","0.91","0.90","0.90","0.88","0.88","0.86","0.84","0.83","0.83","0.75","0.73","0.73","0.71","0.71","0.68","0.67","0.66","0.64","0.63","0.62","0.62","0.58","0.58","0.56","0.53","0.51","0.51","0.51","0.50","0.49","0.49","0.49","0.48","0.44","0.43","0.43","0.41","0.39","0.39","0.39","0.38","0.38","0.37","0.36","0.35","0.34","0.34","0.33","0.33","0.33","0.33","0.32","0.32","0.32","0.31","0.30","0.30","0.30","0.30","0.29","0.29","0.29","0.28","0.28","0.28","0.27","0.27","0.27","0.26","0.26","0.26","0.26","0.26","0.26","0.26","0.25","0.25","0.25","0.24","0.23","0.23","0.23","0.23","0.22","0.22","0.21","0.21","0.21","0.21","0.21","0.21","0.21","0.20","0.20","0.20","0.19","0.18","0.18","0.18","0.18","0.18","0.18","0.17","0.17","0.17","0.17","0.17","0.17","0.16","0.15","0.15","0.15","0.14","0.13","0.13","0.13","0.13","0.13","0.13","0.13","0.13","0.12","0.12","0.12","0.12","0.11","0.11","0.10","0.10","0.10","0.10","0.10","0.10","0.09","0.09","0.09","0.08","0.08","0.08","0.08","0.08","0.08","0.07","0.07","0.07","0.07","0.06","0.06","0.06","0.06","0.06","0.06","0.06","0.05","0.05","0.05","0.05","0.05","0.05","0.05","0.05","0.04","0.04","0.04","0.04","0.04","0.04","0.04","0.04","0.04","0.03","0.03","0.03","0.03","0.03","0.02","0.02","0.02","0.02","0.02","0.01","0.01","0.01","0.00","0.00","0.00","-0.00","-0.00","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.01","-0.02","-0.02","-0.02","-0.02","-0.02","-0.03","-0.03","-0.03","-0.03","-0.03","-0.03","-0.03","-0.03","-0.04","-0.04","-0.04","-0.04","-0.04","-0.04","-0.05","-0.05","-0.05","-0.05","-0.05","-0.06","-0.06","-0.06","-0.06","-0.07","-0.07","-0.07","-0.07","-0.07","-0.07","-0.07","-0.08","-0.08","-0.08","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.09","-0.10","-0.10","-0.10","-0.10","-0.10","-0.10","-0.10","-0.10","-0.10","-0.11","-0.11","-0.11","-0.11","-0.11","-0.11","-0.11","-0.11","-0.12","-0.12","-0.12","-0.12","-0.12","-0.12","-0.12","-0.13","-0.13","-0.13","-0.13","-0.13","-0.13","-0.13","-0.13","-0.13","-0.14","-0.14","-0.15","-0.15","-0.15","-0.15","-0.15","-0.15","-0.15","-0.16","-0.16","-0.16","-0.16","-0.17","-0.17","-0.17","-0.17","-0.17","-0.17","-0.17","-0.18","-0.18","-0.19","-0.19","-0.19","-0.19","-0.19","-0.19","-0.19","-0.19","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.20","-0.21","-0.21","-0.21","-0.21","-0.21","-0.21","-0.21","-0.22","-0.22","-0.22","-0.22","-0.22","-0.23","-0.23","-0.23","-0.23","-0.23","-0.24","-0.24","-0.24","-0.24","-0.24","-0.24","-0.25","-0.25","-0.25","-0.25","-0.26","-0.26","-0.26","-0.27","-0.27","-0.27","-0.27","-0.27","-0.27","-0.27","-0.28","-0.28","-0.29","-0.29","-0.29","-0.29","-0.29","-0.29","-0.30","-0.31","-0.31","-0.31","-0.31","-0.32","-0.32","-0.32","-0.32","-0.32","-0.32","-0.32","-0.32","-0.33","-0.33","-0.33","-0.33","-0.33","-0.33","-0.34","-0.35","-0.35","-0.36","-0.36","-0.37","-0.37","-0.37","-0.38","-0.38","-0.38","-0.38","-0.38","-0.38","-0.38","-0.39","-0.39","-0.40","-0.40","-0.40","-0.40","-0.40","-0.41","-0.41","-0.42","-0.42","-0.43","-0.43","-0.45","-0.45","-0.45","-0.46","-0.47","-0.48","-0.48","-0.49","-0.49","-0.50","-0.51","-0.51","-0.52","-0.52","-0.52","-0.54","-0.54","-0.56","-0.56","-0.56","-0.56","-0.58","-0.59","-0.60","-0.72","-0.73","-0.77"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Attacking<\/th>\n      <th>Defense<\/th>\n      <th>Expected Offense<\/th>\n      <th>Expected Defense<\/th>\n      <th>Expected Margin<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"scrollX":true,"columnDefs":[{"className":"dt-center","targets":[1,2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
