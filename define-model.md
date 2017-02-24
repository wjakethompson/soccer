
# Defining the Model {#define-model}

In soccer, the goals scored by a single team can be thought of as coming from a Poisson distribution. Thus, we can state that the two scores from a given game, $X$ and $Y$, are Poisson distributed.

\begin{equation}
\begin{split}
  X & \sim Poisson(\lambda_1)\\
  Y & \sim Poisson(\lambda_2)
\end{split}
(\#eq:indep-pois)
\end{equation}

This parameterization, however, assumes that the scores $X$ and $Y$ are independent of each other. In this project, the aim is to model the association between the two Poisson distributed variables. There are two methods for modeling this association that will be examined. The first is a bivariate Poisson distribution. The second is a mixed effects model with a random slope for each game.

## The Bivariate Poisson {#biv-pois}

In the bivariate Poisson, the scores for teams $A$ and $B$, $X_A$ and $X_B$, are random variables where $G_i \sim Poisson(\lambda_i),\ i = 0,\ 1,\ 2$.

\begin{equation}
\begin{split}
  X_A & = G_1 + G_0\\
  X_B & = G_2 + G_0
\end{split}
(\#eq:bipois)
\end{equation}

And $X_A$ and $X_B$ are jointly distributed

\begin{equation}
  (X_A,\ X_B) \sim BP(\lambda_1,\ \lambda_2,\ \lambda_0)
(\#eq:jbp)
\end{equation}

In this parameterization, $X_A$ and $X_B$ are Poisson distributed with means equal to $\lambda_1 + \lambda_3$ and $\lambda_2 + \lambda_3$ respectively, with $\lambda_3$ representing the covariance between $X_A$ and $X_B$ [@AlMuhayfith2016; @griffiths1978; @kawamura1973]. We can model these parameters just as we would model, for example, means in a normal distribution. Thus, for a given game, $i$, 

\begin{equation}
\begin{split}
  (X_{Ai},\ X_{Bi}) & \sim BP(\lambda_{1i},\ \lambda_{2i},\ \lambda_{0i}),\\
  log(\lambda_{1i}) & = \omega_{1i}\beta_{1},\\
  log(\lambda_{2i}) & = \omega_{2i}\beta_{2},\\
  log(\lambda_{0i}) & = \omega_{0i}\beta_{0}
\end{split}
(\#eq:bipois-regress)
\end{equation}

where $\omega$ represents a matrix of independent variable, and $\beta$ denotes the regression coefficients [@karlis2003; @karlis2005]. When predicting soccer games, the independent variables are the teams that are playing, and the regression coefficients represent the offensive or defensive strength of the teams [@groll2016]. Specifically, we can model the two scores, $X$, for teams $A$ and $B$, from a given game, $i$, as

\begin{align}
  log(X_{Ai}) &= \lambda_{1i} + \lambda_{0i}, \notag \\
  log(X_{Bi}) &= \lambda_{2i} + \lambda_{0i}, \notag \\
  \lambda_{1i} &= \mu + \eta + \alpha_A + \delta_B, (\#eq:lambda1) \\
  \lambda_{2i} &= \mu + \alpha_B + \delta_A, (\#eq:lambda2) \\
  \lambda_{0i} &= \rho_A + \rho_B (\#eq:lambda0)
\end{align}

Here, $\mu$ denotes the overall intercept, or the expected log goals for a team not playing at home, and $\eta$ represents the increase in expected log goals for a team playing at home. The estimates of team ability come from $\alpha$ and $\delta$, which represent the attacking and defensive abilities of the given team respectively. These can be modeled as fixed effects, or as random effects. Finally, $\rho$ denotes the change in expected covariance for each team [@whitaker2011].

### Implementing the Bivariate Poisson {#imp-bivpois}

The bivariate Poisson model can be fit using the following Stan code [@R-rstan]. In the model code, I have modeled $\alpha$, $\delta$, and $\rho$ as random effects so that $\mu$ represents the overall mean. This means that positive $\alpha$ values and negative $\delta$ are good, as team wants the attack to add goals above the average, and the defense to result in the opponent have below average goals.

I have also reparameterized the random effects so that Stan can sample from a $\mathcal{N}(0,\ 1)$, which reduces computation time and increasing the efficiency of the sampler to avoid divergent transitions [@stan]. 


```stan
data {
  int<lower=1> num_clubs;                           // number of clubs
  int<lower=1> num_games;                           // number of games
  int<lower=1,upper=num_clubs> home[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away[num_games];     // away club for game g
  int<lower=0> h_goals[num_games];                  // home goals for game g
  int<lower=0> a_goals[num_games];                  // away goals for game g
  int<lower=0,upper=1> homeg[num_games];            // home field for game g
}
parameters {
  vector[num_clubs] raw_alpha;                  // attacking intercepts
  vector[num_clubs] raw_delta;                  // defending intercepts
  vector[num_clubs] raw_rho;                    // covariance intercepts

  real mu;                                          // fixed intercept
  real eta;                                         // homefield
  real<lower=0> sigma_a;                            // attacking sd
  real<lower=0> sigma_d;                            // defending sd
  real<lower=0> sigma_r;                            // covariance sd
}
transformed parameters {
  vector[num_clubs] alpha;
  vector[num_clubs] delta;
  vector[num_clubs] rho;
  
  alpha = raw_alpha * sigma_a;
  delta = raw_delta * sigma_d;
  rho = raw_rho * sigma_r;
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  vector[num_games] lambda3;

  // priors
  raw_alpha ~ normal(0, 1);
  raw_delta ~ normal(0, 1);
  raw_rho ~ normal(0, 1);
  mu ~ normal(0, 10);
  eta ~ normal(0, 10);
  sigma_a ~ normal(0, 10);
  sigma_d ~ normal(0, 10);
  sigma_r ~ normal(0, 10);

  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu + (eta * homeg[g]) + alpha[home[g]] + delta[away[g]]);
    lambda2[g] = exp(mu + alpha[away[g]] + delta[home[g]]);
    lambda3[g] = exp(rho[home[g]] + rho[away[g]]);
  }
  h_goals ~ poisson(lambda1 + lambda3);
  a_goals ~ poisson(lambda2 + lambda3);
}
```

## The Mixed Effects Model {#mix-eff}

test

### Implementing the Mixed Effects Model {#imp-mixef}