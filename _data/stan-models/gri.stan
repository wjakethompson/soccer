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
  vector[num_clubs] raw_alpha;                      // attacking intercepts
  vector[num_clubs] raw_delta;                      // defending intercepts
  vector[num_games] raw_gamma;                      // game intercepts

  real mu;                                          // fixed intercept
  real eta;                                         // homefield
  real<lower=0> sigma_a;                            // attacking sd
  real<lower=0> sigma_d;                            // defending sd
  real<lower=0> sigma_g;                            // game sd
}
transformed parameters {
  vector[num_clubs] alpha;
  vector[num_clubs] delta;
  vector[num_games] gamma;

  alpha = sigma_a * raw_alpha;
  delta = sigma_d * raw_delta;
  gamma = sigma_g * raw_gamma;
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;

  // priors
  raw_alpha ~ normal(0, 1);                         // attacking random effects
  raw_delta ~ normal(0, 1);                         // defending random effects
  raw_gamma ~ normal(0, 1);                         // game random effects
  mu ~ normal(0, 10);
  eta ~ normal(0, 10);
  sigma_a ~ normal(0, 10);
  sigma_d ~ normal(0, 10);
  sigma_g ~ normal(0, 10);

  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu + (eta * homeg[g]) + alpha[home[g]] + delta[away[g]] + gamma[g]);
    lambda2[g] = exp(mu + alpha[away[g]] + delta[home[g]] + gamma[g]);
  }
  h_goals ~ poisson(lambda1);
  a_goals ~ poisson(lambda2);
}
