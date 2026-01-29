data {
  int<lower=1> N;
  int<lower=1> K;                 // total outcomes (8)
  int<lower=1> K_count;           // 5
  int<lower=1> K_cont;            // 3
  int<lower=1> P;                 // fixed effects
  int<lower=1> R;                 // random effect predictors (intercept, age, age2)
  int<lower=1> R_player;          // player RE predictors (intercept, age)
  matrix[N, P] X;
  matrix[N, R] Z;
  matrix[N, R_player] Z_player;

  int<lower=1> J_player;
  int<lower=1> J_pos;
  int<lower=1> J_year;

  int<lower=1, upper=J_player> player_id[N];
  int<lower=1, upper=J_pos> pos_id[N];
  int<lower=1, upper=J_year> year_id[N];

  int<lower=0> y_count[N, K_count];
  vector[N] offset_log_pa;
  matrix[N, K_cont] y_cont;

  int<lower=1> N_pred;
  matrix[N_pred, P] X_pred;
  matrix[N_pred, R] Z_pred;
  matrix[N_pred, R_player] Z_player_pred;
  int<lower=1, upper=J_player> player_id_pred[N_pred];
  int<lower=1, upper=J_pos> pos_id_pred[N_pred];
  int<lower=1, upper=J_year + 1> year_id_pred[N_pred];
  vector[N_pred] offset_log_pa_pred;
}

parameters {
  matrix[P, K] beta;

  // Random effects: player (intercept, age)
  matrix[J_player, K] z_player[R_player];
  vector<lower=0>[K] sigma_player[R_player];
  cholesky_factor_corr[K] L_player[R_player];

  matrix[J_pos, K] z_pos[R];
  vector<lower=0>[K] sigma_pos[R];
  cholesky_factor_corr[K] L_pos[R];

  // Year effects with AR(1)
  matrix[K, J_year] year_effect;
  vector<lower=-1, upper=1>[K] rho_year;
  vector<lower=0>[K] sigma_year;

  // Continuous outcome noise
  vector<lower=0>[K_cont] sigma_cont;
}

transformed parameters {
  matrix[J_player, K] u_player[R_player];
  matrix[J_pos, K] u_pos[R];

  for (r in 1:R_player) {
    u_player[r] = z_player[r] * diag_pre_multiply(sigma_player[r], L_player[r])';
  }
  for (r in 1:R) {
    u_pos[r]    = z_pos[r]    * diag_pre_multiply(sigma_pos[r],    L_pos[r])';
  }
}

model {
  // Priors
  to_vector(beta) ~ normal(0, 2.5);

  for (r in 1:R_player) {
    to_vector(z_player[r]) ~ normal(0, 2.5);
    sigma_player[r] ~ normal(0, 1);
    L_player[r] ~ lkj_corr_cholesky(2);
  }

  for (r in 1:R) {
    to_vector(z_pos[r]) ~ normal(0, 2.5);

    sigma_pos[r] ~ normal(0, 1);

    L_pos[r] ~ lkj_corr_cholesky(2);
  }

  rho_year ~ normal(0, 0.5);
  sigma_year ~ normal(0, 1);
  sigma_cont ~ normal(0, 1);

  // AR(1) year effects
  for (k in 1:K) {
    year_effect[k, 1] ~ normal(0, sigma_year[k] / sqrt(1 - square(rho_year[k])));
    for (t in 2:J_year) {
      year_effect[k, t] ~ normal(rho_year[k] * year_effect[k, t - 1], sigma_year[k]);
    }
  }

  // Likelihood
  for (n in 1:N) {
    vector[K] eta;
    eta = (X[n] * beta)';

    for (r in 1:R) {
      eta += Z[n, r] * u_pos[r][pos_id[n]]';
    }
    for (r in 1:R_player) {
      eta += Z_player[n, r] * u_player[r][player_id[n]]';
    }

    eta += year_effect[, year_id[n]];

    for (k in 1:K_count) {
      y_count[n, k] ~ poisson_log(eta[k] + offset_log_pa[n]);
    }

    for (k in 1:K_cont) {
      y_cont[n, k] ~ normal(eta[K_count + k], sigma_cont[k]);
    }
  }
}

generated quantities {
  // Predict 2026 year effect
  vector[K] year_effect_2026;
  for (k in 1:K) {
    year_effect_2026[k] = normal_rng(rho_year[k] * year_effect[k, J_year], sigma_year[k]);
  }

  matrix[N_pred, K] eta_pred;
  for (n in 1:N_pred) {
    vector[K] eta;
    eta = (X_pred[n] * beta)';

    for (r in 1:R) {
      eta += Z_pred[n, r] * u_pos[r][pos_id_pred[n]]';
    }
    for (r in 1:R_player) {
      eta += Z_player_pred[n, r] * u_player[r][player_id_pred[n]]';
    }

    if (year_id_pred[n] <= J_year) {
      eta += year_effect[, year_id_pred[n]];
    } else {
      eta += year_effect_2026;
    }

    eta_pred[n] = eta';
  }
}
