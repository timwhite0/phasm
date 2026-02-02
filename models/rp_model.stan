data {
  int<lower=1> N;
  int<lower=1> K;                 // total outcomes (8)
  int<lower=1> K_count;           // 8
  int<lower=0> K_zip;             // 0+ (SV, HLD, QS when used)
  int<lower=1> P;                 // fixed effects
  int<lower=1> R_player;          // player RE predictors (intercept, age)
  matrix[N, P] X;
  matrix[N, R_player] Z_player;
  int<lower=1, upper=K_count> zip_idx[K_zip];
  matrix[P, K] beta_mean;
  matrix[P, K] beta_sd;
  vector<lower=0>[K] sigma_player_sd;
  vector<lower=0>[K] sigma_year_sd;
  vector<lower=0>[K_zip] beta_zip_sd;

  int<lower=1> J_player;
  int<lower=1> J_year;

  int<lower=1, upper=J_player> player_id[N];
  int<lower=1, upper=J_year> year_id[N];

  int<lower=0> y_count[N, K_count];
  vector[N] offset_log_ip;

  int<lower=1> N_pred;
  matrix[N_pred, P] X_pred;
  matrix[N_pred, R_player] Z_player_pred;
  int<lower=1, upper=J_player> player_id_pred[N_pred];
  int<lower=1, upper=J_year + 1> year_id_pred[N_pred];
  vector[N_pred] offset_log_ip_pred;
}

parameters {
  matrix[P, K] beta;
  matrix[P, K_zip] beta_zip;

  // Random effects: player (intercept, age)
  matrix[J_player, K] z_player[R_player];
  vector<lower=0>[K] sigma_player[R_player];
  cholesky_factor_corr[K] L_player[R_player];

  // Year effects with AR(1)
  matrix[K, J_year] year_effect;
  vector<lower=-1, upper=1>[K] rho_year;
  vector<lower=0>[K] sigma_year;
}

transformed parameters {
  matrix[J_player, K] u_player[R_player];

  for (r in 1:R_player) {
    u_player[r] = z_player[r] * diag_pre_multiply(sigma_player[r], L_player[r])';
  }
}

model {
  // Priors
  for (k in 1:K) {
    for (p in 1:P) {
      beta[p, k] ~ normal(beta_mean[p, k], beta_sd[p, k]);
    }
  }
  for (j in 1:K_zip) {
    beta_zip[, j] ~ normal(0, beta_zip_sd[j]);
  }

  for (r in 1:R_player) {
    to_vector(z_player[r]) ~ normal(0, 2.5);
    sigma_player[r] ~ normal(0, sigma_player_sd);
    L_player[r] ~ lkj_corr_cholesky(2);
  }

  rho_year ~ normal(0, 0.5);
  sigma_year ~ normal(0, sigma_year_sd);

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

    for (r in 1:R_player) {
      eta += Z_player[n, r] * u_player[r][player_id[n]]';
    }

    eta += year_effect[, year_id[n]];

    for (k in 1:K_count) {
      int handled;
      handled = 0;
      for (j in 1:K_zip) {
        if (k == zip_idx[j]) {
          real logit_pi;
          real log_pi;
          real log1m_pi;
          logit_pi = dot_product(X[n], beta_zip[, j]);
          log_pi = -log1p_exp(-logit_pi);
          log1m_pi = -log1p_exp(logit_pi);
          if (y_count[n, k] == 0) {
            target += log_sum_exp(log_pi, log1m_pi + poisson_log_lpmf(0 | eta[k] + offset_log_ip[n]));
          } else {
            target += log1m_pi + poisson_log_lpmf(y_count[n, k] | eta[k] + offset_log_ip[n]);
          }
          handled = 1;
        }
      }
      if (handled == 0) {
        y_count[n, k] ~ poisson_log(eta[k] + offset_log_ip[n]);
      }
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
