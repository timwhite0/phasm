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
  int<lower=1, upper=K> k_svhld;
  vector[N] role_leverage;
  matrix[P, K] beta_mean;
  matrix[P, K] beta_sd;
  vector<lower=0>[K] sigma_player_sd;
  vector<lower=0>[K] sigma_year_sd;
  vector<lower=0>[K_zip] beta_zip_sd;
  vector[K] rho_year_mean;
  vector<lower=0>[K] rho_year_sd;
  real beta_role_svhld_mean;
  real<lower=0> beta_role_svhld_sd;

  int<lower=1> J_player;
  int<lower=1> J_year;

  int<lower=1, upper=J_player> player_id[N];
  int<lower=1, upper=J_year> year_id[N];

  int<lower=0> y_count[N, K_count];
  vector[N] offset_log_ip;
  vector[N] stuff_obs_z;
  vector[N] location_obs_z;
  vector<lower=1>[N] plv_exposure;
  array[N] int<lower=0, upper=1> has_plv;

  int<lower=1> N_pred;
  matrix[N_pred, P] X_pred;
  matrix[N_pred, R_player] Z_player_pred;
  int<lower=1, upper=J_player> player_id_pred[N_pred];
  int<lower=1, upper=J_year + 1> year_id_pred[N_pred];
  vector[N_pred] offset_log_ip_pred;
  vector[N_pred] role_leverage_pred;
}

parameters {
  matrix[P, K] beta;
  matrix[P, K_zip] beta_zip;
  real beta_role_svhld;

  // Random effects: player (intercept, age)
  matrix[J_player, K] z_player[R_player];
  vector<lower=0>[K] sigma_player[R_player];
  cholesky_factor_corr[K] L_player[R_player];

  // Year effects with AR(1)
  matrix[K, J_year] year_effect;
  vector<lower=-1, upper=1>[K] rho_year;
  vector<lower=0>[K] sigma_year;

  // Latent Stuff+/Location+ process
  vector[3] beta_stuff_lat;
  vector[3] beta_location_lat;
  matrix[J_player, 4] z_player_plv;
  vector<lower=0>[4] sigma_player_plv;
  cholesky_factor_corr[4] L_player_plv;
  real<lower=0> sigma_stuff_obs;
  real<lower=0> sigma_location_obs;

  // Outcome effects of latent Stuff+/Location+
  vector[K] beta_stuff_out;
  vector[K] beta_location_out;
}

transformed parameters {
  matrix[J_player, K] u_player[R_player];
  matrix[J_player, 4] u_player_plv;

  for (r in 1:R_player) {
    u_player[r] = z_player[r] * diag_pre_multiply(sigma_player[r], L_player[r])';
  }
  u_player_plv = z_player_plv * diag_pre_multiply(sigma_player_plv, L_player_plv)';
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

  rho_year ~ normal(rho_year_mean, rho_year_sd);
  sigma_year ~ normal(0, sigma_year_sd);
  beta_role_svhld ~ normal(beta_role_svhld_mean, beta_role_svhld_sd);
  beta_stuff_lat ~ normal(0, 1);
  beta_location_lat ~ normal(0, 1);
  to_vector(z_player_plv) ~ normal(0, 1);
  sigma_player_plv ~ normal(0, 1);
  L_player_plv ~ lkj_corr_cholesky(2);
  sigma_stuff_obs ~ normal(0, 1);
  sigma_location_obs ~ normal(0, 1);
  beta_stuff_out ~ normal(0, 0.5);
  beta_location_out ~ normal(0, 0.5);

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
    real age_c;
    real age2;
    real stuff_lat;
    real location_lat;

    age_c = X[n, 2];
    age2 = X[n, 3];

    stuff_lat = beta_stuff_lat[1] + beta_stuff_lat[2] * age_c + beta_stuff_lat[3] * age2
      + u_player_plv[player_id[n], 1]
      + age_c * u_player_plv[player_id[n], 2];

    location_lat = beta_location_lat[1] + beta_location_lat[2] * age_c + beta_location_lat[3] * age2
      + u_player_plv[player_id[n], 3]
      + age_c * u_player_plv[player_id[n], 4];

    if (has_plv[n] == 1) {
      stuff_obs_z[n] ~ normal(stuff_lat, sigma_stuff_obs / sqrt(plv_exposure[n]));
      location_obs_z[n] ~ normal(location_lat, sigma_location_obs / sqrt(plv_exposure[n]));
    }

    eta = (X[n] * beta)';

    for (r in 1:R_player) {
      eta += Z_player[n, r] * u_player[r][player_id[n]]';
    }

    eta += year_effect[, year_id[n]];
    eta += beta_stuff_out * stuff_lat;
    eta += beta_location_out * location_lat;
    eta[k_svhld] += role_leverage[n] * beta_role_svhld;

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
    real age_c;
    real age2;
    real stuff_lat;
    real location_lat;

    age_c = X_pred[n, 2];
    age2 = X_pred[n, 3];

    stuff_lat = beta_stuff_lat[1] + beta_stuff_lat[2] * age_c + beta_stuff_lat[3] * age2
      + u_player_plv[player_id_pred[n], 1]
      + age_c * u_player_plv[player_id_pred[n], 2];

    location_lat = beta_location_lat[1] + beta_location_lat[2] * age_c + beta_location_lat[3] * age2
      + u_player_plv[player_id_pred[n], 3]
      + age_c * u_player_plv[player_id_pred[n], 4];

    eta = (X_pred[n] * beta)';

    for (r in 1:R_player) {
      eta += Z_player_pred[n, r] * u_player[r][player_id_pred[n]]';
    }

    if (year_id_pred[n] <= J_year) {
      eta += year_effect[, year_id_pred[n]];
    } else {
      eta += year_effect_2026;
    }
    eta += beta_stuff_out * stuff_lat;
    eta += beta_location_out * location_lat;
    eta[k_svhld] += role_leverage_pred[n] * beta_role_svhld;

    eta_pred[n] = eta';
  }
}
