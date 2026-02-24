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
  matrix[P, K] beta_mean;
  matrix[P, K] beta_sd;
  vector<lower=0>[K] sigma_player_sd;
  vector<lower=0>[K] sigma_pos_sd;
  vector<lower=0>[K] sigma_year_sd;
  vector[K] rho_year_mean;
  vector<lower=0>[K] rho_year_sd;
  vector<lower=0>[K_cont] sigma_cont_sd;
  vector[3] beta_ev_lat_mean;
  vector<lower=0>[3] beta_ev_lat_sd;
  vector[3] beta_la_lat_mean;
  vector<lower=0>[3] beta_la_lat_sd;
  vector[3] beta_barrel_lat_mean;
  vector<lower=0>[3] beta_barrel_lat_sd;
  vector[3] beta_hardhit_lat_mean;
  vector<lower=0>[3] beta_hardhit_lat_sd;
  vector<lower=0>[4] sigma_player_statcast_sd;
  vector<lower=0>[4] sigma_player_bbe_sd;
  real<lower=0> sigma_ev_obs_sd;
  real<lower=0> sigma_la_obs_sd;
  real<lower=0> sigma_barrel_obs_sd;
  real<lower=0> sigma_hardhit_obs_sd;
  vector[7] beta_ev_out_mean;
  vector<lower=0>[7] beta_ev_out_sd;
  vector[7] beta_la_out_mean;
  vector<lower=0>[7] beta_la_out_sd;
  vector[7] beta_barrel_out_mean;
  vector<lower=0>[7] beta_barrel_out_sd;
  vector[7] beta_hardhit_out_mean;
  vector<lower=0>[7] beta_hardhit_out_sd;
  vector<lower=0>[K_count] phi_count_mean;
  vector<lower=0>[K_count] phi_count_sd;

  int<lower=1> J_player;
  int<lower=1> J_pos;
  int<lower=1> J_year;

  int<lower=1, upper=J_player> player_id[N];
  int<lower=1, upper=J_pos> pos_id[N];
  int<lower=1, upper=J_year> year_id[N];

  int<lower=0> y_count[N, K_count];
  vector[N] offset_log_pa;
  matrix[N, K_cont] y_cont;

  vector[N] ev_obs_z;
  vector[N] la_obs_z;
  vector[N] barrel_obs_logit;
  vector[N] hardhit_obs_logit;
  vector<lower=1>[N] events_bb;

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

  // Latent EV/LA process
  vector[3] beta_ev_lat;
  vector[3] beta_la_lat;
  vector[3] beta_barrel_lat;
  vector[3] beta_hardhit_lat;
  matrix[J_player, 4] z_player_statcast;
  vector<lower=0>[4] sigma_player_statcast;
  cholesky_factor_corr[4] L_player_statcast;
  matrix[J_player, 4] z_player_bbe;
  vector<lower=0>[4] sigma_player_bbe;
  cholesky_factor_corr[4] L_player_bbe;
  real<lower=0> sigma_ev_obs;
  real<lower=0> sigma_la_obs;
  real<lower=0> sigma_barrel_obs;
  real<lower=0> sigma_hardhit_obs;

  // Outcome effects of latent EV/LA for non-SB outcomes:
  // k = 1,2,3,4,6,7,8  -> 7 coefficients per latent metric
  vector[7] beta_ev_out;
  vector[7] beta_la_out;
  vector[7] beta_barrel_out;
  vector[7] beta_hardhit_out;
  vector<lower=0>[K_count] phi_count;
}

transformed parameters {
  matrix[J_player, K] u_player[R_player];
  matrix[J_pos, K] u_pos[R];
  matrix[J_player, 4] u_player_statcast;
  matrix[J_player, 4] u_player_bbe;

  for (r in 1:R_player) {
    u_player[r] = z_player[r] * diag_pre_multiply(sigma_player[r], L_player[r])';
  }
  for (r in 1:R) {
    u_pos[r] = z_pos[r] * diag_pre_multiply(sigma_pos[r], L_pos[r])';
  }

  u_player_statcast = z_player_statcast * diag_pre_multiply(sigma_player_statcast, L_player_statcast)';
  u_player_bbe = z_player_bbe * diag_pre_multiply(sigma_player_bbe, L_player_bbe)';
}

model {
  // Priors
  for (k in 1:K) {
    for (p in 1:P) {
      beta[p, k] ~ normal(beta_mean[p, k], beta_sd[p, k]);
    }
  }

  for (r in 1:R_player) {
    to_vector(z_player[r]) ~ normal(0, 2.5);
    sigma_player[r] ~ normal(0, sigma_player_sd);
    L_player[r] ~ lkj_corr_cholesky(2);
  }

  for (r in 1:R) {
    to_vector(z_pos[r]) ~ normal(0, 2.5);
    sigma_pos[r] ~ normal(0, sigma_pos_sd);
    L_pos[r] ~ lkj_corr_cholesky(2);
  }

  rho_year ~ normal(rho_year_mean, rho_year_sd);
  sigma_year ~ normal(0, sigma_year_sd);
  sigma_cont ~ normal(0, sigma_cont_sd);

  beta_ev_lat ~ normal(beta_ev_lat_mean, beta_ev_lat_sd);
  beta_la_lat ~ normal(beta_la_lat_mean, beta_la_lat_sd);
  beta_barrel_lat ~ normal(beta_barrel_lat_mean, beta_barrel_lat_sd);
  beta_hardhit_lat ~ normal(beta_hardhit_lat_mean, beta_hardhit_lat_sd);
  to_vector(z_player_statcast) ~ normal(0, 1);
  sigma_player_statcast ~ normal(0, sigma_player_statcast_sd);
  L_player_statcast ~ lkj_corr_cholesky(2);
  to_vector(z_player_bbe) ~ normal(0, 1);
  sigma_player_bbe ~ normal(0, sigma_player_bbe_sd);
  L_player_bbe ~ lkj_corr_cholesky(2);
  sigma_ev_obs ~ normal(0, sigma_ev_obs_sd);
  sigma_la_obs ~ normal(0, sigma_la_obs_sd);
  sigma_barrel_obs ~ normal(0, sigma_barrel_obs_sd);
  sigma_hardhit_obs ~ normal(0, sigma_hardhit_obs_sd);
  beta_ev_out ~ normal(beta_ev_out_mean, beta_ev_out_sd);
  beta_la_out ~ normal(beta_la_out_mean, beta_la_out_sd);
  beta_barrel_out ~ normal(beta_barrel_out_mean, beta_barrel_out_sd);
  beta_hardhit_out ~ normal(beta_hardhit_out_mean, beta_hardhit_out_sd);
  phi_count ~ normal(phi_count_mean, phi_count_sd);

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
    real ev_lat;
    real la_lat;
    real barrel_lat;
    real hardhit_lat;

    age_c = X[n, 2];
    age2 = X[n, 3];

    ev_lat = beta_ev_lat[1] + beta_ev_lat[2] * age_c + beta_ev_lat[3] * age2
      + u_player_statcast[player_id[n], 1]
      + age_c * u_player_statcast[player_id[n], 2];

    la_lat = beta_la_lat[1] + beta_la_lat[2] * age_c + beta_la_lat[3] * age2
      + u_player_statcast[player_id[n], 3]
      + age_c * u_player_statcast[player_id[n], 4];

    barrel_lat = beta_barrel_lat[1] + beta_barrel_lat[2] * age_c + beta_barrel_lat[3] * age2
      + u_player_bbe[player_id[n], 1]
      + age_c * u_player_bbe[player_id[n], 2];

    hardhit_lat = beta_hardhit_lat[1] + beta_hardhit_lat[2] * age_c + beta_hardhit_lat[3] * age2
      + u_player_bbe[player_id[n], 3]
      + age_c * u_player_bbe[player_id[n], 4];

    ev_obs_z[n] ~ normal(ev_lat, sigma_ev_obs / sqrt(events_bb[n]));
    la_obs_z[n] ~ normal(la_lat, sigma_la_obs / sqrt(events_bb[n]));
    barrel_obs_logit[n] ~ normal(barrel_lat, sigma_barrel_obs / sqrt(events_bb[n]));
    hardhit_obs_logit[n] ~ normal(hardhit_lat, sigma_hardhit_obs / sqrt(events_bb[n]));

    eta = (X[n] * beta)';

    for (r in 1:R) {
      eta += Z[n, r] * u_pos[r][pos_id[n]]';
    }
    for (r in 1:R_player) {
      eta += Z_player[n, r] * u_player[r][player_id[n]]';
    }

    eta += year_effect[, year_id[n]];

    // Add latent EV/LA terms to non-SB outcomes only.
    for (k in 1:K) {
      int idx;
      if (k != 5) {
        idx = k;
        if (k > 5) idx = k - 1;
        eta[k] += beta_ev_out[idx] * ev_lat +
          beta_la_out[idx] * la_lat +
          beta_barrel_out[idx] * barrel_lat +
          beta_hardhit_out[idx] * hardhit_lat;
      }
    }

    for (k in 1:K_count) {
      y_count[n, k] ~ neg_binomial_2_log(eta[k] + offset_log_pa[n], phi_count[k]);
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
    real age_c;
    real age2;
    real ev_lat;
    real la_lat;
    real barrel_lat;
    real hardhit_lat;

    age_c = X_pred[n, 2];
    age2 = X_pred[n, 3];

    ev_lat = beta_ev_lat[1] + beta_ev_lat[2] * age_c + beta_ev_lat[3] * age2
      + u_player_statcast[player_id_pred[n], 1]
      + age_c * u_player_statcast[player_id_pred[n], 2];

    la_lat = beta_la_lat[1] + beta_la_lat[2] * age_c + beta_la_lat[3] * age2
      + u_player_statcast[player_id_pred[n], 3]
      + age_c * u_player_statcast[player_id_pred[n], 4];

    barrel_lat = beta_barrel_lat[1] + beta_barrel_lat[2] * age_c + beta_barrel_lat[3] * age2
      + u_player_bbe[player_id_pred[n], 1]
      + age_c * u_player_bbe[player_id_pred[n], 2];

    hardhit_lat = beta_hardhit_lat[1] + beta_hardhit_lat[2] * age_c + beta_hardhit_lat[3] * age2
      + u_player_bbe[player_id_pred[n], 3]
      + age_c * u_player_bbe[player_id_pred[n], 4];

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

    for (k in 1:K) {
      int idx;
      if (k != 5) {
        idx = k;
        if (k > 5) idx = k - 1;
        eta[k] += beta_ev_out[idx] * ev_lat +
          beta_la_out[idx] * la_lat +
          beta_barrel_out[idx] * barrel_lat +
          beta_hardhit_out[idx] * hardhit_lat;
      }
    }

    eta_pred[n] = eta';
  }
}
