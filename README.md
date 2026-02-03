# PHASM
## Probabilistic hierarchical autoregressive sabermetric model

Live dashboard: https://twhit.shinyapps.io/phasm/

PHASM is a Bayesian projection system for MLB hitters and pitchers. It combines multivariate outcome
modeling, hierarchical player/position effects, and AR(1) year trends to produce
probabilistic forecasts of per-PA/per-IP rates and rate stats. The system also supports total-count
projections when paired with external PA/IP forecasts.
Category projection outputs include posterior mean and quantiles (p05, p50, p95). Count outcomes are also
stored as totals using ATC PA/IP (e.g., H_mean_t, Ks_mean, W_mean_t, SVHLD_mean_t).

## What this does
- Fits a joint multivariate Bayesian model (rstan) for H, R, RBI, HR, SB (per-PA rates) plus AVG, OBP, SLG.
- Fits a joint multivariate Bayesian model (rstan) for SP: SO, BB, H, ER, W, QS (per-IP rates).
- Fits a joint multivariate Bayesian model (rstan) for RP: SO, BB, H, ER, W, SVHLD (per-IP rates).
- Uses age/aging curve for hitters and pitchers; position effects for hitters; role leverage for RP SVHLD.
- Player random intercepts and age slopes; position random intercepts and age/age^2 slopes.
- Year random intercepts with AR(1) evolution.

## Files
- Hitter Stan model: `models/model.stan`
- Hitter R driver: `models/fit_model.R`
- SP Stan model: `models/sp_model.stan`
- SP R driver: `models/fit_sp_model.R`
- RP Stan model: `models/rp_model.stan`
- RP R driver: `models/fit_rp_model.R`
- Hitter inputs: `data/fangraphs_batters_2018_2025.csv`
- Pitcher inputs: `data/fangraphs_pitchers_2018_2025.csv`
- Hitter outputs (after fitting):
  - `models/model_fit.rds`
  - `models/model_inputs.rds`
  - `results/projections/batters/category_projections_2026.csv`
- Hitter projection refresh (no refit):
  - `results/scripts/build_batter_category_projections_from_fit.R`
- SP outputs (after fitting):
  - `models/sp_model_fit.rds`
  - `models/sp_model_inputs.rds`
  - `results/projections/pitchers/sp_category_projections_2026.csv`
- SP projection refresh (no refit):
  - `results/scripts/build_sp_category_projections_from_fit.R`
- RP outputs (after fitting):
  - `models/rp_model_fit.rds`
  - `models/rp_model_inputs.rds`
  - `results/projections/pitchers/rp_category_projections_2026.csv`
- RP projection refresh (no refit):
  - `results/scripts/build_rp_category_projections_from_fit.R`

## Plots
- Fitted outcome curves:
  - Hitters: all modeled outcomes.
  - Starters: W and QS (per-IP), plus derived ERA/K/9/BB/9/WHIP.
  - Relievers: W and SVHLD (per-IP), plus derived ERA/K/9/BB/9/WHIP.
- Interval projections:
  - Hitters by position; starters/relievers by role.

## Covariates used
- Age (standardized) and age^2
- Hitter position indicators
- RP `role_leverage` indicator for SVHLD (fixed effect)

## SP model notes
- Current SP model is SP-only (2018–2025) and models SO, BB, H, ER, W, and QS as per-IP rates.
- SP outcomes are modeled as Poisson with a log(IP) offset.
- Role effects and SV/HLD are omitted in the current SP-only run.

## RP model notes
- RP model uses 2018–2025 relievers only and models SO, BB, H, ER, W, and SVHLD as per-IP rates.
- RP outcomes are modeled as Poisson with a log(IP) offset.
- RP fit input excludes any pitcher with ATC-projected GS >= 1 (from `data/atc_ip_projections_2026.csv`).
- RP uses a binary `role_leverage` covariate for SVHLD only:
  - Training years: top 3 in SVHLD on each team-season are flagged. For `Team == "2 Tms"`, top 5 are flagged; `3+ Tms` are excluded. Anyone with SVHLD >= 10 or SVHLD/IP >= 0.3 is also flagged.
  - 2026: players listed as Closer/Co-Closer/Closer Committee/Setup Man on the Fangraphs closer depth chart are flagged and cached to `data/closer_depth_chart_2026.csv`.

## Model specification
The sections below describe the hitter model. The pitcher models use the same backbone but
use IP instead of PA for the offset, and model:
- SP: SO, BB, H, ER, W, QS
- RP: SO, BB, H, ER, W, SVHLD (plus `role_leverage` for SVHLD)

## Dashboard
- Shiny app lives in `dashboard/app.R`.
- Run from repo root:
  - `Rscript -e "shiny::runApp('dashboard', port = 3838, launch.browser = TRUE)"`

### Notation
- Players $i = 1..I$, positions $p = 1..P$, years $y = 1..Y$
- Outcomes $k = 1..8$, ordered: $(H, R, RBI, HR, SB, AVG, OBP, SLG)$
- Count outcomes: $k = 1..5$; continuous outcomes: $k = 6..8$
- Observations indexed by $n = 1..N$, each with player $i[n]$, position $p[n]$, year $y[n]$

### Data and transforms
- $PA_n$: plate appearances for observation $n$
- Count outcomes: $y_{n,k}$ for $k=1..5$
- Continuous outcomes:
  - $AVG_n, OBP_n \in (0,1)$ with logit transform
  - $SLG_n > 0$ with log transform
- Transforms:
  - $a_n = \text{logit}(AVG_n)$
  - $o_n = \text{logit}(OBP_n)$
  - $s_n = \log(SLG_n + \varepsilon)$

### Design matrices
- $X_n$: fixed effects row (intercept, age, age$^2$)
- $Z^{\text{pos}}_n$: position random effect predictors (intercept, age, age$^2$)
- $Z^{\text{player}}_n$: player random effect predictors (intercept, age)

### Linear predictors (for each outcome k)

$$
\eta_{n,k} = X_n \beta_k + \sum_{r=1}^{R_{\text{pos}}} Z^{\text{pos}}_{n,r}\,u^{\text{pos}}_{p[n],k,r} + \sum_{r=1}^{R_{\text{player}}} Z^{\text{player}}_{n,r}\,u^{\text{player}}_{i[n],k,r} + \gamma_{k, y[n]}.
$$

### Likelihood
- Count outcomes (per-PA rates via log offset):

$$
 y_{n,k} \sim \text{Poisson}\bigl(\exp(\eta_{n,k}) \cdot PA_n\bigr), \quad k=1..5
$$

equivalently:

$$
 y_{n,k} \sim \text{logPoisson}(\eta_{n,k} + \log(PA_n)).
$$

- Continuous outcomes:

$$
 a_n \sim \mathcal{N}(\eta_{n,6}, \sigma_6), \quad
 o_n \sim \mathcal{N}(\eta_{n,7}, \sigma_7), \quad
 s_n \sim \mathcal{N}(\eta_{n,8}, \sigma_8).
$$

### Random effects
- Player random effects use $\{\text{intercept}, \text{age}\}$:

$$
 u^{\text{player}}_{i,*,r} \sim \mathcal{MVN}(0, \Sigma^{\text{player}}_r).
$$

- Position random effects use $\{\text{intercept}, \text{age}, \text{age}^2\}$:

$$
 u^{\text{pos}}_{p,*,r} \sim \mathcal{MVN}(0, \Sigma^{\text{pos}}_r).
$$

- Each $\Sigma^{\text{group}}_r$ is constructed from scale vector $\sigma^{\text{group}}_r$ and correlation matrix $\Omega^{\text{group}}_r$:

$$
 \Sigma^{\text{group}}_r = \text{diag}(\sigma^{\text{group}}_r)\, \Omega^{\text{group}}_r\, \text{diag}(\sigma^{\text{group}}_r).
$$

### Year effects (AR(1))
- For each outcome $k$:

$$
 \gamma_{k,1} \sim \mathcal{N}\Bigl(0, \frac{\sigma_{\text{year},k}}{\sqrt{1-\rho_k^2}}\Bigr), \quad
 \gamma_{k,y} \sim \mathcal{N}(\rho_k \gamma_{k,y-1}, \sigma_{\text{year},k}),\; y=2..Y.
$$

### 2026 projection
- Draw $\gamma_{k,Y+1} \sim \mathcal{N}(\rho_k\gamma_{k,Y}, \sigma_{\text{year},k})$
- Predict $\eta_{n,k}$ for 2026 using age and age$^2$ (with age incremented by +1 from the most recent season), plus the drawn 2026 year effect

### Priors (aligned with Stan prior recommendations)
- Fixed effects (standardized predictors): $\beta_k \sim \mathcal{N}(0, 2.5^2)$
- Random effect scales (half-normal): $\sigma^{\text{player}}_r, \sigma^{\text{pos}}_r \sim \mathcal{N}^+(0, 1)$
- Non-centered random effects: $z^{\text{player}}_r, z^{\text{pos}}_r \sim \mathcal{N}(0, 2.5^2)$
- Correlations: $\Omega^{\text{group}}_r \sim \text{LKJ}(2)$
- Year AR(1) parameters: $\rho_k \sim \mathcal{N}(0, 0.5)$, $\sigma_{\text{year},k} \sim \mathcal{N}^+(0, 1)$
- Continuous outcome noise: $\sigma_k \sim \mathcal{N}^+(0, 1)$
