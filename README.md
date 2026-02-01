# PHASM
## Probabilistic hierarchical autoregressive sabermetric model

PHASM is a Bayesian projection system for MLB hitters. It combines multivariate outcome modeling,
hierarchical player/position effects, and AR(1) year trends to produce probabilistic forecasts of
per-PA rates and rate stats. The system also supports total-count projections when paired with
external PA forecasts.

## What this does
- Fits a joint multivariate Bayesian model (rstan) for H, R, RBI, HR, SB (per-PA rates) plus AVG, OBP, SLG.
- Uses age/aging curve and position.
- Player random intercepts and age slopes; position random intercepts and age/age^2 slopes.
- Year random intercepts with AR(1) evolution.

## Files
- Stan model: `models/model.stan`
- R driver: `models/fit_model.R`
- Inputs: `data/fangraphs_batters_2018_2025.csv`
- Outputs (after fitting):
  - `models/model_fit.rds`
  - `models/model_inputs.rds`
  - `results/projections/category_projections_2026.csv`

## Covariates used
- Age (standardized) and age^2

## Notes
- 2026 covariates are taken from the most recent season per player (age advanced by +1).
- Count outcomes are modeled as Poisson with a log(PA) offset; projections are per-PA rates.
- AVG/OBP use a logit transform; SLG uses log(SLG + 1e-4).
- Handedness and Statcast covariates are excluded by design.
- Seasons with PA < 100 are excluded from the dataset before fitting.

---

## Workflow

### 1) Generate the dataset
This repo pulls FanGraphs data via `baseballr` and builds the model dataset:
```sh
Rscript data/build_fangraphs_batters_from_baseballr.R
```

That script:
- Fetches 2018–2025 batting leaderboards from FanGraphs (requires internet)
- Keeps seasons with `PA >= 100`
- Keeps players with `>= 100 PA` in either 2024 or 2025
- Writes `data/fangraphs_batters_2018_2025.csv`

### 2) Fit the model (Stan)
```sh
Rscript models/fit_model.R
```

Outputs:
- `models/model_fit.rds`
- `models/model_inputs.rds`
- `results/projections/category_projections_2026.csv`

### 3) Composite projections (optional)
```sh
Rscript results/scripts/build_composite_projections.R
```

Outputs:
- `results/projections/composite_projections_2026.csv`

### 4) Top 20 composites by position (optional)
```sh
Rscript results/scripts/build_top20_composite_by_position.R
```

Outputs:
- `results/projections/top20_composite_by_position.md`

### 5) Latent fit plots (optional)
```sh
Rscript results/scripts/plot_latent_fit_top100_by_category.R
```

Outputs:
- `results/plots/latent_fit_top100_<CATEGORY>.pdf`

## Model specification

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
\eta_{n,k} = X_n \beta_k
          + \sum_{r=1}^{R_{\text{pos}}} Z^{\text{pos}}_{n,r}\,u^{\text{pos}}_{p[n],k,r}
          + \sum_{r=1}^{R_{\text{player}}} Z^{\text{player}}_{n,r}\,u^{\text{player}}_{i[n],k,r}
          + \gamma_{k, y[n]}.
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

### Notes
- Count outcomes are forecasted as rates per PA; totals require a separate PA model.
- Seasons with PA < 100 are excluded from the dataset before fitting.
