# terras
Modeling the social drivers of environmental sustainability among Amazonian indigenous lands using Bayesian networks

## Paper title
Scripts are for the manuscript titled 'Modeling the social drivers of environmental sustainability among Amazonian indigenous lands using Bayesian networks' by Walker RS and J Paige. 

## Data 
Data are from Indigenous Lands Territorial Consolidation Indicators System available at https://terrasmais.eco.br/v1/ and added to the project (terrasmais-2017.csv).

## Phase 1 R package bnlearn
bnlearn (bnlearn.com) is an `R` package for learning the graphical structure of Bayesian networks, estimating their parameters and performing some useful inference. We use the boostrap feature for understanding causal structures in the WNAI data.

## Phase 1 structure learning example: bootstrap multiple network structures
```splus
library(bnlearn)
boot <- boot.strength(df[,c(2:8)], R = 10001, cpdag = FALSE,
                      algorithm = "tabu")
boot[boot$strength > 0.5 & boot$direction >= 0.5, ]
avg.boot <- averaged.network(boot, threshold = .8)
plot(boot)
avg.boot

strength.plot(avg.boot, boot, threshold=.8,
              layout = "dot",
              shape = "rectangle")
```
## Phase 2 R package brms
brms (paul-buerkner.github.io/brms/) is an `R` package that provides an interface to fit Bayesian generalized (non-)linear multivariate multilevel models using Stan. We use it to estimate the regression coefficients of the edges in our network while adjusting for spatial autocorrelation using a Gaussian process.

## Phase 2 parameter learning example: bayesian path model with brms
```splus
library(brms)
mod1 <- bf(Environmental.integrity  ~ 1 + Legal.stability + Environmental.integrity.in.the.buffer.zone + s(lat, long))
mod2 <- bf(Governance ~ 1 + Legal.stability + s(lat, long) )
mod3 <- bf(Absence.of.threats.due.to.infrastructure.projects ~ 1 + Territorial.integrity + s(lat, long))
mod4 <- bf(Environmental.integrity.in.the.buffer.zone ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long))
mod5 <- bf(Absence.of.pressure.from.infrastructure.projects ~ 1 + Absence.of.threats.due.to.infrastructure.projects + s(lat, long))
mv <- mvbf( mod1 + mod2 + mod3 + mod4 + mod5,
            rescor=FALSE )
m1 <- brm(data = df, 
         mv, prior = set_prior("normal(0,.1)", class = "b"), # group = ""),
         iter =  1e4, chains = 4, cores = 4, #save_all_pars = TRUE,
         control = list(adapt_delta = .999, max_treedepth = 20),
         seed = 1, backend = "cmdstanr")
prior_summary(m1)
m1
pl <- plot(m1, N = 4, ask = FALSE)
posterior_summary(m1)
bayes_R2(m1) # 
conditional_effects(m1, points=T)
plot(hypothesis(m1, "Governance_Legal.stability > 0"))
```
