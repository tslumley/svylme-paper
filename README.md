# svylme-paper
Simulations comparing gllamm-style and pairwise-likelihood estimators for linear mixed models under complex sampling

Only covers the setting where the gllamm-style estimators work, so PSUs are the same as model clusters

`fromStata.R` has formatting code, so to replicate the tables in the preprint
```
load("lmesim-eff.rda")
round(100*cvtsum2(rval), 1)
```

Which files correspond to which tables?

| table | R code | results |
|--|--|--|
| 1 | lmesim-eff.R | lmesim-eff.rda|

