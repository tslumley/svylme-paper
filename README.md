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
| 2 | lmesim-eff1.R | lmesim-eff1.rda|
| 3 | lmesim-eff-2.R | lmesim-eff-2.rda|
| 4 | lmesim-eff3.R | lmesim-eff3.rda|
| 5 | lmesim-eff-4.R | lmesim-eff-4.rda|
| 6 | lmesim-small.R | lmesim-small.rda|
|--|---|---|
|7 | lmesim-info-1.R | lmesim-inf1.rda|
|8 | lmesim-info-2.R | lmesim-inf-2.rda|
|9 | lmesim-info-3.R | lmesim-inf-3.rda|
|--|--|--|

.

