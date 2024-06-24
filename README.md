# MStDCS Analysis Scripts & behavioral paradigm (modified SDMT)

This repository contains the scripts used to analyze the data from the MStDCS study. The scripts are written in Stan and R.

## Analysis Scripts

1. Behavioral data analysis of the primary outcomes:

- Task accuracy: `bayes_acc.R`
- Response times: `bayes_rt.R`

2. Secondary outcomes:

- Blinding effectiveness: `blinding.R`
- Adverse effects: `adverse_effects.R`
- Effects on affect (measured with positive and negative affect schedule, i.e., PANAS): `panas.R`

3. Scripts to generate the descriptive statistics:

- `descriptive_stats.R`

4. Helper scripts that are used in the main scripts:

- `util.R`
- `bayes_utils.R`

5. Custom _brms_ families:

- A implementation of a hurdle Gaussian distribution to use in the analysis of PANAS data: `custom_brms/hurdle_gaussian.R`

## Running the scripts

We cannot share the raw data due to data protection regulations. However, we provide the scripts to run the analyses, and the fits of the models. So, while you cannot run the scripts on the raw data, you can run the scripts on the fits that we provide.

## modified SDMT

The modified SDMT is a modification of the Symbol Digit Modalities Test (SDMT) that is used to assess information processing speed. The modified SDMT is a computerized version of the SDMT that is used in the MStDCS study. The task requires participants to match symbols to digits as quickly as possible. The primary outcomes of the MStDCS study are based on the performance of participants on the modified SDMT.

The modified SDMT is implemented using NBS Presentation (R) software. The scripts necessary to run the task are provided in the `mSDMT` directory with
a separate [README](./mSDMT/README.md) file.

## R Session Info

```{r}
R version 4.4.0 (2024-04-24)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 22.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C

time zone: Europe/Berlin
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods
[7] base

other attached packages:
 [1] patchwork_1.2.0 ggh4x_0.2.8     readxl_1.4.2    tidybayes_3.0.6
 [5] lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.2
 [9] purrr_1.0.1     readr_2.1.4     tidyr_1.3.0     tibble_3.2.1
[13] ggplot2_3.5.1   tidyverse_2.0.0 brms_2.21.0     Rcpp_1.0.12

loaded via a namespace (and not attached):
 [1] gtable_0.3.3         tensorA_0.36.2       QuickJSR_1.1.3
 [4] processx_3.8.4       inline_0.3.19        lattice_0.22-5
 [7] tzdb_0.3.0           callr_3.7.3          vctrs_0.6.2
[10] tools_4.4.0          ps_1.7.5             generics_0.1.3
[13] stats4_4.4.0         parallel_4.4.0       sandwich_3.0-1
[16] fansi_1.0.4          pkgconfig_2.0.3      Matrix_1.6-5
[19] checkmate_2.1.0      distributional_0.3.2 RcppParallel_5.1.7
[22] lifecycle_1.0.3      compiler_4.4.0       farver_2.1.1
[25] Brobdingnag_1.2-9    munsell_0.5.0        codetools_0.2-19
[28] bayesplot_1.10.0     pillar_1.9.0         crayon_1.5.2
[31] arrayhelpers_1.1-0   MASS_7.3-60          StanHeaders_2.32.7
[34] bridgesampling_1.1-2 abind_1.4-5          multcomp_1.4-18
[37] nlme_3.1-165         posterior_1.4.1      rstan_2.32.6
[40] svUnit_1.0.6         tidyselect_1.2.0     mvtnorm_1.1-3
[43] stringi_1.7.12       splines_4.4.0        grid_4.4.0
[46] colorspace_2.1-0     cli_3.6.1            magrittr_2.0.3
[49] loo_2.6.0            survival_3.7-0       pkgbuild_1.4.0
[52] utf8_1.2.3           TH.data_1.1-0        withr_2.5.0
[55] prettyunits_1.1.1    scales_1.3.0         backports_1.4.1
[58] timechange_0.2.0     estimability_1.5.1   matrixStats_0.63.0
[61] emmeans_1.10.2       gridExtra_2.3        cellranger_1.1.0
[64] hms_1.1.3            zoo_1.8-12           coda_0.19-4
[67] ggdist_3.3.2         rstantools_2.3.1     rlang_1.1.3
[70] xtable_1.8-4         glue_1.6.2           jsonlite_1.8.8
[73] R6_2.5.1
```
