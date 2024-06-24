library(brms)
library(tidyverse)
library(ggplot2)
source("bayes_utils.R")
source("util.R")

dat <- read.csv("combined_data_patient_controls.csv")

dat <- dat %>%
    mutate(
        subject = gsub(subject, pattern = "-", replacement = ""),
        accuracy = factor(response_type, levels = c("miss", "hit")),
        session = factor(session, levels = c("t1", "t2")),
        group = factor(group, levels = c("control", "patient")),
        stimulation_group = factor(stimulation_group, levels = c("anodal", "cathodal")),
        active = factor(active, levels = c(0, 1)),
    )

covs <- bayes_data_add_covs()

dat <- left_join(dat, covs, by = "subject")

mean(dat$accuracy == "hit")
sd(dat$accuracy == "hit")

rename_scheme <- function(x) {
    x |>
        gsub("control", "Control", x = _) |>
        gsub("patient", "People with MS", x = _) |>
        gsub("active", " ", x = _)
}

acc_summary_table <- dat %>%
    group_by(group, active, stimulation_group) %>%
    summarise(
        M = mean(accuracy == "hit"),
        SD = sd(accuracy == "hit")
    ) %>%
    pivot_wider(
        names_from = c(group, stimulation_group),
        values_from = c(M, SD),
        names_glue = "{group}.{stimulation_group}.{.value}"
    ) %>%
    mutate(active = ifelse(active == 0, "sham", "active")) %>%
    relocate(
        # nice table separate_header = TRUE needs '.' as separator
        active,
        control.anodal.M,
        control.anodal.SD,
        control.cathodal.M,
        control.cathodal.SD,
        patient.anodal.M,
        patient.anodal.SD,
        patient.cathodal.M,
        patient.cathodal.SD
    ) %>%
    rename_with(rename_scheme)

nice_acc_summary_table <- rempsyc::nice_table(
    acc_summary_table,
    title = c(
        "Supplementary Table X",
        "Summary of accuracy data"
    ),
    note = paste0(
        "M=Mean, SD=Standard deviation. ",
        "The table shows the mean and standard deviation of accuracy ",
        "for each group, stimulation group and active condition."
    ),
    separate.header = TRUE
)

format_and_save(nice_acc_summary_table, "Supplementary_Table_Summary_Accuracy")

bform <- bf(accuracy ~ 1 + (1 | subject))

get_prior(bform, data = dat, family = bernoulli())

priors <- c(
    prior(normal(0, 1), class = Intercept),
    prior(exponential(2), class = sd)
)

intercept_only <- brm(
    formula = bform,
    data = dat,
    family = bernoulli(),
    prior = priors,
    chains = 8,
    cores = 8,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.99),
    sample_prior = TRUE,
    file = "fits/acc_intercept_only_4000_steps.rds"
)
bac_0 <- add_criterion(intercept_only, "waic")

bform <- bf(
    accuracy ~ 1 + session + tmt_a_zeit + sdmt_richtige + (1 | subject)
)

get_prior(bform, data = dat, family = bernoulli())

priors <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(exponential(2), class = sd)
)

covariates <- brm(
    formula = bform,
    data = dat,
    family = bernoulli(),
    prior = priors,
    chains = 8,
    cores = 8,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.99),
    sample_prior = TRUE,
    file = "fits/acc_covariates_4000_steps.rds"
)
covariates <- add_criterion(covariates, "waic")

bform <- bf(
    accuracy ~ 1 + session + tmt_a_zeit + sdmt_richtige +
        group * active * stimulation_group +
        (1 | subject)
)

get_prior(bform, data = dat, family = bernoulli())

full_model <- brm(
    formula = bform,
    data = dat,
    family = bernoulli(),
    prior = priors,
    chains = 8,
    cores = 8,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.99),
    sample_prior = TRUE,
    file = "fits/acc_full_model_4000_steps.rds"
)
full_model <- add_criterion(full_model, "waic")

waic_compare <- loo_compare(intercept_only, covariates, full_model, criterion = "waic")[]

nice_waic_compare <- waic_compare %>%
    format_waics(
        c(
            "Model including covariates",
            "Model including covariates and effects of interest",
            "Intercept only model"
        )
    )

nice_tbl <- rempsyc::nice_table(
    nice_waic_compare,
    title = c(
        "Table 3",
        "WAIC comparison of models response latency models"
    ),
    note = paste0(
        "The effect of interest are the effects for subject group, active ",
        "stimulation and stimulation group, as well as their interactions. ",
        "ELPD=Expected log pointwise predictive density, ",
        "SE=Standard error, WAIC=Widely applicable information criterion, ",
        "pWAIC=Effective number of parameters"
    ),
    separate.header = TRUE
)
format_and_save(nice_tbl, "Table_3_waic_comparison_rt_models")

fixef_labels <- c(
    "Intercept",
    "Session",
    "TMT-A time",
    "SDMT n correct"
)
fe <- format_fe(covariates, fixef_labels = fixef_labels)
fe
re <- summary(covariates)[[18]]$subject %>% round(2)

nice_fe <- rempsyc::nice_table(
    fe,
    title = c(
        "Supplementary Table X",
        "Population-level effects of the response accuracy model"
    ),
    note = paste0(
        "Subject group-level intercepts Estimate: ",
        re[1, 1], " (95%CI [", re[1, 3], ", ", re[1, 4], "]). ",
        "Rhat: ", re[1, 5], ", Bulk ESS: ", re[1, 6], "; Tail ESS: ", re[1, 7], ". ",
        "Rhat values should be close to 1 to indicate convergence. ",
        "Bulk ESS and Tail ESS indicate the effective sample size of the ",
        "MCMC chains. Evidence ratio indicate the ratio of draws that were in ",
        "the direction of the estimate relative to the number of draws that were ",
        "in the opposite direction, e.g., for the TMT-A time effect, there were 239 times ",
        "more positive draws than negative draws. Simulations in linear models show, that ",
        "an evidence ratio of 19 is equivalent to a p-value of 0.05 (REF)."
    )
)

format_and_save(nice_fe, "Supplementary_Table_Population_Level_Effects_Accuracy")
