library(brms)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggh4x)
source("bayes_utils.R")
source("util.R")

# Load data
data <- bayes_data_load("data_rt_filter.csv")

get_prior(
    response_time_seconds | trunc(0.2, 6) ~ 1 + (1 | subject),
    data = data,
    family = lognormal
)

priors <- c(
    prior(normal(0, 1), class = "Intercept"),
    prior(exponential(2), class = "sd"),
    prior(exponential(2), class = "sigma")
)

intercept_only <- brm(
    formula = bf(
        response_time_seconds | trunc(0.2, 6) ~ 1 + (1 | subject)
    ),
    data = data,
    family = lognormal,
    prior = priors,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.99),
    file = "fits/rt_intercept_only_4000_steps",
    sample_prior = TRUE
)
intercept_only <- add_criterion(intercept_only, c("waic"))

priors <- c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(exponential(2), class = "sd"),
    prior(exponential(2), class = "sigma")
)

covariates_model <- brm(
    formula = bf(
        response_time_seconds | trunc(0.2, 6) ~ 1 + session
            + tmt_a_zeit + sdmt_richtige
            + (1 | subject)
    ),
    data = data,
    family = lognormal,
    prior = priors,
    chains = 8,
    cores = 8,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.99),
    file = "fits/rt_covariates_4000_steps",
    sample_prior = TRUE
)
covariates_model <- add_criterion(covariates_model, c("waic"))


full_model <- brm(
    formula = bf(
        response_time_seconds | trunc(0.2, 6) ~ 1 + session
            + tmt_a_zeit + sdmt_richtige
            + group * active * stimulation_group
            + (1 | subject)
    ),
    data = data,
    family = lognormal,
    prior = priors,
    chains = 8,
    cores = 8,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.99),
    file = "fits/rt_full_model_4000_steps",
    sample_prior = TRUE
)
full_model <- add_criterion(full_model, c("waic"))

waic_compare <- loo_compare(
    intercept_only, covariates_model, full_model,
    criterion = "waic"
)[]

nice_waic_compare <- waic_compare %>%
    format_waics(
        c(
            "Model including covariates and effects of interest",
            "Model including covariates",
            "Intercept only model"
        )
    )
nice_tbl <- rempsyc::nice_table(
    nice_waic_compare,
    title = c(
        "Table 5",
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
nice_tbl
# format_and_save(nice_tbl, "Table_5_waic_comparison_rt_models")

fixef_labels <- c(
    "Intercept",
    "Session",
    "TMT-A time",
    "SDMT n correct",
    "Subject group",
    "Active",
    "Stimulation type",
    "Subject group × Active",
    "Subject group ×\nStimulation type",
    "Active ×\nStimulation type",
    "Subject group × Active ×\nStimulation type"
)


areas <- bayesplot::mcmc_areas(full_model, regex_pars = "^b_")
areas <- areas +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    my_theme() +
    scale_y_discrete(
        labels = fixef_labels
    )
areas

sum_brt <- summary(full_model)
fe <- sum_brt[[15]]
re <- sum_brt[[18]]$subject %>% round(2)

evid_ratio <- compute_evidence_ratios(full_model)

rnames <- rownames(fe)

fe <- format_fe(full_model, fixef_labels = fixef_labels)

nice_fe <- rempsyc::nice_table(
    fe,
    title = c(
        "Table 6",
        "Population-level effects of the response latency model"
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

# format_and_save(nice_fe, "Table_6_population_level_effects_rt")

h <- c(
    paste0(
        # anodal control faster than ...
        "active1 <",
        # sham anodal control
        " 0"
    ),
    paste0(
        # cathodal control slower than ...
        " stimulation_groupcathodal +",
        " active1 + active1:stimulation_groupcathodal >",
        # ... sham cathodal control
        " stimulation_groupcathodal"
    ),
    paste0(
        # anodal patients slower than ...
        "active1 + grouppatient + grouppatient:active1 >",
        # ... sham anodal patients
        " grouppatient"
    ),
    paste0(
        # cathodal patients faster than ...
        "active1 + grouppatient + stimulation_groupcathodal +",
        "active1:stimulation_groupcathodal +",
        "grouppatient:active1 +",
        "grouppatient:stimulation_groupcathodal +",
        "grouppatient:active1:stimulation_groupcathodal <",
        # ... sham cathodal patients
        "grouppatient + stimulation_groupcathodal +",
        "grouppatient:stimulation_groupcathodal"
    )
)

hyp <- hypothesis(full_model, hypothesis = h)

hyp_samples <- hyp$samples %>%
    rename(
        `anodal control < sham control` = H1,
        `cathodal control > sham control` = H2,
        `anodal patients > sham patients` = H3,
        `cathodal patients < sham patients` = H4
    )


annotation <- list(
    size = 6,
    background = "white"
)
df_annotate <- data.frame(
    x = c(-0.02, 0.08),
    y = c(1.75, 2.75),
    label = c(
        "cathodal > sham",
        "anodal < sham"
    )
)
hyp_crtl <- hyp_samples %>%
    select(contains("control")) %>%
    bayesplot::mcmc_areas() +
    geom_vline(
        xintercept = 0,
        color = "darkgrey",
        linetype = "dashed",
        linewidth = 1
    ) +
    facet_wrap(~"Control", strip.position = "right") +
    annotate(
        "label",
        family = "Times",
        size = annotation$size,
        fill = annotation$background,
        label.size = NA,
        aes(data = df_annotate, x = x, y = y, label = label)
    ) +
    my_theme() +
    xlim(c(-0.25, 0.28)) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank()
    )

df_annotate <- data.frame(
    x = c(-0.01, 0.05),
    y = c(1.75, 2.75),
    label = c(
        "cathodal < sham",
        "anodal > sham"
    )
)
hyp_pwms <- hyp_samples %>%
    select(contains("patient")) %>%
    bayesplot::mcmc_areas() +
    geom_vline(
        xintercept = 0,
        color = "darkgrey",
        linetype = "dashed",
        linewidth = 1
    ) +
    facet_wrap(~"People with MS", strip.position = "right") +
    annotate(
        "label",
        family = "Times",
        size = annotation$size,
        fill = annotation$background,
        label.size = NA,
        aes(data = df_annotate, x = x, y = y, label = label)
    ) +
    my_theme() +
    xlim(c(-0.25, 0.28)) +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

hyp_comb <- hyp_crtl / hyp_pwms

hyp_comb

nice_hyp <- rempsyc::nice_table(hyp$hypothesis %>% mutate(Hypothesis = c(
    "Control: Anodal < sham",
    "Control: Cathodal > sham",
    "People with MS: Anodal > sham",
    "People with MS: Cathodal < sham"
)))
# format_and_save(nice_hyp, "posthoc_hypothesis_rt")

conditions <- make_conditions(brt_5, vars = "group")
ceffects <- conditional_effects(
    brt_5,
    "stimulation_group:active",
    conditions = conditions,
    plot = FALSE
)

ce <- extract_ces(ceffects)
pce <- plot_ces(ce)
# pce


# ggsave(
#   "figures/threeway_interaction_rt_control_session_sdmt_tmt.png",
#   pce,
#   width = 10, height = 6, dpi = 300
# )

rt_comb <- ((pce
+ theme(
        legend.position.inside = c(0.3, 0.95),
        legend.position = "inside",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "lightgrey", color = "grey"),
    )
) | (hyp_comb)) + plot_annotation(tag_levels = list(c("A", "B"))) &
    theme(plot.tag = element_text(size = 20))

rt_comb

ggsave(
    "figures/rt_comb.png",
    rt_comb,
    width = 12, height = 6, dpi = 300
)

ggsave(
    "figures/threeway_interaction_rt_control_session_sdmt_tmt.svg",
    pce
)
