library(brms)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(ggrain)
library(bayesplot)
source("bayes_utils.R")
source("custom_brms/hurdle_gaussian.R")

plot_panas <- function(dat, estimate = "estimate") {
    dodge_width <- 0.5
    ggplot(dat, aes(
        x = active,
        y = estimate,
        color = timepoint,
        group = timepoint
    )) +
        geom_point(
            position = position_dodge(width = dodge_width),
            size = 5,
            shape = 18
        ) +
        geom_errorbar(
            aes(ymin = lower, ymax = upper),
            position = position_dodge(width = dodge_width),
            width = 0.1
        ) +
        facet_nested(
            rows = vars(valence),
            cols = vars(group, stimulation_group),
            labeller = labeller(.default = nice_labels),
            scales = "free_y"
        ) +
        my_theme()
}

n_subjects <- 62

dat <- read.csv("panas.csv")
head(dat)
prots <- bayes_data_load() %>%
    select(subject, session, group, stimulation_group, active) %>%
    distinct()

dat <- dat %>%
    left_join(prots, by = c("subject", "session")) %>%
    drop_na()

if (dat$subject |> unique() |> length() != n_subjects) {
    stop("Number of subjects does not match")
}

dat <- dat %>%
    mutate_if(is.character, str_trim) %>%
    mutate(valence = gsub("postiv", "positiv", valence)) %>%
    mutate(
        session = factor(session, levels = c("t1", "t2")),
        valence = factor(valence, levels = c("positiv", "negativ")),
        timepoint = factor(timepoint, levels = c("prä", "post")),
        group = factor(group, levels = c("control", "patient")),
        stimulation_group = factor(stimulation_group, levels = c("anodal", "cathodal")),
        active = factor(active, levels = c(0, 1)),
        value = value - 10
    ) %>%
    mutate(
        timepoint = recode(timepoint, "prä" = "pre", "post" = "post"),
        color = case_when(
            active == 0 ~ color_scheme$sham,
            active == 1 & stimulation_group == "anodal" ~ color_scheme$anodal,
            active == 1 & stimulation_group == "cathodal" ~ color_scheme$cathodal
        )
    )

active_label <- function(x) {
    ifelse(x == "0", "sham", ifelse(x == "1", "active", "error"))
}

explore_panas <- ggplot(dat, aes(x = timepoint, y = value, fill = color, color = color)) +
    geom_rain(id.long.var = "subject") +
    facet_nested(
        rows = vars(valence, active),
        cols = vars(group, stimulation_group),
        labeller = labeller(
            .default = nice_labels,
            active = active_label
        ),
        # scales  = "free_y",
    ) +
    xlab("Timepoint") +
    ylab("PANAS score") +
    my_theme() +
    scale_color_identity(guide = "legend", labels = c("sham", "anodal", "cathodal")) +
    scale_fill_identity(guide = "legend", labels = c("sham", "anodal", "cathodal"))

explore_panas


ggsave(
    "figures/panas_explore.png",
    plot = explore_panas,
    width = 15,
    height = 10,
    dpi = 300
)

## ---------------------------------------
## TEST HURDLE GAUSSIAN

bform <- bf(
    value ~ valence * timepoint * group * stimulation_group * active + (1 | subject),
    hu ~ valence
)

get_prior(bform, data = dat, family = hurdle_gaussian)

priors <- c(
    prior(normal(0, 1), class = b),
    prior(exponential(2), class = sd)
)

full_fit <- brm(
    formula = bform,
    prior = priors,
    data = dat,
    family = hurdle_gaussian,
    stanvars = stanvars,
    chains = 4,
    iter = 4000,
    warmup = 1000,
    cores = 4,
    file = "fits/full_panas_4000_steps.rds"
)
full_fit <- add_criterion(full_fit, c("loo", "waic"))

simple_bform <- bf(
    value ~ valence * timepoint * group * stimulation_group + active * timepoint + (1 | subject),
    hu ~ valence
)

simple_fit <- brm(
    formula = simple_bform,
    prior = priors,
    data = dat,
    family = hurdle_gaussian,
    stanvars = stanvars,
    chains = 4,
    iter = 4000,
    warmup = 1000,
    cores = 4,
    file = "fits/simple_panas_4000_steps.rds",
    sample_prior = TRUE
)
simple_fit <- add_criterion(simple_fit, c("loo", "waic"))

loo_compare(full_fit, simple_fit, criterion = "waic")
## loo is recommended but waic gives the same results
loo_compare(full_fit, simple_fit, criterion = "loo")

pp_check(simple_fit, ndraws = 500)

conditions <- make_conditions(
    simple_fit,
    vars = c("valence", "stimulation_group")
)
ce <- conditional_effects(
    simple_fit,
    "active:timepoint",
    conditions = conditions
)[[1]]

dodge_width <- 0.5
panas_plot <- ce %>%
    rename_all(str_remove, "__") %>%
    mutate(
        active = recode(
            factor(active, levels = c("0", "1")),
            "0" = "sham",
            "1" = "active"
        )
    ) %>%
    ggplot(aes(x = active, y = estimate, color = timepoint, ymin = lower, ymax = upper)) +
    geom_point(position = position_dodge(dodge_width)) +
    geom_errorbar(position = position_dodge(dodge_width), width = 0.1) +
    facet_nested(
        cols = vars(stimulation_group),
        rows = vars(valence),
        labeller = labeller(.default = nice_labels),
        scales = "free_y"
    ) +
    my_theme() +
    ylab("PANAS score")

panas_plot
ggsave(
    "figures/panas_hurdle_gaussian.png",
    plot = panas_plot,
    width = 15,
    height = 10,
    dpi = 300
)

hyps <- c(
    "active1 = 0",
    "timepointpost:active1 = 0"
)

hypothesis(simple_fit, hyps)


fe <- format_fe(simple_fit)
