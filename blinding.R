library(brms)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(patchwork)
source("bayes_utils.R")
source("util.R")

n_subjects <- 62

dat <- read.csv("blinding.csv")
head(dat)
prots <- bayes_data_load() %>%
    select(subject, session, group, stimulation_group, active) %>%
    distinct() %>%
    filter(active == 1)

dat <- dat %>%
    left_join(prots, by = c("subject", "group")) %>%
    drop_na() %>%
    mutate(
        blinding_guess = factor(
            blinding_guess,
            levels = c("does not know", "t1", "t2")
        )
    ) %>%
    rename(
        active_session = session
    )

if (dat$subject |> unique() |> length() != n_subjects) {
    stop("Number of subjects does not match")
}

bform <- bf(blinding_guess ~ 1 + group * stimulation_group * active_session)

get_prior(
    bform,
    data = dat,
    family = categorical
)

priors <- c(
    prior(normal(0, 1), class = "b", dpar = "mut1"),
    prior(normal(0, 1), class = "b", dpar = "mut2"),
    prior(normal(0, 1), class = "Intercept", dpar = "mut1"),
    prior(normal(0, 1), class = "Intercept", dpar = "mut2")
)

full_model <- brm(
    formula = bform,
    data = dat,
    family = categorical,
    prior = priors,
    sample_prior = TRUE,
    iter = 4000,
    warmup = 1000,
    file = "fits/blinding_full"
)
full_model <- add_criterion(full_model, c("loo", "waic"))

bform <- bf(
    blinding_guess ~ 1 + group * stimulation_group + active_session
)

simple_model <- brm(
    formula = bform,
    data = dat,
    family = categorical,
    prior = priors,
    sample_prior = TRUE,
    iter = 4000,
    warmup = 1000,
    file = "fits/blinding_simple"
)
simple_model <- add_criterion(simple_model, c("loo", "waic"))

loo_compare(
    full_model,
    simple_model
)
loo_compare(
    full_model,
    simple_model,
    criterion = "waic"
)

pp_check(full_model, ndraws = 500, type = "bars_grouped", group = "group")

fe_labels <- c(
    "Intercept_mu_t1",
    "Intercept_mu_t2",
    "Subject group_mu_t1",
    "Stimulation type_mu_t1",
    "Active stimulation_{mu t1}",
    "Subject group × stimulation type_{mu t1}",
    "Subject group × active stimulation_{mu t1}",
    "Stimulation type × active stimulation_{mu t1}",
    "Subject group × stimulation type × active stimulation_{mu t1}",
    "Subject group_mu_t2",
    "Stimulation type_mu_t2",
    "Active stimulation_{mu t2}",
    "Subject group × stimulation type_{mu t2}",
    "Subject group × active stimulation_{mu t2}",
    "Stimulation type × active stimulation_{mu t2}",
    "Subject group × stimulation type × active stimulation_{mu t2}"
)

fe <- format_fe(full_model, fixef_labels = fe_labels)

nice_fe <- rempsyc::nice_table(
    fe,
    title = c(
        "Supplementary Table 3",
        "Fixed effects of the categorical model predicting blinding guess."
    ),
    note = "INSERT NOTE COPY"
)

format_and_save(
    nice_fe,
    "Supplementary_Table_3_BLINDING"
)

fe <- format_fe(full_model)
parameter <- fe$Parameter

hyp <- lapply(parameter, function(string) {
    paste0(string, " = 0")
}) |> unlist()

hypothesis(full_model, hyp)

conditions <- make_conditions(
    full_model,
    vars = c("stimulation_group")
)
ce_stim_group_active_session <- conditional_effects(
    full_model,
    effects = "active_session",
    conditions = conditions,
    categorical = TRUE
)[[1]]

dodge_width <- 0.5
ce_plot_stim_group_active_session <- ce_stim_group_active_session %>%
    rename_all(str_remove, "__") %>%
    mutate(
        group = recode(group, "control" = "Control", "patient" = "People with MS"),
        stimulation_group = recode(
            stimulation_group,
            "anodal" = "Anodal",
            "cathodal" = "Cathodal"
        )
    ) %>%
    ggplot(aes(x = active_session, y = estimate, ymin = lower, ymax = upper, color = cats)) +
    geom_point(position = position_dodge(dodge_width), size = 5, shape = 18) +
    geom_errorbar(position = position_dodge(dodge_width), width = 0.2) +
    facet_nested(
        rows = vars(stimulation_group),
    ) +
    theme(axis.title.x = element_blank()) +
    ylab("Probability of blinding guess")

ce_plot_stim_group_active_session


ce_group <- conditional_effects(full_model, effects = "group", categorical = TRUE)[[1]]
ce_plot_group <- ce_group %>%
    rename_all(str_remove, "__") %>%
    mutate(
        group = recode(group, "control" = "Control", "patient" = "People with MS"),
        stimulation_group = recode(
            stimulation_group,
            "anodal" = "Anodal",
            "cathodal" = "Cathodal"
        )
    ) %>%
    ggplot(aes(x = group, y = estimate, ymin = lower, ymax = upper, color = cats)) +
    geom_point(position = position_dodge(dodge_width), size = 5, shape = 18) +
    geom_errorbar(position = position_dodge(dodge_width), width = 0.2) +
    theme(axis.title.x = element_blank()) +
    ylab("Probability of blinding guess")

ce_plot_group

ce_plot_comb <- ce_plot_group +
    (ce_plot_stim_group_active_session + theme(axis.title.y = element_blank())) +
    plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") &
    theme(
        legend.position = "bottom",
        axis.title.x = element_blank()
    ) * my_theme() &
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())

ce_plot_comb <- wrap_elements(ce_plot_comb) +
    labs(tag = "Probability of blinding guess") +
    theme(
        text = element_text(family = "Times"),
        plot.tag = element_text(size = 19, angle = 90),
        plot.tag.position = "left"
    )

ce_plot_comb

ggsave(
    "figures/blinding_guess_ce_comb.png",
    ce_plot_comb,
    width = 10,
    height = 5,
    dpi = 300
)
