library(brms)
library(tidyverse)
library(ggplot2)
library(ggh4x)
source("bayes_utils.R")
source("util.R")

n_subjects <- 62
dat <- read.csv("adverse_effects.csv")
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
    mutate(
        sensation = recode(
            factor(sensation, levels = c(
                "wärme.hitze", "jucken", "brennen", "schmerz",
                "metallischereisengeschmack", "ermüdung.verringerteaufmerksamtkeit",
                "andere"
            )),
            wärme.hitze = "heat",
            jucken = "itching",
            brennen = "burning",
            schmerz = "pain",
            metallischereisengeschmack = "metallic taste",
            ermüdung.verringerteaufmerksamtkeit = "fatigue",
            andere = "other"
        ),
        intensity = factor(
            case_when(
                intensity == 0 ~ "none",
                intensity == 1 ~ "mild",
                intensity == 2 ~ "moderate",
                intensity == 3 ~ "strong"
            ),
            levels = c("none", "mild", "moderate", "strong"),
            ordered = TRUE
        )
    )

ae_summary <- dat %>%
    group_by(group, stimulation_group, active, sensation, intensity) %>%
    count() %>%
    pivot_wider(names_from = "intensity", values_from = "n")



# explore adverse effects
exp_ae <- dat %>%
    mutate(
        color = case_when(
            active == 0 ~ color_scheme$sham,
            active == 1 & stimulation_group == "anodal" ~ color_scheme$anodal,
            active == 1 & stimulation_group == "cathodal" ~ color_scheme$cathodal
        )
    ) %>%
    ggplot(aes(x = intensity, group = active, fill = color)) +
    geom_bar() +
    facet_nested(
        rows = vars(group, stimulation_group, active),
        cols = vars(sensation)
    ) +
    my_theme() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_fill_identity(guide = "legend", labels = c("cathodal", "anodal", "sham"))

exp_ae

ggsave(
    "figures/Supplementary_Figure_Y_Adverese_effects_exploration.png",
    plot = exp_ae,
    height = 15,
    width = 20,
    dpi = 300
)

# metallic taste never occured. So we will remove it

bform <- bf(
    intensity ~ 1 + group * stimulation_group + active +
        sensation +
        (1 | sensation) +
        (1 | subject)
)

get_prior(
    bform,
    data = dat,
    family = cumulative("probit")
)

priors <- c(
    prior(normal(0, 1), class = b),
    prior(exponential(2), class = sd)
)

simple_fit <- brm(
    formula = bform,
    data = dat,
    family = cumulative("probit"),
    chains = 4,
    cores = 4,
    iter = 2000,
    warmup = 1000,
    prior = priors,
    control = list(adapt_delta = 0.99),
    sample_prior = TRUE,
    file = "fits/adverse_effects_fit.rds"
)
simple_fit <- add_criterion(simple_fit, c("loo", "waic"))


bform2 <- bf(
    intensity ~ 1 + group * active * stimulation_group +
        sensation +
        (1 | sensation) +
        (1 | subject)
)

full_fit <- brm(
    formula = bform2,
    data = dat,
    family = cumulative("probit"),
    chains = 4,
    cores = 4,
    iter = 2000,
    warmup = 1000,
    prior = priors,
    control = list(adapt_delta = 0.99),
    sample_prior = TRUE,
    file = "fits/adverse_effects_fit2.rds"
)
full_fit <- add_criterion(full_fit, c("loo", "waic"))

## comparison with "waic" gives a warning message,
## but "loo" gives essentially the same results
## use "waic" comparison to keep methods consistent
loo_compare(simple_fit, full_fit)
loo_compare(simple_fit, full_fit, criterion = "waic")

pp_check(simple_fit, ndraws = 1000, type = "bars_grouped", group = "stimulation_group")

ce <- conditional_effects(
    simple_fit,
    effects = "active",
    categorical = TRUE
)[[1]]

dodge_width <- 0.5
ce_ae <- ce %>%
    rename_all(str_remove, "__") %>%
    mutate(
        active = recode(
            factor(active, levels = c(0, 1)),
            "0" = "sham",
            "1" = "active"
        )
    ) %>%
    ggplot(aes(x = active, y = estimate, ymin = lower, ymax = upper, color = cats)) +
    geom_point(position = position_dodge(dodge_width), size = 3) +
    geom_errorbar(position = position_dodge(dodge_width), width = 0.2) +
    my_theme() +
    ylab("Probability of adverse effect") +
    theme(axis.title.x = element_blank())
ce_ae

ggsave(
    "figures/Figure_Y_Adverse_effects.png",
    plot = ce_ae,
    height = 5,
    width = 7,
    dpi = 300
)

summary(fit)

hypothesis(fit, "active1 = 0")

fe_labels <- c(
    "Intercept<sub>none, mild</sub>",
    "Intercept<sub>mild, moderate</sub>",
    "Intercept<sub>moderate, strong</sub>",
    "Subject group",
    "Stimulation type",
    "Active stimulation",
    "Itching",
    "Burning",
    "Pain",
    "Metallic taste",
    "Fatigue",
    "Other",
    "Subject group × Stimulation type"
)

fe <- format_fe(fit, fe_labels)
fe

re <- summary(fit)[[18]]
re$sensation <- re$sensation |> round(2)
re$subject <- re$subject |> round(2)

nice_fe <- rempsyc::nice_table(
    fe,
    title = c(
        "Supplementary Table ADVERSEEFFECTS",
        "Fixed effects of the cumulative probit model for the intensity of adverse effects."
    ),
    note = paste0(
        "SD<sub>subject</sub> = ", re$subject[[1]], ", 95%CI=[", re$subject[[3]], ", ", re$subject[[4]], "]; ",
        "SD<sub>sensation</sub> = ", re$sensation[[1]], ", 95%CI=[", re$sensation[[3]], ", ", re$sensation[[4]], "]."
    )
)

format_and_save(
    nice_fe,
    "Supplementary_Table_ADVERSEEFFECTS"
)
