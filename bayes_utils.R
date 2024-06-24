library(ggplot2)
library(brms)
library(tidybayes)

# %% Load Data

#' Load the data for the bayesian analysis
#'
#' @return A tibble with the data
bayes_data_load <- function() {
    magrittr::use_pipe(export = TRUE)
    data <- read.csv("data_rt_filter.csv")
    data <- data %>%
        dplyr::select(-c("X")) %>%
        dplyr::mutate(
            subject = factor(gsub("-", "", subject)),
            block = factor(block),
            response_type = factor(response_type, levels = c("miss", "hit")),
            session = factor(session, levels = c("t1", "t2")),
            group = factor(group, levels = c("control", "patient")),
            stimulation_group = factor(stimulation_group),
            active = factor(active, levels = c(0, 1)),
            trial = factor(trial)
        )
    data
}

#' Add the covariates to the data
#'
#' @return A tibble with the covariates
bayes_data_add_covs <- function() {
    covs <- read.csv("neuropsychological_data.csv")
    names(covs) <- tolower(make.names(names(covs)))

    covs <- covs %>%
        dplyr::select(subject, tmt_a_zeit, tmt_a_fehler, sdmt_richtige) %>%
        dplyr::mutate_if(is.numeric, scale)

    covs
}

#' Get WAIC from a brms fit
#'
#' @param fit A brms fit object
#' @return A tibble with the WAIC values
waic_compare <- function(fit) {
    fit$criteria$waic[1]$estimates %>%
        data.frame() %>%
        dplyr::rownames_to_column() %>%
        dplyr::pivot_wider(
            names_from = rowname,
            values_from = c(Estimate, SE),
            names_glue = "{rowname}.{.value}"
        )
}

#' Format the WAIC table
#'
#' @param waic_table A tibble with the WAIC values
#' @return A tibble with the formatted WAIC values
format_waic <- function(waic_table) {
    waic_table %>%
        dplyr::rename_with(~ stringr::str_replace(., ".Estimate", ".Est")) %>%
        dplyr::relocate(
            elpd_waic.Est, elpd_waic.SE,
            p_waic.Est, p_waic.SE,
            waic.Est, waic.SE
        )
}

# %% Conditional Effects

color_scheme <- list(
    sham = "darkgrey",
    anodal = "#d86969",
    cathodal = "#699dd8"
)

#' Extract the conditional effects
#'
#' @param ce The conditional effects as created by
#' \code{brms::conditional_effects}
#' @return A tibble with the conditional effects
extract_ces_all <- function(ce) {
    ce <- ce[[1]]
    ce %>%
        dplyr::rename_with(function(x) {
            stringr::str_replace_all(x, "__", "")
        }) %>%
        dplyr::mutate(
            color = factor(as.character(dplyr::case_when(
                stimulation_group == "anodal" & active == 1 ~ "#d86969",
                stimulation_group == "cathodal" & active == 1 ~ "#699dd8",
                .default = "darkgrey"
            )), levels = c("darkgrey", "#d86969", "#699dd8")),
            group = dplyr::recode(
                group,
                "control" = "Control", "patient" = "People with MS"
            ),
            stimulation_group = dplyr::recode(
                stimulation_group,
                "anodal" = "Anodal", "cathodal" = "Cathodal"
            )
        )
}

#' Extract the conditional effects without se
#'
#' @param ce The conditional effects as created by
#' \code{brms::conditional_effects}
#' @return A tibble with the conditional effects
extract_ces <- function(ce) {
    ce <- ce[[1]]
    ce <- ce %>%
        dplyr::rename_with(function(x) {
            stringr::str_replace_all(x, "__", "")
        }) %>%
        dplyr::select(
            group, stimulation_group, active, estimate, lower, upper, se
        ) %>%
        dplyr::mutate(
            color = factor(as.character(dplyr::case_when(
                stimulation_group == "anodal" & active == 1 ~ "#d86969",
                stimulation_group == "cathodal" & active == 1 ~ "#699dd8",
                .default = "darkgrey"
            )), levels = c("darkgrey", "#d86969", "#699dd8")),
            group = dplyr::recode(
                group,
                "control" = "Control", "patient" = "People with MS"
            ),
            stimulation_group = dplyr::recode(
                stimulation_group,
                "anodal" = "Anodal", "cathodal" = "Cathodal"
            )
        )
    ce
}

#' Create a custom theme for the plots based on \code{ggplot2::theme_classic}
#'
#' @param text_size_axis The size of the axis text
#' @return A ggplot2 theme
my_theme <- function(text_size_axis = 15) {
    theme_classic() +
        theme(
            text = element_text(family = "Times"),
            legend.position = "bottom",
            legend.title = element_blank(),
            axis.text = element_text(size = text_size_axis, family = "Times"),
            axis.title = element_text(
                size = text_size_axis + 4, family = "Times"
            ),
            strip.text = element_text(size = text_size_axis, family = "Times"),
            legend.text = element_text(size = text_size_axis, family = "Times")
        )
}

#' Plot the conditional effects
#'
#' @param ce The conditional effects as created by
#' \code{brms::conditional_effects}
#' @return A ggplot2 plot
plot_ces <- function(ce) {
    dodge_width <- 0.5
    ggplot(ce, aes(x = stimulation_group, y = estimate, color = color)) +
        geom_point(
            position = position_dodge(width = dodge_width), size = 5, shape = 18
        ) +
        geom_errorbar(
            aes(ymin = lower, ymax = upper),
            width = 0.2,
            position = position_dodge(width = dodge_width)
        ) +
        facet_wrap(~group) +
        scale_color_identity(
            guide = "legend", labels = c("sham", "anodal", "cathodal")
        ) +
        ylab("Response time (s)") +
        xlab("Stimulation groups") +
        my_theme()
}

#' Labeller function for the facet labels
#'
#' @param string The string to be formatted
#' @return The formatted string
nice_labels <- function(string) {
    string |>
        gsub("control", "Control", x = _) |>
        gsub("patient", "People with MS", x = _) |>
        gsub(":", " ", x = _)
}

# %% Model comparison

#' Format the WAIC table
#'
#' @param waics The WAIC table as created by \code{brms::loo_compare}
#' @param modelnames The names of the models
#' @return A tibble with the WAIC table
format_waics <- function(waics, modelnames) {
    waics %>%
        data.frame() %>%
        dplyr::rownames_to_column() %>%
        dplyr::rename(
            Model = rowname,
            `ELPD diff.Est` = `elpd_diff`,
            `ELPD diff.SE` = `se_diff`,
            `ELPD WAIC.Est` = `elpd_waic`,
            `ELPD WAIC.SE` = `se_elpd_waic`,
            `pWAIC.Est` = `p_waic`,
            `pWAIC.SE` = `se_p_waic`,
            `WAIC.Est` = `waic`,
            `WAIC.SE` = `se_waic`
        ) %>%
        dplyr::mutate(
            Model = modelnames
        )
}

#' Compute the evidence ratios for the model fit, i.e.,
#' the ratio of the number of draws where the parameter
#' is greater than zero to the number of draws where the
#' parameter is less than zero, when the effect's direction
#' is positive and vice versa when the effect's direction
#' is negative.
#'
#' @param fit The model fit
#' @return A tibble with the evidence ratios
compute_evidence_ratios <- function(fit) {
    fit %>%
        tidybayes::tidy_draws() %>%
        dplyr::select(dplyr::starts_with("b_")) %>%
        dplyr::map_df(
            .f = ~ list(
                draws_gt = sum(. > 0),
                draws_lt = sum(. < 0)
            ),
            .id = "Parameter"
        ) %>%
        dplyr::mutate(
            evidence_ratio = ifelse(
                draws_gt > draws_lt,
                draws_gt / draws_lt,
                draws_lt / draws_gt
            ),
            Parameter = gsub("b_", "", Parameter)
        ) %>%
        dplyr::select(-draws_gt, -draws_lt) %>%
        dplyr::rename(
            `Evidence ratio` = evidence_ratio
        )
}

#' Format the fixed effects table
#'
#' This function takes the fixed effects table from a model fit
#' and formats it into a nice tibbel. It also computes the evidence
#' ratios for the fixed effects. Always using the direction of the
#' effect as the reference in which direction to test. If the effect
#' is positive, the evidence ratio is the number of draws where the
#' effect is greater than zero divided by the number of draws where
#' the effect is less than zero. If the effect is negative, the evidence
#' ratio is the number of draws where the effect is less than zero divided
#' by the number of draws where the effect is greater than zero.
#' Parameter names are cleaned, so they comply with naming conventions
#' for the \code{rempsyc::nice_table} function.
#'
#' @param fit The model fit
#' @param fixef_labels The labels for the fixed effects. If NULL, the
#' labels will not be changed
#' @return A tibble with the fixed effects table
format_fe <- function(fit, fixef_labels = NULL) {
    ret <- summary(fit)[[15]] %>%
        data.frame() %>%
        dplyr::rownames_to_column() %>%
        dplyr::rename(
            Parameter = rowname, Estimate = Estimate, SE = `Est.Error`,
            CI_lower = l.95..CI, CI_upper = u.95..CI, `Bulk ESS` = Bulk_ESS,
            `Tail ESS` = Tail_ESS
        ) %>%
        dplyr::left_join(compute_evidence_ratios(fit), by = "Parameter") %>%
        dplyr::mutate_if(is.numeric, round, digits = 2)

    if (is.null(fixef_labels)) {
        return(ret)
    }

    ret %>% dplyr::mutate(
        Parameter = gsub("\n", " ", fixef_labels)
    )
}
