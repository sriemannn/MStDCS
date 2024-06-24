library(tidyverse)
library(readxl)
library(ggplot2)


#' Load the applied stimulation protocols from an excel file
#'
#' @param path Path to the excel file
#' @return A tibble with the applied stimulation protocols
get_stim <- function(path) {
    read_excel(
        path,
    ) %>%
        select(
            c("ID", "Protokoll_t1", "Protokoll_t2")
        ) %>%
        drop_na() %>%
        mutate_at("ID", function(x) paste0("MS-tDCS-KG", x)) %>%
        rename(subject = ID) %>%
        pivot_longer(
            cols = c("Protokoll_t1", "Protokoll_t2"),
            names_sep = "_",
            names_to = c("prot", "session"),
            values_to = "applied_protocol"
        ) %>%
        # mutate_at("applied_protocol", str_to_lower) %>%
        select(-c("prot"))
}

#' Load the recruitment list from an excel file
#'
#' @param path Path to the excel file
#' @return A tibble with the recruitment list
get_recruitment_list <- function(path) {
    read_excel(path) %>%
        filter(!is.na(Probandencode)) %>%
        mutate_at(
            "Probandencode", function(x) {
                paste0("MS-tDCS-KG", str_pad(x, 3, pad = "0"))
            }
        ) %>%
        rename(subject = Probandencode)
}

#' Plot an emmeans table
#'
#' @param emmeans_table An emmeans table as returned by
#'  \code{emmeans::emmeans()}
#' @param ytitle The title for the y-axis
#' @param dodge_width The width for dodging the points
#' @return A ggplot object
plot_emmeans <- function(emmeans_table, ytitle, dodge_width = 0.3) {
    ggplot(emmeans_table, aes(
        x = stimulation_group, y = response,
        color = color
    )) +
        geom_point(
            position = position_dodge(width = dodge_width),
            size = 5, shape = 18
        ) +
        geom_errorbar(
            aes(ymin = asymp.LCL, ymax = asymp.UCL),
            position = position_dodge(width = dodge_width),
            width = dodge_width / 2,
            linewidth = 0.8
        ) +
        facet_wrap(~group) +
        scale_color_manual(values = c("#b0b0b2", "#db7070", alpha("#0000ff", 0.6))) +
        scale_x_discrete(labels = c("anodal", "cathodal")) +
        theme_classic() +
        theme(
            text = element_text(size = 15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
        ) +
        ylab(ytitle)
}

trial_counter <- function() {
    trial <- control %>%
        group_by(subject, session, block) %>%
        count()
    unlist(lapply(trial$n, seq))
}

rename_terms_rt <- function(nice_rt) {
    gsub("[(]Intercept[)]", "Intercept", nice_rt$term) |>
        gsub("stimulation_groupcathodal", "Cathodal", x = _) |>
        gsub("grouppatient", "MS Patients", x = _) |>
        gsub("active1", "Active", x = _) |>
        gsub("sessiont2", "Session", x = _)
}

trial_counter <- function(d) {
    d %>%
        group_by(group, stimulation_group, active) %>%
        count()
}

rename_lrt <- function() {
    c(
        "term",
        "Number of parameters",
        "AIC",
        "BIC",
        "log likelihood",
        "deviance",
        "Chi-square",
        "df",
        "p.value"
    )
}

format_and_save <- function(table, name, fontsize = 10, linespacing = 1.15) {
    table <- flextable::fontsize(
        table,
        size = fontsize,
        part = "all"
    )
    table <- flextable::line_spacing(
        table,
        space = linespacing,
        part = "all"
    )
    flextable::save_as_docx(
        table,
        path = paste0("tables/", name, ".docx"),
        pr_section = officer::prop_section(
            page_size = officer::page_size(orient = "landscape"),
        )
    )
}

format_rt_filter <- function(d, interval) {
    d %>%
        rename(
            before = n.x,
            after_miss = n.y,
            after_interval = n.x.x,
            after_outliers = n.y.y
        ) %>%
        mutate(
            group = recode(group, "control" = "Control", "patient" = "MS Patients"),
            active = recode(active, `0` = "Sham", `1` = "Active"),
            stimulation_group = recode(
                stimulation_group,
                `0` = "Anodal", `1` = "Cathodal"
            ),
            after_miss_perc = round(after_miss / before * 100, 2),
            after_interval_perc = round(after_interval / before * 100, 2),
            after_outliers_perc = round(after_outliers / before * 100, 2),
            before_perc = round(before / before * 100, 2),
            before = paste0(before, "\n(", before_perc, "%)"),
            after_miss = paste0(after_miss, "\n(", after_miss_perc, "%)"),
            after_interval = paste0(after_interval, "\n(", after_interval_perc, "%)"),
            after_outliers = paste0(after_outliers, "\n(", after_outliers_perc, "%)")
        ) %>%
        select(
            -c(before_perc, after_miss_perc, after_interval_perc, after_outliers_perc)
        ) %>%
        pivot_longer(
            cols = c(before, after_miss, after_interval, after_outliers),
            names_to = "step"
        ) %>%
        pivot_wider(
            names_from = c(group, stimulation_group, active),
            names_sep = "."
        ) %>%
        mutate(
            step = c(
                "All trials", "After removing miss trials",
                paste0(
                    "After removing trials with response\ntimes outside the ", interval[1],
                    "-", interval[2], "s interval"
                ),
                "After removing trials outside of individual median ± 3 × MAD interval"
            )
        ) %>%
        rename(
            " " = step
        )
}
