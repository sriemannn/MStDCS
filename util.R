library(tidyverse)
library(readxl)
library(ggplot2)


#' Load the applied stimulation protocols from an excel file
#'
#' @param path Path to the excel file
#' @return A tibble with the applied stimulation protocols
#' @export
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
        select(-c("prot"))
}

#' Load the recruitment list from an excel file
#'
#' @param path Path to the excel file
#' @return A tibble with the recruitment list
#' @export
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

#' Plot an emmeans table from the MStDCS study
#'
#' Use a custom color schem ewith red for the anodal stimulation group,
#' blue for the cathodal stimulation group, and a grey for their respective
#' sham session.
#'
#' @param emmeans_table An emmeans table as returned by
#'  \code{emmeans::emmeans()}
#' @param ytitle The title for the y-axis
#' @param dodge_width The width for dodging the points
#' @return A ggplot object
#' @export
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

#' Count the number of trials that were conducted in each block
#'
#' Not every subject completed the same number of trials in each block.
#' This function counts the number of trials that were conducted in each block
#' for each subject and returns a vector that counts up the trials.
#'
#' @param control A tibble with the control data
#' @return A vector that counts up the trials
#' @export
trial_counter <- function(dat) {
    magrittr::use_pipe(export = TRUE)
    trial <- dat %>%
        dplyr::group_by(subject, session, block) %>%
        dplyr::count()
    unlist(lapply(trial$n, seq))
}

rename_terms_rt <- function(nice_rt) {
    gsub("[(]Intercept[)]", "Intercept", nice_rt$term) |>
        gsub("stimulation_groupcathodal", "Cathodal", x = _) |>
        gsub("grouppatient", "MS Patients", x = _) |>
        gsub("active1", "Active", x = _) |>
        gsub("sessiont2", "Session", x = _)
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


#' Format a nice table and save it as a docx file
#'
#' Format a table from \code{rempsyc::nice_table} and save it as a docx file.
#' Needs a "tables" directory in the working directory,
#' where the file will be saved.
#'
#' @param table A table from \code{rempsyc::nice_table}
#' @param name The name of the file
#' @param fontsize The fontsize for the table
#' @param linespacing The linespacing for the table
#' @export
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

#' Format the results of the RT filter.
#'
#' Rename the columns and calculate the percentage of missing values, values
#' outside the interval, and outliers. Assumes that trials were filtered
#' for missing values (1) by applying a 0.2-6s interval (2) by removing
#' all trials that were outside an interval Median +/- 3 * MAD.
#'
#' @param d A tibble with the results of the RT filter
#' @param interval The interval for the RT filter
#' @return A tibble with the formatted results
#' @export
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
