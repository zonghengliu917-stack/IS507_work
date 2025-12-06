
required_packages <- c("tidyverse")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(tidyverse)


extra_packages <- c("broom")
missing_extra <- extra_packages[!sapply(extra_packages, requireNamespace, quietly = TRUE)]
if (length(missing_extra) > 0) {
  install.packages(missing_extra, repos = "https://cloud.r-project.org")
}
library(broom)

train_path <- "train.csv"
test_path  <- "test.csv"

read_one <- function(path, tag) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(.dataset = tag)
}

train_df <- read_one(train_path, "train")
test_df  <- read_one(test_path,  "test")
raw_df   <- dplyr::bind_rows(train_df, test_df)

service_columns <- c(
  "Inflight wifi service",
  "Departure/Arrival time convenient",
  "Ease of Online booking",
  "Gate location",
  "Food and drink",
  "Online boarding",
  "Seat comfort",
  "Inflight entertainment",
  "On-board service",
  "Leg room service",
  "Baggage handling",
  "Checkin service",
  "Inflight service",
  "Cleanliness"
)


service_columns <- intersect(service_columns, colnames(raw_df))


age_breaks <- c(-Inf, 17, 25, 35, 45, 55, 65, Inf)
age_labels <- c("<18", "18-25", "26-35", "36-45", "46-55", "56-65", "66+")


if (length(service_columns) == 0) {
  stop("No find")
}

df <- raw_df |>
  dplyr::filter(!is.na(Age)) |>
  dplyr::mutate(
    age_group = cut(Age, breaks = age_breaks, labels = age_labels, right = TRUE, ordered_result = TRUE)
  )

df <- df |>
  dplyr::mutate(dplyr::across(dplyr::all_of(service_columns), as.numeric))

long_scores <- df |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(service_columns),
    names_to = "service",
    values_to = "score"
  ) |>
  dplyr::mutate(score = dplyr::na_if(score, 0))

summary_scores <- long_scores |>
  dplyr::group_by(age_group, service) |>
  dplyr::summarise(
    mean_score = mean(score, na.rm = TRUE),
    n_rated = sum(!is.na(score)),
    .groups = "drop"
  )

heatmap_plot <- ggplot(summary_scores, aes(x = service, y = age_group, fill = mean_score)) +
  geom_tile(color = "white", size = 0.2) + 
  scale_fill_viridis_c(option = "C", na.value = "grey90", limits = c(1, 5)) +
  labs(
    title = "Average service ratings by age group (0 treated as missing)",
    x = "Service",
    y = "Age group",
    fill = "Avg rating"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.grid = element_blank()
  )

topk <- 5
top5_by_age <- summary_scores |>
  dplyr::group_by(age_group) |>
  dplyr::arrange(dplyr::desc(mean_score), .by_group = TRUE) |>
  dplyr::slice_head(n = topk) |>
  dplyr::ungroup() |>
  dplyr::mutate(service = forcats::fct_reorder(service, mean_score))

bar_plot <- ggplot(top5_by_age, aes(x = service, y = mean_score, fill = age_group)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", mean_score)), 
            hjust = -0.15, 
            size = 3) +
  coord_flip() +
  facet_wrap(~ age_group, scales = "free_y") +
  scale_y_continuous(limits = c(0, 5.2)) +
  labs(
    title = "Top 5 services by age group (mean rating)",
    x = "Service",
    y = "Average rating"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

available_age_groups <- unique(na.omit(df$age_group))
available_age_groups <- available_age_groups[order(available_age_groups)]
ks_pair <- as.character(c(head(available_age_groups, 1), tail(available_age_groups, 1)))

ks_results <- NULL
if (length(ks_pair) == 2) {
  ks_results <- purrr::map_dfr(service_columns, function(sv) {
    sub_data <- long_scores |>
      dplyr::filter(service == sv, !is.na(age_group), !is.na(score), age_group %in% ks_pair)
    g1 <- sub_data |>
      dplyr::filter(age_group == ks_pair[1]) |>
      dplyr::pull(score)
    g2 <- sub_data |>
      dplyr::filter(age_group == ks_pair[2]) |>
      dplyr::pull(score)
    if (length(g1) >= 10 && length(g2) >= 10) {
      ks <- suppressWarnings(stats::ks.test(g1, g2))
      tibble::tibble(
        service = sv,
        group_a = ks_pair[1],
        group_b = ks_pair[2],
        statistic = unname(ks$statistic),
        p_value = ks$p.value
      )
    } else {
      tibble::tibble(
        service = sv,
        group_a = ks_pair[1],
        group_b = ks_pair[2],
        statistic = NA_real_,
        p_value = NA_real_
      )
    }
  })
}

ks_plot <- NULL
if (!is.null(ks_results)) {
  ks_plot <- ggplot(ks_results, aes(x = reorder(service, p_value), y = -log10(p_value))) +
    geom_col(fill = "#4C78A8") +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = paste0("K–S test: ", ks_pair[1], " vs ", ks_pair[2], " (distribution difference by service)"),
      x = "Service",
      y = expression(-log[10](p))
    ) +
    theme_minimal(base_size = 12)
}

logit_df <- df |>
  dplyr::mutate(
    satisfaction_binary = dplyr::case_when(
      !is.na(satisfaction) & satisfaction == "satisfied" ~ 1L,
      !is.na(satisfaction) ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(service_columns), ~ dplyr::na_if(.x, 0))) |>
  dplyr::select(satisfaction_binary, age_group, dplyr::all_of(service_columns)) |>
  tidyr::drop_na()

logit_fit <- NULL
logit_tidy <- NULL
logit_plot <- NULL
if (nrow(logit_df) > 100) {
  rhs <- paste(c("age_group", paste0("`", service_columns, "`")), collapse = " + ")
  fml <- stats::as.formula(paste0("satisfaction_binary ~ ", rhs))
  logit_fit <- stats::glm(fml, data = logit_df, family = stats::binomial())
  
  logit_tidy <- broom::tidy(logit_fit, conf.int = TRUE, exponentiate = TRUE) |>
    dplyr::mutate(
      term_clean = stringr::str_replace_all(term, "`", ""),
      is_age = stringr::str_starts(term_clean, "age_group")
    ) |>
    dplyr::filter(term != "(Intercept)")
  
  top_terms <- logit_tidy |>
    dplyr::arrange(p.value) |>
    dplyr::slice_head(n = 20) |>
    dplyr::mutate(term_clean = forcats::fct_reorder(term_clean, estimate))
  
  logit_plot <- ggplot(top_terms, aes(x = term_clean, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), color = "#F58518") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    coord_flip() +
    labs(
      title = "Logistic regression: factors associated with satisfaction (OR and 95% CI, Top 20 by p-value)",
      x = "Variables",
      y = "Odds ratio (OR)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot",
      plot.margin = margin(t = 10, r = 24, b = 10, l = 10)
    )
}

if (!dir.exists("outputs")) dir.create("outputs")
ggsave(filename = "age_service_heatmap.png", plot = heatmap_plot, width = 12, height = 6, dpi = 160)
ggsave(filename = "age_service_top5.png", plot = bar_plot, width = 12, height = 8, dpi = 160)

readr::write_csv(summary_scores, file.path("outputs", "age_service_summary.csv"))
readr::write_csv(top5_by_age,    file.path("outputs", "age_service_top5.csv"))

print(heatmap_plot)
print(bar_plot)

cat("\nTop5：\n")
top5_by_age |>
  dplyr::arrange(age_group, dplyr::desc(mean_score)) |>
  print(n = 50)

if (!is.null(ks_results)) {
  readr::write_csv(ks_results, file.path("outputs", "ks_results.csv"))
  if (!is.null(ks_plot)) {
    ggsave(filename = "ks_by_service.png", plot = ks_plot, width = 10, height = 6, dpi = 160)
    print(ks_plot)
  }
}

if (!is.null(logit_tidy)) {
  readr::write_csv(logit_tidy, file.path("outputs", "logit_tidy.csv"))
  if (!is.null(logit_plot)) {
    ggsave(filename = "logit_or_top20.png", plot = logit_plot, width = 12, height = 7, dpi = 160)
    print(logit_plot)
  }
}


