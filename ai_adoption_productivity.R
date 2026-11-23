# AI adoption and productivity by industry, UK, 2023 to 2026
# RG Economics - blog analysis (November 2026)
#
# What this does
#   1. Reads ONS BICS wave 165 tables on AI use by industry (Sept 2023 to Oct 2026)
#   2. Reads ONS output per hour worked and hours worked by industry section (quarterly)
#   3. Builds an industry panel and compares the rise in AI use with productivity growth
#   4. Runs robustness checks (time window, smoothing, leave-one-out, pre-trend control,
#      hours weights, robust standard errors, implied size of effect)
#   5. Saves charts (PNG) and tidy CSVs to outputs/
#
# Data (all public, saved in data_raw/)
#   bics_w165.xlsx      ONS Business Insights and Conditions Survey, wave 165 dataset
#   oph_by_section.xlsx ONS Output per hour worked, UK, 18 Aug 2026 release (data to 2026 Q1).
#                       This is the latest industry-level release: the 17 Nov 2026 release
#                       moved to a new method and covers the whole economy only.
#   oph_whole_economy_nov2026.xlsx  ONS Output per hour worked, UK, 17 Nov 2026 release
#                       (whole economy, new component approach, data to 2026 Q3)
#   foi_ai_sic.xlsx     ONS FOI-2026-3675 (AI use in legal, accounting and consultancy)
#
# Main specification (changed from the first draft after review)
#   - AI change = trend in each industry's AI share fitted through every AI wave from
#     Sept 2023 (wave 92) to Mar 2026 (wave 153), expressed as the fitted change in
#     percentage points over that period. This matches the productivity window (to 2026 Q1)
#     and uses the same fitted-trend approach as productivity. Single BICS industry waves are
#     noisy, and the first draft used AI data from after the productivity data ended.
#     Two-wave averages at each end are kept as a robustness check.
#   - Productivity growth = trend growth fitted through every quarter (log-linear OLS),
#     not growth between two single quarters.
#   - Pre-trend is used as a regression control as well as being subtracted one for one.
#
# Caveats
#   - BICS industry estimates cover all firm sizes and are weighted by business count, not
#     by employment. They exclude finance, public admin, agriculture and energy. Some cells
#     are suppressed by ONS. Water and waste has no usable AI data.
#   - AI use is self-reported and counts any use of at least one AI technology.
#   - Industry hours come from the Labour Force Survey, whose quality has been weak since 2023.
#     The industry productivity data are "official statistics in development" and may be revised
#     when ONS moves industry estimates to its new method.
#   - 13 industries only. Correlation, not causation.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(scales); library(ggrepel); library(sandwich); library(lmtest)
})

dir.create("outputs", showWarnings = FALSE)
bics     <- "data_raw/bics_w165.xlsx"
oph_file <- "data_raw/oph_by_section.xlsx"

to_num <- function(x) suppressWarnings(as.numeric(x))   # "[c]" (suppressed) -> NA

# Industry names in BICS and their matching column in the ONS productivity tables
ind_map <- tribble(
  ~bics,                                                                  ~oph,                                                                  ~short,
  "Manufacturing",                                                        "Manufacturing",                                                       "Manufacturing",
  "Water supply, sewerage, waste management and remediation activities",  "Water supply; sewerage, waste management and remediation activities", "Water & waste",
  "Construction",                                                         "Construction",                                                        "Construction",
  "Wholesale and retail trade; repair of motor vehicles and motorcycles", "Wholesale and retail trade; repair of motor vehicles and motorcycles","Wholesale & retail",
  "Transportation and storage",                                           "Transportation and storage",                                          "Transport",
  "Accommodation and food service activities",                            "Accommodation and food service activities",                           "Hospitality",
  "Information and communication",                                        "Information and communication",                                       "Info & comms",
  "Real estate activities",                                               "Real estate activities excluding imputed rental",                     "Real estate",
  "Professional, scientific and technical activities",                    "Professional, scientific and technical activities",                   "Professional services",
  "Administrative and support service activities",                        "Administrative and support service activities",                       "Admin & support",
  "Education",                                                            "Education",                                                           "Education",
  "Human health and social work activities",                              "Human health and social activities",                                  "Health & social care",
  "Arts, entertainment and recreation",                                   "Arts, entertainment and recreation",                                  "Arts & leisure",
  "Other service activities",                                             "Other service activities including households as employers",         "Other services"
)

read_bics <- function(sheet) {
  read_excel(bics, sheet = sheet, skip = 5) |>
    rename(industry = `Industry/Size Band`) |>
    mutate(industry = str_trim(industry))
}

# ---- 1. AI use over time by industry and size --------------------------------
use <- read_bics("AI Current Usage TS (WTD)") |>
  mutate(none     = to_num(`Business does not currently use artificial intelligence technologies`),
         not_sure = to_num(`Not sure`),
         ai_use   = 1 - none - not_sure,      # share using at least one AI technology (ONS headline definition)
         start    = as.Date(str_extract(Dates, "^\\d+ \\w+ \\d{4}"), format = "%d %B %Y")) |>
  select(Dates, Wave, start, industry, ai_use)

use_ind  <- use |> inner_join(ind_map, by = c("industry" = "bics"))
use_all  <- use |> filter(industry == "All businesses") |> arrange(start)
use_10up <- use |> filter(industry == "All size bands excluding 0 - 9") |> arrange(start)

write.csv(use_ind, "outputs/ai_use_by_industry_timeseries.csv", row.names = FALSE)
headline <- bind_rows(use_all |> mutate(group = "All businesses"),
                      use_10up |> mutate(group = "10 or more employees")) |>
  select(group, Wave, Dates, ai_use)
write.csv(headline, "outputs/ai_use_headline.csv", row.names = FALSE)

waves <- use_ind |> distinct(Wave, start) |> arrange(start)
first_two  <- head(waves$Wave, 2)                          # wave 92 (Sept 2023), wave 98 (Dec 2023)
last_two   <- tail(waves$Wave, 2)                          # wave 159 (June 2026), wave 165 (Sept-Oct 2026)
# Last two AI waves that fall within the productivity window (to end of 2026 Q1)
aligned_two <- waves |> filter(start <= as.Date("2026-03-31")) |> tail(2) |> pull(Wave)   # wave 147, wave 153

# ---- 2. Depth of use and training, latest waves -----------------------------
# Base for both questions: businesses using AI OR not sure whether they do (see sheet notes)
latest <- "Wave 165"
extent_all <- read_bics("AI Extent Use TS (WTD)") |>
  transmute(Wave, industry,
            extensive = to_num(`Used extensively`),
            limited   = to_num(`Used on a limited basis`),
            pilots    = to_num(`Used only for artificial intelligence technology testing or pilots`))
training_all <- read_bics("AI Workforce Training TS (WTD)") |>
  mutate(over75 = to_num(`More than 75%`), b50_75 = to_num(`Between 50% and 75%`)) |>
  transmute(Wave, industry,
            # If one band is suppressed the total is unknown, so keep NA rather than guess
            trained_half_plus = over75 + b50_75,
            no_training = to_num(`No staff have received training`))

depth <- ind_map |> select(industry = bics, short) |>
  bind_rows(tibble(industry = "All businesses", short = "All businesses")) |>
  left_join(extent_all |> filter(Wave == latest) |> select(-Wave), by = "industry") |>
  left_join(training_all |> filter(Wave == latest) |> select(-Wave), by = "industry")
write.csv(depth, "outputs/ai_depth_training_latest_wave.csv", row.names = FALSE)

# Two-wave average of extensive use (waves 159 and 165) for the correlation check
ext_avg <- extent_all |> filter(Wave %in% c("Wave 159", "Wave 165")) |> group_by(industry) |>
  summarise(extensive_avg = mean(extensive),             # NA if either wave is suppressed
            n_waves = sum(!is.na(extensive)), .groups = "drop")

# ---- 3. Productivity and hours by industry -----------------------------------
read_ons_q <- function(sheet, value) {
  read_excel(oph_file, sheet = sheet, skip = 3) |>
    filter(str_detect(Quarter, "^\\d{4} Q[1-4]$")) |>
    pivot_longer(-Quarter, names_to = "oph_ind", values_to = value) |>
    mutate(across(all_of(value), to_num),
           year = as.integer(str_sub(Quarter, 1, 4)),
           q    = as.integer(str_sub(Quarter, -1)),
           t    = year + (q - 1) / 4)
}
oph   <- read_ons_q("Table_24", "oph")
hours <- read_ons_q("Table_23", "hours")
latest_q <- tail(sort(unique(oph$Quarter[!is.na(oph$oph)])), 1)   # 2026 Q1

qt <- function(x) as.integer(str_sub(x, 1, 4)) + (as.integer(str_sub(x, -1)) - 1) / 4

# Trend growth: slope of log output per hour on time, fitted through every quarter (% a year)
trend_growth <- function(d, from, to) {
  s <- d |> filter(t >= qt(from), t <= qt(to), !is.na(oph))
  if (nrow(s) < 4) return(NA_real_)
  100 * unname(coef(lm(log(oph) ~ t, data = s))[2])
}
# End-point growth between two single quarters (the first draft's method, kept as a check)
endpoint_growth <- function(d, from, to) {
  a <- d$oph[d$Quarter == from]; b <- d$oph[d$Quarter == to]
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  100 * (log(b) - log(a)) / (qt(to) - qt(from))
}

prod <- oph |> group_by(oph_ind) |>
  group_modify(~ tibble(
    g_pre     = trend_growth(.x, "2015 Q1", "2019 Q4"),     # pre-pandemic trend
    g_post    = trend_growth(.x, "2023 Q3", latest_q),      # since BICS AI questions began
    g_pre_ep  = endpoint_growth(.x, "2015 Q1", "2019 Q4"),
    g_post_ep = endpoint_growth(.x, "2023 Q3", latest_q))) |>
  ungroup() |>
  mutate(g_accel = g_post - g_pre, g_accel_ep = g_post_ep - g_pre_ep)

hours_w <- hours |> filter(year == 2023) |> group_by(oph_ind) |>
  summarise(hours_2023 = mean(hours, na.rm = TRUE), .groups = "drop")

whole_econ <- prod |> filter(oph_ind == "Whole Economy excluding imputed rental")

# Whole economy, latest ONS release (17 Nov 2026, new method, data to 2026 Q3)
we_new <- read_excel("data_raw/oph_whole_economy_nov2026.xlsx", sheet = "Table_11", col_names = FALSE,
                     .name_repair = "minimal")[, 1:2] |>
  setNames(c("Quarter", "oph")) |>
  filter(str_detect(Quarter, "^\\d{4} Q[1-4]$")) |>
  mutate(oph = to_num(oph), t = qt(Quarter))
we_latest_q <- tail(we_new$Quarter, 1)
aggregate <- tibble(
  source = c("18 Aug 2026 release (old method)", "17 Nov 2026 release (new method)", "17 Nov 2026 release (new method)"),
  window_post = c(paste("2023 Q3 to", latest_q), paste("2023 Q3 to", latest_q), paste("2023 Q3 to", we_latest_q)),
  growth_post = c(whole_econ$g_post, trend_growth(we_new, "2023 Q3", latest_q), trend_growth(we_new, "2023 Q3", we_latest_q)),
  growth_2015_19 = c(whole_econ$g_pre, rep(trend_growth(we_new, "2015 Q1", "2019 Q4"), 2)))
write.csv(aggregate, "outputs/whole_economy_productivity.csv", row.names = FALSE)

# ---- 4. Industry panel -------------------------------------------------------
ai_avg <- function(w, name) use_ind |> filter(Wave %in% w) |>
  group_by(short) |> summarise("{name}" := mean(ai_use), .groups = "drop")   # NA if a wave is suppressed
ai_single <- function(w, name) use_ind |> filter(Wave == w) |> select(short, "{name}" := ai_use)

# Fitted change in AI share (pp) between the first AI wave and March 2026, from a linear trend
# through every wave in that window (suppressed waves skipped)
fit_start <- min(waves$start); fit_end <- max(waves$start[waves$Wave %in% aligned_two])
ai_fit <- use_ind |> filter(start <= fit_end, !is.na(ai_use)) |> group_by(short) |>
  summarise(ai_fit_waves = n(),
            ai_change_fit = 100 * unname(coef(lm(ai_use ~ as.numeric(start)))[2]) *
                            as.numeric(fit_end - fit_start), .groups = "drop")

panel <- ind_map |>
  left_join(ai_single(waves$Wave[1], "ai_2023"), by = "short") |>
  left_join(ai_single(latest, "ai_2026"), by = "short") |>
  left_join(ai_single(tail(aligned_two, 1), "ai_mar26"), by = "short") |>
  left_join(ai_avg(first_two, "ai_start2"), by = "short") |>
  left_join(ai_avg(aligned_two, "ai_aligned2"), by = "short") |>
  left_join(ai_avg(last_two, "ai_last2"), by = "short") |>
  left_join(prod, by = c("oph" = "oph_ind")) |>
  left_join(hours_w, by = c("oph" = "oph_ind")) |>
  left_join(depth |> select(short, extensive, trained_half_plus), by = "short") |>
  left_join(ext_avg, by = c("bics" = "industry")) |>
  left_join(ai_fit, by = "short") |>
  mutate(
    ai_change_pp      = ai_change_fit,                     # MAIN: fitted trend, Sept 2023 to Mar 2026
    ai_change_avg2    = 100 * (ai_aligned2 - ai_start2),   # two-wave averages at each end, to Mar 2026
    ai_change_single  = 100 * (ai_2026 - ai_2023),         # first draft: single waves, to Oct 2026
    ai_change_aligned1 = 100 * (ai_mar26 - ai_2023),       # single waves, aligned to Mar 2026
    ai_change_latest2 = 100 * (ai_last2 - ai_start2))      # smoothed, to Oct 2026
write.csv(panel, "outputs/industry_panel.csv", row.names = FALSE)
p13 <- panel |> filter(!is.na(ai_change_pp))

# ---- 5. Correlations and robustness ------------------------------------------
cor_row <- function(x, y, label, spec) {
  ok <- complete.cases(x, y)
  ct <- cor.test(x[ok], y[ok])
  tibble(spec = spec, comparison = label, n = sum(ok), pearson = unname(ct$estimate),
         ci_low = ct$conf.int[1], ci_high = ct$conf.int[2], p_pearson = ct$p.value,
         spearman = cor(x[ok], y[ok], method = "spearman"),
         p_spearman = suppressWarnings(cor.test(x[ok], y[ok], method = "spearman")$p.value))
}
specs <- tribble(
  ~spec,                                     ~x,                    ~y_post,     ~y_accel,
  "Main: AI fitted trend Sept 2023 to Mar 2026; trend growth",            "ai_change_pp", "g_post", "g_accel",
  "AI Sept/Dec 2023 to Dec 2025/Mar 2026 (2-wave avg); trend growth",      "ai_change_avg2", "g_post", "g_accel",
  "First draft: AI Sept 2023 to Oct 2026 (single waves); end-point growth", "ai_change_single", "g_post_ep", "g_accel_ep",
  "AI Sept 2023 to Mar 2026 (single waves); end-point growth",             "ai_change_aligned1", "g_post_ep", "g_accel_ep",
  "AI to Oct 2026 (2-wave avg); trend growth",                            "ai_change_latest2", "g_post", "g_accel",
  "AI to Mar 2026 (2-wave avg); end-point growth",                        "ai_change_avg2", "g_post_ep", "g_accel_ep",
  "AI fitted trend to Mar 2026; end-point growth",                        "ai_change_pp", "g_post_ep", "g_accel_ep")
cors <- bind_rows(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i, ]
  bind_rows(
    cor_row(p13[[s$x]], p13[[s$y_post]],  "Change in AI use vs productivity growth since 2023 Q3", s$spec),
    cor_row(p13[[s$x]], p13[[s$y_accel]], "Change in AI use vs growth minus 2015-19 trend", s$spec))
}))
ex_re <- p13 |> filter(short != "Real estate")
cors <- bind_rows(cors,
  cor_row(ex_re$ai_change_pp, ex_re$g_post,  "Excl. real estate: AI change vs productivity growth", "Main"),
  cor_row(ex_re$ai_change_pp, ex_re$g_accel, "Excl. real estate: AI change vs growth minus trend", "Main"),
  with(filter(p13, !short %in% c("Education", "Health & social care")),
       cor_row(ai_change_pp, g_post, "Excl. education and health: AI change vs productivity growth", "Main")),
  with(filter(p13, !short %in% c("Education", "Health & social care")),
       cor_row(ai_change_pp, g_accel, "Excl. education and health: AI change vs growth minus trend", "Main")),
  cor_row(100 * p13$extensive_avg, p13$g_post,  "Extensive use (avg waves 159 and 165) vs productivity growth", "Main"),
  cor_row(100 * p13$extensive_avg, p13$g_accel, "Extensive use (avg waves 159 and 165) vs growth minus trend", "Main"),
  with(filter(p13, short != "Info & comms"),
       cor_row(100 * extensive_avg, g_post, "Extensive use vs productivity growth, excl. info & comms", "Main")))
write.csv(cors, "outputs/correlations.csv", row.names = FALSE)

# Leave-one-out on the main and first-draft raw correlations
loo <- bind_rows(lapply(p13$short, function(drop) {
  d <- p13 |> filter(short != drop)
  tibble(dropped = drop,
         r_main = cor(d$ai_change_pp, d$g_post),
         p_main = cor.test(d$ai_change_pp, d$g_post)$p.value,
         r_first_draft = cor(d$ai_change_single, d$g_post_ep),
         p_first_draft = cor.test(d$ai_change_single, d$g_post_ep)$p.value)
}))
write.csv(loo, "outputs/leave_one_out.csv", row.names = FALSE)

# Regressions: pre-trend as a control (instead of forcing a one-for-one subtraction),
# HC3 robust standard errors, and hours-weighted versions
reg_row <- function(fit, label) {
  ct <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
  as_tibble(ct[, , drop = FALSE], rownames = "term") |>
    transmute(model = label, term, estimate = Estimate, se_hc3 = `Std. Error`, p_hc3 = `Pr(>|t|)`)
}
m_raw   <- lm(g_post ~ ai_change_pp, data = p13)
m_ctrl  <- lm(g_post ~ ai_change_pp + g_pre, data = p13)
m_w     <- lm(g_post ~ ai_change_pp, data = p13, weights = hours_2023)
m_wctrl <- lm(g_post ~ ai_change_pp + g_pre, data = p13, weights = hours_2023)
m_first <- lm(g_post_ep ~ ai_change_single, data = p13)
regs <- bind_rows(reg_row(m_raw, "Main: growth on AI change"),
                  reg_row(m_ctrl, "Main: growth on AI change + 2015-19 trend"),
                  reg_row(m_w, "Hours-weighted: growth on AI change"),
                  reg_row(m_wctrl, "Hours-weighted: growth on AI change + 2015-19 trend"),
                  reg_row(m_first, "First draft spec: growth on AI change"))
write.csv(regs, "outputs/regressions.csv", row.names = FALSE)

# Implied size of effect: what the cross-industry slope would mean for the whole economy
ua <- use_all |> filter(start <= fit_end)
ai_all_change <- 100 * unname(coef(lm(ai_use ~ as.numeric(start), data = ua))[2]) * as.numeric(fit_end - fit_start)
slope_ci <- confint(m_raw)[2, ]
implied <- tibble(
  spec = c("Main", "First draft"),
  slope_pp_growth_per_pp_ai = c(coef(m_raw)[2], coef(m_first)[2]),
  ai_change_all_businesses_pp = c(ai_all_change,
                                  100 * (tail(use_all$ai_use, 1) - head(use_all$ai_use, 1))),
  implied_extra_growth_pp_a_year = slope_pp_growth_per_pp_ai * ai_change_all_businesses_pp,
  implied_ci_low  = c(slope_ci[1] * ai_all_change, NA),
  implied_ci_high = c(slope_ci[2] * ai_all_change, NA),
  whole_economy_growth_since_2023Q3 = whole_econ$g_post,
  whole_economy_trend_2015_19 = whole_econ$g_pre)
write.csv(implied, "outputs/implied_effect.csv", row.names = FALSE)

# Single-wave noise in the industry AI series
noise <- use_ind |> filter(!is.na(ai_use)) |> arrange(short, start) |> group_by(short) |>
  summarise(mean_abs_wave_change_pp = 100 * mean(abs(diff(ai_use)), na.rm = TRUE),
            min_pct = 100 * min(ai_use, na.rm = TRUE), max_pct = 100 * max(ai_use, na.rm = TRUE),
            .groups = "drop") |> arrange(desc(mean_abs_wave_change_pp))
write.csv(noise, "outputs/ai_series_noise.csv", row.names = FALSE)

# BICS responses by industry in wave 165 (shows how small some industry samples are)
resp <- read_excel(bics, sheet = "Number of Responses", skip = 5, n_max = 14) |>
  select(industry = 1, responses = Total) |> mutate(industry = str_trim(industry))
write.csv(resp, "outputs/bics_responses_by_industry_w165.csv", row.names = FALSE)

# ---- 6. Charts ---------------------------------------------------------------
# RG Economics style: navy and grey, no gridlines, even black box, labels next to lines
navy <- "#051542"; grey <- "#b8bec7"; dgrey <- "#6b7280"; ink <- "#000000"
fam <- if ("Poppins" %in% systemfonts::system_fonts()$family) "Poppins" else "sans"
theme_rg <- theme_classic(base_size = 14, base_family = fam) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = ink, linewidth = 0.7),
        panel.grid = element_blank(), axis.line = element_blank(),
        axis.text = element_text(colour = ink), axis.ticks = element_line(colour = ink, linewidth = 0.7),
        strip.background = element_blank(), strip.text = element_text(face = "bold", colour = ink),
        plot.title = element_text(face = "bold", colour = navy, size = 20),
        plot.subtitle = element_text(colour = ink, size = 13),
        plot.caption = element_text(colour = ink, size = 10, hjust = 0),
        legend.position = "none", plot.margin = margin(10, 12, 10, 10))
src_bics <- "Source: ONS Business Insights and Conditions Survey, wave 165. RG Economics analysis."

# Save chart artwork for a website whose figure box supplies the title and source.
# Standalone copies keep those labels for readers who download the image.
save_web_chart <- function(filename, plot, ..., bg = "white") {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  metadata <- list(file = basename(filename), title = plot$labels$title,
                   subtitle = plot$labels$subtitle, source = plot$labels$caption)
  jsonlite::write_json(metadata, paste0(filename, ".caption.json"), auto_unbox = TRUE, pretty = TRUE)
  standalone <- file.path(dirname(filename), "standalone", basename(filename))
  dir.create(dirname(standalone), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(standalone, plot = plot, ..., bg = bg)
  artwork <- plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL) +
    ggplot2::theme(plot.title = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_blank())
  ggplot2::ggsave(filename, plot = artwork, ..., bg = bg)
}

# Chart 1: AI use has roughly tripled
end_lab <- bind_rows(tail(use_all, 1) |> mutate(lab = "All businesses", col = navy),
                     tail(use_10up, 1) |> mutate(lab = "10+ employees", col = navy))
p1 <- ggplot(use_ind, aes(start, ai_use, group = short)) +
  geom_line(colour = grey) +
  geom_line(data = use_10up, aes(start, ai_use), inherit.aes = FALSE, colour = navy, linewidth = 1, linetype = "22") +
  geom_line(data = use_all, aes(start, ai_use), inherit.aes = FALSE, colour = navy, linewidth = 1.3) +
  geom_text_repel(data = use_ind |> filter(start == max(start), short %in% c("Info & comms", "Professional services", "Construction")),
                  aes(label = short), size = 3.2, nudge_x = 65, seed = 42, min.segment.length = 0,
                  direction = "y", hjust = 0, colour = dgrey, family = fam) +
  geom_text(data = end_lab, aes(start, ai_use, label = lab), inherit.aes = FALSE,
            colour = navy, fontface = "bold", hjust = -0.1, size = 3.4, family = fam) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(breaks = as.Date(c("2024-01-01", "2025-01-01", "2026-01-01")), date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.25))) +
  labs(title = "AI use has roughly tripled since 2023",
       subtitle = "Share of businesses using at least one AI technology (grey lines: industries)",
       x = NULL, y = NULL,
       caption = paste0(src_bics, "\nGaps in grey lines are survey waves where ONS suppressed the figure.")) + theme_rg
save_web_chart("outputs/chart1_ai_use_over_time.png", p1, width = 8, height = 5, dpi = 200, bg = "white")

# Chart 2: but few use it extensively or train staff
d2 <- depth |> filter(!is.na(extensive)) |>
  select(short, `Use AI extensively` = extensive, `At least half of staff trained` = trained_half_plus) |>
  pivot_longer(-short) |>
  mutate(name = factor(name, levels = c("Use AI extensively", "At least half of staff trained")),
         short = reorder(short, ifelse(name == "Use AI extensively", value, 0), sum),
         lab = ifelse(is.na(value), "n/a", percent(value, accuracy = 1)),
         value0 = ifelse(is.na(value), 0, value))
p2 <- ggplot(d2, aes(value0, short, fill = short == "All businesses")) +
  geom_col(width = 0.65) + facet_wrap(~name) +
  geom_text(aes(label = lab), hjust = -0.12, size = 3.8, colour = ink, family = fam) +
  scale_fill_manual(values = c(`TRUE` = navy, `FALSE` = grey)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Most firms that use AI only dabble",
       subtitle = "Share of firms using AI or unsure whether they do, September to October 2026",
       x = NULL, y = NULL, caption = paste0(src_bics, "\nn/a = figure suppressed by ONS. Industries where both figures are suppressed are not shown.")) + theme_rg
save_web_chart("outputs/chart2_depth_and_training.png", p2, width = 9, height = 5.5, dpi = 200, bg = "white")

# Chart 3: rise in AI use vs productivity growth, main specification
d3 <- p13 |>
  select(short, ai_change_pp,
         `Growth since 2023 Q3` = g_post,
         `Growth since 2023 Q3 minus 2015-19 trend` = g_accel) |>
  pivot_longer(-c(short, ai_change_pp), names_to = "measure", values_to = "growth")
lab3 <- d3 |> group_by(measure) |>
  summarise(r = cor(ai_change_pp, growth), p = cor.test(ai_change_pp, growth)$p.value, .groups = "drop") |>
  mutate(label = sprintf("r = %.2f (p = %.2f)", r, p))
p3 <- ggplot(d3, aes(ai_change_pp, growth)) +
  geom_hline(yintercept = 0, colour = grey) +
  geom_smooth(method = "lm", se = TRUE, colour = navy, fill = "grey88", formula = y ~ x) +
  geom_point(size = 2.3, colour = navy) +
  geom_text_repel(aes(label = short), size = 2.8, colour = ink, seed = 7, max.overlaps = Inf, family = fam,
                  box.padding = 0.5, point.padding = 0.3, min.segment.length = 0.2, segment.colour = grey) +
  geom_text(data = lab3, aes(x = -Inf, y = Inf, label = label), hjust = -0.1, vjust = 1.5,
            inherit.aes = FALSE, size = 3.5, fontface = "bold", family = fam) +
  facet_wrap(~measure) +
  labs(title = "No clear link yet between AI take-up and productivity",
       subtitle = sprintf("Each dot is an industry (n = %d). AI change measured to March 2026 to match the productivity data", nrow(p13)),
       x = "Change in share of firms using AI, Sept 2023 to Mar 2026 (percentage points, fitted trend)",
       y = "Output per hour, % a year",
       caption = paste("Sources: ONS BICS wave 165; ONS Output per hour worked, UK. RG Economics analysis.",
                       "\nAI change is a trend fitted through every survey from Sept 2023 to Mar 2026. Productivity is trend growth to 2026 Q1.",
                       "\nIndustry estimates; correlation only. Excludes finance, public administration, agriculture, energy and water.")) +
  theme_rg
save_web_chart("outputs/chart3_ai_vs_productivity.png", p3, width = 10, height = 5.8, dpi = 200, bg = "white")

# ---- 7. Professional services detail (FOI) -----------------------------------
foi <- read_excel("data_raw/foi_ai_sic.xlsx", sheet = "AI Current Usage TS (WTD)", skip = 4) |>
  mutate(ai_use = 1 - to_num(`Business does not currently use artificial intelligence technologies`) - to_num(`Not sure`),
         llm = to_num(`Text generation using Large Language Models`)) |>
  select(Dates, Wave, Industry, ai_use, llm)
write.csv(foi, "outputs/foi_professional_services_ai_use.csv", row.names = FALSE)

# ---- 8. Print key results ----------------------------------------------------
cat("\nLatest productivity quarter:", latest_q, "\n")
cat("AI waves used. Start:", first_two, "| Aligned end:", aligned_two, "| Latest:", last_two, "\n")
cat("\nHeadline AI use (first and last wave):\n"); print(headline |> group_by(group) |> slice(c(1, n())), n = 10)
cat("\nCorrelations:\n"); print(cors |> select(spec, comparison, n, pearson, p_pearson, spearman), n = 30, width = 250)
cat("\nRegressions (HC3):\n"); print(regs, n = 30, width = 200)
cat("\nLeave-one-out:\n"); print(loo, n = 20)
cat("\nImplied effect:\n"); print(implied, width = 200)
cat("\nWhole economy:\n"); print(aggregate, width = 200)
cat("\nRange of r across specifications (all 13 industries):", round(range(cors$pearson[cors$n == 13]), 2), "\n")
print(panel |> select(short, ai_start2, ai_aligned2, ai_change_pp, ai_change_avg2, ai_change_single, g_pre, g_post, g_accel,
                      g_post_ep, hours_2023, extensive_avg) |> arrange(desc(ai_change_pp)), n = 20, width = 250)
