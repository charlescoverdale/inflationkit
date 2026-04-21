# Figure generator for the inflationkit R Journal paper.
#
# Produces five PDF figures and one LaTeX table under paper/figures/ and
# paper/tables/. Run from the package root with:
#   RSTUDIO_PANDOC=/Applications/quarto/bin/tools Rscript paper/make_figures.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
  library(showtext)
  library(scales)
})

font_add("HelveticaNeue",
         regular = "/System/Library/Fonts/Helvetica.ttc",
         bold = "/System/Library/Fonts/Helvetica.ttc",
         italic = "/System/Library/Fonts/Helvetica.ttc")
showtext_auto()
showtext_opts(dpi = 300)

fig_dir <- "paper/figures"
tab_dir <- "paper/tables"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

# Okabe-Ito palette
ok_blue   <- "#0072B2"
ok_orange <- "#E69F00"
ok_green  <- "#009E73"
ok_red    <- "#D55E00"
ok_purple <- "#CC79A7"
ok_yellow <- "#F0E442"
ok_sky    <- "#56B4E9"
ok_grey   <- "#999999"

fam <- "HelveticaNeue"

theme_wp <- function(base_size = 10) {
  theme_bw(base_size = base_size, base_family = fam) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      axis.line = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks.length = unit(2.5, "pt"),
      axis.text = element_text(size = base_size, colour = "grey20"),
      axis.title = element_text(size = base_size, colour = "grey20"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, family = fam),
      legend.key.height = unit(10, "pt"),
      legend.key.width = unit(22, "pt"),
      legend.spacing.x = unit(10, "pt"),
      legend.margin = margin(4, 0, 0, 0),
      plot.margin = margin(6, 10, 6, 6)
    )
}

tex_esc <- function(x) gsub("_", "\\\\_", as.character(x))

# 12-month rolling mean helper; leaves NAs at the start.
rollmean12 <- function(x) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in 12:n) out[i] <- mean(x[(i - 11):i], na.rm = TRUE)
  out
}

# -----------------------------------------------------------------------------
# Build a 10-year monthly CPI panel with a pandemic-era inflation spike.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_months <- 120
dates <- seq(as.Date("2015-01-01"), by = "month", length.out = n_months)

# Six components (fewer than 10) for a readable stacked-area figure.
# Weights sum to 1.
items <- c("Food", "Housing", "Transport", "Energy", "Services", "Other")
weights <- c(Food = 0.15, Housing = 0.30, Transport = 0.12,
             Energy = 0.08, Services = 0.20, Other = 0.15)

# Pandemic shock kernel: zero in normal times, positive bump
# Apr-2021 through Dec-2023.
shock_kernel <- rep(0, n_months)
shock_start <- which(format(dates, "%Y-%m") == "2021-04")
shock_peak  <- which(format(dates, "%Y-%m") == "2022-06")
shock_end   <- which(format(dates, "%Y-%m") == "2023-12")
for (t in seq_len(n_months)) {
  if (t >= shock_start && t <= shock_peak) {
    shock_kernel[t] <- 0.005 *
      (t - shock_start + 1) / (shock_peak - shock_start + 1)
  } else if (t > shock_peak && t <= shock_end) {
    shock_kernel[t] <- 0.005 *
      (1 - (t - shock_peak) / (shock_end - shock_peak))
  }
}

# Component-specific sensitivity to the pandemic shock.
shock_sensitivity <- c(Food = 1.0, Housing = 0.5, Transport = 1.8,
                       Energy = 3.0, Services = 0.4, Other = 0.3)

panel_rows <- vector("list", n_months * length(items))
k <- 0
for (i in seq_along(items)) {
  it <- items[i]
  base_mean <- 0.0015  # 1.8% baseline annualised
  base_sd   <- 0.0030
  for (t in seq_len(n_months)) {
    k <- k + 1
    price_change <- base_mean +
                    shock_sensitivity[it] * shock_kernel[t] +
                    rnorm(1, 0, base_sd)
    panel_rows[[k]] <- data.frame(
      date = dates[t], item = it, weight = weights[it],
      price_change = price_change
    )
  }
}
panel <- do.call(rbind, panel_rows)
panel$item <- factor(panel$item, levels = items)

dec <- ik_decompose(panel)

# Headline YoY-equivalent: smooth 12-month average of monthly, annualised.
hd <- dec$headline
hd$headline_smooth_pct <- 100 * 12 * rollmean12(hd$headline_inflation)

# -----------------------------------------------------------------------------
# Figure 1: headline, core, and HP trend (smoothed, annualised).
# -----------------------------------------------------------------------------
core_tm <- ik_core(panel, method = "trimmed_mean", trim = 0.08)
tr_hp   <- ik_trend(hd$headline_inflation, method = "hp", lambda = 14400)

df1 <- data.frame(
  date = hd$date,
  Headline = 100 * 12 * rollmean12(hd$headline_inflation),
  `Trimmed mean` = 100 * 12 * rollmean12(core_tm$core$core_inflation),
  `HP trend` = 100 * 12 * tr_hp$trend,
  check.names = FALSE
)
df1_long <- data.frame(
  date = rep(df1$date, 3),
  value = c(df1$Headline, df1$`Trimmed mean`, df1$`HP trend`),
  series = factor(
    rep(c("Headline", "Trimmed mean", "HP trend"), each = nrow(df1)),
    levels = c("Headline", "Trimmed mean", "HP trend"))
)

shock_rect <- data.frame(
  xmin = dates[shock_start], xmax = dates[shock_end],
  ymin = -Inf, ymax = Inf
)

p1 <- ggplot() +
  geom_rect(data = shock_rect,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey92") +
  geom_hline(yintercept = 2, linewidth = 0.3, colour = "grey50",
             linetype = "dashed") +
  geom_line(data = df1_long,
            aes(x = date, y = value,
                colour = series, linewidth = series,
                linetype = series),
            na.rm = TRUE) +
  scale_colour_manual(values = c("Headline" = ok_grey,
                                 "Trimmed mean" = ok_blue,
                                 "HP trend" = ok_red)) +
  scale_linewidth_manual(
    values = c("Headline" = 0.6,
               "Trimmed mean" = 0.8,
               "HP trend" = 0.9),
    guide = "none") +
  scale_linetype_manual(
    values = c("Headline" = "solid",
               "Trimmed mean" = "solid",
               "HP trend" = "longdash")) +
  scale_x_date(expand = c(0, 0), date_breaks = "2 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.1))) +
  labs(x = NULL, y = "Annualised inflation, 12-month average") +
  guides(colour = guide_legend(nrow = 1,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig1_headline_core_trend.pdf"),
       p1, width = 5.5, height = 3.2, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 2: stacked-area decomposition using 12-month rolling contributions.
# -----------------------------------------------------------------------------
# Smooth contributions per item.
cn <- dec$contributions
cn$contribution_pct <- 100 * 12 * cn$contribution
cn_smooth <- do.call(rbind, lapply(split(cn, cn$item), function(sub) {
  sub <- sub[order(sub$date), ]
  sub$contribution_pct_smooth <- rollmean12(sub$contribution_pct)
  sub
}))
# Sort items by mean contribution (large at bottom) for clean stacking.
item_means <- tapply(cn_smooth$contribution_pct_smooth,
                     cn_smooth$item, mean, na.rm = TRUE)
stack_order <- names(sort(item_means, decreasing = TRUE))
cn_smooth$item <- factor(cn_smooth$item, levels = stack_order)

fig2_palette <- c(ok_blue, ok_orange, ok_red, ok_green, ok_purple, ok_sky)
names(fig2_palette) <- stack_order

p2 <- ggplot(cn_smooth,
             aes(x = date, y = contribution_pct_smooth, fill = item)) +
  geom_area(alpha = 0.9) +
  geom_hline(yintercept = 2, linewidth = 0.3, colour = "grey40",
             linetype = "dashed") +
  scale_fill_manual(values = fig2_palette) +
  scale_x_date(expand = c(0, 0), date_breaks = "2 years",
               date_labels = "%Y",
               limits = c(dates[12], dates[n_months])) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Contribution to annualised headline, 12m average") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig2_decompose.pdf"),
       p2, width = 5.5, height = 3.4, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 3: core-measure comparison (3 methods + headline, smoothed).
# -----------------------------------------------------------------------------
core_wm   <- ik_core(panel, method = "weighted_median")
core_ex   <- ik_core(panel, method = "exclusion",
                     exclude = c("Food", "Energy"))

to_smooth_pct <- function(df, label) {
  data.frame(
    date = df$core$date,
    value = 100 * 12 * rollmean12(df$core$core_inflation),
    series = label
  )
}

df3 <- rbind(
  data.frame(date = hd$date,
             value = 100 * 12 * rollmean12(hd$headline_inflation),
             series = "Headline"),
  to_smooth_pct(core_tm, "Trimmed mean (8%)"),
  to_smooth_pct(core_wm, "Weighted median"),
  to_smooth_pct(core_ex, "Exclusion (food+energy)")
)
df3$series <- factor(df3$series,
                     levels = c("Headline",
                                "Trimmed mean (8%)",
                                "Weighted median",
                                "Exclusion (food+energy)"))

p3 <- ggplot(df3, aes(x = date, y = value,
                      colour = series,
                      linewidth = series,
                      linetype = series)) +
  geom_line(na.rm = TRUE) +
  scale_colour_manual(
    values = c("Headline" = ok_grey,
               "Trimmed mean (8%)" = ok_blue,
               "Weighted median" = ok_red,
               "Exclusion (food+energy)" = ok_green)) +
  scale_linewidth_manual(
    values = c("Headline" = 0.55,
               "Trimmed mean (8%)" = 0.8,
               "Weighted median" = 0.8,
               "Exclusion (food+energy)" = 0.8),
    guide = "none") +
  scale_linetype_manual(
    values = c("Headline" = "solid",
               "Trimmed mean (8%)" = "solid",
               "Weighted median" = "longdash",
               "Exclusion (food+energy)" = "dotted")) +
  scale_x_date(expand = c(0, 0), date_breaks = "2 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Annualised inflation, 12-month average") +
  guides(colour = guide_legend(nrow = 2,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 2)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig3_core_comparison.pdf"),
       p3, width = 5.5, height = 3.2, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 4: Phillips curve with time-coloured points.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_q <- 40
q_dates <- seq(as.Date("2015-01-01"), by = "quarter", length.out = n_q)
base_cycle <- 0.5 * sin(seq(0, 3*pi, length.out = n_q))
pandemic_idx <- which(q_dates >= as.Date("2020-04-01") &
                      q_dates <= as.Date("2022-12-01"))
pandemic_impulse <- rep(0, n_q)
pandemic_impulse[pandemic_idx] <- c(-3, -2, -1, 0, 0.5, 1.5, 2.5,
                                    2.0, 1.0, 0.5, 0)[seq_along(pandemic_idx)]
output_gap <- base_cycle + pandemic_impulse + rnorm(n_q, 0, 0.3)
infl_pct <- 2 + 0.45 * output_gap + rnorm(n_q, 0, 0.6)
ph <- ik_phillips(infl_pct / 100, output_gap, type = "traditional")

df4 <- data.frame(
  output_gap = output_gap, infl = infl_pct,
  year = as.numeric(format(q_dates, "%Y"))
)

p4 <- ggplot(df4, aes(x = output_gap, y = infl)) +
  geom_hline(yintercept = 2, linewidth = 0.3, colour = "grey70",
             linetype = "dashed") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70",
             linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE,
              colour = ok_red, fill = "grey85",
              linewidth = 0.5, alpha = 0.4) +
  geom_point(aes(fill = year),
             shape = 21, colour = "white", size = 2.6,
             stroke = 0.3, alpha = 0.95) +
  scale_fill_gradient(low = ok_sky, high = ok_red,
                      breaks = c(2016, 2020, 2024),
                      name = "Year") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Output gap (percentage points)",
       y = "Quarterly inflation") +
  guides(fill = guide_colourbar(barwidth = 9, barheight = 0.5,
                                title.position = "left",
                                title.vjust = 0.9)) +
  theme_wp(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 9, family = fam,
                                     colour = "grey20"),
        legend.direction = "horizontal")

ggsave(file.path(fig_dir, "fig4_phillips.pdf"),
       p4, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig4: slope = %.3f, R^2 = %.3f\n",
            ph$slope_estimate * 100, ph$r_squared))

# -----------------------------------------------------------------------------
# Figure 5: Mincer-Zarnowitz scatter.
# -----------------------------------------------------------------------------
set.seed(20260418)
realised <- df1$Headline
valid_idx <- which(!is.na(realised))
# Construct a less-noisy biased forecast: attenuated + drift.
fc <- 0.5 * realised[valid_idx] + 1.0 + rnorm(length(valid_idx), 0, 0.4)
y <- realised[valid_idx]

fe <- ik_forecast_eval(actual = y / 100, forecast = fc / 100)
alpha <- fe$coefficients["alpha"] * 100
beta  <- fe$coefficients["beta"]

df5 <- data.frame(forecast = fc, realised = y)

# Place annotation in a corner where there are no points, using a
# white-boxed label for legibility over the gridlines.
label_text <- sprintf(
  "Mincer-Zarnowitz\nalpha = %.2f%%  (bias)\nbeta  = %.2f    (slope)\np = %.2g",
  alpha, beta, fe$p_value)

# Put annotation in top-left corner where no points or fit-line sit.
xr <- range(df5$forecast); yr <- range(df5$realised)
ann_x <- xr[1] + 0.02 * diff(xr)
ann_y <- yr[2] - 0.02 * diff(yr)

p5 <- ggplot(df5, aes(x = forecast, y = realised)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.35,
              colour = "grey55", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE,
              colour = ok_red, fill = "grey85",
              linewidth = 0.6, alpha = 0.4) +
  geom_point(colour = ok_blue, size = 2.0, alpha = 0.85) +
  annotate("label",
           x = ann_x, y = ann_y, label = label_text,
           family = fam, size = 3.0, colour = "grey20",
           fill = "white", label.size = 0.25,
           hjust = 0, vjust = 1, lineheight = 1.05) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Forecast", y = "Realised inflation (12m average)") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig5_mz.pdf"),
       p5, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig5: alpha = %.3f, beta = %.3f, p = %.3g\n",
            alpha, beta, fe$p_value))

# -----------------------------------------------------------------------------
# Figure 6: real US CPI data from FRED, headline vs core vs HP trend.
# -----------------------------------------------------------------------------
# Read pre-downloaded CSVs to keep the figure script fully reproducible
# without requiring a network call at render time.
cpi <- read.csv("paper/data/CPIAUCSL.csv")
core_cpi <- read.csv("paper/data/CPILFESL.csv")
cpi$date <- as.Date(cpi$observation_date)
core_cpi$date <- as.Date(core_cpi$observation_date)
# Restrict to 2015 onwards to match the synthetic window.
cpi <- cpi[cpi$date >= as.Date("2014-12-01") &
           cpi$date <= as.Date("2026-03-01"), ]
core_cpi <- core_cpi[core_cpi$date >= as.Date("2014-12-01") &
                     core_cpi$date <= as.Date("2026-03-01"), ]

# Year-over-year percent change.
yoy_pct <- function(index) {
  n <- length(index)
  out <- rep(NA_real_, n)
  out[13:n] <- 100 * (index[13:n] / index[1:(n - 12)] - 1)
  out
}

cpi$yoy      <- yoy_pct(cpi$CPIAUCSL)
core_cpi$yoy <- yoy_pct(core_cpi$CPILFESL)

# HP trend on the YoY headline, monthly lambda 14400.
hp_real <- ik_trend(cpi$yoy[!is.na(cpi$yoy)] / 100,
                    method = "hp", lambda = 14400)
cpi_valid <- cpi[!is.na(cpi$yoy), ]
cpi_valid$hp_trend <- 100 * hp_real$trend

# Persistence on the YoY inflation series.
persist <- ik_persistence(cpi_valid$yoy, method = "sum_ar", ic = "bic")

df6 <- rbind(
  data.frame(date = cpi_valid$date,
             value = cpi_valid$yoy, series = "Headline YoY"),
  data.frame(date = core_cpi$date[!is.na(core_cpi$yoy)],
             value = core_cpi$yoy[!is.na(core_cpi$yoy)],
             series = "Core YoY (ex food+energy)"),
  data.frame(date = cpi_valid$date,
             value = cpi_valid$hp_trend, series = "HP trend")
)
df6$series <- factor(df6$series,
                     levels = c("Headline YoY",
                                "Core YoY (ex food+energy)",
                                "HP trend"))

pandemic_real <- data.frame(
  xmin = as.Date("2021-04-01"),
  xmax = as.Date("2023-12-01"),
  ymin = -Inf, ymax = Inf
)

p6 <- ggplot() +
  geom_rect(data = pandemic_real,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey92") +
  geom_hline(yintercept = 2, linewidth = 0.3, colour = "grey50",
             linetype = "dashed") +
  geom_line(data = df6,
            aes(x = date, y = value,
                colour = series, linewidth = series,
                linetype = series)) +
  scale_colour_manual(values = c("Headline YoY" = ok_grey,
                                 "Core YoY (ex food+energy)" = ok_blue,
                                 "HP trend" = ok_red)) +
  scale_linewidth_manual(
    values = c("Headline YoY" = 0.55,
               "Core YoY (ex food+energy)" = 0.85,
               "HP trend" = 0.9),
    guide = "none") +
  scale_linetype_manual(
    values = c("Headline YoY" = "solid",
               "Core YoY (ex food+energy)" = "solid",
               "HP trend" = "longdash")) +
  scale_x_date(expand = c(0, 0), date_breaks = "2 years",
               date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.1))) +
  labs(x = NULL, y = "Annualised inflation (year-over-year)") +
  guides(colour = guide_legend(nrow = 1,
                               override.aes = list(linewidth = 0.8)),
         linetype = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig6_fred.pdf"),
       p6, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig6: sum-AR persistence = %.3f (ar_order = %d)\n",
            persist$value, persist$ar_order))

# -----------------------------------------------------------------------------
# Table: custom comparison of 4 core measures + headline.
# -----------------------------------------------------------------------------
core_asym <- ik_core(panel, method = "asymmetric_trim",
                     trim_lower = 0.08, trim_upper = 0.16)
methods <- list(
  `Trimmed mean (8%)` = core_tm,
  `Weighted median` = core_wm,
  `Exclusion (food+energy)` = core_ex,
  `Asymmetric trim (8/16)` = core_asym
)

tab_lines <- c(
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Core measure & Mean & SD & Pandemic-peak max & Correlation with headline \\\\",
  "\\midrule"
)
headline_vec <- 100 * 12 * hd$headline_inflation
for (nm in names(methods)) {
  m <- methods[[nm]]
  vals <- 100 * 12 * m$core$core_inflation
  tab_lines <- c(tab_lines,
    sprintf("%s & %.2f & %.2f & %.2f & %.3f \\\\",
            tex_esc(nm),
            mean(vals, na.rm = TRUE),
            sd(vals, na.rm = TRUE),
            max(vals, na.rm = TRUE),
            cor(vals, headline_vec, use = "complete.obs")))
}
tab_lines <- c(tab_lines, "\\midrule",
  sprintf("Headline & %.2f & %.2f & %.2f & %.3f \\\\",
          mean(headline_vec), sd(headline_vec),
          max(headline_vec), 1.000))
tab_lines <- c(tab_lines, "\\bottomrule", "\\end{tabular}")
writeLines(tab_lines, file.path(tab_dir, "compare.tex"))

cat("\n--- done ---\n")
