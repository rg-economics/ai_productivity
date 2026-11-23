# AI adoption and productivity by industry (UK, 2023 to 2026)

RG Economics blog analysis for "Is AI showing up in UK productivity yet?" (November 2026).

## How to run
Set the working directory to this folder and run `ai_adoption_productivity.R`. It reads the files in `data_raw/` and writes everything to `outputs/`.

R packages: readxl, dplyr, tidyr, ggplot2, stringr, scales, ggrepel, sandwich, lmtest, jsonlite, systemfonts.

## Files
- `data_raw/` holds the ONS source files (all public, Open Government Licence):
  - `bics_w165.xlsx`: Business Insights and Conditions Survey, wave 165 (AI questions September 2023 to October 2026).
  - `oph_by_section.xlsx`: Output per hour worked, UK, 18 August 2026 release (data to 2026 Q1). Table 24 is output per hour and Table 23 is hours worked. This is the latest release with industry figures: the 17 November 2026 release moved to a new method (the "component approach") and covers the whole economy only for now.
  - `oph_whole_economy_nov2026.xlsx`: Output per hour worked, UK, 17 November 2026 release (whole economy, new method, data to 2026 Q3).
  - `foi_ai_sic.xlsx`: FOI-2026-3675 (AI use among solicitors, accountants and consultancies).
- `outputs/` holds the charts and tidy CSVs. `outputs/standalone/` has chart copies with titles and sources.

## Main findings
1. **AI use has roughly tripled.** Across all businesses it rose from 9% (September 2023) to 30% (September to October 2026). For firms with 10 or more staff it rose from 12% to 38%.
2. **Use is shallow.** Of firms using AI or unsure whether they do, only 15% use it extensively and only 20% have trained at least half their staff.
3. **The industry data are too noisy to tell yet.** Across 13 industries the correlation between the rise in AI use and growth in output per hour is 0.25 (p = 0.41). Against each industry's 2015 to 2019 trend it is 0.15 (p = 0.62). The 95% confidence interval for the first runs from -0.35 to 0.70, so the data cannot rule out either no effect or a sizeable one. Across reasonable alternative specifications r ranges from -0.27 to 0.57.
4. **Whole economy.** On the old (August) basis, output per hour fell by about 0.3% a year from 2023 Q3 to 2026 Q1, against 1.0% a year in 2015 to 2019. On ONS's new (November) basis it grew by about 1.6% a year from 2023 Q3 to 2026 Q3, against 1.7% a year in 2015 to 2019. The industry figures may be revised substantially when they move to the new method.

## Method
- **AI use** = 1 minus "does not use AI" minus "not sure". This matches the ONS headline definition. BICS weights by number of businesses, not employment.
- **AI change (main)** = the change in each industry's AI share between September 2023 and March 2026, from a straight-line trend fitted through every AI survey wave in that window (9 to 11 waves per industry; suppressed waves skipped). The end point matches the productivity data, which stop in 2026 Q1. Using every wave reduces the noise in single readings, since industry samples are small (for instance, 103 real estate responses in wave 165).
- **Productivity growth** = trend growth in output per hour (chained volume index), from a log-linear fit through every quarter from 2023 Q3 to 2026 Q1. The pre-pandemic trend is fitted the same way over 2015 Q1 to 2019 Q4.
- **Robustness** (see `outputs/`):
  - `correlations.csv`: alternative windows and smoothing (including two-wave averages at each end), excluding real estate, excluding education and health, and extensive use.
  - `leave_one_out.csv`: drops each industry in turn.
  - `regressions.csv`: pre-trend as a control, hours-weighted versions and HC3 robust standard errors.
  - `implied_effect.csv`: what each slope would imply for the whole economy, with a confidence interval.
  - `whole_economy_productivity.csv`: whole-economy growth on the old and new ONS bases.
  - `ai_series_noise.csv`: average wave-to-wave movement in each industry's AI share.

## Note on earlier versions
- The first draft compared single waves (September 2023 and October 2026) with growth between two single quarters. It found r = 0.57 (p = 0.04). That result relied on AI data from after the productivity data end and on noisy single-wave readings.
- A second version used two-wave averages at each end (r = 0.10). This made the result depend heavily on which waves were averaged.
- Both are still reported in `correlations.csv` for transparency.

## Caveats
- BICS industry figures cover all firm sizes. They exclude finance, public administration, agriculture and energy. Water and waste is fully suppressed.
- AI use is self-reported and counts any use of one or more AI technologies. Employment-weighted surveys find much higher use (for instance, 88% in the UK Decision Maker Panel).
- Industry hours come from the Labour Force Survey, whose quality has been weak since 2023. The industry productivity data are official statistics in development.
- With 13 industries this is descriptive and not causal. Industry averages can hide very different firms. About 2.5 years of data is an early read: past technologies took years to show up in productivity.
- Extensive use is published for only five or six industries, which is too few to test.
