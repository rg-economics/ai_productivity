# AI adoption and productivity by industry (UK, 2023 to 2026)

RG Economics blog analysis for "Is AI showing up in UK productivity yet?" (November 2026).

## How to run
Set the working directory to this folder and run `ai_adoption_productivity.R`. It reads the files in `data_raw/` and writes everything to `outputs/`.

R packages: readxl, dplyr, tidyr, ggplot2, stringr, scales, ggrepel, sandwich, lmtest, jsonlite, systemfonts.

## Files
- `data_raw/` holds the ONS source files (all public, Open Government Licence):
  - `bics_w165.xlsx`: Business Insights and Conditions Survey, wave 165 (AI questions September 2023 to October 2026)
  - `oph_by_section.xlsx`: Output per hour worked, UK (data to 2026 Q1). Table 24 is output per hour and Table 23 is hours worked.
  - `foi_ai_sic.xlsx`: FOI-2026-3675 (AI use among solicitors, accountants and consultancies)
- `outputs/` holds the charts and tidy CSVs. `outputs/standalone/` has chart copies with titles and sources.

## Main findings
1. **AI use has roughly tripled.** Across all businesses it rose from 9% (September 2023) to 30% (September to October 2026). For firms with 10 or more staff it rose from 12% to 38%.
2. **Use is shallow.** Of firms using AI or unsure whether they do, only 15% use it extensively and only 20% have trained at least half their staff.
3. **No clear link to productivity yet.** Across 13 industries the correlation between the rise in AI use and growth in output per hour is 0.10 (p = 0.75). Against each industry's 2015 to 2019 trend it is -0.02 (p = 0.94). No robustness check gives a significant result.

## Method
- **AI use** = 1 minus "does not use AI" minus "not sure". This matches the ONS headline definition. BICS weights by number of businesses, not employment.
- **AI change (main)** = average of waves 147 and 153 (December 2025 and March 2026) minus the average of waves 92 and 98 (September and December 2023). The end point matches the productivity data, which stop in 2026 Q1. Averaging two waves reduces sampling noise. Industry samples are small (for instance, 103 real estate responses in wave 165).
- **Productivity growth** = trend growth in output per hour (chained volume index), from a log-linear fit through every quarter from 2023 Q3 to 2026 Q1. The pre-pandemic trend is fitted the same way over 2015 Q1 to 2019 Q4.
- **Robustness** (see `outputs/`):
  - `correlations.csv`: alternative windows and smoothing, excluding real estate, and extensive use.
  - `leave_one_out.csv`: drops each industry in turn.
  - `regressions.csv`: pre-trend as a control, hours-weighted versions and HC3 robust standard errors.
  - `implied_effect.csv`: what each slope would imply for the whole economy.
  - `ai_series_noise.csv`: average wave-to-wave movement in each industry's AI share.

## Note on an earlier draft
An earlier version compared single waves (September 2023 and October 2026) with growth between two single quarters. It found r = 0.57 (p = 0.04). That result relied on AI data from after the productivity data end and on noisy single-wave readings. It is still reported in `correlations.csv` and `leave_one_out.csv` as "First draft" for transparency.

## Caveats
- BICS industry figures cover all firm sizes. They exclude finance, public administration, agriculture and energy. Water and waste is fully suppressed.
- AI use is self-reported and counts any use of one or more AI technologies.
- With 13 industries this is descriptive and not causal. Industry averages can hide very different firms.
- Extensive use is published for only five or six industries, which is too few to test.
