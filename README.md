# NFL Late-Game Decision Support Tool

An interactive Shiny application for analyzing offensive play-calling decisions in the final two minutes of an NFL half or game. Built using 10 seasons of NFL play-by-play data (2015–2025), the tool allows coaches, analysts, and fans to compare the historical effectiveness of different play types under specific late-game conditions.

---

## Overview

Late-game clock management is one of the most consequential — and least data-driven — decisions in football. This tool provides a historical decision-support framework that answers the question: *given this game situation, what has worked before?*

The application is organized into three tabs:
- **How-To + Assumptions** — methodology, data scope, and metric definitions
- **Situation Simulator** — compare play types by historical scoring probability, EPA, and clock consumption
- **Historical Evidence** — explore the actual plays and drives behind the simulator results

---

## Data

- **Source:** [`nflreadr`](https://nflreadr.nflverse.com/) R package — NFL public play-by-play data
- **Seasons:** 2015–2025
- **Filtered dataset:** 51,477 plays occurring in the final two minutes of a half or game
- **Play types included:** Run, Pass, Spike, Kneel, Field Goal Attempt

Raw data is not included in this repo. To reproduce the dataset, run `chee_final_project_data_process.Rmd` with `nflreadr` installed.

---

## Methodology

This project uses a **conditional probability framework** rather than a predictive ML model. For each combination of situation type, time bucket, score differential, field position, and next play choice, historical drive outcomes are aggregated to estimate:

| Metric | Description |
|--------|-------------|
| `p_any_score` | Probability the drive results in any score |
| `p_td` | Probability the drive results in a touchdown |
| `p_fg` | Probability the drive results in a field goal |
| `p_turnover` | Probability the drive ends in a turnover |
| `avg_epa` | Average Expected Points Added for the play |
| `avg_sec_elapsed` | Average clock time consumed by the play |

**Why not ML?** Late-game scenarios are relatively rare, and some situation combinations have sparse observations. A historical frequency approach provides interpretable, reliable results without overfitting risk.

---

## Situation Filters

| Filter | Options |
|--------|---------|
| Situation Type | End of Half, End of Game |
| Time Remaining | 0–30s, 31–60s, 61–90s, 91–120s |
| Score Differential | Down 8+, Down 4–7, Down 1–3, Tied, Up 1–3, Up 4–7, Up 8+ |
| Field Position | 1–20, 21–40, 41–60, 61–80, 81–99 yards from goal line |

---

## Project Structure

```
nfl-late-game-decision-tool/
├── app.R                                  # Shiny application
├── chee_final_project_data_process.Rmd    # ETL pipeline and feature engineering
├── chee_technical_documentation.pdf       # Full technical methodology
├── chee_user_guide.pdf                    # User guide with example use cases
└── README.md
```

---

## How to Run

1. Install required R packages:
```r
install.packages(c("shiny", "dplyr", "ggplot2", "DT", "nflreadr"))
```

2. Run the data processing pipeline to generate `simulator_summary_mvp`:
```r
rmarkdown::render("chee_final_project_data_process.Rmd")
```

3. Launch the app:
```r
shiny::runApp("app.R")
```

---

## Key Technical Features

- **ETL pipeline** in R using `nflreadr`, `dplyr`, and `tidyr` to ingest, filter, and transform raw play-by-play data
- **Feature engineering:** time buckets, score differential buckets, field position buckets, time-elapsed-per-play calculation
- **Drive outcome classification:** TD, FG, turnover, no score — derived from grouped play sequences
- **Interactive Shiny app** with dynamic filtering, color-coded bar charts, and drill-down drive exploration
- **Sample size warnings** when historical observations are sparse

---

## Built With

- R, Shiny, dplyr, ggplot2, DT, nflreadr
- Northwestern University — MSDS 456: Sports Management Analytics
