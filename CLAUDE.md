# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a statistical reporting framework built in R for the KIDS24 biohackathon (Team 10). The codebase provides automated statistical analysis functions that generate HTML reports with tables, figures, and properly formatted narratives. It focuses on common statistical tests used in biomedical research.

## Core Architecture

### Report Generation System

The framework uses a global environment (`rpt.env`) to manage report state throughout analysis:

1. **Initialization**: `begin.report()` (report.R:12) creates directory structure and locked environment
2. **Analysis**: Statistical test functions add content to the environment
3. **Finalization**: `complete.report()` (report.R:103) generates HTML narrative, Excel tables, and PDF figures

The `rpt.env` environment is locked using `lockEnvironment()` and `lockBinding()` to prevent inadvertent changes. Updates must use `update.rpt.env()` (report.R:703) which temporarily unlocks, updates, and relocks the environment.

### Statistical Test Functions Pattern

All test functions follow a consistent pattern (see t-test.R, kw-test.R, ranksum-test.R, normality-test.R):

1. Accept `form` parameter: either formula (`y~x`) or character string (`"y"`)
2. Process data using `get.form.vars()` or `get.form.names()` (base.R)
3. Generate summary statistics via `summarize()` (summarize.R)
4. Create visualizations via `box.plot()` or `qq.plot()`
5. Build narrative text with embedded HTML tags
6. Optionally add to report using `report.text()`, `report.table()`, `report.figure()`
7. Return list with: txt, tbl, tbl.cap, fig.cap, mtd, ref

### Key Parameters Across Functions

- `rpt=F`: When TRUE, adds results to report environment
- `txt/fig/tbl`: Level of detail (0=none, 1+=increasing detail)
- `mda`: Missing data alert (0=suppress, 1=show)
- `rxv`: Data archiving (0=none, 1=archive if under size limit, 2=always archive)
- `hdr`: HTML header level (0=none, 1-6 for h1-h6)
- `dgt`: Number of significant digits for display

## Running R Code

### Basic Test Execution

All R files must be sourced before use. See example-script.R:36-41 for the pattern:

```r
source("base.R")
source("boxplot.R")
source("colors.R")
source("report.R")
source("summarize.R")
source("kw-test.R")  # or other test files
```

### Generating Reports

```r
# Initialize report
begin.report(dir.name="./output",
             title="My Analysis",
             author="Researcher Name")

# Run analyses with rpt=T
result <- kw.test(y~treatment, data=mydata, rpt=T)

# Generate final report
complete.report()
```

Report output includes:
- `report-narrative.html`: Main results with embedded references
- `tables/report-tables.xlsx`: All tables with captions
- `figures/Figure_N.pdf`: Individual figure files
- `data/`: Archived raw and analysis datasets
- `technical/technical-details.xlsx`: Session and package info

## File Organization

### Core Infrastructure
- `base.R`: Formula parsing, text formatting utilities
- `report.R`: Report generation system, environment management
- `colors.R`: Color scheme definitions (not yet examined but referenced)

### Statistical Tests
- `t-test.R`: One and two-sample Student's t-test
- `ranksum-test.R`: Wilcoxon rank sum and signed-rank tests
- `kw-test.R`: Kruskal-Wallis test
- `normality-test.R`: Shapiro-Wilk normality test
- `two.variance-test.R`: F-test for variance comparison

### Visualization
- `boxplot.R`: Box plot generation with Tukey outlier detection
- `qqplot.R`: Q-Q plots for normality assessment
- `summarize.R`: Summary statistics computation

### Data Processing
- `summarize()` handles both quantitative (numeric/double/integer) and qualitative (factor/character) variables
- Uses `smry()` (summarize.R:73) for numeric summaries: n, missing, mean, sd, median, quartiles, min, max

## Dependencies

Required R packages (loaded in report.R:713-716):
- `writexl`: Excel file output
- `haven`: SAS data file reading
- `tools`: MD5 checksums

Base R functions used extensively:
- Statistical tests: `t.test()`, `shapiro.test()`, `kruskal.test()`
- Graphics: `boxplot()`, `pdf()`, and other device functions

## Important Notes

### File Paths
- example-script.R contains hardcoded Windows paths (lines 36-41, 47) that need updating
- Report directories are created automatically with timestamps

### Environment Management
- Never assign directly to `rpt.env` - always use `update.rpt.env()`
- The environment persists across analyses until `complete.report()` is called

### Formula Handling
- Functions accept both formula objects (`y~x`) and character strings (`"y"`)
- Use `get.form.vars()` for formula objects (returns list with resp and pred)
- Use `get.form.names()` for character strings or error handling

### Text Formatting
- Use `rpt.num()` (report.R:403) for scientific notation in HTML
- Use `txt.list()` (base.R:55) to format vectors as "a, b, and c"
- Use `paste.text()` (report.R:689) to concatenate with space cleanup
