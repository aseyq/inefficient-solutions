# Cognitive appeal promotes the persistence of inefficient solutions and hinders cumulative cultural evolution

This repository contains the code and data needed to reproduce the analyses, figures, and tables from the paper _"Cognitive appeal promotes the persistence of inefficient solutions and hinders cumulative cultural evolution"_ (Saral, Singh, Jacquet, Jang, and Derex).

## Repository Structure

| Path | Description |
|---|---|
| `code/` | Analysis scripts for all main and supplementary results. Includes optional scripts prefixed with `zzz_optional_`. |
| `data/` | Input datasets and derived data files used by analyses. |
| `data/processed/` | Processed/intermediate data products (for example semantic similarity files). |
| `output/` | Generated figures and tables (`.png`, `.pdf`, `.svg`, `.csv`, `.html`). |
| `ignore/` | Internal verification artifacts and helper files not required for core reproduction. |
| `data_dictionary.md` | Variable descriptions for the datasets. |
| `run_all.R` | Master script that clears `output/` and runs all non-optional R scripts in `code/`. |

## Requirements

### R packages

Core packages used by the analysis scripts:

- `tidyverse`
- `patchwork`
- `lme4`
- `lmerTest`
- `sjPlot`
- `kableExtra`
- `knitr`
- `jsonlite`
- `RColorBrewer`
- `ggrepel`

Install with:

```r
install.packages(c(
  "tidyverse", "patchwork", "lme4", "lmerTest", "sjPlot",
  "kableExtra", "knitr", "jsonlite", "RColorBrewer", "ggrepel"
))
```

### Python packages (optional)

Python dependencies in `requirements.txt` are only needed for optional analyses:

- semantic similarity regeneration (`code/zzz_optional_figA7_calclulate_semantic.py`)
- GPT-based advice coding and comparison scripts (`code/zzz_optional_*`)

Results from these optional analyses are already included in the repository.

## Reproduce Main Analyses

1. Clone the repository:

   ```bash
   git clone https://github.com/aseyq/inefficient-solutions.git
   cd inefficient-solutions
   ```

2. Install required R packages (see command above).

3. Run the full pipeline:

   ```bash
   Rscript run_all.R
   ```

What `run_all.R` does:

- sets the working directory to the repository root when possible
- clears existing files in `output/`
- runs all `.R` files in `code/`, excluding scripts that start with `_` and `zzz`
- stops with an error if any script fails

Generated files are written to `output/`.

If you want to run scripts manually, use the current script names, for example:

```r
source("code/figure2.R")
source("code/figure3.R")
source("code/figure4.R")
source("code/table1.R")
```

## Optional Python Workflows

Install optional Python dependencies:

```bash
pip install -r requirements.txt
```

### Regenerate semantic similarity files (optional)

```bash
python code/zzz_optional_figA7_calclulate_semantic.py
```

This recreates semantic similarity files in `data/processed/`.

Notes:

- The script uses the public model `all-MiniLM-L6-v2`.
- No Hugging Face account is required.
- First-time model download may take a few minutes.

### Regenerate GPT-based advice coding (optional, paid API)

This workflow is included for comparison with manual coding and requires OpenAI API credits.

Set an API key:

- macOS/Linux:

  ```bash
  export OPENAI_API_KEY="your_api_key_here"
  ```

- Windows PowerShell:

  ```powershell
  $env:OPENAI_API_KEY="your_api_key_here"
  ```

Run:

```bash
python code/zzz_optional_classify_gpt5.py
```

This generates `data/df_advice_gpt5.csv`.

## Data

The main experimental dataset is `data/df_long.csv`. See [data_dictionary.md](data_dictionary.md) for variable definitions.

