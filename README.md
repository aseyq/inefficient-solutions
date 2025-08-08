# The Emergence and Cultural Persistence of Ineffective Solutions

This repository contains the code and data necessary to reproduce analyses and figures in the paper _"The Emergence and Cultural Persistence of Ineffective Solutions."_ All analyses are conducted using R.

## Repository Structure
```
├── code/
│   ├── fig2.R                   # Figure 2
│   ├── fig3.R                   # Figure 3
│   ├── most_common_solutions.R  # Figure 4
│   └── table1.R                # Table 1 (regression)
├── data/
│   └── df_long.csv             # Main dataset used for analysis
```

## Requirements

- R packages:
  - `tidyverse`
  - `lme4`
  - `lmerTest`
  - `sjPlot`
  - `purrr`


You can install the required packages with the command:

```r
install.packages(c("tidyverse", "lme4", "lmerTest", "sjPlot", "purrr"))
```

## Usage

1. **Clone the repository**:
   ```bash
   git clone https://github.com/yourusername/inefficient-solutions.git
   ```

2. Run the scripts (in any order):

   ```r
   source("code/table1.R")                 # Generates regression models and tables
   source("code/fig2.R")                   # Generates Figure 2
   source("code/fig3.R")                   # Generates Figure 3
   source("code/most_common_solutions.R") #  Generates information on Figure 4
   ```

3. Output files (e.g., figures, tables) will be saved to `figures/` folder.

## Data

The dataset used in the analysis is `data/df_long.csv`

Please refer to the [data dictionary](data_dictionary.md) for detailed descriptions of each variable.

