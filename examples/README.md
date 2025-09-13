# NANSEN Examples: File Format Guide

This folder contains example files and documentation for the NANSEN package input formats. Use these examples as templates when creating your own input files for AutoPert and Combo analyses.

## AutoPert Specification Files

AutoPert specification files define experimental conditions and expected results for perturbation analysis.

### Format: `spec_*.csv`

**Required Columns:**
- `cell_line` - Cell line or other background identifier (used to group experiments, see autopert.md 'group_vars')
- `paper_title` - Reference paper title (optional)
- `Paper DOI` - DOI of reference paper (optional)
- `source` - Data source identifier (used to group experiments)
- `experiment_overview` - High-level experiment description (optional)
- `experiment_particular` - Specific experimental condition (used to group experiments)
- `gene` - Target gene/node name
- `perturbation` - Perturbation type/strength (can be numeric or qualitative {min, mid, max})
- `expected_result_bma` - Expected BMA result (can be numeric or qualitative {min, mid, max})
- `notes` - Additional notes (optional)

**Example Files:**
- `autopert/helper_spec_1.csv` - Uses genes: in, b, c, out
- `autopert/helper_spec_2.csv` - Uses genes: growth_factor, b, c, output_a

**Data Values:**
- Text fields can contain any descriptive text
- Empty cells are acceptable where data is not available

## Combo Analysis Files

Combo analysis requires background conditions, and can also take files to define drug combinations, and exclusions.

### Background Files: `combo_bkg_*.csv`

Define baseline node activities for different biological contexts.

**Required Columns:**
- `background` - Background condition name (e.g., "wt", "cancer")
- `node` - Node/gene identifier
- `activity` - Baseline activity level (numeric)

**Example:** `combo/helper_combo_bkg_1.csv`
```csv
background,node,activity
wt,growth_factor,2
cancer,growth_factor,2
cancer,e,0
```

### Drug Files: `combo_drugs_*.csv`

Define how drugs affect specific nodes in the network. Be default the combo function will test all possible combinations of single node inhibitions (set to minimum) and activations (set to maximum). When considering drugs (or other perturbations) with more complex effects, either where a perturbation affects multiple nodes at once, or sets nodes to intermediate values, this file is used to run a supplementary analysis after the node based screen. 

If there are drugs that duplicate the effect of a single node perturbation, the perturbation is not run again but the prior result is used for efficiency.

Note that it is possible to specify drugs that cannot be combined e.g. drug A sets node X to 0 and Y to 1, drug B sets node X to 1 and node Z to 0. These are currently not supported and will result in an error. 

**Required Columns:**
- `drug` - Drug identifier/name
- `node` - Target node/gene identifier
- `activity` - Activity level imposed by drug (numeric)

**Example:** `combo/helper_combo_drugs_1.csv`
```csv
drug,node,activity
able,a,0
able,b,0
baker,d,1
charlie,a,0
charlie,b,2
charlie,c,1
```

### Exclude Files: `combo_exclude.csv`

List nodes to exclude from analysis in order to reduce the time taken by the screen. 

**Required Columns:**
- `node` - Node identifier to exclude

**Example:** `combo/helper_combo_exclude.csv`
```csv
node
output_a
output_b
```

## File Format Guidelines

### General CSV Rules
- Use comma-separated values (CSV) format
- Include header row with exact column names as specified
- Empty cells are permitted where data is not available
- No extra spaces around commas
- Save with UTF-8 encoding

### Naming Conventions
- Use descriptive filenames that indicate purpose
- AutoPert specs: `spec_[description].csv`
- Combo backgrounds: `combo_bkg_[description].csv`
- Combo drugs: `combo_drugs_[description].csv`
- Combo excludes: `combo_exclude.csv`

### Data Types
- **Text fields**: Any descriptive text (cell_line, paper_title, etc.)
- **Numeric fields**: Non-zero integer values for activities and perturbations
- **Empty values**: Leave cells empty rather than using "NA" or "NULL"

## Usage

Reference the existing example files in this folder as templates:
- Copy an appropriate example file
- Modify the data while preserving the column structure
- Ensure all required columns are present with exact header names
- Validate your CSV format before running analyses

For more information on running analyses with these files, see the main package documentation and the example R scripts in this folder.