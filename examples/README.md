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

## Configuration Files

NANSEN provides TOML configuration files that allow you to run complete workflows with a single command. These configuration files specify all parameters for AutoPert and Combo analyses, eliminating the need to modify R scripts directly.

### AutoPert Configuration Files: `*_config_example.toml`

AutoPert configuration files define parameters for running automated perturbation analysis.

**Usage:**
```bash
Rscript examples/run_autopert_config.r examples/autopert_config_example.toml
```

**Required Fields:**
- `netw_file_path` - Path to network JSON file (string)
- `spec_path` - Path to specification CSV file (string)
- `out_dir` - Output directory for results (string)

**Optional Fields (with defaults):**
- `nosat = true` - Run without SAT solver (boolean)
- `loserum = false` - Experimental serum option (boolean)
- `missing_nodes_perturbed_overide = false` - Override missing node checks for perturbed nodes (boolean)
- `missing_nodes_expected_overide = false` - Override missing node checks for expected results (boolean)
- `project_path = ""` - Git repository path for logging (string)
- `group_vars = ["source", "cell_line", "experiment_particular"]` - Variables for grouping experiments (array of strings)
- `short_filenames = false` - Use shortened filenames for Windows compatibility (boolean)

**Example Configuration:**
```toml
netw_file_path = "examples/autopert/helper_autopert_1.json"
spec_path = "examples/autopert/helper_spec_1.csv"
out_dir = "auto_pert_results"
nosat = true
loserum = false
missing_nodes_perturbed_overide = false
missing_nodes_expected_overide = false
project_path = ""
group_vars = ["source", "cell_line", "experiment_particular"]
short_filenames = false
```

### Combo Configuration Files: `combo_config_example.toml`

Combo configuration files define parameters for running combination perturbation analysis.

**Usage:**
```bash
Rscript examples/run_combo_config.r examples/combo_config_example.toml
```

**Required Fields:**
- `netw_file_path` - Path to network JSON file (string)
- `backgrounds_path` - Path to backgrounds CSV file (string)
- `out_dir` - Output directory for results (string)

**Key Optional Fields (with defaults):**
- `drug_path = NA` - Path to drug perturbations CSV file (string or NA)
- `combo_exclusions_path = NA` - Path to exclusions CSV file (string or NA)
- `project_path = ""` - Git repository path for logging (string)
- `phenotypes = ["output_a", "output_b"]` - List of phenotypes to measure (array of strings)
- `pheno_only = true` - Only process phenotype nodes for their value under perturbation (boolean). This reduces the time of the processing step from hours to minutes. 
- `skip_autopert = false` - Skip autopert step (boolean)
- `skip_combo_sim = false` - Skip combo simulation (boolean)
- `skip_heatmaps = false` - Skip heatmap generation (boolean)
- `skip_heatmaps_uc = false` - Skip unclustered heatmaps (boolean)
- `use_vmcai = true` - Use VMCAI solver (boolean)
- `node_col_name = "node"` - Column name for node identifier (string)
- `short_filenames = false` - Use shortened filenames for Windows compatibility (boolean)

**Example Configuration:**
```toml
netw_file_path = "examples/combo/helper_combo_1.json"
backgrounds_path = "examples/combo/helper_combo_bkg_1.csv"
out_dir = "combo_results"
skip_autopert = true
skip_combo_sim = true
skip_heatmaps = true
skip_heatmaps_uc = true
pheno_only = true
phenotypes = ["output_a", "output_b"]
project_path = ""
node_col_name = "node"
use_vmcai = true
short_filenames = false
```

### Configuration File Guidelines

**TOML Format Rules:**
- Use standard TOML syntax (https://toml.io/)
- Strings must be quoted: `"path/to/file"`
- Booleans are lowercase: `true`, `false`
- Arrays use square brackets: `["item1", "item2"]`
- Comments start with `#`

**Path Specifications:**
- Use relative paths from your working directory
- Forward slashes work on all platforms: `"examples/data/file.csv"`
- Absolute paths are supported but reduce portability

**File Naming:**
- Use descriptive names: `my_experiment_autopert_config.toml`
- Keep the `_config.toml` suffix for clarity

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

### Using CSV Data Files
Reference the existing example files in this folder as templates:
- Copy an appropriate example file
- Modify the data while preserving the column structure
- Ensure all required columns are present with exact header names
- Validate your CSV format before running analyses

### Using Configuration Files
For automated workflows:
1. **Copy an example configuration file:**
   - `autopert_config_example.toml` for perturbation analysis
   - `combo_config_example.toml` for combination screening
2. **Modify paths and parameters** to match your data and requirements
3. **Run the analysis:**
   ```bash
   Rscript examples/run_autopert_config.r your_config.toml
   # or
   Rscript examples/run_combo_config.r your_config.toml
   ```
4. **Validate configuration** - the scripts will check file paths and required fields before running

### Additional Resources
For more information on:
- Function parameters and advanced options, see the main package documentation
- Example data formats, examine the helper files in the `autopert/` and `combo/` subdirectories
- Command-line usage, see the runner scripts in this folder