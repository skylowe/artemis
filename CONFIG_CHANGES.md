# Configuration Changes - API Keys

## Branch: `config-api-keys`

### Summary
Modified the project to read API keys from the user's home directory `~/.Renviron` file instead of requiring a project-specific `.Renviron` file.

### Changes Made

#### 1. Modified `.Rprofile`
Added code to explicitly load `~/.Renviron` at startup:

```r
# Load API keys from user's home .Renviron file
home_renviron <- path.expand("~/.Renviron")
if (file.exists(home_renviron)) {
  readRenviron(home_renviron)
}
```

This ensures that all API keys defined in the home directory are available to the R session when the project starts.

#### 2. Created Test Script
Added `scripts/test_api_keys.R` to verify API key configuration:

```bash
Rscript scripts/test_api_keys.R
```

This script checks for the presence of expected API keys and reports which ones are configured.

### Benefits

1. **Centralized Configuration**: API keys are stored in one location (`~/.Renviron`) and can be shared across multiple R projects
2. **No Duplication**: No need to copy/paste API keys into each project
3. **Security**: Project `.Renviron` is already in `.gitignore`, and now the project doesn't need its own copy
4. **Flexibility**: Still supports project-specific `.Renviron` if needed (R will load both, with project-specific taking precedence)

### Testing

After merging this branch, verify the configuration:

```bash
# Test that API keys are accessible
Rscript scripts/test_api_keys.R

# Run R interactively and check
R
> Sys.getenv("CENSUS_KEY")
> Sys.getenv("BLS_API_KEY")
```

### API Keys Expected

Based on `CLAUDE.md` documentation, the following keys should be in `~/.Renviron`:

- `CENSUS_KEY` - Census Bureau API
- `BEA_API_KEY` - Bureau of Economic Analysis
- `BLS_API_KEY` - Bureau of Labor Statistics
- `FRED_API_KEY` - Federal Reserve Economic Data
- `EIA_API_KEY` - Energy Information Administration
- `NASSQS_TOKEN` - USDA NASS Quick Stats
- `SP_USER` - S&P credentials
- `SP_PASS` - S&P credentials
- `MOODYS_ACCESS` - Moody's Analytics

### Backwards Compatibility

This change is backwards compatible:
- If a project-specific `.Renviron` exists, it will still be loaded by R's standard behavior
- The home `.Renviron` is loaded first, so project-specific values can override if needed
- All existing code using `Sys.getenv()` continues to work without modification

### Next Steps

1. Test the configuration with `scripts/test_api_keys.R`
2. Run the ARTEMIS pipeline to verify data acquisition works
3. If successful, merge this branch to main
