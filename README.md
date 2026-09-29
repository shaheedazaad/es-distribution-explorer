# Empirical effect size distibution explorer

> **Preprint**  
> These effects were extracted as part of Azaad (2026). *Empirically derived
> effect size guidelines for social, individual differences, and cognitive
> psychology.* [https://osf.io/preprints/psyarxiv/r4xwb_v1](https://osf.io/preprints/psyarxiv/r4xwb_v1).
> Method and validation information can be found in the preprint.

A dependency-light R Shiny app for searching the effect-size dataset, comparing
weighted distributions, and fitting a clustered z-curve.

## Run locally

The runtime R packages are `shiny` and `zcurve` 2.4.2 (with its dependencies).
The WebAssembly bundle uses `zcurve` 2.4.2. Install the same version in a
project-local library so a newer system installation does not change the fit:

```r
dir.create(".local-r-library", showWarnings = FALSE)
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/zcurve/zcurve_2.4.2.tar.gz",
  repos = NULL, type = "source", lib = ".local-r-library"
)
```

The app automatically uses this library when launched from the project. Restart
R after installation if a different `zcurve` version is already loaded.

```r
shiny::runApp("app")
```

The app deliberately waits for **Calculate distributions** before applying any
changed search term or filter. A design is plotted only when at least 10 effects
have valid effect sizes, sample sizes, and article/study identifiers. The other
design can still be plotted when one side is below that threshold.

After a calculation, the between-subjects display can be switched among `r`,
`d`, `r² / partial η²`, Cohen's `f`, and odds ratios without rerunning the
filter or weighting model. The underlying weights stay on the original `r`
scale, as in the reference conversion table; the plotted effects and bootstrap
quartile thresholds are transformed. Odds ratios use a logarithmic x-axis.
For `d`, `f`, and odds ratios, values beyond the labelled plotting maximum are
retained in the rightmost overflow bin rather than discarded.

The conversions follow the preprint's conversion table: `d = 2r / sqrt(1 - r²)`,
`r² = partial η²` for one-degree-of-freedom effects,
`f = sqrt(r² / (1 - r²))`, and `OR = exp(dπ / sqrt(3))`. The `d` relationship
assumes equal group sizes, and the odds-ratio relationship is a logistic
approximation.

After calculation, **Download reproduction package** creates a compressed tar
archive containing the complete data as `effects.csv`, a README, and one
standalone base-R file named `reproduce.R`. The script records the calculated
search and filter snapshot and regenerates the histograms and quartile estimates.
It also records the selected between-subjects display metric.

Keyword matching is case-insensitive and searches across the `keywords`,
`title`, and `description` columns. Comma-separated terms use OR
semantics: an effect is retained when any entered term matches.

The **Z-curve** tab uses the same search and filters. Search first to see the
matching effect count, then fit the significant two-sided tests across both
designs. For between-subjects `r`, it calculates a t statistic with `n - 2`
degrees of freedom. For within-subjects `dz`, it uses `t = |dz| sqrt(n)` with
`n - 1` degrees of freedom. The app calls the [zcurve package's clustered
fit](https://fbartos.github.io/zcurve/reference/zcurve_clustered.html) with
500 bootstrap samples by default, clustering by DOI or DOI/study. It displays
the [package plot](https://fbartos.github.io/zcurve/reference/plot.zcurve.html)
with confidence intervals and annotations. Its reproduction archive contains
the exact fitted object, estimates, calculated p-values, and PDF plot saved by
the Shinylive session. `reproduce.R` reads that fit and draws the package plot;
`refit.R` runs the analysis again from the dataset. A fresh native R refit can
differ from webR at the final floating-point digit.

For numerical stability, z-scores above 8 are passed to `zcurve` as 8. The
package's upper fitting bound is 6, so these values all remain above the
fitting interval. The calculated p-values are retained in the downloaded
effect data.

## Runtime contents

- `app/app.R`: UI and server.
- `app/R/effect_distribution.R`: base-R filtering, multilevel REML weights,
  weighted binning, quartiles, and plots.
- `app/R/zcurve.R`: p-value conversion and clustered z-curve fitting and plot.
- `app/data/effects.rds`: the self-contained runtime dataset, stripped to only
  the columns the app uses and no longer dependent on `reference/`.
- `app/www/styles.css`: local, responsive styling with no web fonts or CDN assets.

## Updating the data

The editable source is `data/effects.csv`; the app loads the compressed
`app/data/effects.rds`. After editing or replacing the CSV, rebuild the RDS from
the repository root:

```sh
Rscript scripts/csv_to_rds.R
```

The conversion script checks required columns, numeric values, logical values,
and field codes, then overwrites `data/effects.csv` itself with a stripped-down
version containing only the columns the app uses, before replacing the app
data. Custom input and output paths can be supplied as its first and second
command-line arguments.

## Shinylive

The ES distribution calculations use base R. The z-curve fit uses `zcurve`,
whose WebAssembly binary and dependencies must be bundled for Shinylive.

Confidence intervals are not implemented yet. Dashed lines show the mean
weighted 25th, 50th, and 75th percentiles from 5,000 article-cluster bootstrap
samples, matching the reference analysis's sampling structure.

Export only the isolated runtime directory so reference data and development
files are not embedded:

```r
shinylive::export(
  "app",
  "site",
  wasm_packages = TRUE,
  template_params = list(
    title = "Empirical effect size distibution explorer"
  )
)
```
