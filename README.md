# Empirical effect size distibution explorer

> **Preprint**  
> These effects were extracted as part of Azaad (2026). *Empirically derived
> effect size guidelines for social, individual differences, and cognitive
> psychology.* [https://osf.io/preprints/psyarxiv/r4xwb_v1](https://osf.io/preprints/psyarxiv/r4xwb_v1).
> Method and validation information can be found in the preprint.

A dependency-light R Shiny app for searching the effect-size dataset and
comparing weighted distributions from between-subjects and within-subjects
designs.

## Run locally

The only runtime R package is `shiny`.

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
`claim`, `title`, and `description` columns. Comma-separated terms use OR
semantics: an effect is retained when any entered term matches.

## Runtime contents

- `app/app.R`: UI and server.
- `app/R/effect_distribution.R`: base-R filtering, multilevel REML weights,
  weighted binning, quartiles, and plots.
- `app/data/effects.rds`: the self-contained runtime dataset. It includes claim text
  joined from the collated source and no longer depends on `reference/`.
- `app/www/styles.css`: local, responsive styling with no web fonts or CDN assets.

## Updating the data

The editable source is `data/effects.csv`; the app loads the compressed
`app/data/effects.rds`. After editing or replacing the CSV, rebuild the RDS from
the repository root:

```sh
Rscript scripts/csv_to_rds.R
```

The conversion script checks required columns, numeric values, logical values,
and field codes before replacing the app data. Custom input and output paths can
be supplied as its first and second command-line arguments.

## Shinylive

The runtime uses Shiny, base R, base graphics, and local files only. It avoids
native-code statistical packages, filesystem writes, parallel processing,
network requests, and server-only features. This keeps it suitable for a future
Shinylive export.

Confidence intervals are not implemented yet. Dashed lines show the mean
weighted 25th, 50th, and 75th percentiles from 5,000 article-cluster bootstrap
samples, matching the reference analysis's sampling structure.

Export only the isolated runtime directory so reference data and development
files are not embedded:

```r
shinylive::export(
  "app",
  "site",
  template_params = list(
    title = "Empirical effect size distibution explorer"
  )
)
```
