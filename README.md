# The `permutes` package

This R packages is intended to be used with densely-sampled time-series data, such as EEG data or mousetracking data. The package runs a permutation test at every timepoint in your dataset and enables you to plot the resulting *p*-values as a heatmap. This allows you to determine empirically where the effect of your experimental manipulation starts and ends, which provides you information on what time window you should take when you run your real analysis.

## Installation

```
remotes::install_github('cvoeten/permutes')
```
