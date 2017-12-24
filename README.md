# The `permutes` package

This R packages is intended to be used with densely-sampled time-series data, such as EEG data or mousetracking data. The package runs a permutation ANOVA at every timepoint in your dataset and enables you to plot the resulting *p*-values as a heatmap. This allows you to determine empirically where the effect of your experimental manipulation starts and ends, which provides you information on what time window you should take when you run your real analysis.

The reason we run a permutation ANOVA instead of a regular ANOVA is that this is common in EEG research; the rationale is that it helps you reduce the Multiple Comparisons Problem (that you will *definitely* have if you test 32 electrodes at a few hundred timepoints) to *some* degree by deriving the null distribution empirically, rather than assuming that your data are asymptotically i.i.d..
Note that the resulting *p*-values are still not valid for purposes of statistical inference; they only serve to inform you about the window you should take when you run your actual statistical procedure (usually a linear mixed-effects model).

## Installation

```
library(devtools)
install_github('cvoeten/permutes')
```
