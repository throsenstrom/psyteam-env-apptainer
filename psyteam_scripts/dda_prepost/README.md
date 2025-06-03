# README #

This README explains how to use the R scripts contained in this repository and pertaining to the academic paper:
Rosenström, T. H., Czajkowski, N. O., Solbakken, O. A., & Saarni, S. E. (2023). Direction of dependence analysis for pre-post assessments using non-Gaussian methods: A tutorial. Psychotherapy Research. https://doi.org/10.1080/10503307.2023.2167526

### What is this repository for? ###

* Obtain software tools for estimating direction of dependence in cross-sectional and pre-post (two-time) assessment settings.
* Obtain example scripts for conducting simulation studies on the direction dependence estimators.
* Store the academic record in a more usable form compared to a pdf supplement (the repository not under further development)

### How do I get set up? ###

This supplementary file contains contents of two text files, `direction_dependence_measures.R` and `simulate_dda.R`, in a format permitted by the journal (plain text files were disallowed). These are complete implementations of our methodology, although we omit non-generalizable parts of the code pertinent to our real data for clarity. The same functions can be applied to any appropriate real data. To load our functions, use the R command `source(“direction_dependence_measures.R”)`.

To estimate direction of dependence between two variables `x` and `y`, use the command `direction_dependence(x, y)`. This defaults to the “mxnt” estimator but may be supplemented with an optional argument `approximation = "mxnt"` (alternatives are mxnt, kgv, tanh, skew, rskew, hsic; see main text for details). Note that one of the estimators depend on the R package `KernelICA` (kgv) and one on the R package `dHSIC` (hsic), which must be installed before using them.

To estimate lagged and instantaneous effects between two variables, use the command `lagged_direction_dependence(X0, X1)`, where the `x` and `y` at the baseline (e.g., pre-therapy values) are now in columns of the n-by-2 input matrix `X0` and their corresponding follow-up (e.g., post-therapy) values in the columns of `X1`. An optional argument for the estimator can be similarly given. Finally, a HSIC-based test for confounding in the estimated instantaneous effects can be run using the command `instantaneous_effect_confounding_test(X0, X1)`.

To run simulations similar to those reported in our main text, the user may first load our functions and then run `source(“simulate_dda.R”)`. However, it should be more interesting to modify the contents of the file `simulate_dda.R` to match the specifics of one’s own data and research question.

### Who do I talk to? ###

* Repository owned by [Tom Rosenström](https://researchportal.helsinki.fi/en/persons/tom-rosenstr%C3%B6m)
* The code is offered freely for academic purposes