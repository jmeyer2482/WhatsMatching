
#### Overview

This app uses some basic matching principles to provide insights into
matching methodology for educational purposes. The plots to the left
demonstrate how matching methods work in practice. Generally, matching
is used to assist in controlling confounding and bias in observational
data prior to estimating the treatment effects. In most cases the
treatment variable that is used for matching is a binary variable which
is this case for the simulated data in this app.

The simulated data used to compare matching methods include a treatment
variable (`t`), covariates (`X1` and `X2`) and an outcome variable
(`y`). The data in all of the simulations are matched on the same
formula (details below).

#### Plot 1 and Plot 2

Plot 1 and 2 show the underlying data the matches were performed on. A
frame by frame progression of how the matches occurred for two different
methods are shown. Blue trianges represent the treated group, red
triangles the control group. When matching occurs, there is a control
unit paired to every treated unit.

#### Plot 3 - Standardised Mean Difference (SMD)

Plot 3 shows the SMD between each of the covariates for the raw values
and each of the matching methods. Each frame shows the changes in the
SMD for the matched data in Plot 1 and Plot 2. The raw SMD does not
change as the underlying data does not change.

#### Plot 4 - Estimates of the Treatment Effect

Plot 4 shows the estimates of the treatment effect for various
propensity score methods, including stratification and weighting. The
cumulative effect of the estimates as units are added to the data is
also visible in relation to the matches seen in Plot 1 and 2. The
formula and the treatment effect used to simulate the outcome is the
same for all methods.
