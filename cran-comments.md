# CRAN submission comments: inflationkit 0.1.1

## Reason for this submission

Patch release. `ik_sample_data()` set the global random seed without
restoring it, so calling it altered the user's random number stream. The
seed now applies only for the duration of the call. No returned value
changes.

## R CMD check results

0 errors | 0 warnings | 0 notes (CRAN settings, R 4.5.2, macOS).

## Downstream dependencies

None on CRAN.
