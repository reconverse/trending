# Reason for updated release
* fit now returns a subclass of tibble instead of list.
* predict now returns a tibble instead of list.
* better compatibility with dplyr verbs.

## Test environments
* Fedora 33, local R installation, R 4.0.3 (2020-10-10)
* Fedora 33, local R installation, R Under Development (2020-11-18 r79444)
* win-builder (devel)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Downstream dependencies
I have also run R CMD check on downstream dependency i2extras without issue.
