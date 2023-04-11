# trending 0.1.0

## Breaking changes

* Reworking of the output from both fit and predict functions:
    - Warnings and errors are captured for both individual and multiple models.
    - By default, these are returned in tibbles.
    - Fitting warnings/errors are no longer carried over in to the predict output.

## Other changes

* Maintainer changed to Thibaut Jombart

# trending 0.0.3

* Default to using ciTools and a simulation approach for prediction intervals.

# trending 0.0.2

* fit now returns a subclass of tibble instead of list.
* predict now returns a tibble instead of list.
* better compatibility with dplyr verbs.

# trending 0.0.1

* Initial release.
