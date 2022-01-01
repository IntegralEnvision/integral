# integral 0.0.2.9001

* Added `%ni%` (not in), the opposite of %in%
* Added `xview()` which opens a table in Excel.  
* Added interactive helper `common_vars()`, which shows common variable names between two or more tibbles.
* Added interactive helper `wc()` (write clip) which copies an object to the clipboard.  If no object is passed, it defaults to `.Last.value`.
* Added `huck()`, a negation of `purrr::pluck()` which discards an element from a list using tidyselect specifications.
* Added `deselect()`, a negation of `dplyr::select()` (ie `select(-c(...))`) since wrapping tidyselect vars in c() annoys me.
* Added `fct_case_when()`, an implementation of `dplyr::case_when()` where the output is an ordered factor in the order of the conditions.
* Added `zscore()` as a convenience function wrapper around `scale()` that returns a vector instead of a matrix.

# integral 0.0.1.9000

* Added beta version of interactive helper `cheat()` which lists cheatsheets and opens the selected one.
* Added interactive helper `idf()` (ID filter). This function makes it easy to filter on a key column in multiple tables from the same dataset. The key column is set in options (function will ask for it if it is not set already) so that it isn't necessary to type it with each call, and the previous key value is is used until it is updated. See ?idf for details.
* Added `state_locations()` which returns a tibble of the location of U.S. states on a map (top, middle, bottom) and can handle Albers. Useful for dynamically adjusting pop-up directions (above/below) on maps to prevent panning.

# integral 0.0.0.9000

* Added `ic_update` which checks for a new dev version of `integral` on github, and if found automatically unloads `integral`, downloads the new version and installs it, and reloads `integral`.
* Added loading message that informs the user if there is a new version of `integral`
* Added `ic_news()` which shows news updates for version bumps
