#integral 1.0.2.9002
* Added `slice_ht()`, a modified version of dplyr::slice_*() that pulls from the head and tail.


# integral 1.0.2.9001
* Potentially confusing change: ic_qa() has been updated to fix an issue with AVD file paths being changed to symbolic links to \\\\AFWS1W and with truncating long file names to work with Excel.  You may find that running ic_qa() on a file you have previously run it on will create a new sheet in your Excel QA form.  You can delete the old one.
* When the package is loaded, it will now notify you if there is a newer version on github.

# integral 1.0.1.9001
* Added str_squish_line_breaks() - an analogue of stringr::str_squish() that better handles line break characters (\n and \r)

# integral 1.0.1.9000
* Refined cheat() to accept bare, unquoted cheat sheet names
* Added git cheat sheet
* Updated get_system() to detect AVD and remove old Citrix detection. Removed is_citrix().

# integral 1.0.0
* Repo is now public and can be installed more easily!
* copy_file_wlog() and save_csv_wlog() renamed to ic_copy_file_wlog() and ic_save_csv_wlog() 
* qa() renamed to ic_qa()
* ic_scale_big_dollar() renamed to scale_big_dollar()
* ic_scale_si_unit() renamed to scale_si_unit()
* ic_visdat_grouped() renamed to ic_visdat_grouped()
* packagedown website now set up [here](https://integralenvision.github.io/integral/)


# integral 0.0.5
* Several bug patches and first packagedown documentation

# integral 0.0.4
* Patch to fix save_csv_wlog() function and export to namespace
* Added copy_file_wlog() function

# Integral 0.0.3
* added ic_new_proj(), ic_new_r_file(), ic_new_python_file(), ic_new_rmd_file(), 
ic_new_script() and ic_new_git() for project and file creation.
* They are supported by ic_new_script() for creating none Rmarkdown files, 
convert_winpath() to convert Windows paths and a series of ask() functions.

# integral 0.0.2.9006

* Added experimental helper functions for searching Stack Overflow and Google

  * `stackoverflow()` and alias `so()` search Stack Overflow for the terms you pass. Automatically adds the "\[r\]" tag.

  * `google()` searches Google for the terms you pass. Automatically adds the required "R" tag and omits results from web sites that provide the same information as the help documentation in R.
  * `so_last_error()` and `google_last_error()` collect the traceback of the most recent error and searches for it using Stack Overflow and Google, respectively. Automatically adds the R tag and (for google) omits results from web sites that have duplicated the R manual.

# integral 0.0.2.9005

-   Added `quote_values()` which converts a series of comma separated values into a vector input format. Copy of `Hmisc::Cs()` for convenience.
-   Added `ic_visdat_grouped()` which extends visdat to allow separate plots grouped by a variable in the data. This is very useful for annual data or other groupings that may explain missinginess.

# integral 0.0.2.9004

-   Added `ic_scale_si_unit()` which converts numeric values to International System units
-   Added `ic_scale_big_dollar()` which converts numeric values to dollar units using suffixes, ie: 1000 becomes \$1k.

# integral 0.0.2.9003

-   Added `%ni%` (not in), the opposite of %in%
-   Added `xview()` which opens a table in Excel.\
-   Added interactive helper `common_vars()`, which shows common variable names between two or more tibbles.
-   Added interactive helper `wc()` (write clip) which copies an object to the clipboard. If no object is passed, it defaults to `.Last.value`.
-   Added `huck()`, a negation of `purrr::pluck()` which discards an element from a list using tidyselect specifications.
-   Added `deselect()`, a negation of `dplyr::select()` (ie `select(-c(...))`) since wrapping tidyselect vars in c() annoys me.
-   Added `fct_case_when()`, an implementation of `dplyr::case_when()` where the output is an ordered factor in the order of the conditions.
-   Added `zscore()` as a convenience function wrapper around `scale()` that returns a vector instead of a matrix.

# integral 0.0.1.9000

-   Added beta version of interactive helper `cheat()` which lists cheatsheets and opens the selected one.
-   Added interactive helper `idf()` (ID filter). This function makes it easy to filter on a key column in multiple tables from the same dataset. The key column is set in options (function will ask for it if it is not set already) so that it isn't necessary to type it with each call, and the previous key value is is used until it is updated. See ?idf for details.
-   Added `state_locations()` which returns a tibble of the location of U.S. states on a map (top, middle, bottom) and can handle Albers. Useful for dynamically adjusting pop-up directions (above/below) on maps to prevent panning.

# integral 0.0.0.9000

-   Added `ic_update` which checks for a new dev version of `integral` on github, and if found automatically unloads `integral`, downloads the new version and installs it, and reloads `integral`.
-   Added loading message that informs the user if there is a new version of `integral`
-   Added `ic_news()` which shows news updates for version bumps
