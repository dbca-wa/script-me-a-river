
<!-- README.md is generated from README.Rmd. Please edit that file -->

# script-me-a-river

<!-- badges: start -->

<!-- badges: end -->

The goal of script-me-a-river is to …

# Setup a new user

Open your `.Rprofile` with:

``` r
usethis::edit_r_profile()
#> ● Edit '/home/florian/.Rprofile'
#> ● Restart R for changes to take effect
```

Add to `.Rprofile`:

``` r
Sys.setenv(CKAN_URL="https://data.dpaw.wa.gov.au")
Sys.setenv(CKAN_API_KEY = "your CKAN API key")
```
