
<!-- README.md is generated from README.Rmd. Please edit that file -->

# astho.viz

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

A data visualization package to generate themed, accessible highcharter
visualizations using an ASTHO theme.

## Installation

You can install the development version of astho.viz from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lindsayjorgenson/astho.viz")
```

## Example

Create a categorical map.

``` r
# library(astho.viz)
# 
# categorical_map(data = astho_governance,
#                 fips_column = "fips",
#                 selected_value = "governance",
#                 title_text = 
#                   "Governance of State and Island Area Public Health Agencies", 
#                 subtitle_text = "2022", # content under the title
#                 caption_text =
#                   "<b> Source: </b> Association of State and Territorial Health 
#                   Officials.ASTHO Profile of State and Territorial Public Health, 
#                   Volume Six. Arlington, VA: Association of State and 
#                 Territorial Health Officials. 2023.",
#                 accessible_desc =
#                   "This is a map of governance classification, with most 
#                   jurisdictions classified as decentralized followed by 
#                   centralized. Few jurisdictions have mixed or shared 
#                   governance. Enter the map to review classification at the 
#                   jurisdiction level.",
#                 my_tooltip = "<b>{point.JurisdictionName}</b><br> {point.value}",
# )
```

## Next

We will soon add additional geographic maps, tile maps, bar charts, line
graphs, and pie charts all customized with the ASTHO theme.
