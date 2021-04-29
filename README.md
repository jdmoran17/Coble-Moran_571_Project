# Coble-Moran_571_Project
A repository for Wharton STAT 571 Final Project, Impact of Universities’ Online Covid-19 Dashboards on County COVID-19 Cases

This repository contains the data and code necessary to run the data gathering and analyses for `Impact of Universities’ Online Covid-19 Dashboards on County COVID-19 Cases`.

We have included all R scripts necessary to conduct the project from start to finish. Below is a description of the purpose of each script:

`0 Wayback Gathering` - Reads in raw list of universities' dashboard URLs, accesses Wayback Machine API via `wayback`, and stores URLs for all Wayback Machine archives of each university's dashboard.

`1 Web Scraping` - Reads in list of URLs for all Wayback Machine archives and uses `rvest` and `xml2` to scrape each Wayback archive and generate measures of content from the underlying HTML.

`2 Create Content Table` - Reads in results of `1 Web Scraping` and creates a long table of all Wayback archives and their associated content measures. Saves output for easy use.

`3 Data Cleaning` - Performs all data cleaning, merging, and preparation for analyses run in `4 Analyses`. Merges data on COVID-19 cases and universities to content table. Saves long observation-level tables for use in difference in differences and event study analyses.

`4 Analyses` - Executes LASSO classification model to identify factors associated with universities' dashboard adoption, event study analyzing changes in new COVID-19 cases after updates to dashboards, and difference-in-differences study analyzing the differences in COVID-19 case rates between universities with and without dashboards. Runs OLS model diagnoses.

As `0 Wayback Gathering` and `1 Web Scraping` are computationally intensive and involve web scraping, and `2 Create Content Table` involves analyzing the raw outputs of `1 Web Scraping` which, for size issues, we have **NOT** included here, we suggest not running those scripts (if you do run the scripts, you must start from the beginning and run all scripts).

In addition, we have provided cleaned versions of the data tables necessary to run `4 Analyses` as a standalone, which we suggest in the interests of time. However, we have also provided the files necessary to run `3 Data Cleaning` as a standalone.

Acknowledgments: We thank Linda Zhao, Jeff Cai, and Sam Rosenberg for allowing use of their cleaned version of the NYT COVID-19 reporting data.
