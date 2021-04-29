`content table` Raw data on content of scraped Wayback Machine archives.

`content_table_analysis` Cleaned data with additional measures of content (i.e., PC scores).

`DiDtable` Data table necessary to run the difference-in-differences study per script `4 Analyses`.

`EStable` Data table necessary to run the event study per script `4 Analyses`.

`IPEDS_data_clean` Cleaned version of IPEDS data export (following file).

`IPEDS_data_full` Raw version of IPEDS data export.

`IPEDS_Match_Key` Match key between name of university per IPEDS and per College Crisis Initiative. Uses fuzzy matching and manual review of cases where R was not able to find a high-quality match.

`No_Content` Data table containing observations which do not have any content available per Wayback Machine (likely due to errors in the API and/or `wayback` package).

`No_Mementos` Data table containing observations which do not have Wayback Machine archives.

`only_2021_Content` Data table containing observations which only have Wayback Machine archives in 2021 (our analyses only cover 2020).

`School_List_Clean` Data table containing cleaned version of university listing per College Crisis Initiative.

`School_List_Clean_w_IPEDS` Version of the above file with unique university-level identifier per IPEDS.

`School_List_Merged_w_IPEDS` Version of the above file merged with `IPEDS_data_clean`.

`school_names` Output of fuzzy match to review.

`School_Reopening_Status_Dashboard_Links` List of universities with links to COVID-19 dashboards.
