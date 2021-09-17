library(dplyr)
vg_sales <- readr::read_csv("./data/vgsales.csv", col_types = "dccdccddddd")
clean_vg_sales <- vg_sales %>% 
  dplyr::filter(!is.na(Year))
clean_vg_sales <- vg_sales %>%
  dplyr::filter(Year <= 2010)
tidy_vg_sales <- clean_vg_sales %>% 
  tidyr::pivot_longer(cols = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales),
                      names_to = c("Country", NA), values_to = "Sales", names_sep = "_"
  )
smmrsd_vg_sales <- tidy_vg_sales %>%
  dplyr::group_by(Publisher, Year, Country) %>%
  dplyr::summarise(Total_Sales = sum(Sales), .groups = "drop") %>%
  dplyr::mutate(Publisher = forcats::fct_lump_n(Publisher, n = 5, w = Total_Sales)) %>%
  dplyr::group_by(Publisher, Year, Country) %>%
  dplyr::summarise(Total_Sales = sum(Total_Sales), .groups = "drop")