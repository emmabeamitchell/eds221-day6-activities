rm(list = ls())

library(tidyverse)
library(here)
library(janitor)
library(dplyr)


Lobster_Abundance_data <- read_csv(here("data", "Lobster_Abundance_All_Years_20210412.csv"), na = c("-99999", "")) |> #without the na, many size_mm values are -99999 which doesn't make sense
  clean_names()|> #makes it all lower snake case
  uncount(count)#opposite of count, reversing a previous count addition

 lobster_summary_table <- Lobster_Abundance_data |>
   group_by(year, site) |>
   summarize(individuals = n(), carapace_mean = mean(size_mm, na.rm = TRUE))


 ggplot(lobster_individuals, aes(x = year, y = individuals)) + geom_point(aes(color = site)) +facet_wrap(vars(site))

 subset_2020 <- Lobster_Abundance_data |>
   filter(year == 2020)

 legal_2020 <- subset_2020 |>
   mutate(legal = case_when(size_mm <= 79.76 ~ 'YES',
                            size_mm > 79.76 ~ 'NO')) |>
   group_by(site, legal) |>
   summarize(total_lobsters = n())

 ggplot(legal_2020, aes(x = site, y = total_lobsters)) + geom_col(aes(fill = legal, position = "fill"))


 ggplot(legal_2020, aes(x = site, y = total_lobsters)) + geom_col(aes(fill = legal), position = "fill")

 ## it looks like Mohk ratio of legal is biggest

 ex_a <- Lobster_Abundance_data |>
   filter(site == c("IVEE", "CARP", "NAPL"))

 ex_b <- Lobster_Abundance_data |>
   filter(month == 8)

 ex_c <- Lobster_Abundance_data |>
   filter(site == "AQUE"| size_mm > 70)

 ex_d <- Lobster_Abundance_data |>
   filter(site != "NAPL")

 ex_e <- Lobster_Abundance_data |>
   group_by(site) |>
   summarize(carapace_mean = mean(size_mm, na.rm = TRUE), stdev_lobster = sd(size_mm, na.rm = TRUE))

 ex_f <- Lobster_Abundance_data |>
   group_by(site, month) |>
   summarize(lobster_max = max(size_mm, na.rm = TRUE))


 ex_g <- Lobster_Abundance_data |>
   mutate(size_cm = size_mm * 10)

 ex_h <- Lobster_Abundance_data |>
   mutate(site = str_to_lower(site))

 ex_i <- Lobster_Abundance_data |>
   mutate(area = as.character(area))
 class(ex_i$area)

 ex_j <- Lobster_Abundance_data|>
   mutate(size_bin = case_when(size_mm <= 70 ~ 'small', size_mm > 70 ~ 'big'))

 ex_k <- Lobster_Abundance_data |>
   mutate(designation = case_when(site %in% c("IVEE","NAPL") ~ 'MPA', TRUE ~ "NOT MPA"))
