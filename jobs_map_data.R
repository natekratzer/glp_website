#Jobs Map Data

library(tidyverse)
library(magrittr)

setwd("~/Desktop/glp_website")

unemp <- read_csv("ACS_15_5YR_B23025_with_ann.csv", skip = 1,
                  col_types = cols(Id2 = col_character(),
                                   Id = col_character()))

unemp %<>% select(Id, Id2, pop = `Estimate; In labor force: - Civilian labor force:`,
                  unemp = `Estimate; In labor force: - Civilian labor force: - Unemployed`)%>%
  transmute(unemp_pct = unemp/pop, Id, Id2)


write_csv(unemp, "map data/jobs_map_data.csv")

