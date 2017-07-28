library(tidyverse)
library(magrittr)


setwd("~/Desktop/glp_website")

#Functions
##using z-scores to identify MPI neighborhoods
NormZ<-function(x){
  (x-mean(x))/sd(x)
}

#Reading in Data
pc_ed_data<-read_csv("input data/qp_input_data/ACS_14_5YR_B23006_with_ann.csv", skip = 1)
pc_ed_data%<>%select(Id,Id2, ed_population = `Estimate; Total:`, 
                     less_than_hs = `Estimate; Less than high school graduate:`,
                     bachelors_up = `Estimate; Bachelor's degree or higher:`)%>%
  mutate(bachelors_degree = bachelors_up/ed_population,
         no_hs_degree = less_than_hs/ed_population)

pc_health_data<-read_csv("input data/qp_input_data/ACS_14_5YR_S2701_with_ann.csv", skip=1)
pc_health_data%<>%select(Id,Id2, health_population = `Total; Estimate; Total civilian noninstitutionalized population`, 
                         uninsured = `Percent Uninsured; Estimate; Total civilian noninstitutionalized population`)
pc_health_data$uninsured = as.numeric(as.character(pc_health_data$uninsured))

pc_low_inc_data<-read_csv("input data/qp_input_data/ACS_14_5YR_C17002_with_ann.csv", skip=1)
pc_low_inc_data%<>%
  select(
    Id2,Id,
    low_inc_population = `Estimate; Total:`,
    less_than_50 = `Estimate; Total: - Under .50`,
    less_than_1 = `Estimate; Total: - .50 to .99`,
    less_than_1.25 = `Estimate; Total: - 1.00 to 1.24`,
    less_than_1.5 = `Estimate; Total: - 1.25 to 1.49`
  ) %>% 
  mutate(
    low_income = (less_than_50 + less_than_1 + less_than_1.25 + less_than_1.5) /
      low_inc_population
  )

pc_unemp_data<-read_csv("input data/qp_input_data/ACS_14_5YR_S2301_with_ann.csv",skip=1)
pc_unemp_data%<>%
  select(Id2, Id,
         unemp_population = `Total; Estimate; Population 16 years and over`,
         unemployed = `Unemployment rate; Estimate; Population 16 years and over`)
pc_unemp_data$unemployed = as.numeric((pc_unemp_data$unemployed))

#Joining Data
pc_data = left_join(pc_ed_data, pc_health_data, by = c("Id2", "Id"))
pc_data = left_join(pc_data, pc_low_inc_data, by = c("Id2", "Id"))
pc_data = left_join(pc_data, pc_unemp_data, by = c("Id2", "Id"))

#Scaling PCTs
pc_data = pc_data %>%
  mutate(low_income = low_income*100, bachelors_degree = bachelors_degree*100, 
         no_hs_degree = no_hs_degree*100)

FIPS = substr(pc_data$Id2, 1,5)
FIPS = data.frame(FIPS)
pc_data = bind_cols(pc_data, FIPS)
pc_data = filter(pc_data, FIPS == "21111" & ed_population > 0) #Removes the airport

pc_mpi_data = pc_data %>%
  group_by(FIPS)%>%
  mutate(z_low_income = NormZ(low_income),
         z_unemployed = NormZ(unemployed),
         z_no_hs_degree = NormZ(no_hs_degree),
         z_uninsured = NormZ(uninsured),
         z_overall = (z_low_income+z_unemployed+z_no_hs_degree+z_uninsured)/4)

neighborhoods <- read_csv("~/Desktop/glp_website/map data/census tract neighborhoods.csv",
                          col_types = cols(Id2 = col_character()))

pc_mpi_data <- left_join(pc_mpi_data, neighborhoods, by = c("Id2"))


setwd("~/Desktop/glp_website/input data/qp_input_data")

#White Data
white_dat <- read_csv("ACS_14_5YR_B02001_with_ann.csv", skip = 1)
white_dat %<>% select(Id, Id2, population = `Estimate; Total:`, 
                      white_pop = `Estimate; Total: - White alone`)%>%
  mutate(pct_white = white_pop/population)

FIPS = substr(white_dat$Id2, 1,5)
FIPS = data.frame(FIPS)
white_dat = bind_cols(white_dat, FIPS)
white_dat = filter(white_dat, FIPS == "21111" & population > 0 ) #Removes the airport

total <- white_dat%>%
  group_by(FIPS)%>%
  summarize(total_pop = sum(population), 
            total_white = sum(white_pop))%>%
  mutate(total_white_pct = total_white/total_pop)


white_dat$total_white_pct = 0.7317378

white_dat %<>% mutate(dif_score = abs(pct_white - total_white_pct))

white_dat$pct_white = white_dat$pct_white*100
white_dat$dif_score = white_dat$dif_score*100

map_data <- left_join(pc_mpi_data, white_dat, by = c("Id2", "Id"))

setwd("~/Desktop/glp_website")

#ACS Data Analysis
med_inc <- read_csv("ACS_15_5YR_B19013_with_ann.csv", skip = 1,
                    col_types = cols(Id2 = col_character(),
                                     Id = col_character()))
# unemp <- read_csv("ACS_15_5YR_B23025_with_ann.csv", skip = 1,
#                   col_types = cols(Id2 = col_character(),
#                                    Id = col_character()))


med_inc %<>% select(Id, Id2, median_income = `Estimate; Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)`)
# unemp %<>% select(Id, Id2, pop = `Estimate; In labor force: - Civilian labor force:`, 
#                   unemp = `Estimate; In labor force: - Civilian labor force: - Unemployed`)%>%
#   transmute(unemp_pct = unemp/pop, Id, Id2)

map_data <- map_data%>%
  left_join(., med_inc, by = c("Id2", "Id"))

setwd("~/Desktop/glp_website/map data")
write_csv(map_data, "qp_map_data.csv")

