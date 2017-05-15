library(tidyverse)

setwd("C:/Users/natek/Documents/GeoFRED data/")
dat <- read_csv("GeoFRED_unemployment.csv", skip = 1)
dat <- dat %>% rename(FIPS = `Region Code`)


pull_peers_FIPS <- function(dat){
  all.peers <- subset(dat, dat$FIPS == 1073 | dat$FIPS == 37119
                    |dat$FIPS == 39061 |dat$FIPS == 39049
                    |dat$FIPS == 26081 |dat$FIPS == 37081
                    |dat$FIPS == 45045 |dat$FIPS == 18097
                    |dat$FIPS == 29095 |dat$FIPS == 47093
                    |dat$FIPS == 21111 |dat$FIPS == 47157
                    |dat$FIPS == 47037 |dat$FIPS == 40109
                    |dat$FIPS == 31055 |dat$FIPS == 29189
                    |dat$FIPS == 29510 |dat$FIPS == 40143
                    |dat$FIPS == 12031 |dat$FIPS == 37183
                    |dat$FIPS == 39113 |dat$FIPS == 51760)
  all.peers$baseline <- 1
  all.peers$current <- 1
  all.peers$baseline[all.peers$FIPS==26081|all.peers$FIPS==29189
                     |all.peers$FIPS==29510|all.peers$FIPS==40109
                     |all.peers$FIPS==40143|all.peers$FIPS==45045
                     |all.peers$FIPS==47093]<-0
  all.peers$current[all.peers$FIPS== 12031|all.peers$FIPS==37183|
                      all.peers$FIPS==39113|all.peers$FIPS==51760]<-0
  all.peers
}

dat <- pull_peers_FIPS(dat)

unemp_dat <- dat %>%
  gather(4:24, key = "year", value = "unemployment") #4:224 refers to column indexes for all the years


##all the other geofred data

house_dat <- read_csv("all_transactions_house_price_index.csv", skip = 1)
burdened_dat <- read_csv("burdened_households.csv", skip = 1)
disconnected_dat <- read_csv("disconnected youth.csv", skip = 1)
home_own_dat <- read_csv("homeownership_rate.csv", skip = 1)
income_inequality_dat <- read_csv("income_inequality.csv", skip = 1)
commute_time_dat <- read_csv("mean_commuting_time.csv", skip = 1)
median_household_income_dat <- read_csv("median_household_income.csv", skip = 1)
net_migration_flow_dat <- read_csv("net_migration_flow.csv", skip = 1)
personal_income_per_cap_dat <- read_csv("per capita personal income.csv", skip = 1)
race_dat <- read_csv("white to nonwhite racial dissimilarity index.csv", skip = 1)

##pulling peers

#renaming
house_dat <- house_dat %>% rename(FIPS = `Region Code`)
burdened_dat <- burdened_dat %>% rename(FIPS = `Region Code`)
disconnected_dat <- disconnected_dat %>% rename(FIPS = `Region Code`)
home_own_dat <- home_own_dat %>% rename(FIPS = `Region Code`)
income_inequality_dat <- income_inequality_dat %>% rename(FIPS = `Region Code`)
commute_time_dat <- commute_time_dat %>% rename(FIPS = `Region Code`)
median_household_income_dat <- median_household_income_dat %>% rename(FIPS = `Region Code`)
net_migration_flow_dat <- net_migration_flow_dat %>% rename(FIPS = `Region Code`)
personal_income_per_cap_dat <- personal_income_per_cap_dat %>% rename(FIPS = `Region Code`)
race_dat <- race_dat %>% rename(FIPS = `Region Code`)

#pulling

house_dat <- pull_peers_FIPS(house_dat)
burdened_dat <- pull_peers_FIPS(burdened_dat)
disconnected_dat <- pull_peers_FIPS(disconnected_dat)
home_own_dat <- pull_peers_FIPS(home_own_dat)
income_inequality_dat <- pull_peers_FIPS(income_inequality_dat)
commute_time_dat <- pull_peers_FIPS(commute_time_dat)
median_household_income_dat <- pull_peers_FIPS(median_household_income_dat)
net_migration_flow_dat <- pull_peers_FIPS(net_migration_flow_dat)
personal_income_per_cap_dat <- pull_peers_FIPS(personal_income_per_cap_dat)
race_dat <- pull_peers_FIPS(race_dat)




##reformatting
house_dat <- house_dat %>%
  gather(4:24, key = "year", value = "housing_price_index") #4:224 refers to column indexes for all the years

burdened_dat <- burdened_dat %>%
  gather(4:9, key = "year", value = "burdened_households") #4:224 refers to column indexes for all the years

disconnected_dat <- disconnected_dat %>%
  gather(4:10, key = "year", value = "disconnected_youth") #4:224 refers to column indexes for all the years

home_own_dat <- home_own_dat %>%
  gather(4:10, key = "year", value = "home_ownership") #4:224 refers to column indexes for all the years

income_inequality_dat <- income_inequality_dat %>%
  gather(4:9, key = "year", value = "income_inequality") #4:224 refers to column indexes for all the years

commute_time_dat <- commute_time_dat %>%
  gather(4:10, key = "year", value = "commute_time") #4:224 refers to column indexes for all the years

median_household_income_dat <- median_household_income_dat %>%
  gather(4:24, key = "year", value = "median_household_income") #4:224 refers to column indexes for all the years

net_migration_flow_dat <- net_migration_flow_dat %>%
  gather(4:8, key = "year", value = "net_migration_flow") #4:224 refers to column indexes for all the years

personal_income_per_cap_dat <- personal_income_per_cap_dat %>%
  gather(4:24, key = "year", value = "personal_income_per_cap") #4:224 refers to column indexes for all the years

race_dat <- race_dat %>%
  gather(4:10, key = "year", value = "racial_geography") #4:224 refers to column indexes for all the years


fred_dat <- full_join(burdened_dat, commute_time_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, disconnected_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, home_own_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, house_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, income_inequality_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, median_household_income_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, net_migration_flow_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, personal_income_per_cap_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, race_dat, by = c("FIPS", "year"))
fred_dat <- full_join(fred_dat, unemp_dat, by = c("FIPS", "year"))

fred_dat <- fred_dat %>% select(FIPS, year, baseline, current, 
                                burdened_households, commute_time, disconnected_youth,
                                home_ownership, housing_price_index, income_inequality, 
                                median_household_income, net_migration_flow, personal_income_per_cap,
                                racial_geography, unemployment)


write_csv(fred_dat, "GeoFRED data processed.csv")


#current peers
fred_dat <- fred_dat %>% filter(current == 1)
fred_dat$FIPS <- as.numeric(fred_dat$FIPS)
fred_dat$housing_price_index <- as.numeric(fred_dat$housing_price_index)
fred_dat$median_household_income <- as.numeric(fred_dat$median_household_income)
fred_dat$racial_geography <- as.numeric(fred_dat$racial_geography)


#
##Now to give the FIPS codes names
names <- read.csv("C:/Users/natek/Dropbox/GLP/FIPS two stl.csv")

data_named <- left_join(fred_dat, names, by = "FIPS")

##add population data
population_data <- read.csv("C:/users/natek/Dropbox/GLP/Web Update/population_data.csv")
data_named <- left_join(data_named, population_data, by = c("FIPS", "year"))

##and merge St. Louis city and St. Louis county

dat <- data_named %>%
  select(-county, -state) %>%
  group_by(city, year) %>%
  summarise_each(funs(weighted.mean(.,population)), -population)

# give St. Louis Merged a new FIPS
dat$FIPS <- as.character(dat$FIPS)
dat$FIPS[dat$city == "St. Louis"] <- "MERGED"
dat$FIPS <- as.factor(dat$FIPS)

# and then add the other stuff for each city again
names <- read.csv("C:/users/natek/Dropbox/GLP/FIPS one stl.csv")
names$FIPS <- as.factor(names$FIPS)

fred_dat = left_join(dat, names, by = "FIPS")

write_csv(fred_dat, "GeoFRED data processed.csv")



rank_and_nb_group<-function(df, var, order="Descending", peers="Current",
                            plot_title="", y_title = "Percent", caption_text = ""){
  df$var <- df[[var]]
  if(peers=="Current"){
    df<-subset(df,current_peer ==1)
  }
  if(peers=="Baseline"){
    df<-subset(df,baseline_peer ==1)
  }
  if(order=="Descending"){
    d.order<-df[order(-df$var),]
  }
  if(order=="Ascending"){
    d.order<-df[order(df$var),]
  }
  ranks<-1:length(df$var)
  d.rank<-cbind(d.order,ranks)
  names<-paste(d.rank$ranks,".",sep="")
  names<-paste(names,d.rank$city)
  d.graph<-cbind(d.rank,names)
  
  breaks<-classIntervals(d.graph$var,3,style="jenks")
  d.graph$color<-NA
  d.graph$color[d.graph$var<=breaks$brks[2]]<-"green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]]<-"yellow"
  d.graph$color[d.graph$var>breaks$brks[3]]<-"red"
  d.graph$round<-format(round(d.graph$var,1),nsmall=1)
  d.graph$textfont<-"Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"]<-"Museo Sans Italic"
  d.graph$linecolor<-"white"
  d.graph$linecolor[d.graph$city == "Louisville"]<-"#00a9b7"
  d.graph$textcolor<-"black"
  d.graph$textcolor[d.graph$city == "Louisville"]<-"#00a9b7"
  
  
  p<-ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                             y=var,fill=factor(color)))+guides(fill=FALSE)
  p<-p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p<-p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p<-p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
  }
  p <- p + theme(text = element_text(family = "Museo Sans 300"),
                 plot.title = element_text(size = 18, hjust = 0.5),
                 axis.text.y=element_text(hjust=0, family = rev(d.graph$textfont),
                                          size=12, color = rev(d.graph$textcolor)),
                 axis.ticks=element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(),
                 plot.subtitle = element_text(hjust = 0.5))
  p <- p+geom_text(aes(label=round),hjust=1.1, size=5, family = "Museo Sans 300")
  p<-p+labs(title = plot_title, y= y_title,
            x = "", caption = caption_text)
  p
}

font.add("Museo Sans 300", "C:/Users/natek/Documents/fonts/MuseoSans/MuseoSans_300.otf")
font.add("Museo Sans Italic", "C:/Users/natek/Documents/fonts/MuseoSans/MuseoSans_300_Italic.otf")

setwd("C:/Users/natek/Documents/images")

jpeg("edu_under_5_pov_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(educ_data_15, "under_5_per", order = "Ascending",
                  plot_title = "Children Under 5 in Poverty, 2015", 
                  caption_text = "Source: Greater Louisville Project \nData from the American Community Survey, Table B17001")
showtext.end()
dev.off()






