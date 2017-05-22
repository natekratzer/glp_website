library(showtext)
library(reshape2)
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


#current peers
fred_dat$year <- as.numeric(fred_dat$year)
fred_dat$housing_price_index <- as.numeric(fred_dat$housing_price_index)
fred_dat$median_household_income <- as.numeric(fred_dat$median_household_income)
fred_dat$racial_geography <- as.numeric(fred_dat$racial_geography)


#
##Now to give the FIPS codes names
names <- read_csv("C:/Users/natek/Dropbox/GLP/FIPS two stl.csv")

names$FIPS <- as.numeric(names$FIPS)
data_named <- left_join(fred_dat, names, by = "FIPS")

##add population data
population_data <- read_csv("C:/users/natek/Dropbox/GLP/Web Update/population_data.csv")
data_named$year <- as.numeric(data_named$year)
data_named <- left_join(data_named, population_data, by = c("FIPS", "year"))

##and merge St. Louis city and St. Louis county

dat <- data_named %>%
  select(-county, -state) %>%
  group_by(city, year) %>%
  summarise_each(funs(weighted.mean(.,population)), -population) %>%
  ungroup()

# give St. Louis Merged a new FIPS
dat$FIPS <- as.character(dat$FIPS)
dat$FIPS[dat$city == "St. Louis"] <- "MERGED"
dat$FIPS <- as.factor(dat$FIPS)

# and then add the other stuff for each city again
names <- read.csv("C:/users/natek/Dropbox/GLP/FIPS one stl.csv")
names$FIPS <- as.factor(names$FIPS)

fred_dat <- left_join(dat, names, by = c("FIPS", "city"))

population_data$FIPS <- as.character(population_data$FIPS)
fred_dat <- left_join(fred_dat, population_data, by = c("FIPS", "year"))


fred_dat <- fred_dat %>% 
  group_by(city, year) %>%
  mutate(population = sum(population)) %>%
  ungroup()

fred_dat <- fred_dat %>% 
  mutate(migration_pop = net_migration_flow/population)

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
  
  breaks <- classIntervals(d.graph$var,3,style="jenks")
  d.graph$color <- NA
  d.graph$color[d.graph$var<=breaks$brks[2]] <- "green"
  d.graph$color[d.graph$var>breaks$brks[2] & d.graph$var<=breaks$brks[3]] <- "yellow"
  d.graph$color[d.graph$var>breaks$brks[3]] <- "red"
  d.graph$round <- format(round(d.graph$var,1),nsmall=1)
  d.graph$textfont <- "Museo Sans 300"
  d.graph$textfont[d.graph$city == "Louisville"] <- "Museo Sans Italic"
  d.graph$linecolor <- "white"
  d.graph$linecolor[d.graph$city == "Louisville"] <- "#00a9b7"
  d.graph$textcolor <- "black"
  d.graph$textcolor[d.graph$city == "Louisville"] <- "#00a9b7"
  
  
  p <- ggplot(data=d.graph,aes(x=factor(names, levels=rev(unique(names))),
                             y=var,fill=factor(color)))+guides(fill=FALSE)
  p <- p+geom_bar(stat="identity",color=rev(d.graph$linecolor), size = 1)+coord_flip()+theme_tufte()
  if(order=="Ascending"){
    p <- p+scale_fill_manual(values=c("#96ca4f","#db2834","#ffd600"))
  }
  if(order=="Descending"){
    p <- p+scale_fill_manual(values=c("#db2834","#96ca4f","#ffd600"))
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
  p <- p+labs(title = plot_title, y= y_title,
            x = "", caption = caption_text)
  p
}

##rolling mean functions for trendlines
rollmean3 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-1],x[i],x[i+1]))
    y[1] <- NA
  }
  y
}

rollmean5 <- function(x){
  n <- length(x)
  y <- NA
  for(i in 1:n){
    y[i] <- mean(c(x[i-2],x[i-1],x[i],x[i+1],x[i+2]))
    y[1] <- NA
    y[2] <- NA
  }
  y
}

##
graph_trendline<-function(df,var, plot_title="",y_title="Percent", peers = "Current", 
                          caption_text = "", subtitle_text = "", rollmean = 3,
                          break_settings = seq(2005, 2015, 2), xmin = 1996, xmax = 2016){
  df$var <- df[[var]]
  df = df %>% filter(year != 2016)
  
  if(peers=="Current"){
    df.wol <- subset(df,current == 1 & FIPS!=21111)
  }
  
  if(peers=="Baseline"){
    df.wol <- subset(df,baseline == 1 & FIPS!=21111)
  }
  
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  
  lville = df %>% 
    filter(FIPS == 21111) %>% 
    select(var, year)
  
  dat = full_join(lville, output_wol, by = "year")
  
  if (rollmean == 3){
    dat$var = rollmean3(dat$var)
    dat$first_quarter = rollmean3(dat$first_quarter)
    dat$mean = rollmean3(dat$mean)
    dat$third_quarter = rollmean3(dat$third_quarter)
    dat <- dat %>% filter(year > 2005 & year < 2015)
  }
  
  if (rollmean == 5){
    dat$var = rollmean5(dat$var)
    dat$first_quarter = rollmean5(dat$first_quarter)
    dat$mean = rollmean5(dat$mean)
    dat$third_quarter = rollmean5(dat$third_quarter)
    dat = dat %>% filter(year > 2006 & year < 2014)
  }
  
  data_long <- melt(dat, id="year")
  data_long$variable = factor(data_long$variable, levels = c("var", "third_quarter", "mean", "first_quarter"))
  
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=variable,linetype=variable))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value)+min(data_long$value))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value) - border_space, max(data_long$value + border_space)))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  p<-p+scale_colour_manual(values=cPalette,
                           labels=c("Louisville", "75th Percentile", "Peer City Mean", "25th Percentile"))+
    scale_linetype_manual(values=c("solid","dashed","dashed","dashed"),
                          labels=c("Louisville", "75th Percentile", "Peer City Mean", "25th Percentile"))
  p<-p+theme(text = element_text(family = "Museo Sans 300"),
             legend.title=element_blank(),
             legend.position = "top",
             axis.text=element_text(size=12, family = "Museo Sans 300"),
             axis.ticks.y=element_blank(),
             plot.title=element_text(size=18, hjust=.5, family = "Museo Sans 300",
                                     margin=margin(b=10,unit="pt")),
             legend.text=element_text(size=12, family = "Museo Sans 300"),
             plot.caption = element_text(family = "Museo Sans 300"),
             plot.subtitle = element_text(family = "Museo Sans 300", hjust = 0.5))
  p<-p+labs(title=plot_title,x="Year",
            y=y_title, caption = caption_text, subtitle = subtitle_text)
  p
}


font.add("Museo Sans 300", "C:/Users/natek/Documents/fonts/MuseoSans/MuseoSans_300.otf")
font.add("Museo Sans Italic", "C:/Users/natek/Documents/fonts/MuseoSans/MuseoSans_300_Italic.otf")

setwd("C:/Users/natek/Documents/images")


##trendlines
jpeg("qop_burdened_households_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "burdened_households",
                plot_title = "Housing Cost Burdened Households",
                subtitle = "Spending over 30% of Income on Housing",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2010, 2014, 2),
                xmin = 2010, xmax = 2014)
showtext.end()
dev.off()

jpeg("qop_commute_time_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "commute_time",
                plot_title = "Mean Commute Time",
                subtitle = "Annual",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2009, 2015, 2),
                xmin = 2009, xmax = 2015, y_title = "Minutes")
showtext.end()
dev.off()

jpeg("qop_disconnected_youth_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "disconnected_youth",
                plot_title = "Disconnected Youth",
                subtitle = "Persons age 16-19 who are not in school and not in the labor force",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2009, 2015, 2),
                xmin = 2009, xmax = 2015)
showtext.end()
dev.off()

jpeg("qop_home_ownership_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "home_ownership",
                plot_title = "Home Ownership",
                subtitle = "Annual",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2009, 2015, 2),
                xmin = 2009, xmax = 2015)
showtext.end()
dev.off()

jpeg("qop_housing_price_index_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "housing_price_index",
                plot_title = "Housing Price Index",
                subtitle = "Annual",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(1996, 2015, 2),
                xmin = 1996, xmax = 2015, y_title = "Index")
showtext.end()
dev.off()

jpeg("qop_income_inequality_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "income_inequality",
                plot_title = "Income Inequality",
                subtitle = "Mean income of top quintile divided by mean income of bottom quintile",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2010, 2015, 2),
                xmin = 2010, xmax = 2015, y_title = "Ratio")
showtext.end()
dev.off()

jpeg("jobs_median_household_income_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "median_household_income",
                plot_title = "Median Household Income",
                subtitle = "Annual",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(1998, 2014, 2),
                xmin = 1997, xmax = 2014, y_title = "Dollars")
showtext.end()
dev.off()

jpeg("qop_net_migration_flow_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "net_migration_flow",
                plot_title = "Net Migration Flow",
                subtitle = "Annual",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2009, 2013, 2),
                xmin = 2009, xmax = 2013, y_title = "")
showtext.end()
dev.off()

jpeg("jobs_personal_income_per_cap_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "personal_income_per_cap",
                plot_title = "Per Capita Personal Income",
                subtitle = "Annual",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(1996, 2015, 2),
                xmin = 1996, xmax = 2015, y_title = "Dollars")
showtext.end()
dev.off()

jpeg("qop_racial_geography_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "racial_geography",
                plot_title = "Geographic Racial Dissimilarity Index",
                subtitle = "Percent of Whites who would need to move census tracts to equalize percent White across all tracts.",
                rollmean = 1,
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                break_settings = seq(2009, 2015, 2),
                xmin = 2009, xmax = 2015)
showtext.end()
dev.off()

jpeg("jobs__unemployment_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline(fred_dat, "unemployment",
                plot_title = "Unemployment",
                caption_text = "Source: Greater Louisville Project \nData from The Federal Reserve via GeoFRED",
                subtitle = "Annual",
                rollmean = 1,
                break_settings = seq(1996, 2015, 2),
                xmin = 1996, xmax = 2015)
showtext.end()
dev.off()





##rankings

jpeg("edu_under_5_pov_ranking.jpg", 900, 600, res = 100)
showtext.begin()
rank_and_nb_group(educ_data_15, "under_5_per", order = "Ascending",
                  plot_title = "Children Under 5 in Poverty, 2015", 
                  caption_text = "Source: Greater Louisville Project \nData from the American Community Survey, Table B17001")
showtext.end()
dev.off()






