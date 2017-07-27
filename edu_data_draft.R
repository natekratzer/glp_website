library(tidyverse)
library(magrittr)
library(reshape2)
library(showtext)
library(wesanderson)

setwd("~/Desktop/glp_website/input data/educ_input_data")

ccr_read <- function(folder) {
  initial_wd <- getwd()
  directory <- paste(initial_wd, folder, sep = "/")
  file_names <- list.files(directory)
  n <- length(file_names)
  for (i in 1:n) {
    data <-
      read_csv(paste(directory, file_names[i], sep = "/"),
               skip = 0,
               progress = FALSE, 
               col_types = cols(PCT_CCR_NO_BONUS = col_double(),
                                NBR_GRADUATES_WITH_DIPLOMA = col_double()))
    data$SCH_NAME[data$SCH_NAME == "--District Total--"] = "---District Total---"
    data %<>% filter(SCH_NAME == "---District Total---")%>%
      select(school_year = SCH_YEAR, dist_name = DIST_NAME, SCH_NAME,
                     label = DISAGG_LABEL, pct_ccr = PCT_CCR_NO_BONUS,
                     number_grads = NBR_GRADUATES_WITH_DIPLOMA)

    if (i == 1) {
      df <- data
    }
    else{

      df <- bind_rows(df, data)
    }
  }
  df
}

act_read <- function(folder) {
  initial_wd <- getwd()
  directory <- paste(initial_wd, folder, sep = "/")
  file_names <- list.files(directory)
  n <- length(file_names)
  for (i in 1:n) {
    data <-
      read_csv(paste(directory, file_names[i], sep = "/"),
               skip = 0,
               progress = FALSE, 
               col_types = cols(STDNT_TESTED_CNT = col_double(),
                                ENGLISH_MEAN_SCORE = col_double(),
                                MATHEMATICS_MEAN_SCORE = col_double(),
                                READING_MEAN_SCORE = col_double(),
                                SCIENCE_MEAN_SCORE = col_double(),
                                COMPOSITE_MEAN_SCORE = col_double()))
    data$SCH_NAME[data$SCH_NAME == "--- District Total ---"] = "---District Total---"
    data %<>% filter(SCH_NAME == "---District Total---")%>%
      select(school_year = SCH_YEAR, dist_name = DIST_NAME, SCH_NAME,
             label = DISAGG_LABEL,
             num_students_tested = STDNT_TESTED_CNT,
             english_score = ENGLISH_MEAN_SCORE,
             math_score = MATHEMATICS_MEAN_SCORE,
             reading_score = READING_MEAN_SCORE,
             science_score = SCIENCE_MEAN_SCORE,
             composite_score = COMPOSITE_MEAN_SCORE)
    
    if (i == 1) {
      df <- data
    }
    else{
      
      df <- bind_rows(df, data)
    }
  }
  df
}

ccr <- ccr_read("CCR_acc_data")
act<- act_read("ACT_data")

#CCR Scores


ccr_new <- ccr %>%
  filter((
    label == "All Students" |
      label == "Male" |
      label == "Female" |
      label == "White (Non-Hispanic)" |
      label == "African American" |
      label == "Hispanic" |
      label == "Asian"
  )
  )

# ccr_jeff <- ccr %>%
#   filter(dist_name == "Jefferson County" & (label == "All Students" | 
#            label == "Male" | label == "Female" | 
#            label == "White (Non-Hispanic)" | 
#            label == "African American" |
#            label == "Hispanic" | 
#              label == "Asian"))%>%
#   select(dist_name, pct_ccr, school_year, label)
# 
# ccr_ky_average <- ccr %>%
#   filter(
#     dist_name != "Jefferson County" & number_grads >= 0 &
#     (
#       label == "All Students" |
#         label == "Male" | 
#         label == "Female" |
#         label == "White (Non-Hispanic)" |
#         label == "African American" | #Data for 58 school districts for at least one year
#         label == "Hispanic" |#Data for 37 school districts for at least one year
#         label == "Asian" #Data for 38 school districts for at least one year
#     )
#   ) %>%
#   group_by(school_year, label) %>%
#   summarize(pct_ccr = weighted.mean(pct_ccr, number_grads, na.rm = TRUE))%>%
#   ungroup()
# ccr_ky_average$dist_name = "Kentucky Average"
# 
# #Combine the Data
# ccr_data<-bind_rows(ccr_ky_average, ccr_jeff)


#ACT Scores


act_new <- act %>%
  filter((
    label == "All Students" |
      label == "Male" |
      label == "Female" |
      label == "White (Non-Hispanic)" |
      label == "African American" |
      label == "Hispanic" |
      label == "Asian"
  )
  )

ky_new <-left_join(act_new, ccr_new, by = c("school_year","dist_name","SCH_NAME", "label"))
ky_new %<>%  mutate(year = as.numeric(substr(school_year,5,8)))%>%
  select(year,dist_name,label,num_students_tested:number_grads)
  




# act_jeff <- act %>%
#   filter(dist_name == "Jefferson County" & (label == "All Students" | 
#                                               label == "Male" | label == "Female" | 
#                                               label == "White (Non-Hispanic)" | 
#                                               label == "African American" |
#                                               label == "Hispanic" | 
#                                               label == "Asian"))%>%
#   select(-c(SCH_NAME, num_students_tested))
# 
# act_ky_average <- act %>%
#   filter(
#     dist_name != "Jefferson County" & num_students_tested >= 0 &
#       (
#         label == "All Students" |
#           label == "Male" | 
#           label == "Female" |
#           label == "White (Non-Hispanic)" | #Data for 170 school districts for at least one year
#           label == "African American" | #Data for 155 school districts for at least one year
#           label == "Hispanic" |#Data for 158 school districts for at least one year
#           label == "Asian" #Data for 112 school districts for at least one year
#       )
#   ) %>%
#   group_by(school_year, label) %>%
#   summarize(composite_score = weighted.mean(composite_score, num_students_tested, na.rm = TRUE),
#             english_score = weighted.mean(english_score, num_students_tested, na.rm = TRUE),
#             math_score = weighted.mean(math_score, num_students_tested, na.rm = TRUE),
#             science_score = weighted.mean(science_score, num_students_tested, na.rm = TRUE),
#             reading_score = weighted.mean(reading_score, num_students_tested, na.rm = TRUE))%>%
#   ungroup()
# act_ky_average$dist_name = "Kentucky Average"
# 
# #Combine the data
# act_data<-bind_rows(act_ky_average, act_jeff)
# 
# ky_ed_data <- left_join(act_data, ccr_data, by = c("school_year", "label", "dist_name"))
# ky_ed_data%<>% mutate(year = as.numeric(substr(school_year,5,8)))%>%
#   select(year, label, dist_name, composite_score:reading_score,pct_ccr)

#Tidy this very untidy dataset - This solution is very inelegant, and there
#is probably a better way to do this, but alas this is what I thought of in the
#moment

rename_cols <- function(df, text){
  names <- colnames(df)
  new <- paste(names, text, sep = "_")
  colnames(df) <- new
  df
}

tidy <- function(data, var, key){
  data$var <- data[[var]]
  data$key <- data[[key]]
  data %<>% mutate(key = tolower(gsub(" ", "_", key)))
  data$key[data$key == "white_(non-hispanic)"] = "white"
  data %<>% select(var, key, year, dist_name) %>%
    spread(key = key, value = var)
  data = bind_cols(data[,1:2], rename_cols(data[,3:9], var))
  data
}

restore_ky_ed_data <- function(data){
  var_names <- colnames(data)
  var_names <- var_names[4:11]
  n <- length(var_names)
  for(i in 1:n){
    df = tidy(data, var_names[i], key = "label")
    if(i == 1){
      tidy_df <- df
    }
    else{
      tidy_df <- left_join(tidy_df, df, by = c("year", "dist_name"))
    }
  }
  return(tidy_df)
}

ky_tidy_new <- restore_ky_ed_data(ky_new)








#NAEP data
naep_math <- read_csv("naep_math_data.csv")
naep_reading <- read_csv("naep_reading_data.csv")

naep_reading %<>% select(Year, Jurisdiction, naep_reading_score = `Average scale score`, 
                         reading_sd = `Standard Error`)
naep_math %<>% select(Year, Jurisdiction, naep_math_score = `Average scale score`, 
                      math_sd = `Standard Error`)

naep_data <- left_join(naep_reading, naep_math, by = c("Year", "Jurisdiction"))
naep_data %<>% rename(year = Year)


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
graph_trendline_ky_ed<-function(df,var, plot_title="",y_title="Percent", 
                          caption_text = "", subtitle_text = "", rollmean = 1,
                          break_settings = seq(2005, 2015, 2), xmin = 1996, xmax = 2016,
                          order = "Descending"){
  df$var <- df[[var]]
  # df = df %>% filter(year != 2016)
  output_wol = df %>% 
    group_by(year) %>%
    summarise(first_quarter = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              third_quarter = quantile(var, prob = 0.75, na.rm = TRUE))
  lville = df %>% 
    filter(dist_name == "Jefferson County") %>% 
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
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- c("#00a9b7","grey50", "black","grey50")
  if(order == "Descending") {
    p <- p + scale_colour_manual(
      values = cPalette,
      labels = c(
        "JCPS",
        "75th Percentile",
        "KY School District Mean",
        "25th Percentile"
      )
    ) +
      scale_linetype_manual(
        values = c("solid", "dashed", "dashed", "dashed"),
        labels = c(
          "JCPS",
          "75th Percentile",
          "KY School District Mean",
          "25th Percentile"
        )
      )
  }
  if(order == "Ascending"){
    p <- p + scale_colour_manual(
      values = cPalette,
      labels = c(
        "JCPS",
        "25th Percentile",
        "KY School District Mean",
        "75th Percentile"
      )
    ) +
      scale_linetype_manual(
        values = c("solid", "dashed", "dashed", "dashed"),
        labels = c(
          "JCPS",
          "25th Percentile",
          "KY School District Mean",
          "75th Percentile"
        )
      )
  }
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







ky_ed_data_long <- function(data_long, var = "var", value = "value", plot_title="",y_title="Percent", 
                            caption_text = "", subtitle_text = "", rollmean = 1,
                            break_settings = seq(2005, 2015, 2), xmin = 1996, xmax = 2016,
                            order = "Descending", labels, color_pal){
  data_long$var <- data_long[[var]]
  data_long$value<-data_long[[value]]
  data_long %<>% select(year, var, value)
  p <- ggplot(data=data_long,aes(x=year,y=value,colour=var))+
    geom_point(size = 1.8)+
    geom_line(size = 1)
  p <- p + theme_bw()
  midpoint <- (max(data_long$value, na.rm = TRUE)+min(data_long$value, na.rm = TRUE))/2
  border_space <- .1 * midpoint
  p <- p + ylim(c(min(data_long$value, na.rm = TRUE) - border_space, max(data_long$value, na.rm=TRUE) + border_space))
  p<-p+scale_x_continuous(limits = c(xmin, xmax), breaks = break_settings)
  cPalette <- color_pal
  p <- p + scale_colour_manual(values = cPalette, labels = labels)
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

#Plot ACT by Ethnicity for JCPS
jeff_eth_act <- ky_tidy_new %>%
  filter(dist_name == "Jefferson County")%>%
  select(year, white_composite_score, african_american_composite_score,
         asian_composite_score, hispanic_composite_score)%>%
  gather(white_composite_score:hispanic_composite_score, key = "var",
         value = "value")
jeff_eth_act$var <- factor(jeff_eth_act$var, levels = unique(jeff_eth_act$var))




#Plot CCR by Ethnicity for JCPS
jeff_eth_ccr <- ky_tidy_new %>%
  filter(dist_name == "Jefferson County")%>%
  select(year, white_pct_ccr, african_american_pct_ccr,
         asian_pct_ccr, hispanic_pct_ccr)%>%
  gather(white_pct_ccr:hispanic_pct_ccr, key = "var", value = "value")
jeff_eth_ccr$var <- factor(jeff_eth_ccr$var, levels = unique(jeff_eth_ccr$var))





#Graphing
setwd("~/Desktop/glp_website/Images/Edu_Images")

font.add("Museo Sans 300", "/Users/BryceRowland/Desktop/GLP/R Code/MuseoSans_300.otf")
font.add("Museo Sans 300 Italic", "/Users/BryceRowland/Desktop/GLP/R Code/MuseoSans_300_Italic.otf")

jpeg("edu_ccr_ethnicity_trendline.jpg", 900, 600, res = 100)
showtext.begin()
ky_ed_data_long(jeff_eth_ccr, break_settings = seq(2012, 2016,2), xmin = 2012,
                xmax = 2016, plot_title = "College and Career Readiness by Ethnicity, JCPS", 
                labels = c("White", "African American", "Asian", "Hispanic"), 
                y_title = "Percent", color_pal = wes_palette("Moonrise3"))
showtext.end()
dev.off()


jpeg("edu_act_ethnicity_trendline.jpg", 900, 600, res = 100)
showtext.begin()
ky_ed_data_long(jeff_eth_act, break_settings = seq(2012, 2016,2), xmin = 2012,
                xmax = 2016, plot_title = "ACT Scores by Ethnicity, JCPS", 
                labels = c("White", "African American", "Asian", "Hispanic"), 
                y_title = "Composite Score",color_pal = wes_palette("Moonrise3"))
showtext.end()
dev.off()

jpeg("edu_act_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_ky_ed(
  ky_tidy_new,
  "all_students_composite_score",
  plot_title = "ACT Composite Scores",
  y_title = "Composite Score",
  xmin = 2012,
  xmax = 2016,
  break_settings = seq(2012, 2016, 2)
)
showtext.end()
dev.off()

jpeg("edu_ccr_trendline.jpg", 900, 600, res = 100)
showtext.begin()
graph_trendline_ky_ed(
  ky_tidy_new,
  "all_students_pct_ccr",
  plot_title = "College and Career Readiness",
  y_title = "Percent",
  xmin = 2012,
  xmax = 2016,
  break_settings = seq(2012, 2016, 2)
)
showtext.end()
dev.off()


#Plot NAEP Data
jpeg("edu_naep_reading_trendline.jpg", 900, 600, res = 100)
showtext.begin()
ky_ed_data_long(naep_data, var = "Jurisdiction", value = "naep_reading_score", break_settings = seq(2009, 2016,2), xmin = 2009,
                xmax = 2015, plot_title = "NAEP 4th Grade Reading Score",
                labels = c("JCPS", "Kentucky School District Mean"),
                y_title = "Composite Score", color_pal = c("#00a9b7", "black"))
showtext.end()
dev.off()

jpeg("edu_naep_math_trendline.jpg", 900, 600, res = 100)
showtext.begin()
ky_ed_data_long(naep_data, var = "Jurisdiction", value = "naep_math_score", break_settings = seq(2009, 2016,2), xmin = 2009,
                xmax = 2015, plot_title = "NAEP 4th Grade Math Score",
                labels = c("JCPS", "Kentucky School District Mean"),
                y_title = "Composite Score", color_pal = c("#00a9b7", "black"))
showtext.end()
dev.off()


