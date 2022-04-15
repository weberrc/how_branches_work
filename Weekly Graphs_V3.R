####################################
# Weekly Graphs Summarizing OBs
# Author: Rachel Weber
# Created: December 1, 2021
####################################

library(anytime)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)
library(grid)
library(magrittr)
library(RColorBrewer)
library(extrafont)
library(covidr)
loadfonts(device = "win")

dat <- read.csv(file = "C:/Users/rweber/Downloads/OB Data 12 15 2021.csv",
                sep = ",", na.strings = c("NA", "N/A", "n/a", "", " "))

# for any outbreaks where we cannot identify date determined
dat <- subset(dat, !is.na(dat$Date.met.outbreak.definition))

# convert to a date variable
dat$Date.met.outbreak.definition <- anydate(dat$Date.met.outbreak.definition)

# rename this cause that name is way too long
dat$OB_date <- dat$Date.met.outbreak.definition


# get week of outbreak from first week an outbreak was detected
# this will later be relabeled by first day of that week for clarity
dat <- dat %>% 
  mutate(week_start = floor_date(Date.met.outbreak.definition, unit= "month"))

# create broader categories
dat %<>%
  mutate(setting_group = case_when(COVID.Setting.Type == "Healthcare - Group Home" |
                                     COVID.Setting.Type == "Healthcare - Psychiatric Hospital" |
                                     COVID.Setting.Type == "Healthcare - Skilled Nursing" |
                                     COVID.Setting.Type == "Healthcare - Rehab Facility" |
                                     COVID.Setting.Type == "Healthcare - Acute Care Hospital" |
                                     COVID.Setting.Type == "Healthcare - Combined Care" |
                                     COVID.Setting.Type == "Healthcare - Long-Term Acute Care" |
                                     COVID.Setting.Type == "Healthcare - Memory Care" |
                                     str_detect(COVID.Setting.Type, "inpatient") |
                                     str_detect(COVID.Setting.Type, "Living") |
                                     str_detect(COVID.Setting.Type, "Hospice") ~ 'Residential Healthcare',
                                   
                                   COVID.Setting.Type == "Healthcare - Ambulatory Surgery Center" |
                                     COVID.Setting.Type == "Healthcare - Outpatient" |
                                     str_detect(COVID.Setting.Type, "outpatient") ~ "Outpatient Healthcare",
                                   
                                   COVID.Setting.Type == 'Grocery Store' | 
                                     COVID.Setting.Type == 'Caterer' | 
                                     COVID.Setting.Type == 'Convenience/Corner Store' | 
                                     COVID.Setting.Type == 'Retailer' |
                                     COVID.Setting.Type == 'Specialty Food Retailer' ~ "Retail/Grocery",
                                   
                                   COVID.Setting.Type == 'Banquet Facility' |
                                     COVID.Setting.Type == 'Bar/Tavern/Brewery' |
                                     COVID.Setting.Type == 'Indoor Entertainment/Rec' |
                                     COVID.Setting.Type == 'Nightclub' |
                                     COVID.Setting.Type == 'Outdoor Entertainment/Rec' |
                                     COVID.Setting.Type == 'Restaurant - Buffet' |
                                     COVID.Setting.Type == 'Restaurant - Fast Food' |
                                     COVID.Setting.Type == 'Restaurant - Other' |
                                     COVID.Setting.Type == 'Restaurant - Sit Down' ~ 'Bars/Restaurants/Entertainment',
                                   
                                   COVID.Setting.Type == 'Child Care Center' |
                                     COVID.Setting.Type == 'Youth Sports/Activities' |
                                     COVID.Setting.Type == 'College/University' |
                                     COVID.Setting.Type == 'School/College Dorm' |
                                     COVID.Setting.Type == "School K-12" |
                                     COVID.Setting.Type == "Overnight Camp" |
                                     COVID.Setting.Type == "Day Camp" |
                                     COVID.Setting.Type == "Trade School" |
                                     COVID.Setting.Type == "School Administration" ~ "Camps/Educ/Childcare",
                                   
                                   COVID.Setting.Type == 'Jail' |
                                     COVID.Setting.Type == 'Correctional, Other' |
                                     str_detect(COVID.Setting.Type, "Enforcement") |
                                     str_detect(COVID.Setting.Type, "Prison") ~ "Corrections/Law Enforcement",
                                   
                                   COVID.Setting.Type == 'Agriculture - Other' |
                                     COVID.Setting.Type == 'Farm/Dairy' |
                                     COVID.Setting.Type == "Farmer's Market" |
                                     COVID.Setting.Type == 'Food Distribution' |
                                     COVID.Setting.Type == 'Food Manufacturing/Packaging' |
                                     COVID.Setting.Type == 'Food Warehouse' |
                                     COVID.Setting.Type == 'Meat Processing/Packaging' ~ "Agriculture/Food Supply",
                                   
                                   COVID.Setting.Type == 'Distribution Center/Business' |
                                     COVID.Setting.Type == 'Materials Supplier' |
                                     COVID.Setting.Type == "Non-Food Manufacturer/Warehouse" |
                                     COVID.Setting.Type == "Home Maintenance Services" |
                                     str_detect(COVID.Setting.Type, "Construction") ~ "Manufacturing/Construction",
                                   
                                   COVID.Setting.Type == 'Homeless Shelter' ~ "Homeless Shelters",
                                   
                                   COVID.Setting.Type == 'Office/Indoor Workspace' ~ "Office/Indoor Workplace",
                                   
                                   COVID.Setting.Type == 'Hotel/Lodge/Resort' |
                                     COVID.Setting.Type == 'Casino' |
                                     COVID.Setting.Type == "Travel" |
                                     str_detect(COVID.Setting.Type, "Airport") ~ "Travel/Hospitality",
                                   
                                   COVID.Setting.Type == 'Fair/Festival/Temp. Mobile Event' |
                                     COVID.Setting.Type == 'Social Gathering' |
                                     COVID.Setting.Type == "Religious Facility" ~ "Gathering/Social Event",
                                   TRUE ~ 'Other'))


# count how many per week
OB_dates_sum2 <- dat %>% 
  group_by(week_start, setting_group) %>% 
  count()

# make a factor
OB_dates_sum2$setting_group <- as.factor(OB_dates_sum2$setting_group)

# sort the factor level so the colors look better in the graph
OB_dates_sum2$setting_group <- factor(OB_dates_sum2$setting_group,
                                      levels = c("Outpatient Healthcare", "Residential Healthcare", "Corrections/Law Enforcement", 
                                                 "Manufacturing/Construction", "Retail/Grocery", "Bars/Restaurants/Entertainment",
                                                 "Office/Indoor Workplace", "Camps/Educ/Childcare", "Homeless Shelters", "Agriculture/Food Supply",
                                                 "Travel/Hospitality", "Other", "Gathering/Social Event"))

# subset to only weeks I want displayed in the graph
ob_short2 <- subset(OB_dates_sum2, OB_dates_sum2$week_start > ymd("2021-09-05"))

dates <- ob_short2 %>% 
            ungroup() %>% 
            select(week_start) %>% 
            distinct() %>% 
            mutate(date_label =  paste0(month.abb[month(week_start)], " ", day(week_start))) 
            

# plot
ggplot(ob_short2, aes(x = week_start, y = n, fill = setting_group)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Number of Outbreaks") +
  xlab("Week Outbreak was Identified") +
  ggtitle(paste("COVID-19 Outbreaks by Week (n = ", nrow(dat),")", sep = "")) +
  scale_x_date(breaks = dates$week_start,
                     labels = dates$date_label) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6",
                               "#6A3D9A", "grey55", "grey20", "deeppink")) +
  theme(legend.position = "right", text = element_text(family = "Trebuchet MS"),
        axis.text.x = element_text(angle = -50, hjust = -.3, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14)) +
  labs(fill = "")

##################################################################
# Monthly Graph
##################################################################

# make monthly intervals
dat$my <- as.Date(cut(dat$OB_date, breaks = "month"))

# summary by month
ob_sum_mon <- dat %>% 
  group_by(my,setting_group) %>% 
  count()

# make a factor
ob_sum_mon$setting_group <- as.factor(ob_sum_mon$setting_group)

# sort the factor level so the colors look better in the graph
ob_sum_mon$setting_group <- factor(ob_sum_mon$setting_group,
                                   levels = c("Outpatient Healthcare", "Residential Healthcare", "Corrections/Law Enforcement", 
                                              "Manufacturing/Construction", "Retail/Grocery", "Bars/Restaurants/Entertainment",
                                              "Office/Indoor Workplace", "Camps/Educ/Childcare", "Homeless Shelters", "Agriculture/Food Supply",
                                              "Travel/Hospitality", "Other", "Gathering/Social Event"))


# Subset to months in 2021
ob_sum_mon <- subset(ob_sum_mon, ob_sum_mon$my > "2020-12-01")

# change back to factor for graphing
ob_sum_mon$my <- as.factor(ob_sum_mon$my)

# plot
ggplot(ob_sum_mon, aes(x = my, y = n, fill = setting_group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Number of Outbreaks") +
  xlab("Month Outbreak was Identified") +
  ggtitle(paste("COVID-19 Outbreaks by Month (n = ", nrow(dat),")", sep = "")) +
  scale_x_discrete(labels = c("Jan/21", "Feb/21", "Mar/21", "Apr/21",
                              "May/21", "Jun/21", "Jul/21", "Aug/21",
                              "Sep/21", "Oct/21", "Nov/21", "Dec/21")) +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6",
                               "#6A3D9A", "grey55", "grey20", "deeppink")) +
  theme(legend.position = "bottom", text = element_text(family = "Trebuchet MS"),
        axis.text.x = element_text(angle = -50, hjust = -.3, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        plot.caption = element_text(hjust = -.07, face= "italic")) +
  labs(fill = "",
       caption = "*On 7/1 outbreak prioritization ended. Such began 12/7/2020.\n**Starting 6/1 OB definitions increased from 2 to 5 cases in most settings.") +
  coord_cartesian(ylim = c(0,800), clip = 'off')

#########################################################################################################
# weekly active outbreaks
########################################################################

# convert to date
dat$Date.outbreak.was.considered.closed. <- anydate(dat$Date.outbreak.was.considered.closed.)

# get when an OB opened and closed
dat$week_open <- cut.Date(dat$Date.met.outbreak.definition, breaks = "1 week", labels = FALSE)
dat$week_closed <- cut.Date(dat$Date.outbreak.was.considered.closed., breaks = "1 week", labels = FALSE)

# cut date counts from the first week of that variable, so it isn't actually counting from 
# the first week of the data set. I need to add 5 to each closed date week value since the first OB that closed
# did so 5 weeks after the first OB confirmed week

dat$week_closed <- dat$week_closed + 5

# count how many OBs opened each week
ob_open <- dat %>% 
  group_by(week_open) %>% 
  count()

# count how many closed each week
ob_closed <- dat %>% 
  group_by(week_closed) %>% 
  count()

# rename so we can join the data sets
colnames(ob_open) <- c("week", "ob_count")
colnames(ob_closed) <- c("week", "ob_closed")

# join
ob_sum2 <- left_join(ob_open, ob_closed)

# replace NA totals with zeroes
ob_sum2 %<>% 
  mutate_at(c(3), ~replace(., is.na(.), 0))

# calculate the cumulative sum for each variable
ob_sum2$cum_total <- cumsum(ob_sum2$ob_count)
ob_sum2$cum_closed <- cumsum(ob_sum2$ob_closed)

# get active OBs by subtracting those closed from the total each week
ob_sum2$active <- ob_sum2$cum_total - ob_sum2$cum_closed

# susbet to only weeks I want displayed in the graph
ob_short <- subset(ob_sum2, ob_sum2$week > 78)


# we gotta add week 73 ourselves
#ew <- c(83,0,0,6201,5738,463)
 
#names(ew) <- c("week","ob_count","ob_closed","cum_total","cum_closed","active")
 
# ob_short <- rbind(ob_short, ew)

# plot
ggplot(ob_short, aes(x = week, y = active)) +
  
  geom_area(fill = "blueviolet", alpha = .5) +
  geom_point(color = "blueviolet", size = 2) +
  geom_line(color = "blueviolet", size = 1) +
  
  geom_text(aes(label = active) , hjust = .6, vjust = 1.6, color = "darkorchid4", size = 4) +
  theme_minimal() +
  ylab("Active Outbreaks") +
  xlab("Week") +
  ggtitle("Total Active Outbreaks by Week") +
  scale_x_continuous(breaks = c(79,80,81,82,83,84,85,86,87,
                                88,89,90,91,92),
                     labels = dates$date_label) +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700,800)) +
  theme(text = element_text(family = "Trebuchet MS"),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18)) +
  geom_label(aes(x = 84, y = 760, 
                 label = paste("Cumulative Outbreaks: ", max(cum_total), sep = "")), 
             fill = "orange",
             label.size = NA,
             size = 5)

#################################################################################
# School Graphs
#################################################################################
token <- "7C88885A13A217C9EBF83110E1BF2BA5"
url <- "https://cdphe.redcap.state.co.us/api/"
formData <- list("token"=token,
                 content='report',
                 format='csv',
                 report_id='1029',
                 csvDelimiter='',
                 rawOrLabel='label',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='csv'
)

response <- httr::POST(url, body = formData, encode = "form")
dat <- httr::content(response)



# for any outbreaks where we cannot identify date determined
dat <- subset(dat, !is.na(dat$date_ob_detected))

# make school settings
dat %<>%
  mutate(kiddos = case_when(covid_settingtype == "School K-12" |
                              covid_settingtype == "School Administration" ~ "School",
                            
                            covid_settingtype == "College/University" | 
                              covid_settingtype == "School/College Dorm" ~ "Higher Ed",
                            
                            covid_settingtype == "Child Care Center" ~ "Childcare",
                            TRUE ~ "Other"))

# convert to a date variable
dat$date_ob_detected <- anydate(dat$date_ob_detected)

# rename this cause that name is way too long
dat$OB_date <- dat$date_ob_detected


dat$my <- as.Date(cut(dat$OB_date, breaks = "month"))

# subset to relevant settings and kick out everything before 8/2020
dsub <- subset(dat, dat$kiddos != "Other" & dat$my > "2020-11-30")

# change back to factor for graphing
dsub$my <- as.factor(dsub$my)


# summary by month
vac_report <- dsub %>% 
  group_by(my,kiddos) %>% 
  count()


# plot as rolling 12 months bar plot
ggplot(vac_report, aes(x = my, y = n, fill = kiddos, label = n)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  ylab("Number of Outbreaks") +
  xlab("Month Outbreak was Identified") +
  ggtitle("Education Outbreaks by Month Confirmed") +
  scale_x_discrete(labels = c("Dec '20", "Jan '21", "Feb",
                              "Mar", "Apr", "May", "Jun", "Jul",
                              "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = c(0,40,80,120,160,200,240,280)) +
  scale_fill_manual(values = c("#33478e", "#ffd100", "#89617c")) +
  labs(fill = "Setting") +
  geom_text(aes(label = ifelse(n > 5, n, NA)),
            size = 3.5, position = position_stack(vjust = 0.5),
            fontface = "bold",
            colour = ifelse(vac_report$kiddos == "Childcare", "white", "black")) +
  theme(legend.position = "right", text = element_text(family = "Trebuchet MS"))

# outbreak tables --------------------------------------
dat <- read.csv(file = "C:/Users/rweber/Downloads/OB Data 10 20 2021.csv",
                sep = ",", na.strings = c("NA", "N/A", "n/a", "", " "))

dat %<>%
  mutate_at(c(9:14), ~replace(., is.na(.), 0))

dat$n_cases <-  dat$Total.Resident.Cases +
  dat$Total.Staff.Cases +
  dat$Total.attendee.cases


dat$n_deaths <- dat$Total.Resident.Deaths +
  dat$Total.Staff.Deaths +
  dat$Total.Attendee.Deaths

dat %<>%
  mutate(setting_group = case_when(COVID.Setting.Type == "Healthcare - Group Home" |
                                     COVID.Setting.Type == "Healthcare - Psychiatric Hospital" |
                                     COVID.Setting.Type == "Healthcare - Skilled Nursing" |
                                     COVID.Setting.Type == "Healthcare - Rehab Facility" |
                                     COVID.Setting.Type == "Healthcare - Acute Care Hospital" |
                                     COVID.Setting.Type == "Healthcare - Combined Care" |
                                     COVID.Setting.Type == "Healthcare - Long-Term Acute Care" |
                                     COVID.Setting.Type == "Healthcare - Memory Care" |
                                     str_detect(COVID.Setting.Type, "inpatient") |
                                     str_detect(COVID.Setting.Type, "Living") |
                                     str_detect(COVID.Setting.Type, "Hospice") ~ 'Residential Healthcare',
                                   
                                   COVID.Setting.Type == "Healthcare - Ambulatory Surgery Center" |
                                     COVID.Setting.Type == "Healthcare - Outpatient" |
                                     str_detect(COVID.Setting.Type, "outpatient") ~ "Outpatient Healthcare",
                                   
                                   COVID.Setting.Type == 'Grocery Store' | 
                                     COVID.Setting.Type == 'Caterer' | 
                                     COVID.Setting.Type == 'Convenience/Corner Store' | 
                                     COVID.Setting.Type == 'Retailer' |
                                     COVID.Setting.Type == 'Specialty Food Retailer' ~ "Retail/Grocery",
                                   
                                   COVID.Setting.Type == 'Banquet Facility' |
                                     COVID.Setting.Type == 'Bar/Tavern/Brewery' |
                                     COVID.Setting.Type == 'Indoor Entertainment/Rec' |
                                     COVID.Setting.Type == 'Nightclub' |
                                     COVID.Setting.Type == 'Outdoor Entertainment/Rec' |
                                     COVID.Setting.Type == 'Restaurant - Buffet' |
                                     COVID.Setting.Type == 'Restaurant - Fast Food' |
                                     COVID.Setting.Type == 'Restaurant - Other' |
                                     COVID.Setting.Type == 'Restaurant - Sit Down' ~ 'Bars/Restaurants/Entertainment',
                                   
                                   COVID.Setting.Type == 'Child Care Center' |
                                     COVID.Setting.Type == 'Youth Sports/Activities' |
                                     COVID.Setting.Type == 'College/University' |
                                     COVID.Setting.Type == 'School/College Dorm' |
                                     COVID.Setting.Type == "School K-12" |
                                     COVID.Setting.Type == "Overnight Camp" |
                                     COVID.Setting.Type == "Day Camp" |
                                     COVID.Setting.Type == "Trade School" |
                                     COVID.Setting.Type == "School Administration" ~ "School/Univ/Childcare",
                                   
                                   COVID.Setting.Type == 'Jail' |
                                     COVID.Setting.Type == 'Correctional, Other' |
                                     str_detect(COVID.Setting.Type, "Enforcement") |
                                     str_detect(COVID.Setting.Type, "Prison") ~ "Corrections/Law Enforcement",
                                   
                                   COVID.Setting.Type == 'Agriculture - Other' |
                                     COVID.Setting.Type == 'Farm/Dairy' |
                                     COVID.Setting.Type == "Farmer's Market" |
                                     COVID.Setting.Type == 'Food Distribution' |
                                     COVID.Setting.Type == 'Food Manufacturing/Packaging' |
                                     COVID.Setting.Type == 'Food Warehouse' |
                                     COVID.Setting.Type == 'Meat Processing/Packaging' ~ "Agriculture/Food Supply",
                                   
                                   COVID.Setting.Type == 'Distribution Center/Business' |
                                     COVID.Setting.Type == 'Materials Supplier' |
                                     COVID.Setting.Type == "Non-Food Manufacturer/Warehouse" |
                                     COVID.Setting.Type == "Home Maintenance Services" |
                                     str_detect(COVID.Setting.Type, "Construction") ~ "Manufacturing/Construction",
                                   
                                   COVID.Setting.Type == 'Homeless Shelter' ~ "Homeless Shelters",
                                   
                                   COVID.Setting.Type == 'Office/Indoor Workspace' ~ "Office/Indoor Workplace",
                                   
                                   COVID.Setting.Type == 'Hotel/Lodge/Resort' |
                                     COVID.Setting.Type == 'Casino' |
                                     COVID.Setting.Type == "Travel" |
                                     str_detect(COVID.Setting.Type, "Airport") ~ "Travel/Hospitality",
                                   
                                   COVID.Setting.Type == 'Fair/Festival/Temp. Mobile Event' |
                                     COVID.Setting.Type == 'Social Gathering' |
                                     COVID.Setting.Type == "Religious Facility" ~ "Gathering/Social Event",
                                   TRUE ~ 'Other'))


# count how cases and deaths per setting
dat %>% 
  group_by(setting_group) %>% 
  summarise(cases = sum(n_cases),
            deaths = sum(n_deaths),
            Total_Outbreaks = n())

# count staff cases and deaths per setting
dat %>% 
  group_by(setting_group) %>% 
  summarise(cases = sum(Total.Staff.Cases),
            deaths = sum(Total.Staff.Deaths),
            Total_Outbreaks = n())