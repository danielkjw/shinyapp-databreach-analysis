# list of packages required
list.of.packages <- c("shiny","shinydashboard","RColorBrewer","shinythemes","dplyr","tidyverse","ggplot2",
                      "lubridate","shinywidgets","DT","verisr","highcharter","jsonlite","scales","ggpubr")

# ---- To Do: Need to clean up library list ---- #####
library(rjson)
library(shiny)
library(shinyjs)
library(readr)
library(argonR)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(argonDash)
library(scales)
library(dplyr)
library(plotly)
library(lubridate)
library(DT)
library(data.table)
library(jsonlite)
library(ggpubr)
library(ggthemes)
library(highcharter)
library(rjson)

data <- rjson::fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
options(shiny.sanitize.errors = TRUE)

visdata <- read_csv('./data/visdata.csv')
prcbreach <- read_csv('./data/PRCBreachCombined.csv')
statsdf <- read_csv('./data/databreach_stats.csv')
defs <- read_csv("./data/defintions.csv")


# states key used to match state name, abbreviations
states <- read.csv("./data/stateskey.csv")


visdata <- as_tibble(visdata)
prcbreach <- as_tibble(prcbreach)
statsdf <- as_tibble(statsdf)
defs <- as_tibble(defs)
states <- as_tibble(states)


prcbreach <- prcbreach %>%
  mutate(MadePubDate = as.Date(MadePubDate, "%m/%d/%Y")) %>%
  mutate(Month = month(MadePubDate))


pbreach <- prcbreach %>%
  select(Date = MadePubDate, Year = BreachYear, Month,region, Company, State = StateActual, Incident_Description = IncidentDesc,
         Breach_Method = BreachType, Breach_Description = Breach_desc, Business_Type = OrganType,Business_Description= Organ_desc, TotalRecords, region)
statekey <- unique(states$State)
statechoices <- prcbreach %>%
  filter(!is.na(StateActual)) %>%
  filter(StateActual %in% statekey) %>%
  select(StateActual) %>%
  arrange(StateActual)

statechoices <- unique(statechoices)

regionchoices <- prcbreach %>%
  filter(!is.na(region)) %>%
  select(region) %>%
  arrange(region)

regionchoices <- prcbreach %>%
  filter(!is.na(region)) %>%
  select(region) %>%
  arrange(region)

regionchoices <- unique(regionchoices)

breach_options <- defs %>%
  filter(id %in% c(9:16)) %>%
  select(id, category, abbrev,def)

boptions <- defs %>%
  filter(id %in% c(1:8)) %>%
  select(id, category, abbrev,def)

breach_options <- defs %>%
  filter(id %in% c(9:16)) %>%
  select(id, category, abbrev,def)

boptions$abbrev


choice4 <- sort(prcbreach$OrganType) %>%
  unique(desc(prcbreach$OrganType))

getdates <-function(x){
  x = as.Date(x,"%Y-%m-%d")
  return(x)
}

p1 <- prcbreach %>%
  dplyr::group_by(City) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  ungroup()

prc1 <- prcbreach %>%
  dplyr::group_by(BreachType,BreachYear ) %>%
  dplyr::summarise(Year = unique(BreachYear),Count = dplyr::n()) %>%
  ungroup()




# Data and Calculations ####
#Year
#StateActual
#City
#BreachType
#Breach_desc
#OrganType
#Organ_desc
#TotalRecords
#IncidentDesc
#MadePubDate
#Company

# Organ Type vs. Daily,  Monthly, Yearly Breach Rate
# Boxplot of distribution

# BreachType vs. Daily,  Monthly, Yearly Breach Rate
# Boxplot of distribution

# State vs. Daily,  Monthly, Yearly Breach Rate
# Boxplot of distribution

# Features ####
# MadePubDate
# City
# Company
# State
# BreachType
# Breach_desc
# OrganType
# Organ_desc
# TotalRecords
# IncidentDesc
# InfoSource
# SourceURL
# BreachYear
# Latitude
# Longitude
# StateActual
# Month

#Average Records Lost
prc1 <- prcbreach %>%
  dplyr::group_by(BreachYear) %>%
  dplyr::summarise(Year = unique(BreachYear), "Records Lost" = mean(TotalRecords, na.rm = T))


#highest csategory of breach
prc4 <- prcbreach %>%
  dplyr::group_by(BreachYear, OrganType, BreachType) %>%
  dplyr::summarise(Year = unique(BreachYear), Records_Lost = mean(TotalRecords, na.rm = T))

#state and average
prc3 <- prcbreach %>%
  dplyr::group_by(BreachYear,StateActual) %>%
  dplyr::summarise(Year = unique(BreachYear),
                   State_=unique(StateActual),
                   Breach_count = n(),
                   Records_Lost = mean(TotalRecords, na.rm = T)) %>%
  top_n(n=5,Records_Lost)

#Total Incident Count By State (better for top 5)
prc2 <- prcbreach %>%
  group_by(BreachYear, StateActual) %>%
  # mutate("Incidents" = n()) %>%
  summarise(State = unique(StateActual), unique_states = n_distinct(StateActual),Incidents = n()) %>%
  top_n(n=5, Incidents) %>%
  arrange(desc(Incidents))

#Total Incident Count By State (better for top 5)
prc2top5 <- prcbreach %>%
  filter(!is.na(StateActual)) %>%
  group_by(BreachYear, StateActual) %>%
  summarise(State = unique(StateActual),unique_states = n_distinct(StateActual),Incidents = n()) %>%
  top_n(n=5, Incidents) %>%
  arrange(desc(Incidents))

#Total Incident Count By State (better for top 5)
prc2box <- prcbreach %>%
  group_by(BreachYear, StateActual) %>%
  # mutate("Incidents" = n()) %>%
  summarise(unique_states = n_distinct(StateActual),Incidents = n()) %>%
  arrange(desc(Incidents))

#Total Incident Count By State (better for top 5)
prc2percent <- prcbreach %>%
  rename(Year = BreachYear, State1 = State, State= StateActual) %>%
  filter(!is.na(State)) %>%
  group_by(Year, State) %>%
  summarise(unique_states = n_distinct(State),Incidents = n()) %>%
  mutate(freq = (n() / sum(Incidents))*100) %>%
  top_n(n=5, Incidents) %>%
  arrange(desc(Incidents))

# ----------- 1st page of dashboard ----------- ####
statsdf4 <- statsdf %>%
  filter(data_id == 4) %>%
  select(Year,Half, Region, Total, data_category, data_title, survey_period)


#----------- Top Breaches of All time ----------- ####
visdata1 <- visdata[,c(1:9)]

toplist <- visdata1 %>% #paste(format(round(x / 1e6, 1), trim = TRUE), "M")
  select(Entity,other_name, records_lost,industry,year, breach_method)

# Biggest breaches ####
toplist11 <- toplist %>%
  filter(year == 2016)

top1 <- visdata1 %>%
  select(Entity, records_lost,industry,year) %>%
  group_by(year) %>%
  mutate(YearTotalCount = n())

top2 <- visdata1 %>%
  select(Entity, records_lost,industry,year) %>%
  arrange(desc(records_lost))


choice1 <- sort(top2$year) %>%
  unique(desc(top2$year))

choice2 <- choice1

test1 <- top2 %>%
  filter(year == choice1[3]) %>%
  ggplot(aes(x = 'records_lost', y = 'Entity')) %>%
  +geom_bar(stat = 'identity',fill = "steelblue") %>%
  +theme_minimal() + scale_fill_brewer(palette = "Blues")
