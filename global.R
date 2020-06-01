# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# list of packages required
list.of.packages <- c("shiny","shinydashboard","RColorBrewer","shinythemes","dplyr","tidyverse","ggplot2",
                      "lubridate","shinywidgets","DT","verisr","highcharter","jsonlite","scales","ggpubr")


# library(verisr)
# library(RColorBrewer)
# library(wesanderson)
# library(shinyWidgets)

# library(ggplot2)
# library(shinydashboardPlus)
# library(shiny.semantic)
# library(semantic.dashboard)
# library(shinydashboard)
# library(viridis)# library(shinymaterial)
# library(shinydashboardPlus)
# library(httpuv8)
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
# library(tidyverse)
library(lubridate)
library(DT)
library(data.table)
library(jsonlite)
library(plotly)
library(ggpubr)
library(ggthemes)
library(highcharter)
library(rjson)

data <- rjson::fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
# library(gapminder)

# regionchoices[[1]]
# 
# 
# ps <- pbreach %>% 
#   group_by(region) %>% 
#   summarise(Percentage=n()) %>% 
#   group_by(State) %>% 
#   mutate(Percentage=Percentage/sum(Percentage)*100)
# #  
# ps <- pbreach %>% 
#   count()
#   mutate(freq = sum(Percentage)/su )
# ps

# p <- gapminder %>%
#   filter(year==1977) %>%
#   ggplot( aes(gdpPercap, lifeExp, size = pop, color=continent)) +
#   geom_point() +
#   theme_bw()
# 
# pbreach %>% 
#   ggplot( aes(Year, region, color=region)) +
#   geom_bar() +
#   theme_bw()
# 
# 
# + scale_colour_wsj("colors6")
# # 
# py1 <- plotly()
# out1 <- py1$ggplotly(p1, kwargs = list(filename = "PlotlyThemeExample", fileopt = "overwrite"))
# url1 <- out1$response$url

# options(error = recover)
options(shiny.sanitize.errors = TRUE)
# update.packages(ask = FALSE, checkBuilt = TRUE)
# rstudioapi::viewer("http://127.0.0.1:6088")
#abbrev	latitude	longitude	state


# fig <- plot_ly() 
# fig <- fig %>% add_trace(
#   type="choropleth",
#   geojson=geojson,
#   locations=m$StateActual,
#   z=m$Company,
#   colorscale="Viridis",
#   featureidkey="properties.district"
# )
# fig <- fig %>% layout(
#   geo = g
# )
# fig <- fig %>% colorbar(title = "Bergeron Votes")
# fig <- fig %>% layout(
#   title = "2013 Montreal Election"
# )
# fig


visdata <- read_csv('./data/visdata.csv')
prcbreach <- read_csv('./data/PRCBreachCombined.csv')
statsdf <- read_csv('./data/databreach_stats.csv')
defs <- read_csv("./data/defintions.csv")



# data('industry2', package='verisr')
# vcdb.dir <- "./VCDB/data/json/validated"

states <- read.csv("./data/stateskey.csv")
# states

# Please don't laugh at my code :(
visdata <- as_tibble(visdata)
prcbreach <- as_tibble(prcbreach)
statsdf <- as_tibble(statsdf)
defs <- as_tibble(defs)
states <- as_tibble(states)
# getdates <-function(x){
#   x = as.Date(x,"%Y-%m-%d")
#   return(x)
# }

# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()


# states <- read_csv('./data/stateskey.csv')
# states <- states %>%
#   dplyr::rename(.,StateActual = State, state_code = `State Code`, region = Region, division = Division)
# prcbreach_j <- left_join(x = prcbreach, y = states, all.x = TRUE)
# write_csv(prcbreach, "PRCBreachCombined1.csv")
# summary(prcbreach_j)
# lapply(prcbreach$MadePubDate, function(x) ymd(x))
# class(prcbreach$MadePubDate)

prcbreach <- prcbreach %>%
  mutate(MadePubDate = as.Date(MadePubDate, "%m/%d/%Y")) %>%
  mutate(Month = month(MadePubDate))


#
# prcbreach %>%
# select(Date = MadePubDate, Company, State = StateActual, BreachType, Breach_desc, OrganType,Organ_desc, TotalRecords, region)

# prcbreach %>%
#   select(Date = MadePubDate, Company, State = StateActual, BreachType, Breach_desc, OrganType,Organ_desc, TotalRecords, region)
# prcbreach <- prcbreach %>%
#   mutate(Month = lubridate::month(MadePubDate))

# prc1 <- prcbreach %>%
#   select(Date = MadePubDate, Year = BreachYear, Month,region,Company, State = StateActual,
#          BreachType, Breach_desc, OrganType, Organ_desc, TotalRecords, region)

# pbreach <- prcbreach %>%
#   select(Date = MadePubDate, Year = BreachYear, Month,region,Company, State = StateActual,
#          BreachType, Breach_desc, OrganType, Organ_desc, TotalRecords, region)

pbreach <- prcbreach %>%
  select(Date = MadePubDate, Year = BreachYear, Month,region, Company, State = StateActual, Incident_Description = IncidentDesc,
         Breach_Method = BreachType, Breach_Description = Breach_desc, Business_Type = OrganType,Business_Description= Organ_desc, TotalRecords, region)
# 
# 
# 
# m<-prcbreach %>%
#   group_by(StateActual) %>%
#   summarize(Company = n())
# 
# plot_ly(m, type="choropleth", locations=m$StateActual,
#         locationmode="USA-states", z=m$Company) %>% layout(geo=list(scope="usa"),colorscale="Blues")

# 
# l <- list(color = toRGB("white"), width = 2)
# # specify some map projection/options
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showlakes = TRUE,
#   lakecolor = toRGB('white')
# )
# 
# fig <- plot_geo(m, locationmode = 'USA-states')
# fig <- fig %>% add_trace(
#   z = ~m$Company, text = ~m$StateActual,locations = ~m$StateActual,
#   color = ~m$total.Company, colors = 'Purples'
# )
# fig <- fig %>% colorbar(title = "Millions USD")
# fig <- fig %>% layout(
#   title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
#   geo = g
# )
# 
# fig




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

# data<-data[!is.na(DMDEDUC2),]
# glimpse(prcbreach)
# min(prcbreach$MadePubDate, na.rm=TRUE)

# getdates <-function(x){
#   x = as.Date(x,"%Y-%m-%d")
#   return(x)
# }


# cnames <- colnames(states,do.NULL = TRUE, prefix = "col")

# startdate = dateschoice$startdate
# enddate = dateschoice$maxdate

# m1 = min(prcbreach$MadePubDate, na.rm=TRUE)
# m2 = max(prcbreach$MadePubDate, na.rm=TRUE)
# class(m1)
# m2

#
# summary(prcbreach)
# prcbreach

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

# rlang::last_error() ####
####








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

# getdates <- function(x){
#   dateschoice <- x %>%
#     summarise(startdate = min(MadePubDate), maxdate = max(MadePubDate))
#     startdate = dateschoice$startdate
#     enddate = dateschoice$maxdate
#     datevect = c(startdate,enddate)
#     return(datevect)
# }
#
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

# mutate("Incidents"= n())%>%

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
# mutate("Incidents"= n())%>%

# 1st page of dashboard ####
#statsdf1 < - statsdf %>%
statsdf4 <- statsdf %>%
  filter(data_id == 4) %>%
  select(Year,Half, Region, Total, data_category, data_title, survey_period)


#----------- Top Breaches of All time####
# spec(lbreach)


# spec(visdata)

visdata1 <- visdata[,c(1:9)]

toplist <- visdata1 %>% #paste(format(round(x / 1e6, 1), trim = TRUE), "M")
  select(Entity,other_name, records_lost,industry,year, breach_method)

# Biggest breaches ####
toplist11 <- toplist %>%
  filter(year == 2016)


# datatable(toplist1)
#
# summary(toplist1)
#
# lbreach1 <- lbreach[]
#
# datatable(toplist1)

# top1 <- top1 %>%
#   group_by(year)

# mBreaches10g <- visdata1 %>%
#   group_by(Entity) %>%
#   summarize(records_lost = n()) %>%
#   arrange(desc(records_lost)) %>%
#   top_n(n = 10)
#
# View(mBreaches10g)
#

# choice1[3] = 2006

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
# Debugging Code Block ######################################################################

#rstudioapi::viewer("http://127.0.0.1:3487")

# Clear global environment except 'currentTab'

# currentTab = "businesstab"
# rm(list = setdiff(ls(), currentTab))
#
# # If current tab exists, restore to that tab. Otherwise, start at home screen
# if(!exists(currentTab)){
#   currentTab <- 'hometab'
# }



# Calculations #################################################################################
# s1<-statsdf %>%
#   filter(data_id %in% c(6,8)) %>%
#   select(Year,Region, Total, data_category, data_title, data_id)


# cubed <- data.table::cube(
#   prcbreach,
#   .(distance = sum(distance)),
#   by = c("month", "origin")
# )
# cubed

