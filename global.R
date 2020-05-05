
#list of packages required
list.of.packages <- c("shiny","shinydashboard","RColorBrewer","shinythemes","dplyr","tidyverse","ggplot2",
                      "lubridate","DT","verisr","DT","jsonlite","scales","ggpubr")


library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(dashboardthemes)
library(tidyverse)
library(scales)
# library(dplyr)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(DT)
# library(data.table)
library(jsonlite)
library(verisr)
library(ggpubr)
library(highcharter)

# lbreach <- read_csv('./data/largest_breaches.csv')
visdata <- read_csv('./data/visdata.csv')
prcbreach <- read_csv('./data/PRCBreach.csv')
statsdf <- read_csv('./data/databreach_stats.csv')

#lbreach %>%  VCDB ####
load('./data/vcdb.dat')
vcdb_back <- vcdb


prcbreach <- prcbreach %>%
  mutate(MadePubDate = as.Date(MadePubDate, "%m/%d/%Y")) %>% 
  mutate(Month = month(MadePubDate))
  


choice4 <- sort(prcbreach$OrganType) %>%
  unique(desc(prcbreach$OrganType))




# CARD Fraud Involving Debit and Credit  Cards Not Via Hacking (skimming devices at point-of-service  terminals, etc.)
# HACK	Hacked by an Outside Party or Infected by Malware
# INSD	Insider (employee, contractor or customer)
# PHYS	Physical (paper documents that are lost, discarded or stolen)
# PORT	Portable Device(lost, discarded or stolen laptop, PDA,
#                       smartphone, memory stick, CDs, hard drive, data tape, etc.)
# STAT	Stationary Computer Loss (lost, inappropriately accessed,
#                                discarded or stolen computer or server not designed for mobility)
# DISC	Unintended Disclosure Not Involving Hacking, Intentional
# Breach or Physical Loss (sensitive information posted publicly, mishandled or
#                          sent to the wrong party via publishing online, sending in an email, sending
#                          in a mailing or sending via fax)
# UNKN	Unknown (not enough information about breach to know how
#               exactly the information was exposed)

# BSF	Businesses (Financial and Insurance Services)
# BSO	Businesses (Other)
# BSR	Businesses (Retail/Merchant including Online Retail)
# EDU	Educational Institutions
# GOV	Government & Military
# MED	Healthcare, Medical Providers and Medical Insurance Services
# NGO	Nonprofits
# UNKN	Unknown


#Average Records Lost

prc1 <- prcbreach %>% 
  group_by(BreachYear) %>% 
  summarise(Year = unique(BreachYear), "Records Lost" = mean(TotalRecords, na.rm = T))


#highest csategory of breach
prc4 <- prcbreach %>% 
  group_by(BreachYear, OrganType, BreachType) %>% 
  summarise(Year = unique(BreachYear), Records_Lost = mean(TotalRecords, na.rm = T))



#state and average
prc3 <- prcbreach %>% 
  group_by(BreachYear,StateActual) %>% 
  summarise(Year = unique(BreachYear), 
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
  mutate(freq = n / sum(Incidents)) %>% 
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

print(test1)
class(choice1)





# Obtains Categories of Actors and the
actors <- getenumCI(vcdb, "actor")
# actors is a data frame
print(actors)

# requires object: vcdb (7-6)
actors <- getenum(vcdb, "actor", add.n=TRUE, add.freq=TRUE)
print(actors)


getenumCI(vcdb, "action", "att")
grep("disclosure",names(vcdb), value = TRUE)


vcdb20 <-vcdb %>%
  filter(plus.dbir_year %in% 2016:2020) %>%
  filter(attribute.confidentiality.data_disclosure.Yes) %>%
  getenumCI("action.*.variety", short.names=TRUE) #or FALSE

vcdb20na <-vcdb %>%
  filter(plus.dbir_year %in% 2016:2020) %>%
  filter(attribute.confidentiality.data_disclosure.Yes) %>%
  getenumCI("victim.*country", short.names=FALSE,na.rm=TRUE) #or FALSE
vcdb20na



vcdb19 <-vcdb %>%
  filter(plus.dbir_year %in% 2016:2020) %>%
  filter(attribute.confidentiality.data_disclosure.Yes) %>%
  getenumCI("asset.variety")

vcdb18 <-vcdb %>%
  filter(plus.dbir_year %in% 2016:2020) %>%
  filter(attribute.confidentiality.data_disclosure.Yes) %>%
  getenumCI("action")

vcdb17 <-vcdb %>%
  filter(plus.dbir_year %in% 2016:2020) %>%
  filter(attribute.confidentiality.data_disclosure.Yes) %>%
  getenumCI("action")

vcdb20
vcdb19
vcdb18
vcdb17

# vcdb.dir ####

#data('industry2', package='verisr')
# #-------ch6----------------------------------------------------------------------------
# 
# # Start ###############
# 
# 
# 
# # CHECKED WORKS Graphing Function Bar 1 VERIS PLOT FUNCTION#########################################################
# # take in the vcdb object and the field to plot
verisplot <- function(vcdb, field, name) {
  # get the data.frame for the field with frequency
  localdf <- getenumCI(vcdb, field, add.freq=T)
  # now let's take first 5 fields in the data frame.
  localdf <- localdf[c(1:5), ]
  # add a label to the data.frame
  localdf$lab <- paste(round(localdf$freq*100, 0), "%", sep="")
  # now we can create a ggplot2 instance
  gg <- ggplot(localdf, aes(x=enum, y=freq, label=lab))
  gg <- gg + geom_bar(stat="identity", fill="steelblue")
  # add in text, adjusted to the end of the bar
  gg <- gg + geom_text(hjust=-0.1, size=3)
  # flip the axes and add in a title
  gg <- gg + coord_flip() + ggtitle(field)
  # remove axes labels and add bw theme
  gg <- gg + xlab("") + ylab("") + theme_economist() + scale_fill_economist()
  # fix the y scale to remove padding and fit our label (add 7%)
  gg <- gg + scale_y_continuous(expand=c(0,0),
                                limits=c(0, max(localdf$freq)*1.1))
  # make it slightly prettier than the default
  gg <- gg + theme(panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks = element_blank())
}
# 
# 
# 
# #action
localdf <- vcdb %>%
  filter(plus.dbir_year %in% 2015:2020) %>% 
  filter(attribute.confidentiality.data_disclosure.Yes) %>% 
  getenumCI(c("actor.*.motive" ,"victim.country"), na.rm=T, add.freq=T)

# getenumCI(vcdb, c("action", "asset.assets"), add.freq=T)
glimpse(localdf)
# try the following (code not in book)
print(verisplot(vcdb, "action"))
print(verisplot(vcdb, "actor.external.variety"))
print(verisplot(vcdb, "action.physical.variety"))
print(verisplot(vcdb, "action.hacking.vector"))




