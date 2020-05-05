list.of.packages <- c("shiny","shinydashboard","dplyr","tidyr","plotly","ggplot2","dygraphs","DT","shinyjs","wordcloud",
                      "DT","tm","wordcloud","flipPlots", "rbokeh","jsonlite","scales")


library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)

library(flipPlots)
library(jsonlite)
library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)
library(verisr)
library(scales)




library(wordcloud)
library(tm)
library(dygraphs)
library(rbokeh)

library(jsonlite)
library(verisr)


load('./data/vcdb.dat')
# ---Beginning ####
summary(vcdb)
# Obtains Categories of Actors and the 
actors <- getenum(vcdb, "actor")
# actors is a data frame 
print(actors)

# requires object: vcdb (7-6)
actors <- getenum(vcdb, "actor", add.n=TRUE, add.freq=TRUE)
print(actors)





# Figure 7-1 UNUSED BECAUES OF BUG    #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
# get the count of incidents with an actor
actors <- getenumCI(vcdb, "actor", na.rm=TRUE)
actors
actors$enum
actfields <- c("external", "internal", "partner")
str(actors)
dim(actors)
# 
# actors.n <- actors %>%
#   mutate(n = sum(actors$n, na.rm=TRUE))
# 



# get the count of incidents with an actor

actors.n <- sum(sapply(actors, function(x) {
  any(actfields %in% x)
}))

str(actors.n) 

# now let's grab all three sections for motives.
motive <- getenumCI(vcdb, "actor.external.motive", na.rm=TRUE)
motive <- rbind(motive, getenum(vcdb, "actor.internal.motive",na.rm=TRUE))
motive <- rbind(motive, getenum(vcdb, "actor.partner.motive",na.rm=TRUE))
# combine them 
allmotive <- aggregate(x ~ enum, data=motive, FUN=sum)
# have to re-order the primary enum.
allmotive <- allmotive[with(allmotive, order(x)), ]
allmotive$enum <- factor(allmotive$enum, levels=allmotive$enum, ordered=T)
# and add in the frequency with actor count above
allmotive$freq <- allmotive$x/actors.n

## remove unknown as a proportion, focusing on "known" here.
finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]

gg <- ggplot(finalmotive, aes(x=enum, y=freq,label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(allmotive$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())

print(gg)
# vcdb.dir ####

#data('industry2', package='verisr')


# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes. NOTE: we are using a specific version of the data
# in these examples, so we are pulling it from an alternate
# book-specific location.
avURL <-"http://datadrivensecurity.info/book/ch03/data/reputation.data"

# use relative path for the downloaded data
avRep <- "data/reputation.data"

# using an if{}-wrapped test with download.file() vs read.xxx()
# directly avoids having to re-download a 16MB file every time
# we run the script
if (file.access(avRep)) {
  download.file(avURL, avRep)
}


# read in the IP reputation db into a data frame
# this data file has no header, so set header=FALSE
av <- read.csv(avRep,sep="#", header=FALSE)

# assign more readable column names since we didn't pick
# any up from the header
colnames(av) <- c("IP", "Reliability", "Risk", "Type",
                  "Country", "Locale", "Coords", "x")

str(av) # get an overview of the data frame
head(av) # take a quick look at the first few rows of data

# require object: av (3-4)
summary(av$Reliability)

summary(av$Risk)
# require object: av (3-4)
table(av$Reliability)
## 1         2      3      4      5      6      7      8      9
## 5612 149117  10892  87040      7   4758    297     21    686
##  10
## 196

table(av$Risk)
## 1       2      3      4      5      6      7
## 39 213852  33719   9588   1328     90     10

# summary sorts by the counts by default
# maxsum sets how many factors to display
summary(av$Type, maxsum=10)

summary(av$Country, maxsum=40)
#count of quantiative variables not covered by summary()

# Bar graph of counts (sorted) by Country (top 20)
# get the top 20 countries' names
country.top20 <- names(summary(av$Country))[1:20]
# give ggplot a subset of our data (the top 20 countries) 
# map the x value to a sorted count of country
gg <- ggplot(data=subset(av,Country %in% country.top20), 
             aes(x=reorder(Country, Country, length)))
# tell ggplot we want a bar chart
gg <- gg + geom_bar(fill="#000099")
# ensure we have decent labels
gg <- gg + labs(title="Country Counts", x="Country", y="Count")
# rotate the chart to make this one more readable
gg <- gg + coord_flip()
# remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
# display the image
print(gg)


# Listing 3-12
# requires packages: ggplot2
# require object: av (3-4)
# See corresponding output in Figure 3-3
# Bar graph of counts by Risk
gg <- ggplot(data=av, aes(x=Risk))
gg <- gg + geom_bar(fill="#000099")
# force an X scale to be just the limits of the data
# and to be discrete vs continuous
gg <- gg + scale_x_discrete(limits=seq(max(av$Risk)))
gg <- gg + labs(title="'Risk' Counts", x="Risk Score", y="Count")
# remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)

# Listing 3-13
# requires packages: ggplot2
# require object: av (3-4)
# See corresponding output in Figure 3-4
# Bar graph of counts by Reliability
gg <- ggplot(data=av, aes(x=Reliability))
gg <- gg + geom_bar(fill="#000099")
gg <- gg + scale_x_discrete(limits=seq(max(av$Reliability)))
gg <- gg + labs(title="'Reliabiity' Counts", x="Reliability Score",
                y="Count")
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)


# Listing 3-17
# require object: av (3-4)
country10 <- summary(av$Country, maxsum=10)
# now convert to a percentage by dividing by number of rows 
country.perc10 <- country10/nrow(av)
# and print it
print(country.perc10)
##         CN         US         TR                    DE         NL 
## 0.26518215 0.19482573 0.05396983 0.03887854 0.03848414 0.03066590 
##         RU         GB         IN    (Other) 
## 0.02453736 0.02433243 0.02118890 0.30793501 

# Listing 3-19
# require object: av (3-4)
# See corresponding output in Figure 3-8
# compute contingency table for Risk/Reliability factors which 
# produces a matrix of counts of rows that have attributes at
# each (x, y) location
rr.tab <- xtabs(~Risk+Reliability, data=av)
print(ftable(rr.tab)) # print table

# virtually identical output to pandas (below)

# graphical view
# need to use levelplot function from lattice package
library(lattice)
# cast the table into a data frame
rr.df = data.frame(table(av$Risk, av$Reliability))
# set the column names since table uses "Var1" and "Var2"
colnames(rr.df) <- c("Risk", "Reliability", "Freq")
# now create a level plot with readable labels
levelplot(Freq~Risk*Reliability, data=rr.df, main="Risk ~ Reliabilty", 
          ylab="Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


# Listing 3-21
# require object: av (3-4), lattice (3-19)
# See corresponding output in Figure 3-10
# generate random samples for risk & reliability and re-run xtab
# starting PRNG from reproducable point
set.seed(1492) # as it leads to discovery
# generate 260,000 random samples
rel=sample(1:7, 260000, replace=T)
rsk=sample(1:10, 260000, replace=T)
# cast table into data frame
tmp.df = data.frame(table(factor(rsk), factor(rel)))
colnames(tmp.df) <- c("Risk", "Reliability", "Freq")
levelplot(Freq~Reliability*Risk, data=tmp.df, main="Risk ~ Reliabilty", 
          ylab = "Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


# Listing 3-22
# require object: av (3-4), lattice (3-19)
# See corresponding output in Figure 3-11
# Create a new varible called "simpletype" 
# replacing mutiple categories with label of "Multiples"
av$simpletype <- as.character(av$Type)
# Group all nodes with mutiple categories into a new category
av$simpletype[grep(';', av$simpletype)] <- "Multiples"
# Turn it into a factor again
av$simpletype <- factor(av$simpletype)

rrt.df = data.frame(table(av$Risk, av$Reliability, av$simpletype))
colnames(rrt.df) <- c("Risk", "Reliability", "simpletype", "Freq")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20))


# Listing 3-24
# from the existing rrt.df, filter out 'Scanning Host'
rrt.df <- subset(rrt.df, simpletype != "Scanning Host")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20))

# Listing 3-26
# require object: av (3-4), lattice (3-19), rrt.df (3-24)
# See corresponding output in Figure 3-15
rrt.df = subset(rrt.df, 
                !(simpletype %in% c("Malware distribution",
                                    "Malware Domain")))
sprintf("Count: %d; Percent: %2.1f%%",
        sum(rrt.df$Freq),
        100*sum(rrt.df$Freq)/nrow(av))
## [1] Count: 15171; Percent: 5.9%

levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20)) 



#-------ch6----------------------------------------------------------------------------

# Start ###############



# Graphing Function Bar 1 #########################################################
# requires package : verisr, ggplot2
# requires object: vcdb (7-6)
library(ggplot2)
# take in the vcdb object and the field to plot
verisplot <- function(vcdb, field, name) {
  # get the data.frame for the field with frequency
  localdf <- getenum(vcdb, field, add.freq=T)
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
  gg <- gg + xlab("") + ylab("") + theme_bw()
  # fix the y scale to remove padding and fit our label (add 7%) 
  gg <- gg + scale_y_continuous(expand=c(0,0), 
                                limits=c(0, max(localdf$freq)*1.1))
  # make it slightly prettier than the default
  gg <- gg + theme(panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks = element_blank())
}


vcdb$freq


print(verisplot(vcdb, "action"))

# try the following (code not in book)
print(verisplot(vcdb, "action"))
print(verisplot(vcdb, "actor.external.variety"))
print(verisplot(vcdb, "action.physical.variety"))
print(verisplot(vcdb, "action.hacking.vector"))
print(verisplot(vcdb, "attribute.confidentiality.data.variety"))
print(verisplot(vcdb, "asset.assets"))

a2 <- getenum(vcdb, enum="action", primary="asset.assets", add.freq=T)
a2 <- a2[which(a2$enum!="environmental" & a2$primary!="Unknown"),]
slim.a2 <- a2[which(a2$x!=0,)]

# Order you factors and Change to factor 
#asset$enum <- factor(asset$enum, levels=asset$enum, ordered=T)

# ACTIONS TO ASSETS #########################################################
# requires package : verisr, ggplot2
# requires object: vcdb (7-6)
# get a data.frame comparing the actions to the assets
# this will add zero's in missing squares and include a frequency
a2 <- getenum(vcdb, enum="action", primary="asset.assets", add.freq=T)
# trim unknown asset and environment action for space
a2 <- a2[which(a2$enum!="environmental" & a2$primary!="Unknown"), ]
# so we should create a "slim" version without zeros to color it
slim.a2 <- a2[which(a2$x!=0), ]
# could sort these by converting to factors (we did in Fig 7-6)

# now make a nice plot  
gg <- ggplot(a2, aes(x=enum, y=primary, fill=freq))
gg <- gg + geom_tile(fill="white", color="gray80")
gg <- gg + geom_tile(data=slim.a2, color="gray80")
gg <- gg + scale_fill_gradient(low = "#F0F6FF", 
                               high = "#4682B4", guide=F)
gg <- gg + xlab("") + ylab("") + theme_bw()
gg <- gg + scale_x_discrete(expand=c(0,0))
gg <- gg + scale_y_discrete(expand=c(0,0))
gg <- gg + theme(axis.ticks = element_blank())
# and view it
print(gg)



# Figure 7-1 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
# get the count of incidents with an actor
actors <- getenumlist(vcdb, "actor")
actfields <- c("external", "internal", "partner")
actors.n <- sum(sapply(actors, function(x) {
  any(actfields %in% x)
}))
# now let's grab all three sections for motives.
motive <- getenum(vcdb, "actor.external.motive")
motive <- rbind(motive, getenum(vcdb, "actor.internal.motive"))
motive <- rbind(motive, getenum(vcdb, "actor.partner.motive"))
# combine them 
allmotive <- aggregate(x ~ enum, data=motive, FUN=sum)
# have to re-order the primary enum.
allmotive <- allmotive[with(allmotive, order(x)), ]
allmotive$enum <- factor(allmotive$enum, levels=allmotive$enum, ordered=T)
# and add in the frequency with actor count above
allmotive$freq <- allmotive$x/actors.n
## remove unknown as a proportion, focusing on "known" here.
finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(finalmotive, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(allmotive$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)


# Figure 7-2 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)

# should have verisr loaded and vcdb object created

actions <- getenumlist(vcdb, "action")
actfields <- c("malware", "hacking", "social", "error", "misuse", "physical") # sorry env
actions.n <- sum(sapply(actions, function(x) {
  any(actfields %in% x)
}))
# falling in love with "do.call"
action <- do.call(rbind, lapply(actfields, function(a) {
  rawaction <- getenum(vcdb, paste('action.', a, '.variety', sep=''))
  rawaction$enum <- paste(rawaction$enum, ' (', substr(a, 1, 3) ,')', sep='')
  rawaction
}))
allaction <- action[with(action, order(-x)), ]
# gotta do a top 5
topaction <- allaction[which(allaction$enum %in% allaction$enum[1:10]), ]
topaction$enum <- factor(topaction$enum, levels=rev(topaction$enum), ordered=T)
topaction$freq <- topaction$x/actions.n
## remove unknown as a proportion
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown" | allmotive$enum=="NA"), ]
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(topaction, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(topaction$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)


# Figure 7-3 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
# this is easy compared to the rest!
asset <- getenum(vcdb, "asset.assets", add.freq=T)

# flip it
asset <- asset[(with(asset, order(x))), ]
asset$enum <- factor(asset$enum, levels=asset$enum, ordered=T)

gg <- ggplot(asset, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(asset$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)

# Figure 7-4 #########################################################
# requires package : verisr, ggplot2, scales
# requires object: vcdb (7-6)
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
## this is a bit of a hack, as verisr does not do industry well (yet).
data("industry2")  # part of verisr package
rawi <- getenum(vcdb, "victim.industry2", add.n=T)
industrytext <- merge(rawi, industry2, by.x="enum", by.y="code", all.x=T)
aggind <- aggregate(x ~ title + n, data=industrytext, FUN=sum)
# flip it
industry <- aggind[(with(aggind, order(x))), ]
industry <- industry[c((nrow(industry)-4):nrow(industry)), ]
# hack until I fix industry2 data set with short title
industry$title <- as.character(industry$title)
#industry$title[which(industry$title=="Professional, Scientific, and Technical Services")] <- "Professional Services"
industry$title[which(industry$title=="Health Care and Social Assistance")] <- "Health Care"
industry$title <- factor(industry$title, levels=industry$title, ordered=T)
industry$freq <- industry$x/industry$n
gg <- ggplot(industry, aes(x=title, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(industry$freq)+.05)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)


# Figure 7-5 #########################################################
# requires package : verisr, ggplot2, scales, gridExtra, grid
# requires object: vcdb (7-6), verisplot (7-10)
library(grid)
library(gridExtra)
aa <- verisplot(vcdb, "action")
bb <- verisplot(vcdb, "actor.external.variety")
cc <- verisplot(vcdb, "action.physical.variety")
dd <- verisplot(vcdb, "action.hacking.vector")
ee <- verisplot(vcdb, "attribute.confidentiality.data.variety")
ff <- verisplot(vcdb, "asset.assets")
grid.arrange(aa, bb, cc, dd, ee, ff, ncol=2, clip=T)







#-----^ code for practice-----

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

print("test")

##PUT GRAPHS HERE
