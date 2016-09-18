rm(list = ls())

##############
## Packages ##
##############
# install.packages("ggmap", dependencies = T)
# install.packages("stringr", dependencies = T)
# install.packages("rgdal", dependencies = T)
# install.packages("raster", dependencies = T)
library(ggplot2)
library(ggmap)
library(stringr)
library(rgdal)
library(raster)
library(dplyr)


###################################
### Importing/Managing Our Data ###
###################################
## Import raw data
inst_raw_data <- read.csv("data/hd2014.csv") #use name() to see all columns
admin_raw_data <- read.csv("data/adm2014.csv")
raw_data <- merge(x = inst_raw_data, y = admin_raw_data, by = "UNITID", all.x = TRUE)
rm(inst_raw_data, admin_raw_data)

## Subset
institutions <- subset(raw_data, ICLEVEL == 1)
institutions <- subset(raw_data, HLOFFER == 9)
institutions <- institutions[,c(1:2,4:5,66:67, 80, 86, 92, seq(118, 144, 2))]
institutions$percent_admit <- institutions$ADMSSN/institutions$APPLCN


#####################
### Initial Plots ###
#####################
#USA <- get_map(location = "USA", zoom = 4, maptype = "toner") #Ugly Map
USA <- get_map(location = "USA", zoom = 4, maptype = "watercolor")
USA <- ggmap(USA) + geom_point(aes(LONGITUD, LATITUDE), data = institutions)
USA


##################
### Subsetting ###
##################
## Creating Reference Values to W&M ##
percent_admit_wm <- institutions[which(institutions$INSTNM == "College of William and Mary"), 24]
totalSAT75_wm <- sum(institutions[which(institutions$INSTNM == "College of William and Mary"), c(11, 13, 15)])
totalSAT25_wm <- sum(institutions[which(institutions$INSTNM == "College of William and Mary"), c(10, 12, 14)])

## Subsetting into top universities ##
selective <- subset(institutions, percent_admit < percent_admit_wm)
selective <- selective[order(selective$percent_admit),]
selective$totalSAT75 <- rowSums(selective[,c(11, 13, 15)])
selective$totalSAT25 <- rowSums(selective[,c(10, 12, 14)])

## Subsetting into the most selective universities
most_selective <- subset(selective, totalSAT75 > totalSAT75_wm)
most_selective <- subset(selective, totalSAT25 > totalSAT25_wm)
#most_selective <- most_selective[order(most_selective$STABBR),]
most_selective <- most_selective[order(most_selective$percent_admit),]
rm(totalSAT25_wm, totalSAT75_wm, percent_admit_wm)

################################
### Most Selective Uni. Plot ###
################################
mselect <- get_map(location = "USA", zoom = 4, maptype = "watercolor")
mselect <- ggmap(mselect)
mselect <- mselect + geom_point(aes(LONGITUD, LATITUDE), data = most_selective)
mselect <- mselect + annotate('text', x = most_selective$LONGITUD, y = most_selective$LATITUDE + .5
                              , label = most_selective$INSTNM
                              , size = 2)
mselect


############################
### Beautifying the Data ###
############################
## Getting rid of common strings ##
most_selective$INSTNM <- gsub(" University", "", most_selective$INSTNM)
most_selective$INSTNM <- gsub("University", "", most_selective$INSTNM)
most_selective$INSTNM <- gsub(" of ", "", most_selective$INSTNM)
most_selective$INSTNM <- gsub("College", "", most_selective$INSTNM)

## Abbreviating School Names ##
most_selective$INSTNM <- sub("California InstituteTechnology", "Cal Tech", most_selective$INSTNM)
most_selective$INSTNM <- sub("Southern California", "USC", most_selective$INSTNM)
most_selective$INSTNM <- sub("Massachusetts InstituteTechnology", "MIT", most_selective$INSTNM)
most_selective$INSTNM <- sub("Michigan-Ann Arbor", "Michigan", most_selective$INSTNM)
most_selective$INSTNM <- sub("Washington in St Louis", "Washington", most_selective$INSTNM)
most_selective$INSTNM <- sub("Columbia in the CityNew York", "Colombia", most_selective$INSTNM)
most_selective$INSTNM <- sub("Pennsylvania", "U. Penn", most_selective$INSTNM)

## Label Matrix Object ##
rm(txt)  ##Comment on/off as needed
names <- most_selective$INSTNM
x <- most_selective$LONGITUD
y <- most_selective$LATITUDE
selec <- most_selective$percent_admit
sat75 <- most_selective$totalSAT75
txt <- cbind.data.frame(names, x, y, selec, sat75)
rm(names, x, y, selec, sat75)
txt <- txt[order(txt$selec),]

## Adjusting Annotation Positions ##
txt$names #to view which row to change

## X:Lat is [,2] and Y:Long is [,3]
## West Coast Adjustments ##
txt[which(txt$names == "USC"),3]        <- txt[which(txt$names == "USC"),3] + -0.5 # USC lat
txt[which(txt$names == "Cal Tech"), 3]  <- txt[which(txt$names == "Cal Tech"), 3] + 0.5 
txt[which(txt$names == "Stanford"), 3]  <- txt[which(txt$names == "Stanford"), 3] + 0.5

## Midwest Adjustments ##
txt[which(txt$names == "Chicago"), 3]         <- txt[which(txt$names == "Chicago"), 3] + -0.5 # Chicago
txt[which(txt$names == "Northwestern"), 3]    <- txt[which(txt$names == "Northwestern"), 3] + 0.5 #Northwestern
txt[which(txt$names == "Michigan"), 3]        <- txt[which(txt$names == "Michigan"), 3] + 0.5
txt[which(txt$names == "Carnegie Mellon"), 3] <- txt[which(txt$names == "Carnegie Mellon"), 3] + 0.5
txt[which(txt$names == "Washington"), 3]      <- txt[which(txt$names == "Washington"), 3] + 0.5

## South Adjustments ##
txt[which(txt$names == "Rice"), 3]               <- txt[which(txt$names == "Rice"), 3] + 0.5
txt[which(txt$names == "Emory"), 3]              <- txt[which(txt$names == "Emory"), 3] + 0.5
txt[which(txt$names == "Vanderbilt"), 3]         <- txt[which(txt$names == "Vanderbilt"), 3] + 0.5
txt[which(txt$names == "Washington and Lee"), 3] <- txt[which(txt$names == "Washington and Lee"), 3] + -0.5
txt[which(txt$names == "Duke"), 3]               <- txt[which(txt$names == "Duke"), 3] + 0.5
txt[which(txt$names == "Johns Hopkins"), 3]      <- txt[which(txt$names == "Johns Hopkins"), 3] + -0.5

## Northeast Adjustments ##
txt[which(txt$names == "U. Penn"), 3]     <- txt[which(txt$names == "U. Penn"), 3] + 1
txt[which(txt$names == "Princton"), 3]    <- txt[which(txt$names == "Princton"), 3] + 1
txt[which(txt$names == "Colombia"), 3]    <- txt[which(txt$names == "Colombia"), 3] + 1
txt[which(txt$names == "Yale"), 3]        <- txt[which(txt$names == "Yale"), 3] + 1
txt[which(txt$names == "Brown"), 3]       <- txt[which(txt$names == "Brown"), 3] + 1
txt[which(txt$names == "Middlebury "), 3] <- txt[which(txt$names == "Middlebury "), 3] + 0.5
txt[which(txt$names == "Dartmouth "), 3]  <- txt[which(txt$names == "Dartmouth "), 3] + -0.5

## Boston-Cambridge Adjustments
txt[which(txt$names == "Harvard"), 3] <- txt[which(txt$names == "Harvard"), 3] + 1
txt[which(txt$names == "MIT"), 3]     <- txt[which(txt$names == "MIT"), 3] + -1

## Creating object to add for W&M ##
wm     <- institutions[which(institutions$INSTNM == "College of William and Mary"), c(2,5,6)]
wm[,1] <- "William and Mary"


##########################
### Plotting Nice Data ###
##########################
mselect <- get_map(location = c(-130,25, -65, 50), zoom = 4, maptype = "watercolor")
mselect <- ggmap(mselect)
mselect <- mselect + geom_point(aes(LONGITUD, LATITUDE), data = most_selective, size = 1.25)
mselect <- mselect + annotate('text', x = txt$x, y = txt$y, label = paste(1:25, txt$names, ""), size = 2.5)
mselect <- mselect + geom_point(size = 3, shape = 18, aes(LONGITUD, LATITUDE), data = wm)
mselect <- mselect + annotate('text', x = wm$LONGITUD, y = wm$LATITUDE + -0.5, label = paste(25, wm$INSTNM, ""), size = 4)
mselect <- mselect + ggtitle("The Most Selective Universities in the USA Relative to W&M \n Yaseen Lotfi")
mselect


###################
### NE Close-Up ###
###################
## State shapefiles
# gClip <- function(shp, bb) {
#   if (class(bb) == "matrix") {
#     b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
#   } else {
#     b_poly <- as(extent(bb), "SpatialPolygons")
#     proj4string(b_poly) <- proj4string(shp)
#     gIntersection(shp, b_poly, byid = T)
#   }
# }
# states <- gClip(states, matrix(c(-81, 35, -69.5, 45.5), ncol = 2))

states <- spTransform(readOGR(dsn = "shapefiles", layer = "states", stringsAsFactors = F, verbose = F), CRS("+proj=longlat +datum=WGS84"))
states <- fortify(states)

## Recreate txt df
rm(txt)  ##Comment on/off as needed
names <- most_selective$INSTNM
x <- most_selective$LONGITUD
y <- most_selective$LATITUDE
selec <- most_selective$percent_admit
sat75 <- most_selective$totalSAT75
txt <- cbind.data.frame(names, x, y, selec, sat75)
rm(names, x, y, selec, sat75)

attach(institutions)
wm     <- institutions[which(INSTNM == "College of William and Mary"), c(2, 5, 6, 24)]
wm$totalSAT75 <- rowSums(institutions[which(INSTNM == "College of William and Mary"),c(11, 13, 15)])
wm[,1] <- "William and Mary"
names(wm) <- c("names", "x", "y", "selec", "sat75")
detach(institutions)

## Separate point data from labels
data <- rbind.data.frame(txt, wm)
data <- data[order(data$selec),]

## Label Adjustments ##
txt <- rbind.data.frame(txt, wm)
txt[which(txt$names == "U. Penn"), 2]     <- txt[which(txt$names == "U. Penn"), 2] + -0.75
txt[which(txt$names == "Princeton"), 2]   <- txt[which(txt$names == "Princeton"), 2] + -0.75
txt[which(txt$names == "Colombia"), 2]    <- txt[which(txt$names == "Colombia"), 2] + -0.75
txt[which(txt$names == "Yale"), 2]        <- txt[which(txt$names == "Yale"), 2] + -0.5
txt[which(txt$names == "Brown"), 2]       <- txt[which(txt$names == "Brown"), 2] + -0.5
txt[which(txt$names == "Middlebury "), 3] <- txt[which(txt$names == "Middlebury "), 3] + 0.25
txt[which(txt$names == "Dartmouth "), 3]  <- txt[which(txt$names == "Dartmouth "), 3] + -0.25
txt[which(txt$names == "Harvard"), 2] <- txt[which(txt$names == "Harvard"), 2] + -0.75
txt[which(txt$names == "MIT"), 2]     <- txt[which(txt$names == "MIT"), 2] + -0.75
txt[which(txt$names == "MIT"), 3]     <- txt[which(txt$names == "MIT"), 3] + 0.25
txt[which(txt$names == "Northeastern"), 3] <- txt[which(txt$names == "Northeastern"), 3] + 0.5
txt[which(txt$names == "Tufts"), 3] <- txt[which(txt$names == "Tufts"), 3] + 0.25
txt[which(txt$names == "Washington and Lee"), 3] <- txt[which(txt$names == "Washington and Lee"), 3] + 0.25
txt[which(txt$names == "Duke"), 2]               <- txt[which(txt$names == "Duke"), 2] + -0.5
txt[which(txt$names == "Johns Hopkins"), 2]      <- txt[which(txt$names == "Johns Hopkins"), 2] + -0.85
txt[which(txt$names == "Carnegie Mellon"), 3] <- txt[which(txt$names == "Carnegie Mellon"), 3] + 0.25
txt[which(txt$names == "William and Mary"), 3] <- txt[which(txt$names == "William and Mary"), 3] + -0.25

## Build the map
NE.map <- get_map(location = c(-81, 35, -69.5, 45.5), zoom = 6, source = "google", maptype = "satellite")
NE.map <- ggmap(NE.map)
NE.map <- NE.map + geom_map(data = states, map = states, aes(x = long, y = lat, map_id = id)
                            , color = "black"
                            , fill = "#737373" #Gray overlay
                            , alpha = 0.7
                            , size = 0.2)
NE.map <- NE.map + geom_point(data = data, aes(x, y, color = selec, size = sat75))
NE.map <- NE.map + scale_size(name = "SAT 75th Score")
NE.map <- NE.map + scale_colour_gradient(limits = c(min(data$selec) - 0.00086916, max(data$selec) + 0.0501952)
                                         , name = "Admit Rate"
                                         , labels = c("30%", "20%", "10%")
                                         , breaks = c(0.30, 0.20, 0.10)
                                         , high = "white", low = "red"
                                         , space = "Lab")
NE.map <- NE.map + ggtitle(bquote(atop(.("Institutions of Higher Education"), atop(italic(.("Relative to William & Mary")), "")))) +
  labs(x = "Longitude", y = "Latitude")
NE.map <- NE.map + annotate('text', x = txt$x, y = txt$y, label = txt$names, size = 3.5, color = "white")
NE.map
