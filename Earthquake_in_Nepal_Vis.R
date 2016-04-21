##################################################################################################################
#                                         2015 earthquake in nepal                                               #
##################################################################################################################


##lets load all required packages
pacman::p_load(rvest, ggmap,leaflet, lubridate, googleVis, dplyr)


##lets download data from seismonepal.gov.np website
url <- "www.seismonepal.gov.np/"

#As there  are five page of data
# for( i in 1:5){
#   i = as.numeric(i)
#   url <- paste("http://www.seismonepal.gov.np/index.php?action=earthquakes&show=recent&page=", i, sep="")
#   if (i==1){
#     table <- url %>%
#       read_html() %>%
#       html_nodes(xpath='//*[@class="block2-content"]/table[1]') %>%
#       html_table(header = TRUE) %>%
#       as.data.frame()
#   }else{
#   temp <- url %>%
#     read_html() %>%
#     html_nodes(xpath='//*[@class="block2-content"]/table[1]') %>%
#     html_table(header = TRUE) %>%
#     as.data.frame()
#   
#   table = rbind(table, temp)
#   }
# }


#Or you can download from githib
table = read.csv("https://raw.githubusercontent.com/BkrmDahal/Nepal_earthquake/master/2015_earthquake_nepal.csv", stringsAsFactors = F)


##lets do some feature engineering and conversion
table$Date_full = paste(table$Date, table$Local.Time)
table$Date_full = mdy_hm(table$Date_full)
table$hour = hour(table$Date_full)
table$month = month(table$Date_full)
table$year = year(table$Date_full)
table$Latitude = as.double(table$Latitude)
table$Longitude = as.double(table$Longitude)
table$Magnitude.ML. = as.numeric(table$Magnitude.ML.)
table$Energy = 10^((1.5 * table$Magnitude.ML.) + 4.8)*10
table$Energy_relative_to_7.6 = table$Energy/max(table$Energy)
table$Rating = ifelse(table$Magnitude.ML.>7, "M greater than 7",
                      ifelse(table$Magnitude.ML.>6, "M greater btn 6-7", 
                             ifelse(table$Magnitude.ML.>5, "M greater btn 5-6", "M greater below 5")))
table = table[complete.cases(table), ]


# ##let make ggplot plot
# #IRkernel::set_plot_options(width=800, height=800, units='px')
# #lets download nepal map
# map <- get_map(location = "Nepal", zoom = 7, source="osm", color = 'bw')
# 
# (ggmap(map, extent = "device") + 
#   geom_point(data = table, aes(y = Latitude, x = Longitude, color = factor(Rating), size = Energy),  alpha = I(0.6))
# + scale_size_continuous(range = c(1,50), guide = 'none'))


##lets make some interactive googlechart
#gvisgeochart
table$LatLong = paste(table$Latitude ,table$Longitude, sep=":")
table$Tip = paste("Epicentre:", table$Epicentre,sep="")
table = table[order(table$Magnitude.ML., decreasing = T), ]
geomarker = table[which(table$Magnitude.ML.>=4.5), c("LatLong",'Energy_relative_to_7.6', "Magnitude.ML.", "Tip")]
GeoMarker <- gvisGeoChart(geomarker, "LatLong", 
                          hovervar = "Tip",
                          sizevar='Energy_relative_to_7.6',
                          colorvar="Magnitude.ML.", 
                          options=list(height=350, 
                                       width=600,
                                       region="NP", 
                                       title="Location of 1015 April earthquake and aftersock greater than 5M epicentre",  
                                       chartid="Geomap",
                                       displayMode='markers',
                                       tableOptions="bgcolor=\"#AABBCC\"",
                                       colorAxis="{values:[4.5,5.5,6.5,8],
                                       colors:[\'grey', \'orange\', \'pink',\'red']}"))

plot(GeoMarker)

#lets make calender chart
calender <- summarise(group_by(table, Date), No_of_earthquake = length(Date))
calender$Date <- mdy(calender$Date)
Cal <- gvisCalendar(calender, 
                    datevar="Date", 
                    numvar="No_of_earthquake",
                    options=list(
                      title="Daily no of aftersock after april 2015 earthquake",
                      calendar="{cellSize:10,
                      yearLabel:{fontSize:20, color:'#444444'},
                      focusedCellColor:{stroke:'red'}}",
                      width=600, height=200),
                    chartid="Calendar")

###bubble chart
table$time = sapply(strsplit(table$Local.Time, ":"),
                    function(x) {
                      x <- as.numeric(x)
                      x[1]+x[2]/60
                    })
bubble = table[, c("Date_full", "time","Magnitude.ML.", "Energy_relative_to_7.6","Energy", "Rating", "Date", "Epicentre")]
bubble <- gvisBubbleChart(bubble, idvar="Date",
                          xvar="time", yvar="Magnitude.ML.",
                          colorvar="Rating", sizevar="Energy",
                          options = list(width=600, height=350,
                                         title="Time of Earthquake",
                                        chartArea="{left:50,top:50,width:\"75%\",height:\"75%\"}",
                                         hAxis= "{title:'Time (hrs)'}", chartid = "bubble chart"))
plot(bubble)

#Bar graph

table$greater_5 = ifelse(table$Magnitude.ML.>=5,1,0)
table$less_5 = ifelse(table$Magnitude.ML.>=5,0,1)
table$Epicentre = tolower(table$Epicentre)
table$Epicentre = ifelse(table$Epicentre=="sindhupalchok" |
                           table$Epicentre=="sindupalchok", 'sindhupalchowk',
                         ifelse(table$Epicentre=="kabre" |
                                  table$Epicentre=="kavrepalanchok" |
                                  table$Epicentre=="kavrepalanchowk" , "kavre", table$Epicentre))
pie = summarise(group_by(table, Epicentre), No_of_quakes_greater_than_5 = sum(greater_5),No_of_quakes_less_than_5 = sum(less_5) )
pie = pie[which(pie$No_of_quakes_less_than_5>5), ]

Pie <- gvisBarChart(pie, options = list(width=600, height=400,
                                           title="No of quakes below 5 and above 5"))
plot(Pie)


##lets merge
Combine <- gvisMerge(gvisMerge(GeoMarker, Pie), gvisMerge(bubble, Cal),
                     horizontal=FALSE, tableOptions="bgcolor=\"#AABBCC\"", "Earthquake in Nepal") 

plot(Combine)

print(Combine, file="Earthquake in Nepal.html")


################################################Bikram Dahal####################################
