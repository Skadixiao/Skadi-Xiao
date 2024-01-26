library(sf)
library(dplyr)
library(raster)
library(units)
library(openxlsx)
library(tmap)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)


# read data ---------------------------------------------------------------
boundary<-st_read("/Users/skadi/Desktop/dissertation/whole_ENG_1/boundary1.geojson")
charger<-st_read("/Users/skadi/Desktop/dissertation/all data/charger1_1.geojson")
classification<-st_read("/Users/skadi/Desktop/dissertation/all data/classfication1.geojson")
demand <- read.xlsx("/Users/skadi/Desktop/dissertation/all data/demand.xlsx")
classification <- merge(classification, demand, by = "LSOA01CD", all.x = TRUE)
#contain some na value
charger = charger[, c("chargeDevi", "connector1ChargeMethod","longitude.x", "latitude.x", "geometry")]
charger$capacity<-4
charger[which(charger$connector1ChargeMethod=="DC"), "capacity"]=48
classification = classification[, c("LSOA01CD", "GlobalID", "class", "car", "long", "lat", "geometry")]
classification$car[is.na(classification$car)] <- 0

urban2 <- subset(classification2, class == "urban")
rural2 <- subset(classification2, class == "rural")
st_write(urban2, "/Users/skadi/Desktop/dissertation/all data/urban2.geojson")
st_write(rural2, "/Users/skadi/Desktop/dissertation/all data/rural2.geojson")





# urban accessibility -----------------------------------------------------
# urban decay function
#urban_di = 2000
#the supposed distance between centroid and charger point
urban_d0 = 2500
#buffer (can be same as catchment size)
#urban_di0 = (urban_di/urban_d0)^2
#urban_decay_index = (exp(-0.5*urban_di0) - exp(-0.5) ) / (1-exp(-0.5)) 

#step 1: supply to demand ratio
urban.centroids <- st_centroid(urban)
urban.buff <- st_buffer(urban.centroids, dist = urban_d0)
charger.buff  <- st_buffer(charger, dist = urban_d0)
#tm_shape(urban.centroids)+tm_dots()+
#  tm_shape(urban.buff)+tm_borders(col="red", alpha=.4)+ 
#  tm_shape(classification)+tm_borders()
#join lsoa centroids  to charger buffer.
join1_urban = st_join(urban.centroids, charger.buff)
#Which LSOA centroids fall within which charger buffer?
#pay attention to the order, points should be in the front, 
#so that we can see how many losa are there in one charger's buffer services.
join1_urban_df = st_drop_geometry(join1_urban)
nrow(join1_urban_df) 
#815075
#To calculate decay index, we need to calculate distance first.
#First, we need to get rid of rows with NA in join2_1_df.
#The row with NA are those losa whose centroids are not covered by any charger buffers.
join1_urban_df = na.omit(join1_urban_df)
nrow(join1_urban_df) 
#580262
#Point1 is composed of LONG, LAT, which are coordinates for LSOA centroid.
point1_urban<-as.data.frame (join1_urban_df$long)
point1_urban$LAT<-join1_urban_df$lat
colnames(point1_urban)[1] = 'LONG'
#Point 2 is composed of longtitude, latitude, which are are coordinates for chargers.
point2_urban<-as.data.frame (join1_urban_df$long.1)
colnames(point2_urban)[1] = 'longitude'
point2_urban$latitude<-join1_urban_df$lat.1
#to get the distance between point 1 and point 2, we use pointDistance {raster} function.
dist1 =  pointDistance(point1_urban, point2_urban, lonlat=TRUE) 
join1_urban_df$distance = dist1
#Calculate decay index based on distance
join1_urban_df$di0 = ifelse( join1_urban_df$distance>urban_d0, 0, (join1_urban_df$distance/urban_d0)^2)
join1_urban_df$decay_index = (exp(-0.5*join1_urban_df$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decay demand factored by decay index
join1_urban_df$decay_demand = join1_urban_df$decay_index * join1_urban_df$car
#Summarise demand
demand1 = summarise(demand1 = sum(decay_demand), group_by(join1_urban_df,chargeDevi) )
### why demand1 contains na???
charger = left_join(charger , demand1, by="chargeDevi") 
#Calculate supply to demand ratio
charger$sd_ratio2 <- charger$capacity / charger$demand1
###so that sd_ratio2 still contains na, should i get rid of them???


#step 2: aggregate sd ratio
join3_urban=st_join(charger, urban.buff)
join3_urban_df<-st_drop_geometry(join3_urban)
#Calculate the distance between lsoa centroids and charger points.
point3_urban <- as.data.frame (join3_urban_df$long)
point3_urban$LAT<-join3_urban_df$lat
colnames(point3_urban)[1] = 'LONG'
point4_urban<-as.data.frame (join3_urban_df$long.1)
point4_urban$latitude<-join3_urban_df$lat.1
colnames(point4_urban)[1] = 'longitude'
dist2 <-  pointDistance(point3_urban, point4_urban, lonlat=TRUE)
join3_urban_df$distance <- dist2
#Calculate decay index first based on distance
join3_urban_df$di0 = ifelse( join3_urban_df$distance>urban_d0, 0, (join3_urban_df$distance/urban_d0)^2)
join3_urban_df$decay_index = (exp(-0.5*join3_urban_df$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decayed sd
join3_urban_df$decay_service = join3_urban_df$decay_index * join3_urban_df$sd_ratio2
#Aggregate decay services at LSOA level
acc_e2sfca_1 = summarise(acc_e2sfca_1=sum(decay_service), group_by(join3_urban_df, LSOA01CD) ) 
print(acc_e2sfca_1)
classification = left_join(classification,acc_e2sfca_1, by="LSOA01CD")



classification$acc_e2sfca_1[is.na(classification$acc_e2sfca_1)] <- 0




# visualization -----------------------------------------------------------
tmap_mode('view')
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa1)+ 
  tm_fill(col="acc_e2sfca_1", style="jenks", n=7, palette ="BuGn", title="Accessibility_E2SFCA" , alpha=.7)+
  tm_layout(main.title = "Accessibility of EV chargers using E2SFCA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary1)+tm_borders()
















# xxx ---------------------------------------------------------------------

# devide classification1 to urban and rural, and combine it with classification
classification1_df = st_drop_geometry(classification1)
selected_columns <- c("LSOA01CD", "GlobalID", "long","lat","class")
classification1_df <- classification1_df %>% 
  select(all_of(selected_columns))
#combine it with 2SFCA
lsoa1_df = st_drop_geometry(lsoa1)
selected_columns1 <- c("LSOA01CD", "long","lat","num_points", "car", "charger_density", 
                       "acc_e2sfca_1")
lsoa1_df <- lsoa1_df %>% 
  select(all_of(selected_columns1))
lsoa1_df = left_join(lsoa1_df, classification1_df, by="LSOA01CD" )
lsoa1_df <- lsoa1_df %>% 
  select(c("LSOA01CD", "long.x", "lat.x", "num_points", "car", "charger_density", "acc_e2sfca_1", "GlobalID", "class"))
class1_urban <- lsoa1_df %>%
  filter(class == "urban")
class1_rural <- lsoa1_df %>%
  filter(class == "rural")
# could be useful for the regression, but not useful for the visualization.
#st_write(class1_urban, "/Users/skadi/Desktop/dissertation/all data/ML/class1_urban.csv")
#st_write(class1_rural, "/Users/skadi/Desktop/dissertation/all data/ML/class1_rural.csv")
