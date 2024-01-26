# split to four parts -----------------------------------------------------
library(sf)
library(dplyr)
library(raster)
library(tmap)
tmap_mode("view")
#charger_ENG<-st_read("/Users/skadi/Desktop/dissertation/all data/charger_eng_sf.geojson")
boundary<-st_read("/Users/skadi/Desktop/dissertation/all_data/boundary.geojson")
#lsoa<-st_read("/Users/skadi/Desktop/dissertation/all data/lsoa_eng.geojson")
#classification<-read.csv("/Users/skadi/Desktop/dissertation/all data/classification.csv")
#classification = classification[, c("LSOA01CD", "Morphology.Code")]
#classification$class <- ifelse(classification$Morphology.Code == 1, "urban", "rural")
tm_shape(boundary) + tm_borders()+
  tm_scale_bar(position = c("left", "bottom"))
  
#split boundary to four parts
#split_boundary <- split(boundary, boundary$OBJECTID)  
#for (i in 1:length(split_boundary)) {
#  st_write(split_boundary[[i]], paste0("boundary", i, ".geojson"), driver = "GeoJSON")
#}
boundary1<-st_read("boundary1.geojson")
boundary2<-st_read("boundary2.geojson")
boundary3<-st_read("boundary3.geojson")
boundary4<-st_read("boundary4.geojson")


#split lsoa
#boundary1<-st_read("/Users/skadi/Desktop/dissertation/workshop1/workshop_materials/data/boundary.geojson")
#boundary1 <- st_transform(boundary1, crs = st_crs(lsoa))
#lsoa1 <- st_intersection(st_make_valid(lsoa), st_make_valid(boundary1))
#lsoa1$geometry <- st_cast(lsoa1$geometry, "POLYGON")
#lsoa1<-na.omit(lsoa1)
#lsoa1 <- st_cast(lsoa1, "MULTIPOLYGON")
#st_write(lsoa1, "/Users/skadi/Desktop/dissertation/all data/lsoa1.geojson")
#lsoa2 <- st_intersection(lsoa, boundary2)
#st_write(lsoa2, "/Users/skadi/Desktop/dissertation/all data/lsoa2.geojson")
#lsoa3 <- st_intersection(lsoa, boundary3)
#st_write(lsoa3, "/Users/skadi/Desktop/dissertation/all data/lsoa3.geojson")
#lsoa4 <- st_intersection(lsoa, boundary4)
#st_write(lsoa4, "/Users/skadi/Desktop/dissertation/all data/lsoa4.geojson")
lsoa1<-st_read("/Users/skadi/Desktop/dissertation/all_data/lsoa1.geojson")
lsoa2<-st_read("/Users/skadi/Desktop/dissertation/all_data/lsoa2.geojson")
lsoa3<-st_read("/Users/skadi/Desktop/dissertation/all_data/lsoa3.geojson")
lsoa4<-st_read("/Users/skadi/Desktop/dissertation/all_data/lsoa4.geojson")
#lsoa1 = lsoa1[, c("chargeDevi", "connector1ChargeMethod","longitude.x", "latitude.x", "geometry")]



#split classification
classification1 <- st_read("/Users/skadi/Desktop/dissertation/all_data/classfication1.geojson")
classification2 <- st_read("/Users/skadi/Desktop/dissertation/all_data/classfication2.geojson")
classification3 <- st_read("/Users/skadi/Desktop/dissertation/all_data/classfication3.geojson")
classification4 <- st_read("/Users/skadi/Desktop/dissertation/all_data/classfication4.geojson")
#st_write(classification1, "/Users/skadi/Desktop/dissertation/all data/classfication1.geojson")
#st_write(classification2, "/Users/skadi/Desktop/dissertation/all data/classfication2.geojson")
#st_write(classification3, "/Users/skadi/Desktop/dissertation/all data/classfication3.geojson")
#st_write(classification4, "/Users/skadi/Desktop/dissertation/all data/classfication4.geojson")

#split charger points
charger1<-st_read("/Users/skadi/Desktop/dissertation/all_data/charger1_1.geojson")
charger2<-st_read("/Users/skadi/Desktop/dissertation/all_data/charger2_1.geojson")
charger3<-st_read("/Users/skadi/Desktop/dissertation/all_data/charger3_1.geojson")
charger4<-st_read("/Users/skadi/Desktop/dissertation/all_data/charger4_1.geojson")
#charger_ENG <- charger_ENG %>%
#  rename(chargeDevi = chargeDeviceID)
#charger_test<-read.csv("/Users/skadi/Desktop/dissertation/all data/charger3_eng.csv")
#charger_test <- charger_test %>%
#  rename(chargeDevi = chargeDeviceID)
#charger1_test <- merge(charger1, charger_test, by = "chargeDevi", all.x = TRUE)
#charger2_test <- merge(charger2, charger_test, by = "chargeDevi", all.x = TRUE)
#charger3_test <- merge(charger3, charger_test, by = "chargeDevi", all.x = TRUE)
#charger4_test <- merge(charger4, charger_test, by = "chargeDevi", all.x = TRUE)

#charger1 <- st_join(charger1, charger_ENG, join = st_within, left = TRUE)
#charger2 <- st_join(charger2, charger_ENG, join = st_within, left = TRUE)
#charger3 <- st_join(charger3, charger_ENG, join = st_within, left = TRUE)
#charger4 <- st_join(charger4, charger_ENG, join = st_within, left = TRUE)

#charger1 <- st_join(charger_ENG, boundary1, join = st_intersects)
#charger1 <- na.omit(charger1)
#charger2 <- st_join(charger_ENG, boundary2, join = st_intersects)
#charger2 <- na.omit(charger2)
#charger3 <- st_join(charger_ENG, boundary3, join = st_intersects)
#charger3 <- na.omit(charger3)
#charger4 <- st_join(charger_ENG, boundary4, join = st_intersects)
#charger4 <- na.omit(charger4)
#st_write(charger1_test, "/Users/skadi/Desktop/dissertation/all data/charger1_1.geojson")
#st_write(charger2_test, "/Users/skadi/Desktop/dissertation/all data/charger2_1.geojson")
#st_write(charger3_test, "/Users/skadi/Desktop/dissertation/all data/charger3_1.geojson")
#st_write(charger4_test, "/Users/skadi/Desktop/dissertation/all data/charger4_1.geojson")

#combine demand to lsoa
#car_van <- read.csv("/Users/skadi/Desktop/dissertation/all data/car_van_availability.csv")
#lsoa1 <- merge(lsoa1, car_van, by = "LSOA01CD", all.x = TRUE)
#lsoa2 <- merge(lsoa2, car_van, by = "LSOA01CD", all.x = TRUE)
#lsoa3 <- merge(lsoa3, car_van, by = "LSOA01CD", all.x = TRUE)
#lsoa4 <- merge(lsoa4, car_van, by = "LSOA01CD", all.x = TRUE)

# boundary1 London -------------------------------------------------------
charger1 = charger1[, c("chargeDevi", "connector1ChargeMethod", "geometry")]
charger1$capacity<-4
charger1[which(charger1$connector1ChargeMethod=="DC"), "capacity"]=48
charger1$capacity2 = ifelse(charger1$capacity == 4, "blue", "red")

tmap_mode("view")
tmap_options(check.and.fix = TRUE)

tm_shape(classification1)+tm_borders(alpha=0.2)+tm_fill(col = "class", style = "jenks", palette = "Blues")+
  tm_shape(boundary1)+tm_borders()+
  tm_shape(charger1)+tm_dots(col="capacity2", size = 0.001)+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(legend.show = TRUE)

#the number of chargers at lsoa level
joined1 <- st_join(charger1, lsoa1, join = st_within)
counts.sf1 <- summarize(num_points = n(),group_by(joined1, LSOA01CD))
counts1= st_drop_geometry(counts.sf1)
lsoa1 = left_join(lsoa1, counts1, by="LSOA01CD" )
lsoa1[is.na(lsoa1$num_points), "num_points"] = 0

tm_shape(lsoa1)+ 
  tm_fill(col="num_points", style="jenks", n=7, palette ="BuGn", title="Number of chargers" , alpha=.7)+
  tm_layout(main.title = "Number of EV chargers per LSOA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary1)+tm_borders()

#The density of charger at lsoa level
lsoa1$charger_density = lsoa1$num_points/ lsoa1$car
hist(lsoa1$charger_density)
lsoa1[is.na(lsoa1$charger_density), "charger_density"] = 0
tm_shape(lsoa1)+ 
  tm_fill(col="charger_density", style="fisher", n=7, palette ="BuPu", alpha=0.7, title="Charger density_charger per car or van" )+
  tm_layout(main.title = "Charger density_charger per car or van",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary1)+tm_borders()






#accessibility
di = 400
#e.g., the distance between centroid of lsoa and charger is 350 metre, di should be no greater than d0.
d0 = d = 400
#d0 is the radius of buffer
di0 = (di/d)^2
decay_index = (exp(-0.5*di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#note: try specifying di as 0, 100, 300, and 400 and see how decay_index change.(sensitivity analysis)

#step 1: supply to demand ratio
lsoa1.centroids <- st_centroid(lsoa1)
lsoa1.buff <- st_buffer(lsoa1.centroids, dist = d)
charger1.buff  <- st_buffer(charger1, dist = d)
tm_shape(lsoa1.centroids)+tm_dots()+
  tm_shape(lsoa1.buff)+tm_borders(col="red", alpha=.4)+ 
  tm_shape(lsoa1)+tm_borders()
#join lsoa centroids  to charger buffer.
join2_1 = st_join(lsoa1.centroids, charger1.buff)
#Which LSOA centroids fall within which charger buffer?
#pay attention to the order, points should be in the front, 
#so that we can see how many losa are there in one charger's buffer services.
join2_1_df = st_drop_geometry(join2_1)
nrow(join2_1_df) 
#26651
#To calculate decay index, we need to calculate distance first.
#First, we need to get rid of rows with NA in join2_1_df.
#The row with NA are those losa whose centroids are not covered by any charger buffers.
join2_1_df.s = na.omit(join2_1_df)
nrow(join2_1_df.s) 
#17714
#Point1 is composed of LONG, LAT, which are coordinates for LSOA centroid.
point1_1<-as.data.frame (join2_1_df.s$long)
point1_1$LAT<-join2_1_df.s$lat
colnames(point1_1)[1] = 'LONG'
#Point 2 is composed of longtitude, latitude, which are are coordinates for chargers.
point2_1<-as.data.frame (join2_1_df.s$long.1)
colnames(point2_1)[1] = 'longitude'
point2_1$latitude<-join2_1_df.s$lat.1
#to get the distance between point 1 and point 2, we use pointDistance {raster} function.
dist1 =  pointDistance(point1_1, point2_1, lonlat=TRUE) 
join2_1_df.s$distance = dist1
#Calculate decay index based on distance
join2_1_df.s$di0 = ifelse( join2_1_df.s$distance>d, 0, (join2_1_df.s$distance/d)^2)
join2_1_df.s$decay_index = (exp(-0.5*join2_1_df.s$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decay demand factored by decay index
join2_1_df.s$decay_demand = join2_1_df.s$decay_index * join2_1_df.s$car
#Summarise demand
demand1 = summarise(demand1 = sum(decay_demand), group_by(join2_1_df.s,chargeDevi) )
### why demand1 contains na???
charger1 = left_join(charger1 , demand1, by="chargeDevi") 
#Calculate supply to demand ratio
charger1$sd_ratio2 <- charger1$capacity / charger1$demand1
###so that sd_ratio2 still contains na, should i get rid of them???

#step 2: aggregate sd ratio
join3_1=st_join(charger1, lsoa1.buff)
join3_1_df<-st_drop_geometry(join3_1)
#Calculate the distance between lsoa centroids and charger points.
point3_1 <- as.data.frame (join3_1_df$long)
point3_1$LAT<-join3_1_df$lat
colnames(point3_1)[1] = 'LONG'
point4_1<-as.data.frame (join3_1_df$long.1)
point4_1$latitude<-join3_1_df$lat.1
colnames(point4_1)[1] = 'longitude'
dist2 <-  pointDistance(point3_1, point4_1, lonlat=TRUE)
join3_1_df$distance <- dist2
#Calculate decay index first based on distance
join3_1_df$di0 = ifelse( join3_1_df$distance>d, 0, (join3_1_df$distance/d)^2)
join3_1_df$decay_index = (exp(-0.5*join3_1_df$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decayed sd
join3_1_df$decay_service = join3_1_df$decay_index * join3_1_df$sd_ratio2
#Aggregate decay services at LSOA level
acc_e2sfca_1 = summarise(acc_e2sfca_1=sum(decay_service), group_by(join3_1_df, LSOA01CD) ) 
lsoa1 = left_join(lsoa1,acc_e2sfca_1, by="LSOA01CD")
lsoa1$acc_e2sfca_1[is.na(lsoa1$acc_e2sfca_1)] <- 0
#Visualize accessibility
tmap_mode('view')
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa1)+ 
  tm_fill(col="acc_e2sfca_1", style="jenks", n=7, palette ="BuGn", title="Accessibility_E2SFCA" , alpha=.7)+
  tm_layout(main.title = "Accessibility of EV chargers using E2SFCA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary1)+tm_borders()








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


# boundary2 ---------------------------------------------------------------
charger2 = charger2[, c("chargeDevi", "connector1ChargeMethod", "geometry")]
charger2$capacity<-4
charger2[which(charger2$connector1ChargeMethod=="DC"), "capacity"]=48
charger2$capacity2 = ifelse(charger2$capacity == 4, "blue", "red")

tm_shape(classification2)+tm_borders(alpha=0.2)+tm_fill(col = "class", style = "jenks", palette = "Blues")+
  tm_shape(boundary2)+tm_borders()+
  tm_shape(charger2)+tm_dots(col="capacity2", size = 0.001)+
  tm_scale_bar(position = c("left", "bottom"))

#the number of chargers at lsoa level
joined2 <- st_join(charger2, lsoa2, join = st_within)
counts.sf2 <- summarize(num_points = n(),group_by(joined2, LSOA01CD))
counts2= st_drop_geometry(counts.sf2)
lsoa2 = left_join(lsoa2, counts2, by="LSOA01CD" )
lsoa2[is.na(lsoa2$num_points), "num_points"] = 0

tm_shape(lsoa2)+ 
  tm_fill(col="num_points", style="jenks", n=7, palette ="BuGn", title="Number of chargers" , alpha=.7)+
  tm_layout(main.title = "Number of EV chargers per LSOA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary2)+tm_borders()

#The density of charger at lsoa level
lsoa2$charger_density = lsoa2$num_points/ lsoa2$car
hist(lsoa2$charger_density)
lsoa2[is.na(lsoa2$charger_density), "charger_density"] = 0
tm_shape(lsoa2)+ 
  tm_fill(col="charger_density", style="fisher", n=7, palette ="BuPu", alpha=0.7, title="Charger density_charger per car or van" )+
  tm_layout(main.title = "Charger density_charger per car or van",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary2)+tm_borders()

#accessibility
di = 2500
#e.g., the distance between centroid of lsoa and charger is 350 metre, di should be no greater than d0.
d0 = d = 2500
#d0 is the radius of buffer
di0 = (di/d)^2
decay_index = (exp(-0.5*di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#note: try specifying di as 0, 100, 300, and 400 and see how decay_index change.(sensitivity analysis)

#step 1: supply to demand ratio
lsoa2.centroids <- st_centroid(lsoa2)
lsoa2.buff <- st_buffer(lsoa2.centroids, dist = d)
charger2.buff  <- st_buffer(charger2, dist = d)
tm_shape(lsoa2.centroids)+tm_dots()+
  tm_shape(lsoa2.buff)+tm_borders(col="red", alpha=.4)+ 
  tm_shape(lsoa2)+tm_borders()
#join lsoa centroids  to charger buffer.
join2_2 = st_join(lsoa2.centroids, charger2.buff)
#Which LSOA centroids fall within which charger buffer?
#pay attention to the order, points should be in the front, 
#so that we can see how many losa are there in one charger's buffer services.
join2_2_df = st_drop_geometry(join2_2)
nrow(join2_2_df) 
#12668
#To calculate decay index, we need to calculate distance first.
#First, we need to get rid of rows with NA in join2_2_df.
#The row with NA are those losa whose centroids are not covered by any charger buffers.
join2_2_df.s = na.omit(join2_2_df)
nrow(join2_2_df.s) 
#2582
#Point1 is composed of LONG, LAT, which are coordinates for LSOA centroid.
point1_2<-as.data.frame (join2_2_df.s$long)
point1_2$LAT<-join2_2_df.s$lat
colnames(point1_2)[1] = 'LONG'
#Point 2 is composed of longtitude, latitude, which are are coordinates for chargers.
point2_2<-as.data.frame (join2_2_df.s$long.1)
colnames(point2_2)[1] = 'longitude'
point2_2$latitude<-join2_2_df.s$lat.1
#to get the distance between point 1 and point 2, we use pointDistance {raster} function.
dist1_2 =  pointDistance(point1_2, point2_2, lonlat=TRUE) 
join2_2_df.s$distance = dist1_2
#Calculate decay index based on distance
join2_2_df.s$di0 = ifelse( join2_2_df.s$distance>d, 0, (join2_2_df.s$distance/d)^2)
join2_2_df.s$decay_index = (exp(-0.5*join2_2_df.s$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decay demand factored by decay index
join2_2_df.s$decay_demand = join2_2_df.s$decay_index * join2_2_df.s$car
#Summarise demand
demand1_2 = summarise(demand1_2 = sum(decay_demand), group_by(join2_2_df.s,chargeDevi) )
### why demand1 contains na???
charger2 = left_join(charger2 , demand1_2, by="chargeDevi") 
#Calculate supply to demand ratio
charger2$sd_ratio2 <- charger2$capacity / charger2$demand1_2
###so that sd_ratio2 still contains na, should i get rid of them???

#step 2: aggregate sd ratio
join3_2=st_join(charger2, lsoa2.buff)
join3_2_df<-st_drop_geometry(join3_2)
#Calculate the distance between lsoa centroids and charger points.
point3_2 <- as.data.frame (join3_2_df$long)
point3_2$LAT<-join3_2_df$lat
colnames(point3_2)[1] = 'LONG'
point4_2<-as.data.frame (join3_2_df$long.1)
point4_2$latitude<-join3_2_df$lat.1
colnames(point4_2)[1] = 'longitude'
dist2_2 <-  pointDistance(point3_2, point4_2, lonlat=TRUE)
join3_2_df$distance <- dist2_2
#Calculate decay index first based on distance
join3_2_df$di0 = ifelse( join3_2_df$distance>d, 0, (join3_2_df$distance/d)^2)
join3_2_df$decay_index = (exp(-0.5*join3_2_df$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decayed sd
join3_2_df$decay_service = join3_2_df$decay_index * join3_2_df$sd_ratio2
#Aggregate decay services at LSOA level
acc_e2sfca_2 = summarise(acc_e2sfca_2=sum(decay_service), group_by(join3_2_df, LSOA01CD) ) 
lsoa2 = left_join(lsoa2,acc_e2sfca_2, by="LSOA01CD")
lsoa2$acc_e2sfca_2[is.na(lsoa2$acc_e2sfca_2)] <- 0
#Visualize accessibility
tmap_mode('view')
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa2)+ 
  tm_fill(col="acc_e2sfca_2", style="jenks", n=7, palette ="BuGn", title="Accessibility_E2SFCA" , alpha=.7)+
  tm_layout(main.title = "Accessibility of EV chargers using E2SFCA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary2)+tm_borders()
#Use "fisher" instead of "jenks" for larger data sets




# devide classification1 to urban and rural, and combine it with classification
classification2_df = st_drop_geometry(classification2)
selected_columns <- c("LSOA01CD", "GlobalID", "long","lat","class")
classification2_df <- classification2_df %>% 
  select(all_of(selected_columns))
#combine it with 2SFCA
lsoa2_df = st_drop_geometry(lsoa2)
selected_columns1 <- c("LSOA01CD", "long","lat","num_points", "car", "charger_density", 
                       "acc_e2sfca_2")
lsoa2_df <- lsoa2_df %>% 
  select(all_of(selected_columns1))
lsoa2_df = left_join(lsoa2_df, classification2_df, by="LSOA01CD" )
lsoa2_df <- lsoa2_df %>% 
  select(c("LSOA01CD", "long.x", "lat.x", "num_points", "car", "charger_density", "acc_e2sfca_2", "GlobalID", "class"))
class2_urban <- lsoa2_df %>%
  filter(class == "urban")
class2_rural <- lsoa2_df %>%
  filter(class == "rural")
# could be useful for the regression, but not useful for the visualization.
st_write(class2_urban, "/Users/skadi/Desktop/dissertation/all data/ML/class2_urban.csv")
st_write(class2_rural, "/Users/skadi/Desktop/dissertation/all data/ML/class2_rural.csv")



# boundary3 ---------------------------------------------------------------
charger3 = charger3[, c("chargeDevi", "connector1ChargeMethod", "geometry")]
charger3$capacity<-4
charger3[which(charger3$connector1ChargeMethod=="DC"), "capacity"]=48
charger3$capacity2 = ifelse(charger3$capacity == 4, "blue", "red")

tm_shape(classification3)+tm_borders(alpha=0.2)+tm_fill(col = "class", style = "jenks", palette = "Blues")+
  tm_shape(boundary3)+tm_borders()+
  tm_shape(charger3)+tm_dots(col="capacity2", size = 0.001)+
  tm_scale_bar(position = c("left", "bottom"))

#the number of chargers at lsoa level
joined3 <- st_join(charger3, lsoa3, join = st_within)
counts.sf3 <- summarize(num_points = n(),group_by(joined3, LSOA01CD))
counts3= st_drop_geometry(counts.sf3)
lsoa3 = left_join(lsoa3, counts3, by="LSOA01CD" )
lsoa3[is.na(lsoa3$num_points), "num_points"] = 0

tm_shape(lsoa3)+ 
  tm_fill(col="num_points", style="jenks", n=7, palette ="BuGn", title="Number of chargers" , alpha=.7)+
  tm_layout(main.title = "Number of EV chargers per LSOA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary3)+tm_borders()

#The density of charger at lsoa level
lsoa3$charger_density = lsoa3$num_points/ lsoa3$car
hist(lsoa3$charger_density)
lsoa3[is.na(lsoa3$charger_density), "charger_density"] = 0
tm_shape(lsoa3)+ 
  tm_fill(col="charger_density", style="fisher", n=7, palette ="BuPu", alpha=0.7, title="Charger density_charger per car or van" )+
  tm_layout(main.title = "Charger density_charger per car or van",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary3)+tm_borders()

#accessibility
di = 2500
#e.g., the distance between centroid of lsoa and charger is 350 metre, di should be no greater than d0.
d0 = d = 2500
#d0 is the radius of buffer
di0 = (di/d)^2
decay_index = (exp(-0.5*di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#note: try specifying di as 0, 100, 300, and 400 and see how decay_index change.(sensitivity analysis)

#step 1: supply to demand ratio
lsoa3.centroids <- st_centroid(lsoa3)
lsoa3.buff <- st_buffer(lsoa3.centroids, dist = d)
charger3.buff  <- st_buffer(charger3, dist = d)
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa3.centroids)+tm_dots()+
  tm_shape(lsoa3.buff)+tm_borders(col="red", alpha=.4)+ 
  tm_shape(lsoa3)+tm_borders()

#join lsoa centroids  to charger buffer.
join2_3 = st_join(lsoa3.centroids, charger3.buff)
#Which LSOA centroids fall within which charger buffer?
#pay attention to the order, points should be in the front, 
#so that we can see how many losa are there in one charger's buffer services.
join2_3_df = st_drop_geometry(join2_3)
nrow(join2_3_df) 
#11401
#To calculate decay index, we need to calculate distance first.
#First, we need to get rid of rows with NA in join2_3_df.
#The row with NA are those losa whose centroids are not covered by any charger buffers.
join2_3_df.s = na.omit(join2_3_df)
nrow(join2_3_df.s) 
#2286
#Point1 is composed of LONG, LAT, which are coordinates for LSOA centroid.
point1_3<-as.data.frame (join2_3_df.s$long)
point1_3$LAT<-join2_3_df.s$lat
colnames(point1_3)[1] = 'LONG'
#Point 2 is composed of longtitude, latitude, which are are coordinates for chargers.
point2_3<-as.data.frame (join2_3_df.s$long.1)
colnames(point2_3)[1] = 'longitude'
point2_3$latitude<-join2_3_df.s$lat.1
#to get the distance between point 1 and point 2, we use pointDistance {raster} function.
dist1_3 =  pointDistance(point1_3, point2_3, lonlat=TRUE) 
join2_3_df.s$distance = dist1_3
#Calculate decay index based on distance
join2_3_df.s$di0 = ifelse( join2_3_df.s$distance>d, 0, (join2_3_df.s$distance/d)^2)
join2_3_df.s$decay_index = (exp(-0.5*join2_3_df.s$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decay demand factored by decay index
join2_3_df.s$decay_demand = join2_3_df.s$decay_index * join2_3_df.s$car
#Summarise demand
demand1_3 = summarise(demand1_3 = sum(decay_demand), group_by(join2_3_df.s,chargeDevi) )
### why demand1 contains na???
charger3 = left_join(charger3 , demand1_3, by="chargeDevi") 
#Calculate supply to demand ratio
charger3$sd_ratio2 <- charger3$capacity / charger3$demand1_3
###so that sd_ratio2 still contains na, should i get rid of them???

#step 2: aggregate sd ratio
join3_3=st_join(charger3, lsoa3.buff)
join3_3_df<-st_drop_geometry(join3_3)
#Calculate the distance between lsoa centroids and charger points.
point3_3 <- as.data.frame (join3_3_df$long)
point3_3$LAT<-join3_3_df$lat
colnames(point3_3)[1] = 'LONG'
point4_3<-as.data.frame (join3_3_df$long.1)
point4_3$latitude<-join3_3_df$lat.1
colnames(point4_3)[1] = 'longitude'
dist2_3 <-  pointDistance(point3_3, point4_3, lonlat=TRUE)
join3_3_df$distance <- dist2_3
#Calculate decay index first based on distance
join3_3_df$di0 = ifelse( join3_3_df$distance>d, 0, (join3_3_df$distance/d)^2)
join3_3_df$decay_index = (exp(-0.5*join3_3_df$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decayed sd
join3_3_df$decay_service = join3_3_df$decay_index * join3_3_df$sd_ratio2
#Aggregate decay services at LSOA level
acc_e2sfca_3 = summarise(acc_e2sfca_3=sum(decay_service), group_by(join3_3_df, LSOA01CD) ) 
lsoa3 = left_join(lsoa3,acc_e2sfca_3, by="LSOA01CD")
lsoa3$acc_e2sfca_3[is.na(lsoa3$acc_e2sfca_3)] <- 0
#Visualize accessibility
tmap_mode('view')
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa3)+ 
  tm_fill(col="acc_e2sfca_3", style="jenks", n=7, palette ="BuGn", title="Accessibility_E2SFCA" , alpha=.7)+
  tm_layout(main.title = "Accessibility of EV chargers using E2SFCA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary3)+tm_borders()




# devide classification1 to urban and rural, and combine it with classification
classification3_df = st_drop_geometry(classification3)
selected_columns <- c("LSOA01CD", "GlobalID", "long","lat","class")
classification3_df <- classification3_df %>% 
  select(all_of(selected_columns))

#combine it with 2SFCA
lsoa3_df = st_drop_geometry(lsoa3)
selected_columns1 <- c("LSOA01CD", "long","lat","num_points", "car", "charger_density", 
                       "acc_e2sfca_3")
lsoa3_df <- lsoa3_df %>% 
  select(all_of(selected_columns1))
lsoa3_df = left_join(lsoa3_df, classification3_df, by="LSOA01CD" )
lsoa3_df <- lsoa3_df %>% 
  select(c("LSOA01CD", "long.x", "lat.x", "num_points", "car", "charger_density", "acc_e2sfca_3", "GlobalID", "class"))
class3_urban <- lsoa3_df %>%
  filter(class == "urban")
class3_rural <- lsoa3_df %>%
  filter(class == "rural")
# could be useful for the regression, but not useful for the visualization.
st_write(class3_urban, "/Users/skadi/Desktop/dissertation/all data/ML/class3_urban.csv")
st_write(class3_rural, "/Users/skadi/Desktop/dissertation/all data/ML/class3_rural.csv")



# boundary4 ---------------------------------------------------------------
charger4 = charger4[, c("chargeDevi", "connector1ChargeMethod", "geometry")]
charger4$capacity<-4
charger4[which(charger4$connector1ChargeMethod=="DC"), "capacity"]=48
charger4$capacity2 = ifelse(charger4$capacity == 4, "blue", "red")

tm_shape(classification4)+tm_borders(alpha=0.2)+tm_fill(col = "class", style = "jenks", palette = "Blues")+
  tm_shape(boundary4)+tm_borders()+
  tm_shape(charger4)+tm_dots(col="capacity2", size = 0.001)+
  tm_scale_bar(position = c("left", "bottom"))

#the number of chargers at lsoa level
joined4 <- st_join(charger4, lsoa4, join = st_within)
counts.sf4 <- summarize(num_points = n(),group_by(joined4, LSOA01CD))
counts4= st_drop_geometry(counts.sf4)
lsoa4 = left_join(lsoa4, counts4, by="LSOA01CD" )
lsoa4[is.na(lsoa4$num_points), "num_points"] = 0

tm_shape(lsoa4)+ 
  tm_fill(col="num_points", style="jenks", n=7, palette ="BuGn", title="Number of chargers" , alpha=.7)+
  tm_layout(main.title = "Number of EV chargers per LSOA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary4)+tm_borders()

#The density of charger at lsoa level
lsoa4$charger_density = lsoa4$num_points/ lsoa4$car
hist(lsoa4$charger_density)
lsoa4[is.na(lsoa4$charger_density), "charger_density"] = 0
tm_shape(lsoa4)+ 
  tm_fill(col="charger_density", style="fisher", n=7, palette ="BuPu", alpha=0.7, title="Charger density_charger per car or van" )+
  tm_layout(main.title = "Charger density_charger per car or van",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary4)+tm_borders()


#accessibility
di = 350 
#e.g., the distance between centroid of lsoa and charger is 350 metre, di should be no greater than d0.
d0 = d = 400
#d0 is the radius of buffer
di0 = (di/d)^2
decay_index = (exp(-0.5*di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#note: try specifying di as 0, 100, 300, and 400 and see how decay_index change.(sensitivity analysis)

#step 1: supply to demand ratio
lsoa4.centroids <- st_centroid(lsoa4)
lsoa4.buff <- st_buffer(lsoa4.centroids, dist = d)
charger4.buff  <- st_buffer(charger4, dist = d)
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa4.centroids)+tm_dots()+
  tm_shape(lsoa4.buff)+tm_borders(col="red", alpha=.4)+ 
  tm_shape(lsoa4)+tm_borders()

#join lsoa centroids  to charger buffer.
join2_4 = st_join(lsoa4.centroids, charger4.buff)
#Which LSOA centroids fall within which charger buffer?
#pay attention to the order, points should be in the front, 
#so that we can see how many losa are there in one charger's buffer services.
join2_4_df = st_drop_geometry(join2_4)
nrow(join2_4_df) 
#10953
#To calculate decay index, we need to calculate distance first.
#First, we need to get rid of rows with NA in join2_4_df.
#The row with NA are those losa whose centroids are not covered by any charger buffers.
join2_4_df.s = na.omit(join2_4_df)
nrow(join2_4_df.s) 
#2673
#Point1 is composed of LONG, LAT, which are coordinates for LSOA centroid.
point1_4<-as.data.frame (join2_4_df.s$long)
point1_4$LAT<-join2_4_df.s$lat
colnames(point1_4)[1] = 'LONG'
#Point 2 is composed of longtitude, latitude, which are are coordinates for chargers.
point2_4<-as.data.frame (join2_4_df.s$long.1)
colnames(point2_4)[1] = 'longitude'
point2_4$latitude<-join2_4_df.s$lat.1
#to get the distance between point 1 and point 2, we use pointDistance {raster} function.
dist1_4 =  pointDistance(point1_4, point2_4, lonlat=TRUE) 
join2_4_df.s$distance = dist1_4
#Calculate decay index based on distance
join2_4_df.s$di0 = ifelse( join2_4_df.s$distance>d, 0, (join2_4_df.s$distance/d)^2)
join2_4_df.s$decay_index = (exp(-0.5*join2_4_df.s$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decay demand factored by decay index
join2_4_df.s$decay_demand = join2_4_df.s$decay_index * join2_4_df.s$car
#Summarise demand
demand1_4 = summarise(demand1_4 = sum(decay_demand), group_by(join2_4_df.s,chargeDevi) )
### why demand1 contains na???
charger4 = left_join(charger4 , demand1_4, by="chargeDevi") 
#Calculate supply to demand ratio
charger4$sd_ratio2 <- charger4$capacity / charger4$demand1_4
###so that sd_ratio2 still contains na, should i get rid of them???

#step 2: aggregate sd ratio
join3_4=st_join(charger4, lsoa4.buff)
join3_4_df<-st_drop_geometry(join3_4)
#Calculate the distance between lsoa centroids and charger points.
point3_4 <- as.data.frame (join3_4_df$long)
point3_4$LAT<-join3_4_df$lat
colnames(point3_4)[1] = 'LONG'
point4_4<-as.data.frame (join3_4_df$long.1)
point4_4$latitude<-join3_4_df$lat.1
colnames(point4_4)[1] = 'longitude'
dist2_4 <-  pointDistance(point3_4, point4_4, lonlat=TRUE)
join3_4_df$distance <- dist2_4
#Calculate decay index first based on distance
join3_4_df$di0 = ifelse( join3_4_df$distance>d, 0, (join3_4_df$distance/d)^2)
join3_4_df$decay_index = (exp(-0.5*join3_4_df$di0) - exp(-0.5) ) / (1-exp(-0.5)) 
#Calculate decayed sd
join3_4_df$decay_service = join3_4_df$decay_index * join3_4_df$sd_ratio2
#Aggregate decay services at LSOA level
acc_e2sfca_4 = summarise(acc_e2sfca_4=sum(decay_service), group_by(join3_4_df, LSOA01CD) ) 
lsoa4 = left_join(lsoa4,acc_e2sfca_4, by="LSOA01CD")
lsoa4$acc_e2sfca_4[is.na(lsoa4$acc_e2sfca_4)] <- 0
#Visualize accessibility
tmap_mode('view')
tmap_options(check.and.fix = TRUE)
tm_shape(lsoa4)+ 
  tm_fill(col="acc_e2sfca_4", style="jenks", n=7, palette ="BuGn", title="Accessibility_E2SFCA" , alpha=.7)+
  tm_layout(main.title = "Accessibility of EV chargers using E2SFCA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary4)+tm_borders()




# devide classification1 to urban and rural, and combine it with classification
classification4_df = st_drop_geometry(classification4)
selected_columns <- c("LSOA01CD", "GlobalID", "long","lat","class")
classification4_df <- classification4_df %>% 
  select(all_of(selected_columns))

#combine it with 2SFCA
lsoa4_df = st_drop_geometry(lsoa4)
selected_columns1 <- c("LSOA01CD", "long","lat","num_points", "car", "charger_density", 
                       "acc_e2sfca_4")
lsoa4_df <- lsoa4_df %>% 
  select(all_of(selected_columns1))

lsoa4_df = left_join(lsoa4_df, classification4_df, by="LSOA01CD" )
lsoa4_df <- lsoa4_df %>% 
  select(c("LSOA01CD", "long.x", "lat.x", "num_points", "car", "charger_density", "acc_e2sfca_4", "GlobalID", "class"))
class4_urban <- lsoa4_df %>%
  filter(class == "urban")
class4_rural <- lsoa4_df %>%
  filter(class == "rural")
# could be useful for the regression, but not useful for the visualization.
st_write(class4_urban, "/Users/skadi/Desktop/dissertation/all data/ML/class4_urban.csv")
st_write(class4_rural, "/Users/skadi/Desktop/dissertation/all data/ML/class4_rural.csv")



