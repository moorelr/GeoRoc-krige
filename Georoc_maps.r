# Import filtered georoc data
print("Importing data...")
Import <- read.csv("Georoc_parsed.csv", stringsAsFactors = FALSE)

window <- c(
  quantile(x = Import$LONGITUDE.MIN, 0.01, na.rm = TRUE)
  , quantile(Import$LONGITUDE.MAX, 0.99, na.rm = TRUE)
  , quantile(Import$LATITUDE.MIN, 0.01, na.rm = TRUE)
  , quantile(Import$LATITUDE.MAX, 0.99, na.rm = TRUE)
)

library("maptools")
data(wrld_simpl)

coords <- matrix(window[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol=2)
l <- Line(coords)
ls <- Lines(list(l),ID="1")
sls <- SpatialLines(list(ls))
df <- data.frame(province="Import_area")
sldf <- SpatialLinesDataFrame(sls,df)

print("Generating world map...")
pdf("Figures/Overview_map.pdf", width = 4, height = 4, useDingbats = FALSE)
plot(wrld_simpl)
plot(sldf,add=T,col='red', lwd = 2)
dev.off()

s_points <- matrix(c(0, 0), ncol = 2)

poly_i <- Polygon(matrix(c(1, 2, 3, 4, 1, 1, 2, 3, 4, 1), ncol = 2))
poly_i <- Polygons(list(poly_i), ID = "1")
poly_list <- list(poly_i)

s_type <- rep(NA, nrow(Import))

for(i in 1:nrow(Import)){
  lon1 <- Import$LONGITUDE.MIN[i]; lon2 <- Import$LONGITUDE.MAX[i]
  lat1 <- Import$LATITUDE.MIN[i]; lat2 <- Import$LATITUDE.MAX[i]
  if(all(is.na(c(lat1, lon1, lat2, lon2)))){
      s_type[i] <- "skip"
      #print(c("skip", lat1, lon1, lat2, lon2))
      next
    }
  if(lat1 == lat2 & lon1 == lon2){
    # plot as point
    p_i <- c(lon1, lat1)
    s_points <- rbind(s_points, p_i)
    
    s_type[i] <- "point"
    #print(c("point", lat1, lon1, lat2, lon2))
    
  } else {
    # plot as polygon
    poly_i <- Polygon(matrix(c(lon1, lon1, lon2, lon2, lon1
                               , lat1, lat2, lat2, lat1, lat1)
                             , ncol = 2)
                      )
    poly_i <- Polygons(list(poly_i), ID = as.character(length(poly_list)+1))
    poly_list <- c(poly_list, poly_i)
    
    s_type[i] <- "poly"
    #print(c("poly", lat1, lon1, lat2, lon2))
  }
}

df_points <- data.frame(location = rep("GRdata", nrow(s_points)))
s_points <- SpatialPoints(s_points)
df_points <- SpatialPointsDataFrame(s_points, df_points)

poly_list <- SpatialPolygons(poly_list)
df_polys <- data.frame(location = rep("GRdata", length(poly_list)))
df_polys <- SpatialPolygonsDataFrame(poly_list, df_polys)

print("Generating sample map...")
pdf("Figures/Zoom_map.pdf", width = 7, height = 7, useDingbats = FALSE)
plot(wrld_simpl, xlim = window[1:2], ylim = window[3:4])
plot(df_points, add = TRUE, pch = 19, col = rgb(1, 0, 0, 0.2))
plot(df_polys, add = TRUE, col = rgb(1, 0, 0, 0.2))
dev.off()

print("Done!")
Sys.sleep(2)