# moorelr/georoc-krige is licensed under The MIT License
# Copyright 2019 Lowell R. Moore

print("Importing data...")

# Import libraries and data
library("geoR")
library("maptools")
data(wrld_simpl)

# Parsed georoc file
Import <- read.csv("Georoc_parsed.csv", stringsAsFactors = FALSE)

# Import existing border file if it exists
border_exists <- "border.csv" %in% list.files()
if(border_exists){
  border <- read.csv("border.csv", stringsAsFactors = FALSE)
}

# Import existing variogram model if it exists
varg_params_exist <- "varg_params.csv" %in% list.files()
if(varg_params_exist){
  varg_params <- read.csv("varg_params.csv", stringsAsFactors = FALSE)
  
  # Sorry, kind of hacky...
  rownames(varg_params) <- varg_params$X
  max_dist <- as.numeric(varg_params["max_dist", 2])
  r_nug <- as.numeric(varg_params["r_nug", 2])
  sill <- as.numeric(varg_params["sill", 2])
  rrange <- as.numeric(varg_params["rrange", 2])
  nugget <- as.numeric(varg_params["nugget", 2])
  cov_model <- varg_params["cov_model", 2]
  element <- as.numeric(varg_params["element", 2])
  log_element <- as.logical(varg_params["log_element", 2])
  krige_res <- as.numeric(varg_params["krige_res", 2])
  
  variog_bins <- seq(0, max_dist, length.out = 30)
}

# Select element for Krige
if(!varg_params_exist){
  if(interactive()){
    element <- menu(choices = colnames(Import), graphics = TRUE, title = "Select krige parameter")
    choice <- menu(choices = c("Yes", "No"), graphics = TRUE, title = "Log-transform data?")
  } else{
    print(cbind(colnames(Import)))
      cat("Choose a parameter for to krige (enter a number): ")
    element <- as.numeric(readLines("stdin",n=1))
    cat("Log-transform data? (enter y or n)")
    choice <- readLines("stdin",n=1)
  }
  
  # Choose whether data will be log transformed
  if(choice %in% c("y", 1)){
    log_element <- TRUE
  } else{
    log_element <- FALSE
  }
}

# Flag locations of data for Krige
flag_rows <- which(
  Import$LATITUDE.MIN == Import$LATITUDE.MAX
  & Import$LATITUDE.MIN == Import$LATITUDE.MAX
  & !is.na(Import[,element])
  & Import[,element] > 0
  
  #& Import$LONGITUDE.MIN < 0 # added for debugging purposes
)

# Convert to formatted dataframe (X, Y, Value)
ele.data <- data.frame(Import$LONGITUDE.MIN[flag_rows], Import$LATITUDE.MIN[flag_rows]
                      , Import[flag_rows,element]
                      )

# Filter data for collocated points using median value
print("Filtering for co-located data...")
locations <- unique(ele.data[,1:2])
toss <- numeric(0)
for(point_i in 1:nrow(locations)){
  flag_i <- which(ele.data[,1] == locations[point_i,1]
                  & ele.data[,2] == locations[point_i,2]
                  )
  if(length(flag_i) > 1){
    print(paste("Consolidating ", length(flag_i), " co-located points at X = "
                , locations[point_i,1], " and Y = ", locations[point_i,2]
                , "...", sep = ""))
    toss <- c(toss, flag_i)
    new_row <- cbind(locations[point_i,1:2], median(ele.data[flag_i, 3]))
    colnames(new_row) <- colnames(ele.data)
    ele.data <- rbind(ele.data, new_row)
  }
}
ele.data <- ele.data[-toss,]

# Assign window for plot area
window <- c(min(ele.data[,1]), max(ele.data[,1]), min(ele.data[,2]), max(ele.data[,2]))
df_points <- data.frame(location = rep("GRdata", nrow(ele.data)))
s_points <- SpatialPoints(ele.data[,1:2])
df_points <- SpatialPointsDataFrame(s_points, df_points)

# Assign border for Krige
if(!border_exists){
  if(interactive()){
    # Generate plot so the user can select a border
    plot(wrld_simpl, xlim = window[1:2], ylim = window[3:4])
    plot(df_points, add = TRUE, pch = 19, col = rgb(1, 0, 0, 0.2))
    border <- locator()
  }
  
  if(!interactive()){
    border <- data.frame(
      x = window[c(1, 2, 2, 1)], y = window[c(4, 4, 3, 3)]
    )
  }
  # Save the selected border
  write.csv(border, "border.csv", row.names = FALSE, quote = FALSE)
  
  # Save the image showing the location of the selected border
  pdf("Figures/Identify border.pdf", width = 5, height = 5, useDingbats = FALSE)
  plot(wrld_simpl, xlim = window[1:2], ylim = window[3:4])
  plot(df_points, add = TRUE, pch = 19, col = rgb(1, 0, 0, 0.2))
  coords <- cbind(border$x, border$y)
  l <- Line(coords)
  ls <- Lines(list(l),ID="1")
  sls <- SpatialLines(list(ls))
  df <- data.frame(province="Import_area")
  sldf <- SpatialLinesDataFrame(sls,df)
  plot(sldf, add = T, col = "gray")
  dev.off()
}

# Log transform data if necessary
if(log_element){ele.data[,3] <- log10(ele.data[,3])}

# Convert to datatype "geodata"
geo.ele <- as.geodata(ele.data)

# Summary plot
pdf("Figures/Geostats summary plot.pdf", width = 5, height = 5, useDingbats = FALSE)
plot.geodata(geo.ele)
dev.off()

# Import varg params
if(!varg_params_exist){
  # Generate an initial guess for spatial correlation model
  max_dist <- 0.5*((max(ele.data[,1]) - min(ele.data[,1])) + (max(ele.data[,2]) - min(ele.data[,2])))
  r_nug <- 0.3
  sill <- 0.7*var(ele.data[,3])
  rrange <- max_dist/3
  nugget <- r_nug*var(ele.data[,3])
  cov_model <- "spherical"
  
  # parameter controlling grid resolution for krige    <--- should put this in settings file with varg params
  # Fast = 10ish; Pretty = 100ish
  krige_res <- 10
  
  varg_params <- rbind(max_dist, r_nug, sill, rrange, nugget, cov_model, element, log_element, krige_res)
  write.csv(varg_params, "varg_params.csv", quote = FALSE)
  
  variog_bins <- seq(0, max_dist, length.out = 30)
}

# Plot experimental and model semivariagram
pdf("Figures/Semivariogram.pdf", width = 6, height = 5, useDingbats = FALSE)
lin_col <- "black"
plot(variog(geo.ele, uvec = variog_bins))
lines.variomodel(cov.model = cov_model, cov.pars = c(sill, rrange)
                 , nugget = nugget, max.dist = max(variog_bins)
                 , lwd = 2, col = lin_col)
dev.off()

# Directional
if(FALSE){ # Can't run this because of collocated data!
  geo.ele.var4 <- variog4(geo.ele, omnidirectional = T, uvec = variog_bins)
  plot(geo.ele.var4, omni = T) # Not sure if I have a typo here
  lines.variomodel(cov.model = cov_model, cov.pars = c(sill, rrange)
                   , nugget = nugget, max.dist = 2
                   , lwd = 2, col = lin_col)
}

# Assign grid for kriging
nodes <- expand.grid(seq(min(border$x), max(border$x), length.out = 5*krige_res) # grid points in x-direction
                     , seq(min(border$y), max(border$y), length.out = 6*krige_res)  # grid points in y-direction
)

# Get user permission before krige calculation
if(interactive()){
  krige_choice <- menu(choices = c("Yes", "No"), graphics = TRUE, title = "Do krige calculation?")
} else{
  cat("Fit spatial prediction model (krige)? (enter y or n)")
  krige_choice <- readLines("stdin",n=1)
}
# Choose whether data will be log transformed
if(krige_choice %in% c("y", 1)){
  do_krige <- TRUE
} else{
  do_krige <- FALSE
}


if(do_krige){
  
  # Create an object for simple kriging
  # Use m0 = "av" for simple kriging
  # Use m0 = "ok" for ordinary kriging
  kcSK <- ksline(geo.ele, m0 = "av", loc = nodes
                 , nwin = "full" # Use all data in moving window
                 , cov.model = cov_model
                 , cov.pars = c(sill, rrange) # Sill for covariance model
                 , nugget = nugget
                 , border = border
  )
  
  # Draw krige model figure
  pdf("Figures/GeoRoc krige.pdf", useDingbats = FALSE
      , width = 6, height = 6
  )
  #image(kcSK)
  #legend.krige(x.leg=c(-0.5, -0.25), y.leg = c(2, 5.75), vert = TRUE, kcSK$pred)
  
  plot(wrld_simpl, xlim = window[1:2], ylim = window[3:4], main = colnames(Import)[element])
  image(kcSK, add = T)
  plot(df_points, add = TRUE, pch = 19, col = rgb(0, 0, 0, 0.3))
  plot(wrld_simpl, add = T, lwd = 2)
  
  dev.off()
}
