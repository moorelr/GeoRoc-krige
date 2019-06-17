# moorelr/georoc-krige is licensed under The MIT License
# Copyright 2019 Lowell R. Moore

# Run this first if using RStudio 
georoc_url <- ""
if(TRUE){
  # Input URL for GeoRoc file
  if(interactive()){
    georoc_url <- readline("Input URL for precompilled GeoRoc data:")
  } else{
    cat("Input URL for precompilled GeoRoc data:")
    georoc_url <- readLines("stdin", n = 1)
  }
}

if(!grepl(pattern = "http://georoc.mpch-mainz.gwdg.de", georoc_url)){
  print("No GeoRoc URL was entered."); Sys.sleep(0.5)
  print("Running demo with Cascades precompiled file..."); Sys.sleep(1)
  georoc_url <- "http://georoc.mpch-mainz.gwdg.de/georoc/Csv_Downloads/Convergent_Margins_comp/CASCADES.csv"
}

# Import georoc data from URL
print("Downloading GeoRoc data...")
Import <- read.csv(url(georoc_url), stringsAsFactors = FALSE, na.strings = c(""))

# Remove any commas from the parsed file
for(i in 1:nrow(Import)){
  for(j in 1:ncol(Import)){
    if(grepl(x = Import[i,j], pattern = ",")){
      Import[i,j] <- gsub(x = Import[i,j], pattern = ",", replacement = "")
    }
  }
}

print("Done! (Thanks Bärbel!)"); Sys.sleep(0.5)


  
# Separate references and data into separate variables
print("Parsing references...")
ref_start <- which(Import$CITATIONS == "References:")
Import_refs <- Import$CITATIONS[(ref_start+1):length(Import$CITATIONS)]
Import <- Import[1:ref_start,]

# Remove duplicate references (just in case)
Import_refs <- unique(Import_refs)
# Parse reference list to extract year and reference code
ref_year <- numeric(0)
ref_number <- numeric(0)
for(i in 1:length(Import_refs)){
  date_start <- gregexpr(pattern = " \\[", text = Import_refs[i])[[1]][1]
  ref_year[i] <- substr(Import_refs[i], start = date_start+2, stop = date_start + 5)
  
  num_start <- gregexpr(pattern = "\\[", text = Import_refs[i])[[1]][1]
  num_stop <- gregexpr(pattern = "\\]", text = Import_refs[i])[[1]][1]
  ref_number[i] <- substr(Import_refs[i], start = num_start, stop = num_stop)
}

# flag data collected pre-1990
# Debug: i <- 11; j <- 2
old_papers <- which(as.numeric(ref_year) < 1990)
is_old <- rep(FALSE, nrow(Import))
for(i in old_papers){
  num_i <- ref_number[i]
  for(j in 1:length(Import$CITATIONS)){
    if(grepl(num_i, Import$CITATIONS[j], fixed = TRUE)){
      is_old[j] <- TRUE
    }
  }
}
print("Done parsing references"); Sys.sleep(0.5)

# Calculate FeO_total
print("Standardizing Fe...")
#colnames(Import)
FeO_total <- numeric(0)
# Debug : i <- 11
for(i in 1:nrow(Import)){
  if(is.na(Import$FEOT.WT..[i]) & is.na(Import$FEO.WT..[i]) & is.na(Import$FE2O3.WT..[i])){
    FeO_total[i] <- NA
    next
  }
  else if(!is.na(Import$FEOT.WT..[i])){
    FeO_total[i] <- Import$FEOT.WT..[i]#1
    next
  }
  else if(is.na(Import$FEOT.WT..[i]) & !is.na(Import$FEO.WT..[i]) & !is.na(Import$FE2O3.WT..[i])){
    FeO_total[i] <- Import$FEO.WT..[i] + (0.8998*Import$FE2O3.WT..[i])#2
    next
  }
  else if(is.na(Import$FEOT.WT..[i]) & !is.na(Import$FEO.WT..[i]) & is.na(Import$FE2O3.WT..[i])){
    FeO_total[i] <- Import$FEO.WT..[i]#3
    next
  }
  else if(is.na(Import$FEOT.WT..[i]) & is.na(Import$FEO.WT..[i]) & !is.na(Import$FE2O3.WT..[i])){
    FeO_total[i] <- 0.8998*Import$FE2O3.WT..[i]#4
    next
  }
}
Import$FEOT.WT.. <- FeO_total

# Debug: i <- round(runif(1, 1, nrow(Import)), 0)
print("Standardizing other trace elements...")
for(i in 1:nrow(Import)){
  if(is.na(Import$CR2O3.WT..[i]) & !is.na(Import$CR.PPM.[i])){
    Import$CR2O3.WT..[i] <- 1.462*Import$CR.PPM.[i]*10^-4
  }
  if(is.na(Import$TIO2.WT..[i]) & !is.na(Import$TI.PPM.[i])){
    Import$TIO2.WT..[i] <- 1.668*Import$TI.PPM.[i]*10^-4
  }
  if(is.na(Import$MNO.WT..[i]) & !is.na(Import$MN.PPM.[i])){
    Import$MNO.WT..[i] <- 1.291*Import$MN.PPM.[i]*10^-4
  }
  if(is.na(Import$NIO.WT..[i]) & !is.na(Import$NI.PPM.[i])){
    Import$NIO.WT..[i] <- 1.273*Import$NI.PPM.[i]*10^-4
  }
  if(is.na(Import$K2O.WT..[i]) & !is.na(Import$K.PPM.[i])){
    Import$K2O.WT..[i] <- 1.205*Import$K.PPM.[i]*10^-4
  }
  if(is.na(Import$P2O5.WT..[i]) & !is.na(Import$P.PPM.[i])){
    Import$P2O5.WT..[i] <- 2.292*Import$P.PPM.[i]*10^-4
  }
  
  # Also standardize to PPM for Ti and K
  if(!is.na(Import$K2O.WT..[i]) & is.na(Import$K.PPM.[i])){
    Import$K.PPM.[i] <- Import$K2O.WT..[i]/(1.205*10^-4)
  }
  if(!is.na(Import$TIO2.WT..[i]) & is.na(Import$TI.PPM.[i])){
    Import$TI.PPM.[i] <- Import$TIO2.WT..[i]/(1.668*10^-4)
  }
  if(!is.na(Import$P2O5.WT..[i]) & is.na(Import$P.PPM.[i])){
    Import$P.PPM.[i] <- Import$P2O5.WT..[i]/(2.292*10^-4)
  }
}

# Calculate totals
print("Calculating totals...")
#colnames(Import)
Totals <- numeric(0)
for(i in 1:nrow(Import)){
  majors_i <- c(Import$SIO2.WT..[i]
                , Import$TIO2.WT..[i]
                , Import$AL2O3.WT..[i]
                , Import$CR2O3.WT..[i]
                , FeO_total[i]
                , Import$CAO.WT..[i]
                , Import$MGO.WT..[i]
                , Import$MNO.WT..[i]
                , Import$NIO.WT..[i]
                , Import$K2O.WT..[i]
                , Import$NA2O.WT..[i]
                , Import$P2O5.WT..[i]
                #, Import$H2O.WT..[i]
                #, Import$CO2.WT..[i]
                #, Import$F.WT..[i]
                #, Import$CL.WT..[i]
                #, Import$SO2.WT..[i]
  )
  majors_i <- majors_i[!is.na(majors_i)]
  Totals[i] <- sum(majors_i)
} # hist(Totals)
print("Done!"); Sys.sleep(0.5)

# Apply other filters
print("No filters set."); Sys.sleep(0.5)

# Save parsed GeoRoc files
print("Saving parsed georoc files...")
write.csv(x = Import, file = "Georoc_parsed.csv"
            , quote = FALSE, row.names = FALSE)
write.csv(x = Import_refs, file = "Georoc_refs_parsed.csv"
            , quote = FALSE, row.names = FALSE)
print("Done!"); Sys.sleep(0.5)

# Check for/load required packages
print("Checking for required R packages...")
required_packages <- c("gstat", "geoR", "maptools")
pkg_list <- rownames(installed.packages())
for(pkg in required_packages){
  installed <- pkg %in% pkg_list
  
  # If the package is installed, load it to test
  if(installed){
    library(pkg, character.only = TRUE)
  }
  
  # If the package is not installed, check for user permission and install
  if(!installed){
    # Check for user permission
    user_permission <- readline(paste(
      "Would you like to install the following package: ", pkg
      , "(y or n)" 
    ))
    if(user_permission == "y"){
      install.packages(pkg)
    }
  }
}
# Create a subfolder to save output figures
if(!dir.exists("Figures")){
  dir.create("Figures")
}

print("Done!")
Sys.sleep(2)
