# moorelr/georoc-krige is licensed under The MIT License
# Copyright 2019 Lowell R. Moore

print("Importing parsed GeoRoc file")
Import <- read.csv("Georoc_parsed.csv", stringsAsFactors = FALSE)

# Plot figures involving major elements
majors <- c("SIO2.WT.."
              , "TIO2.WT.."
              , "AL2O3.WT.."
              , "CR2O3.WT.."
              , "FEOT.WT.."
              , "CAO.WT.."
              , "MGO.WT.."
              , "MNO.WT.."
              , "NIO.WT.."
              , "K2O.WT.."
              , "NA2O.WT.."
              , "P2O5.WT.."
)

print("Generating major element histograms...")
pdf("Figures/Major elements hist.pdf", width = 6, height = 8, useDingbats = FALSE)
par(mfrow = c(4, 3))
for(ele in majors){
  hist(Import[,ele], main = ele, xlab = ele)
}
par(mfrow = c(1, 1))
dev.off()

print("Generating major element scatter plots...")
pdf("Figures/Major elements scatter.pdf", width = 6, height = 8, useDingbats = FALSE)
xs <- Import$MGO.WT..
par(mfrow = c(4, 3))
for(ele in majors[-7]){
  ys <- Import[,ele]
  plot(xs, ys, main = "", xlab = "MGO.WT..", ylab = ele, pch = 19, col = rgb(0, 0, 0, 0.15))
}
par(mfrow = c(1, 1))
dev.off()

print("Generating trace element histograms...")
traces <- 72:146
pdf("Figures/Trace elements hist.pdf", width = 8, height = 11, useDingbats = FALSE)
par(mfrow = c(4, 3))
for(i in traces){
  if(all(is.na(Import[,i]))){next}
  hist(Import[,i], xlab = colnames(Import)[i], main = colnames(Import)[i])
}
par(mfrow = c(1, 1))
dev.off()

print("Generating isotope histograms...")
isotopes <- 147:170
pdf("Figures/Isotopes hist.pdf", width = 8, height = 11, useDingbats = FALSE)
par(mfrow = c(4, 3))
for(i in isotopes){
  if(all(is.na(Import[,i]))){next}
  hist(Import[,i], xlab = colnames(Import)[i], main = colnames(Import)[i])
}
par(mfrow = c(1, 1))
dev.off()

print("Done!")
Sys.sleep(2)
