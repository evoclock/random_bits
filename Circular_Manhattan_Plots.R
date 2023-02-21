# SNP density plot with CMplot 
# Circular Manhattan plots mainly

library(CMplot)

dat1 = data(pig60K) #calculated p-values by MLM
dat2 = data(cattle50K)   #calculated SNP effects by rrblup

# The function saves to file with format and dpi arguments
CMplot(pig60K, 
       type = "p",
       plot.type = "d",
       bin.size = 1e6,
       chr.den.col = c("darkgreen", "yellow", "red"),
       file = "jpg",
       memo = "",
       dpi = 300,
       file.output = TRUE,
       verbose = TRUE,
       width = 9,
       height = 6)

# users can personally set the windowsize and the min/max of legend by:
# bin.size=1e6
# bin.range=c(min, max)
# memo: add a character to the output file name
# chr.labels: change the chromosome names

# Circulat Manhattan plot
CMplot(pig60K,
       type = "p",
       plot.type = "c",
       chr.labels = paste("Chr",c(1:18,"X","Y"), sep=""), 
       r=0.4, 
       cir.legend = TRUE,
       outward = FALSE,
       cir.legend.col = "black",
       cir.chr.h = 1.3,
       chr.den.col = "black", 
       file = "jpg",
       memo = "",
       dpi = 300,
       file.output = TRUE,
       verbose = TRUE,
       width = 10,
       height = 10)

# Circular Manhattan plot 2
CMplot(pig60K,
       type = "p",
       plot.type = "c",
       r = 0.4,
       col = c("grey30","grey60"),
       chr.labels = paste("Chr", c(1:18,"X","Y"), sep=""),
       threshold = c(1e-6,1e-4),
       cir.chr.h = 1.5,
       amplify = TRUE,
       threshold.lty = c(1,2),
       threshold.col = c("red", "blue"),
       signal.line = 1,
       signal.col = c("red","green"),
       chr.den.col = c("darkgreen","yellow","red"),
       bin.size = 1e6,
       outward = FALSE,
       file = "jpg",
       memo = "",
       dpi = 300,
       file.output = TRUE,
       verbose = TRUE,
       width = 10,
       height = 10)

#Note: 
# 1. if signal.line=NULL, the lines that crosse circles won't be added.
# 2. if the length of parameter 'chr.den.col' is not equal to 1, 
# SNP density that counts  the number of SNP within given size('bin.size') 
# will be plotted around the circle.

# Genomic selection/prediction

CMplot(cattle50K,
       type = "p",
       plot.type = "c",
       LOG10 = FALSE,
       outward = TRUE,
       col = matrix(c("#4DAF4A",NA,NA,"dodgerblue4",
                               "deepskyblue",NA,"dodgerblue1", 
                               "olivedrab3","darkgoldenrod1"), 
                               nrow = 3, byrow = TRUE), 
       chr.labels = paste("Chr", c(1:29), sep=""), 
       threshold = NULL,
       r = 1.2,
       cir.chr.h = 1.5,
       cir.legend.cex = 0.5,
       cir.band = 1,
       file = "jpg", 
       memo = "",
       dpi = 300,
       chr.den.col = "black",
       file.output = TRUE,
       verbose = TRUE,
       width = 10,
       height = 10)

#Note: parameter 'col' can be either vector or matrix, if a matrix, 
# each trait can be plotted in different colors.
