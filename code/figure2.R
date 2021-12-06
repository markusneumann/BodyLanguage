library(data.table)
library(ggplot2)
source("convex_hull_handmovement_functions.R")

# Read in the candidate landmarks
f <- fread("../results/combined_fr_pd/face_recognition_2020_PRES_STEYER_DEBATE_STAGE_60_landmarks.csv")
# Read in a still image from the video
img <- png::readPNG("../data/steyer.png")

# Select 5 seconds from the video
section_x <- f$x4[80:230]
section_y <- f$y4[80:230]

# Calculate the (vertical) space crossed by Steyer's right hand
inner <- getInnerPoints(x = section_x,
                        y = section_y,
                        threshold = 0.25)

#Plot with base R
x <- seq(0,480, length.out = 100)
y <- seq(0,320, length.out = 100)

pdf('../figures/steyer.pdf', width = 16, height = 9)
#Set up the plot area
plot(NULL, ylim = rev(range(y)), xlim = range(x), type='n', main="", xlab="", ylab="", xaxt='n', yaxt='n', frame.plot=F)
#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
points(x=inner$x, y=inner$y, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
points(x=section_x, y=section_y, col = "orange", pch = 16, cex = 1.5)
dev.off()
# The figure for the paper was cropped manually with paint.net

# With low alpha orange points
pdf('../figures/steyer_low_alpha_points.pdf', width = 16, height = 9)
#Set up the plot area
plot(NULL, ylim = rev(range(y)), xlim = range(x), type='n', main="", xlab="", ylab="", xaxt='n', yaxt='n', frame.plot=F)
#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
points(x=inner$x, y=inner$y, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.6))
points(x=section_x, y=section_y, col = rgb(red = 1, green = 0.7, blue = 0, alpha = 0.2), pch = 16, cex = 1.5)
dev.off()
# Cropping the figure and adding the arrows was done manually with paint.net
