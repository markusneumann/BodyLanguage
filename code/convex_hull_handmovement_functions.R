library(ks) #Hpi
library(reshape2)
library(phonR)

#return x and y values above the threshold inside a density plot
getInnerPoints <- function(x, y, threshold = .25){
  
  fData <- data.frame(x = x, y = y)
  # Select bandwidth
  # If x and y are effectively the same values throughout, this can lead to an error
  H_hpi <- ks::Hpi(x = fData[, c(2, 1)], pilot = "samse", pre = "scale", binned = T)
  # Compute 2d kde
  k <- ks::kde(x = fData[, c(2, 1)], H = H_hpi, binned = T)
  #melt into long format
  mat.melted <- reshape2::melt(k$estimate)
  names(mat.melted) <- c("x", "y", "density")
  #add two more colums to preserve the axes units
  mat.melted$y <- rep(k$eval.points[[1]], times = nrow(k$estimate))
  mat.melted$x <- rep(k$eval.points[[2]], each = nrow(k$estimate))
  #normalize
  mat.melted$density <- mat.melted$density/max(mat.melted$density)
  #keep only values that are at least 25% of max
  mat.melted$inside_hull <- mat.melted$density>threshold
  
  mat.melted <- mat.melted[mat.melted$inside_hull==T,]
  mat.melted <- subset(mat.melted, select = c(x,y))
  
  return(mat.melted)
  
}

# Not used for now
getOuterHullPoints <- function(x, y){
  
  hull_rows <- chull(x, y)
  
  #get the points, and in the right order (important so the circle can be done right)
  x_hull <- x[hull_rows]
  y_hull <- y[hull_rows]
  
  #add an artificial last point identical to the first one, so ggplot completes the cricle
  hullpoints <- data.frame(x = x_hull, y = y_hull)
  hullpoints <- rbind(hullpoints, hullpoints[1,])
  
  return(hullpoints)
  
}

landmark_area <- function(landmarks_x, landmarks_y){
  
  mask <- is.na(landmarks_x)  
  # Number chunks of non-NA blocks in the vector
  # Using run length encoding
  r <- rle(as.numeric(mask))
  indices <- rep(seq(from = 0, length = length(r$lengths)), times = r$lengths)
  non_na_indices <- unique(indices[mask==F])
  
  df_fill <- rep(NA, length(non_na_indices))
  df_area <- data.frame(area = df_fill,
                        mean_horizontal_length = df_fill,
                        mean_vertical_length = df_fill,
                        section_length = df_fill,
                        section_start = df_fill,
                        section_end = df_fill)
  
  for(i in 1:length(non_na_indices)){
    
    # Get the current section and its length
    ind <- non_na_indices[i]
    section_indices <- which(indices==ind)
    df_area$section_length[i] <- length(section_indices)
    df_area$section_start[i] <- min(section_indices)
    df_area$section_end[i] <- max(section_indices)
    
    # If the section is at least 1 second (24 frames) long, continue
    if(df_area$section_length[i]>24){
      
      section_x <- landmarks_x[section_indices]
      section_y <- landmarks_y[section_indices]
      
      # Inner points of the convex hull
      inner <- getInnerPoints(x = section_x,
                              y = section_y,
                              threshold = 0.25) #0.25 default, values between 0.2 and 0.3 get about the same, above and below that it is worse
      
      # Measure all vertical bins
      uix <- unique(inner$x)
      bindists_x_l <- list()
      for(j in 1:length(uix)){
        low_high <- range(inner$y[inner$x == uix[j]])
        if(length(low_high)>1){
          bindists_x_l[[j]] <- low_high[2]-low_high[1]
        }
      }
      df_area$mean_vertical_length[i] <- mean(unlist(bindists_x_l))
      
      # Measure all horizontal bins
      uiy <- unique(inner$y)
      bindists_y_l <- list()
      for(j in 1:length(uiy)){
        low_high <- range(inner$x[inner$y == uiy[j]])
        if(length(low_high)>1){
          bindists_y_l[[j]] <- low_high[2]-low_high[1]
        }
      }
      df_area$mean_horizontal_length[i] <- mean(unlist(bindists_y_l))
      
      # Area of the convex hull
      df_area$area[i] <- convexHullArea(inner$x, inner$y)
      
    }
  }
  
  return(df_area)

}
