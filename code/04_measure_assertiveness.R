library(data.table)
source("convex_hull_handmovement_functions.R")
# Get the list of validation videos
# then find the directory where the pose detection results are

files <- list.files("../results/combined_fr_pd/",
                    recursive = T,
                    full.names = T,
                    pattern = "landmarks.csv")

df <- data.frame(path = files,
                 horizontal = NA,
                 vertical = NA,
                 area = NA,
                 combined_length = NA)

for(i in 1:nrow(df)){
  
  f <- fread(df$path[i])
  
  # If the entire video has at least 1 second's worth of data for both hands
  # Note that ther also has to be at least one contiguous segment of one second - this is done inside the landmark_area function
  # So practically, this if statement does nothing little (it does require both hands though) unless the number 24 is raised higher, this just provides another knob to turn
  if(length(which(is.na(f$x4)==F))>=24 & length(which(is.na(f$x7)==F))>=24){
    
    
    #Sometimes if there is virtually no movement, this will fail
    try({
    # Calculate the landmark height/width/area for both hands
    # For individual segments of contiguos frames with hands visible
    out4 <- landmark_area(f$x4, f$y4)
    out7 <- landmark_area(f$x7, f$y7)
    out4$hand <- "right"
    out7$hand <- "left"
    out <- rbind(out4, out7)
    
    # Aggregate across those segments
    # Multiply each segment's landmark height/width/area by its length
    # making longer segments more important 
    # (since they are more reliable & more impactful to the viewer)
    # Then divide by the sum of the section lengths
    horiz <- sum(out$mean_horizontal_length*out$section_length, na.rm = T)/sum(out$section_length[is.na(out$mean_horizontal_length)==F])
    vert <- sum(out$mean_vertical_length*out$section_length, na.rm = T)/sum(out$section_length[is.na(out$mean_vertical_length)==F])
    area <- sum(out$mean_vertical_length*out$area, na.rm = T)/sum(out$section_length[is.na(out$area)==F])
    
    # Assign everything to the dataframe
    df$horizontal[i] <- horiz
    df$vertical[i] <- vert
    df$area[i] <- area
    df$combined_length[i] <- sum(out$section_length[is.na(out$mean_vertical_length)==F])
    
    })
  }
  
  print(i)
  
}

save(df, file = "../results/dominance/convhull_on_all_data.rdata")
