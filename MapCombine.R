# library(EBImage)
# 
# f_name_1 <- "colors1.png"
# f_name_2 <- "colors2.png"
# 
# map_1 <- readImage(f_name_1)
# map_2 <- readImage(f_name_2)
# map_1_data <- imageData(map_1)
# map_2_data <- imageData(map_2)
# map_1_days <- calc_ups_days(map_1_data)
# map_2_days <- calc_ups_days(map_2_data)
# 
# map_2_logical_mat <- (map_2_days < map_1_days)
# map_1_logical_mat <- !map_2_logical_mat
# 
# map_2_logical_mat <- map_2_logical_mat * 1
# map_1_logical_mat <- map_1_logical_mat * 1
# 
# 
# map_2_logical_mx <- (map_2_days < map_1_days)
# map_1_logical_mx <- !map_2_logical_mx
# 
# map_2_logical_mx <- map_2_logical_mx * 1
# map_1_logical_mx <- map_1_logical_mx * 1
# 
# 
# 
# 
# 
# 
# 
# 
# map_x <- readImage("colors1.png")
# 
# map_data <- imageData(map_x)
# r1 <- round(map_data[,,1],4)
# r2 <- round(map_data[,,2],4)
# r3 <- round(map_data[,,3],4)
# mat_concat <- matrix(paste(r1, r2, r3, sep = "|"), dim(map_data)[1], dim(map_data)[2])
# mat_days <- matrix(99,dim(map_data)[1],dim(map_data)[2])
# 
# col_1_day <- paste(round(255/255, 4), round(209/255, 4), round( 36/255, 4), sep = "|")
# col_2_day <- paste(round(201/255, 4), round(132/255, 4), round(  0/255, 4), sep = "|")
# col_3_day <- paste(round(147/255, 4), round(167/255, 4), round(  8/255, 4), sep = "|")
# col_4_day <- paste(round(133/255, 4), round(  6/255, 4), round(  0/255, 4), sep = "|")
# col_5_day <- paste(round(255/255, 4), round(120/255, 4), round(  0/255, 4), sep = "|")
# col_6_day <- paste(round(176/255, 4), round(166/255, 4), round(150/255, 4), sep = "|")
# col_7_day <- paste(round(  0/255, 4), round(129/255, 4), round(152/255, 4), sep = "|")
# 
# 
# mat_concat == col_1_day
# 
# mat_days[mat_concat == col_1_day] <- 1
# mat_days[mat_concat == col_2_day] <- 2
# mat_days[mat_concat == col_3_day] <- 3
# mat_days[mat_concat == col_4_day] <- 4
# mat_days[mat_concat == col_5_day] <- 5
# mat_days[mat_concat == col_6_day] <- 6
# mat_days[mat_concat == col_7_day] <- 7



#--------------------------------------------

calc_ups_days <- function(map_data){
  r1 <- round(map_data[,,1],4)
  r2 <- round(map_data[,,2],4)
  r3 <- round(map_data[,,3],4)
  
  mat_concat <- matrix(paste(r1, r2, r3, sep = "|"), dim(map_data)[1], dim(map_data)[2])
  mat_days <- matrix(99,dim(map_data)[1],dim(map_data)[2])
  
  # 1 day = 255, 209,  36
  # 2 day = 201, 132,   0
  # 3 day = 147, 167,   8
  # 4 day = 133,   6,   0
  # 5 day = 255, 120,   0
  # 6 day = 176, 166, 150
  # 7 day =   0, 129, 152
  col_1_day <- paste(round(255/255, 4), round(209/255, 4), round( 36/255, 4), sep = "|")
  col_2_day <- paste(round(201/255, 4), round(132/255, 4), round(  0/255, 4), sep = "|")
  col_3_day <- paste(round(147/255, 4), round(167/255, 4), round(  8/255, 4), sep = "|")
  col_4_day <- paste(round(133/255, 4), round(  6/255, 4), round(  0/255, 4), sep = "|")
  col_5_day <- paste(round(255/255, 4), round(120/255, 4), round(  0/255, 4), sep = "|")
  col_6_day <- paste(round(176/255, 4), round(166/255, 4), round(150/255, 4), sep = "|")
  col_7_day <- paste(round(  0/255, 4), round(129/255, 4), round(152/255, 4), sep = "|")
  
  mat_days[mat_concat == col_1_day] <- 1
  mat_days[mat_concat == col_2_day] <- 2
  mat_days[mat_concat == col_3_day] <- 3
  mat_days[mat_concat == col_4_day] <- 4
  mat_days[mat_concat == col_5_day] <- 5
  mat_days[mat_concat == col_6_day] <- 6
  mat_days[mat_concat == col_7_day] <- 7
  
  return(mat_days)
}


#----------------------------------------------

combine_maps <- function(f_name_1, f_name_2){
  require(EBImage)
  map_1 <- readImage(f_name_1)
  map_2 <- readImage(f_name_2)
  map_1_data <- imageData(map_1)
  map_2_data <- imageData(map_2)
  map_3_data <- map_1_data
  
  map_1_days <- calc_ups_days(map_1_data)
  map_2_days <- calc_ups_days(map_2_data)
  
  map_2_logical_mx <- (map_2_days < map_1_days)
  map_1_logical_mx <- !map_2_logical_mx
  
  map_2_logical_mx <- map_2_logical_mx * 1
  map_1_logical_mx <- map_1_logical_mx * 1
  
  map_1_data[ , , 1] <- map_1_data[ , , 1] * map_1_logical_mx
  map_1_data[ , , 2] <- map_1_data[ , , 2] * map_1_logical_mx
  map_1_data[ , , 3] <- map_1_data[ , , 3] * map_1_logical_mx
  
  map_2_data[ , , 1] <- map_2_data[ , , 1] * map_2_logical_mx
  map_2_data[ , , 2] <- map_2_data[ , , 2] * map_2_logical_mx
  map_2_data[ , , 3] <- map_2_data[ , , 3] * map_2_logical_mx
  
  map_3_data[ , , 1] <- map_1_data[ , , 1] + map_2_data[ , , 1]
  map_3_data[ , , 2] <- map_1_data[ , , 2] + map_2_data[ , , 2]
  map_3_data[ , , 3] <- map_1_data[ , , 3] + map_2_data[ , , 3]
  
  map_3 <- Image(map_3_data, colormode = "color")
  
  return(map_3)
}

combine_maps2 <- function(map_1, map_2){
  require(EBImage)

  map_1_data <- imageData(map_1)
  map_2_data <- imageData(map_2)
  map_3_data <- map_1_data
  
  map_1_days <- calc_ups_days(map_1_data)
  map_2_days <- calc_ups_days(map_2_data)
  
  map_2_logical_mx <- (map_2_days < map_1_days)
  map_1_logical_mx <- !map_2_logical_mx
  
  map_2_logical_mx <- map_2_logical_mx * 1
  map_1_logical_mx <- map_1_logical_mx * 1
  
  map_1_data[ , , 1] <- map_1_data[ , , 1] * map_1_logical_mx
  map_1_data[ , , 2] <- map_1_data[ , , 2] * map_1_logical_mx
  map_1_data[ , , 3] <- map_1_data[ , , 3] * map_1_logical_mx
  
  map_2_data[ , , 1] <- map_2_data[ , , 1] * map_2_logical_mx
  map_2_data[ , , 2] <- map_2_data[ , , 2] * map_2_logical_mx
  map_2_data[ , , 3] <- map_2_data[ , , 3] * map_2_logical_mx
  
  map_3_data[ , , 1] <- map_1_data[ , , 1] + map_2_data[ , , 1]
  map_3_data[ , , 2] <- map_1_data[ , , 2] + map_2_data[ , , 2]
  map_3_data[ , , 3] <- map_1_data[ , , 3] + map_2_data[ , , 3]
  
  map_3 <- Image(map_3_data, colormode = "color")
  
  return(map_3)
}