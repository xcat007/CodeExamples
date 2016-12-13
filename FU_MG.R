FU_MG <- function(mgdata){
  counter <- 1
  df <- data.frame(matrix(ncol = 2, nrow = 2))
  # e<- strsplit(as.character(d)," ")[[1]]
  
  mglite<- mgdata[,1:2]
  mgrows <- nrow(mgdata)
  
  for (i1 in 1:mgrows){
    #loop 1 begin
    shipment <- mglite[i1,1]
    #print(shipment)
    loads <- mglite[i1,2]
    splitloads<- strsplit(as.character(loads)," ")[[1]]
    numloads <- length(splitloads)
    
      for (i2 in 1:numloads){
        #loop 2 begin
        df[counter,1]<- as.character(shipment)
        df[counter,2]<- splitloads[i2]
        
        counter<- counter + 1
        
        
        #loop 2 end
      }
    
    
    #loop 1 end
  }
  
  return (df)
}


convert_MG <- function(){
  a1 <- read.csv("C:/Users/steven.phillips01/Documents/Projects/Pluto/Profitibility Reconciliation/Import Export folder/RawMGShiptoLoadOutput.csv")
  a2 <- a1[,1:2]
  a3 <- FU_MG(a2)
  
  write.csv(a3,"C:/Users/steven.phillips01/Documents/Projects/Pluto/Profitibility Reconciliation/Import Export folder/ShipToLoadMapping.csv")

  
  
}