# Sentienl-2 BFAST
This contains the original NDVI data extracted from Sentinel-2 time series and the R code to implement BFAST to detect forest change.

## bfastlite_geedata.R

It contains the function to adjust the data format and implement BFAST lite. It also includes a parallel computing function which could significantly decrease the computing time.

The data should be written in a .csv file and read as:

    args[1] <- "ndvi.csv"
    
It should be ouput as:

    write.table(output, file = paste0("./breakpoints_", args[1]), row.names = TRUE,col.names = FALSE, sep = ",")
    
