#read arguments from command line
args = commandArgs(trailingOnly = TRUE) 

#安装并加载包
library(remotes) 
install_github("bfast2/strucchangeRcpp") 
install_github("bfast2/bfast")
install.packages("pacman")
pacman::p_load(zoo,lubridate,bfast,plyr,magrittr,foreach,doParallel)

#读取数据
args[1] <- "ndvi.csv"
NDVI <- t(read.csv(paste0("../", args[1]))) %>% 
  write.table(., file=paste0("../", args[1]), row.names = TRUE,col.names = FALSE, sep = ",")

NDVI <- read.table(file=paste0("../", args[1]), sep=",", header=TRUE, fill=TRUE, check.names =FALSE) 
colnames(NDVI)[1]="Date"
NDVI[NDVI == -9.999000e+03] <- NA 
NDVI$Date <- ymd(gsub("X","",NDVI$Date)) 

#快速计算函数体
bpdetect <- function(j){
  tryCatch({ 
    #make a ts object  
    ts <-bfastts(NDVI[,j],NDVI$Date, type="irregular")     #apply bfastlite for different breaks scenarios      
    fit <-bfastlite(ts)            #optimal 1-segment partition     
    fit1 <-bfastlite(ts, breaks=1) #breaks = 1     
    fit2 <-bfastlite(ts, breaks=2) #breaks = 2 
    #pritn a garph of each result 
    file_name = paste("./BFAST_OUT_GRAPHS/", colnames(NDVI[j]), ".tiff", sep="")     
    tiff(file_name) 
    print(plot(fit, magstat = 'RMSD', xlab="Time", ylab="NDVI"))     
    dev.off() 
  }, error=function(e){cat("ERROR :", conditionMessage(e), 
                           "\n")}) 
  
  #change breaktime to fractional days i.e. 2018.56   
  dates.no.na <-as.numeric(time(ts))   
  dates.no.na[is.na(ts)]<-NA   
  dates.no.na <- na.omit(dates.no.na) 
  
  #extract wanted variables and store  
  bdate0 <- dates.no.na[fit$breakpoints$breakpoints[1]] #date of breakpoint  
  bdate1 <- dates.no.na[fit1$breakpoints$breakpoints[1]]    
  bdate2 <- dates.no.na[fit2$breakpoints$breakpoints[1]]   
  bdate3 <- dates.no.na[fit2$breakpoints$breakpoints[2]]   
  mag1 <- magnitude(fit$breakpoints, breaks = 1)$Mag[4] 
  #magnitude of breakpoint 'RMSD' 
  mag2 <- magnitude(fit$breakpoints, breaks = 2)$Mag[4]
  return(
    rbind(colnames(NDVI[j]),
        bdate0[1],
        fit$breakpoints$breakpoints[1], #breakpoint id in the series
        colnames(NDVI[j]),
        bdate1[1],
        fit1$breakpoints$breakpoints[1],
        colnames(NDVI[j]),
        bdate2[1],
        fit2$breakpoints$breakpoints[1],
        bdate3[1],
        fit2$breakpoints$breakpoints[2],
        colnames(NDVI[j]),
        mag1,
        mag2
    )
  )
}

#快速计算代码
cl = makeCluster(detectCores(logical=FALSE))                                                  
registerDoParallel(cl)       
output <- foreach(
  k= 2 :ncol(NDVI), #k 改动到你要的范围
  .combine=cbind,
  .packages = c("bfast")
  ) %dopar% bpdetect(k)
stopCluster(cl)

#导出csv文件
rownames(output) <- c("SITE_ID","BP1_DATE_FIT","BP1_ID_FIT","SITE_ID","BP1_DATE_FIT1","BP1_ID_FIT1","SITE_ID","BP1_DATE_FIT2","BP1_ID_FIT2","BP2_DATE_FIT2","BP2_ID_FIT2","SITE_ID","BP1_MAG_RMSD","BP2_MAG_RMSD")
write.table(output, file = paste0("./breakpoints_", args[1]), row.names = TRUE,col.names = FALSE, sep = ",")