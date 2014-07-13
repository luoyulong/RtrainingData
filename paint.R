library(RODBC)
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
TOPDIR <- dirname(frame_files[[length(frame_files)]])
setwd(TOPDIR)
source("pDBfunctions.R")

GetNumberFromStr <- function(strs, idx) {
  # Get the idx field of strs which splits by ','
  # Args: 
  #   strs: The string which contains multiple numbers like 'a,b,c,d'
  #   idx: The idx fields of strs 
  # 
  # Returns:
  #   The idx number
  tmparray <- unlist(strsplit(strs, ","))
  tmparray <- as.integer(tmparray)
  return(tmparray[idx])
}
GetRGB <- function(col)
{
  maxval <- max(col)
  r <- rgb(1-(col/maxval)^4,1-(col/maxval)^4,1-(col/maxval)^4)
  return (r)
}

CompareTuning <- function(id_target,id_predict,opttype,per)
{
  variants <- performanceDB.GetVariantInfo_noOrder(id_target,opttype)
  variants2 <- performanceDB.GetVariantInfo(id_predict,opttype)
  for(i in 1:nrow(variants2))
  {
    prdconfig <- variants2[i,]$OptConfig
    prdGflops <- variants[variants$OptConfig==prdconfig,]$Gflops
    if(length(prdGflops)>0) 
      variants2[i,]$Gflops <- prdGflops
    else
      variants2[i,]$Gflops <- 0.00001
  }
  
  tuningprocess <- data.frame(step=1,max=variants[1,]$Gflops)
  for(i in 2:nrow(variants))
  {
    if(variants[i,]$Gflops > tuningprocess[i-1,]$max)
      submax <- variants[i,]$Gflops
    else
      submax <- tuningprocess[i-1,]$max
    tuningprocess[i,]$step <- i
    tuningprocess[i,]$max <- submax
  }
  
  prdprocess <- data.frame(step=1,max=variants2[1,]$Gflops)
  for(i in 2:nrow(variants2))
  {
    if(variants2[i,]$Gflops > prdprocess[i-1,]$max)
      submax <- variants2[i,]$Gflops
    else
      submax <- prdprocess[i-1,]$max
    prdprocess[i,]$step <- i
    prdprocess[i,]$max <- submax
  }
  #prdprocess <- rbind(data.frame(step=0,max=min(tuningprocess$max)),prdprocess)
  
  plot(tuningprocess$step,tuningprocess$max,
       ylim=c(min(tuningprocess$max),max(tuningprocess$max)),
       xlim=c(1,max(tuningprocess$step)),
       pch=19,type="l",xlab="tuning steps",ylab="best performance (Gflops)")
  lines(prdprocess$step,prdprocess$max,col="red")
  
  maxperf <- max(prdprocess$max)
  for(i in 1:nrow(prdprocess))
  {
    
    if(prdprocess[i,]$max >= maxperf*per)
    {
      print(sprintf("achieve in %d,%f \n",i,prdprocess[i,]$max))
      lines(i,prdprocess[i,]$max, type="h", col=2, lwd=2, lty=2);
      break;
    }
  }
  
}








CompareTuning(12698,12703,"CUDABlocking",0.99)




#performanceDB.GetSimilarFactor(12699,12682,basesize)
#performanceDB.GetSimilarFactor(12699,12683,basesize)
#performanceDB.GetSimilarFactor(12699,12686,basesize)
#performanceDB.GetSimilarFactor(12699,12687,basesize)


basesize <- 30
performanceDB.GetSimilarFactor(12699,12701,basesize,"Tiling")
#performanceDB.GetSimilarFactor(12699,12704,basesize)
#performanceDB.GetSimilarFactor(12699,12705,basesize)
#performanceDB.GetSimilarFactor(12699,12708,basesize)


performanceDB.GetSimilarFactor(12698,12711,basesize,"CUDABlocking")

result <- performanceDB.GetVariantInfo(12698,"CUDABlocking")



if(FALSE)
{
  
  result$x <- unlist(lapply(result$OptConfig, function(x) GetNumberFromStr(x, 2)))
  result$y <- unlist(lapply(result$OptConfig, function(x) GetNumberFromStr(x, 3)))
  
  result$col <- result$Gflops
  
  plot(result$x,result$y,
       ylim=c(0,max(result$y)),
       xlim=c(0,max(result$x)),
       pch=19,col=GetRGB(result$col),type="p",xlab="block size $X",ylab="block size $Y")
  
  bestresult <- result[1:100,]
  points(bestresult$x,bestresult$y,
         pch=19,col="red",type="p",xlab="block size $X",ylab="block size $Y ")
} 



#close(conn)
