
library(RODBC)
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
  r <- rgb((col/maxval)^2,(col/maxval)^2,(col/maxval)^2)
  return (r)
}


conn <- odbcConnect("myhps","hps","hps")
selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId>12695 and OptType="CUDABlocking" order by Gflops desc;')
sqlQuery(conn,"use hps")
result <- sqlQuery(conn,selcmd,stringsAsFactors = FALSE)
result$x <- unlist(lapply(result$OptConfig,
                          function(x) GetNumberFromStr(x, 2)))
result$y <- unlist(lapply(result$OptConfig,
                          function(x) GetNumberFromStr(x, 3)))

result$col <- as.integer(result$Gflops)

plot(result$x,result$y,
     ylim=c(0,max(result$y)),
     xlim=c(0,max(result$x)),
     lty=1,col=GetRGB(result$col),type="p",xlab="block size $X",ylab="block size $Y")



close(conn)
