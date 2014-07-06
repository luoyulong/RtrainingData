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
  r <- rgb(1-(col/maxval)^4,1-(col/maxval)^4,1-(col/maxval)^4)
  return (r)
}

GetVariantInfo <- function(sid,number=-1,opttype=""){
  conn <- odbcConnect("myhps","hps","hps")
  if(opttype=="")
    selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d order by Gflops desc;',sid)
  else
    selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d and OptType="%s" order by Gflops desc;',sid,opttype)
  sqlQuery(conn,"use hps")
  result <- sqlQuery(conn,selcmd,stringsAsFactors = FALSE)
  close(conn)
 # print(nrow(result))
  if(number==-1)
    return (result)
  else 
    return (result[1:number,])
}

GetSimilarNumber <- function(A,B)
{
  A <- as.data.frame(A,stringsAsFactors = FALSE)[!is.na(A),]
  B <- as.data.frame(B,stringsAsFactors = FALSE)[!is.na(B),]
  both <- union(A,B)
  equals <- length(A)+length(B)-length(both)
  return (equals)
}

GetSimilarFactor <- function(a_id,b_id,size)
{
  
  result1  <- GetVariantInfo(a_id,size,"Tiling")
  result2 <- GetVariantInfo(b_id,size,"Tiling")
  similarnumber <- GetSimilarNumber(result1$OptConfig,result2$OptConfig)
  print(sprintf("Similar Rate: %d/%d",similarnumber,nrow(result1)))
}
basesize <- 10
#GetSimilarFactor(12699,12682,basesize)
#GetSimilarFactor(12699,12683,basesize)
#GetSimilarFactor(12699,12686,basesize)
#GetSimilarFactor(12699,12687,basesize)

GetSimilarFactor(12699,12701,basesize)
GetSimilarFactor(12699,12704,basesize)
GetSimilarFactor(12699,12705,basesize)
GetSimilarFactor(12699,12708,basesize)


#result <- GetVariantInfo(12703)




#result$x <- unlist(lapply(result$OptConfig, function(x) GetNumberFromStr(x, 2)))
#result$y <- unlist(lapply(result$OptConfig, function(x) GetNumberFromStr(x, 3)))

#result$col <- result$Gflops

#plot(result$x,result$y,
#    ylim=c(0,max(result$y)),
#   xlim=c(0,max(result$x)),
#  pch=19,col=GetRGB(result$col),type="p",xlab="block size $X",ylab="block size $Y")

#bestresult <- result[1:100,]
#points(bestresult$x,bestresult$y,
#      pch=19,col="red",type="p",xlab="block size $X",ylab="block size $Y ")




#close(conn)
