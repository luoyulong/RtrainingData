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



#CompareTuning(12698,12703,"CUDABlocking",0.99)



NOS_Similarty <- function(id_a,id_b,opttype)
{
  
  FN <- nrow(performanceDB.GetVariantInfo(id_a,opttype))
  r <- data.frame(x=FN,y=FN)
  for(i in as.integer(FN/100):1)
  {
    tmpx <- i*100
    tmpy <- performanceDB.GetSimilarFactor(id_a,id_b,tmpx,opttype)
    tmpr <- data.frame(x=tmpx,y=tmpy)
    r <- rbind(r,tmpr)
  }
  for(i in as.integer((90)/10):1)
  {
    tmpx <- i*10
    tmpy <- performanceDB.GetSimilarFactor(id_a,id_b,tmpx,opttype)
    tmpr <- data.frame(x=tmpx,y=tmpy)
    r <- rbind(r,tmpr)
  }
  return (r)
}


searchMostsimilarByModel <- function(id,pm)
{
  snames=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
           #specifics
           "ProblemSize","DataType","Fdensity","workset","n_add","n_sub","n_mul","n_div",
           "loop_radius","num_align","num_unalign","num_array","num_readcachelines")
  target_spec <- performanceDB.SQL.selectSpecifics(attnames=snames,sprintf("specifics.id=%d",id))
  target_spec_num <- performanceDB.preprocess2num(target_spec)
  candidate_spec <- performanceDB.SQL.selectSpecifics(attnames=append(snames,c("specifics.id")),sprintf("ProgrammingModel = '%s'",pm))
  prd_sim <- NA
  for(i in 1:nrow(candidate_spec))
  {
    cid <- candidate_spec[i,"id"]
    candidate_spec_num <- performanceDB.preprocess2num(candidate_spec[i,])
    candidate_spec_num$id <- NULL
    tmp <- data.frame(id=cid,similarty=performanceDB.similarity(a=target_spec_num,b=candidate_spec_num,"cuda")[[1]])
    if(is.na(prd_sim))
      prd_sim <- tmp
    else
      prd_sim <- rbind(prd_sim,tmp)
  }
  return (prd_sim[order(prd_sim$similarty,decreasing=T),])
}



searchMostsimilarByExp <- function(id,pm,base=50)
{
  specsSet <- performanceDB.SQL.selectSpecifics(attnames=c("specifics.id","ProgrammingModel","FunctionName"),sprintf("ProgrammingModel='%s'",pm))
  exp_sim <- NA
  for(i in 1:nrow(specsSet))
  {
    tmp <- data.frame(id=specsSet[i,"id"],similarty=performanceDB.GetSimilarFactor(specsSet[i,"id"],id,base,""))
    if(is.na(exp_sim))
      exp_sim <- tmp
    else
      exp_sim <- rbind(exp_sim,tmp)
  }
  return (exp_sim[order(exp_sim$similarty,decreasing=T),])
}






if(FALSE){
  
  
  prd_sim <- searchmostsimilar(12743,"cuda")
  basesize <- 100
  
  performanceDB.GetSimilarFactor(12750,12777,basesize,"CUDABlocking")
  
  cudaspecs <- performanceDB.SQL.selectSpecifics(attnames=c("specifics.id","ProgrammingModel","FunctionName"),"ProgrammingModel='cuda'")
  for(i in 1:nrow(cudaspecs))
  {
    if(i+1<=nrow(cudaspecs))
      for(j in (i+1):nrow(cudaspecs))
      {
        print(sprintf("%d-%d : ",cudaspecs[i,"id"],cudaspecs[j,"id"]))
        performanceDB.GetSimilarFactor(cudaspecs[i,"id"],cudaspecs[j,"id"],10,"")
        
      }  
  }
  exp_sim <- searchMostsimilarByExp(12746,"cuda")
  prd_sim <- searchMostsimilarByModel(12746,"cuda")
  
  
  
  
  
  cpuspecs <- performanceDB.SQL.selectSpecifics(attnames=c("specifics.id","ProgrammingModel","FunctionName"),"ProgrammingModel='cpu'")
  for(i in 1:nrow(cpuspecs))
  {
    if(i+1<=nrow(cpuspecs))
      for(j in (i+1):nrow(cpuspecs))
      {
        print(sprintf("%d-%d : ",cpuspecs[i,"id"],cpuspecs[j,"id"]))
        equalnumber <- performanceDB.GetSimilarFactor(cpuspecs[i,"id"],cpuspecs[j,"id"],50,"")
        
      }  
  }
  
  
  global.model[["cuda"]]=NA
  global.model[["cpu"]]=NA
  cudamodel <- performanceDB.BuildSimilarModel(pm="cuda",opttype="")
  cpumodel <- performanceDB.BuildSimilarModel(pm="cpu",opttype="")
  
  
  
  NSdata <- NOS_Similarty(12781,12783,"")
  
  
  
  plot((NSdata$x),NSdata$y/(NSdata$x),
       xlim=c(max(NSdata$x),1),
       col="blue",pch=19,type="b",size=2,xlab="Size of optimal space",ylab="Number of euqal configurations")
  abline(h=0,col="red")
  
  #(xlim=c(max(NSdata$x)+100,1),ylim=c(max(NSdata$y)+100,1))
  p <- ggplot(NSdata,aes(x,y)) +geom_point(col="blue") 
  #p <- p+labs(xlab="NOS: size of optimal space",ylab="Number of equal configuration")
  p<- xlim(1,10)  #scale_x_reverse(limits=c(0, 5000))
  p
  print(NSdata[1,])
  #performanceDB.GetSimilarFactor(12699,12683,basesize)
  #performanceDB.GetSimilarFactor(12699,12686,basesize)
  #performanceDB.GetSimilarFactor(12699,12687,basesize)
  
  
  
  
  
  
  
  
  performanceDB.GetSimilarFactor(12699,12701,basesize,"Tiling")
  #performanceDB.GetSimilarFactor(12699,12704,basesize)
  #performanceDB.GetSimilarFactor(12699,12705,basesize)
  #performanceDB.GetSimilarFactor(12699,12708,basesize)
  
  
  performanceDB.GetSimilarFactor(12698,12711,basesize,"CUDABlocking")
  
  result <- performanceDB.GetVariantInfo(12698,"CUDABlocking")
  
  
  CompareTuning (12747,12754,"",0.95)
}

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
