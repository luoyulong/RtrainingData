library(RODBC)
library(plyr)
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

CompareTuning <- function(id_target,id_predict,opttype,per, function_name)
{
  variants <- performanceDB.GetVariantInfo_noOrder(id_target,opttype)
  variants2 <- performanceDB.GetVariantInfo(id_predict,opttype)
  for(i in 1:nrow(variants2))
  {
    prdconfig <- variants2[i,]$OptConfig
    prdGflops <- variants[variants$OptConfig==prdconfig,]$Gflops
    if(length(prdGflops)>0) 
      variants2[i,]$Gflops <- prdGflops[1]
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
  #browser()
  plot(tuningprocess$step,tuningprocess$max,
       ylim=c(min(tuningprocess$max),max(tuningprocess$max)),
       xlim=c(1,max(tuningprocess$step)),
       col="blue",lwd=4,cex.main=3,cex.lab=2,
       pch=19,type="l",
       main=function_name,
       xlab="Tuning Steps",ylab="Best Performance (Gflops)")
  #points(tuningprocess$step[1],min(tuningprocess$max), col="red",cex=1.2,pch=16)
  maxperf <- max(tuningprocess$max)
  for(i in 1:nrow(tuningprocess))
  {
    
    if(tuningprocess[i,]$max >= maxperf*per)
    {
      print(sprintf("achieve in %d,%f \n",i,tuningprocess[i,]$max))
      lines(i,tuningprocess[i,]$max, type="h", lwd=2, lty=2,col="blue");
      text(i,min(tuningprocess$max), as.character(i),cex=2,pos=4,col="blue")
      
      break;
    }
  }
  
  lines(prdprocess$step,prdprocess$max,col="red",lwd=3)
  
  maxperf <- max(prdprocess$max)
  for(i in 1:nrow(prdprocess))
  {
    
    if(prdprocess[i,]$max >= maxperf*per)
    {
      print(sprintf("achieve in %d,%f \n",i,prdprocess[i,]$max))
      lines(i,prdprocess[i,]$max, type="h", lwd=2, lty=2,col="red");
      text(i,min(tuningprocess$max), as.character(i),cex=2,pos=2,col="red")
      
      break;
    }
  }
  
}





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



comparisonValues <- function(specificsId, programmingModel, 
                             candicate_instance_cnt, candicate_config_in_instance_cnt) {
  # get the comparison of origin and model selection of the specific id
  # 
  # Args:
  #   specificsId: The specifics Id
  #   programmingModel: Such as cpu and cuda
  #   candicate_instance_cnt: The size of candicate instance selected by similar model
  #   candicate_config_in_instance_cnt: In each candicate instance, the selected config
  #
  # Returns:
  #   The dataframe contains the best gflops of each opttype
  #   For example, if the programmingModel is cpu, the dataframe 
  #   contains origin, Tiling, Unrolling values, otherwise cuda, the
  #   dataframe contains origin, CUDABlocking, Unrolling values.
  #
  
  origin_gflops <- performanceDB.GetVariantInfo(specificsId, "origin", 1)$Gflops
  optimized_gflops_configs <- performanceDB.GetVariantInfo(specificsId, "")
  candicate_instance_ids <- searchMostsimilarByModel(specificsId, programmingModel)[,1][1: candicate_instance_cnt]
  
  get_gest_gflops <- function(optType) {
    best_gflops <- 0;
    for( candicate_id in candicate_instance_ids) {      
      configs <- performanceDB.GetVariantInfo(candicate_id, optType)[1:candicate_config_in_instance_cnt]$OptConfig
      
      tmp_best_gflops <- optimized_gflops_configs[which(optimized_gflops_configs$OptConfig %in% configs),][1,1]
      if (best_gflops < tmp_best_gflops) {
        best_gflops <- tmp_best_gflops
      }
    }
    return (best_gflops)
  }
  
  if (programmingModel == "cpu") {
    opttypes <- c("Tiling", "Unrolling")
  } else {
    opttypes <- c("CUDABlocking", "Unrolling")
  }
  
  res = c()
  res["origin"] = origin_gflops
  for (opttype in opttypes) {
    res[opttype] = get_gest_gflops(opttype)
  }
  
  return(res)
}

PlotSimilary_exp_prd<- function(pm,a,base=100)
{
  snames=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
           "ProblemSize","DataType","Fdensity","workset","n_add","n_sub","n_mul","n_div",
           "loop_radius","num_align","num_unalign","num_array","num_readcachelines")
  
  specsSet <- performanceDB.SQL.selectSpecifics(
    attnames=c("specifics.id","ProgrammingModel","FunctionName"),
    sprintf("num_array <16  and ProgrammingModel='%s'",pm))
  similary_exp_prd <- a
  all_tmp <- NA
  for(i in 1:nrow(specsSet))
  {
    if(i<nrow(specsSet))
      for(j in (i+1):nrow(specsSet))
      {
        i_spec <- performanceDB.SQL.selectSpecifics(attnames=snames,sprintf("specifics.id=%d",specsSet[i,"id"]))
        i_spec_num <- performanceDB.preprocess2num(i_spec)
        j_spec <- performanceDB.SQL.selectSpecifics(attnames=snames,sprintf("specifics.id=%d",specsSet[j,"id"]))
        j_spec_num <- performanceDB.preprocess2num(j_spec)
        i_spec_num$id <- NULL
        j_spec_num$id <- NULL
        
        
        tmp <- data.frame(x=exp(performanceDB.similarity(a=i_spec_num,b=j_spec_num,pm)[[1]]),y=performanceDB.GetSimilarFactor(specsSet[i,"id"],specsSet[j,"id"],base,""))
        tmp2 <- data.frame(y=tmp$y,a=specsSet[i,"id"],b=specsSet[j,"id"])
        
        if(is.na(all_tmp))
          all_tmp <- tmp2
        else
          all_tmp <- rbind(all_tmp,tmp2)
        
        print(sprintf("%f,%f\n",tmp$x,tmp$y))
        if(is.na(similary_exp_prd))
          similary_exp_prd <- tmp
        else
          similary_exp_prd <- rbind(similary_exp_prd,tmp)
      }
  }
  
  return(similary_exp_prd)
  #plot(similary_exp_prd$x,similary_exp_prd$y,xlab="Similary of running instances",ylab="Similarity of Optimization space")
  
  #browser()
}


GetEachOptBestGflops <- function() {
  # Get the best Gflops of each opttype including SIMD, Origin, Tiling, Unrolling
  # 
  # Returns: 
  #    The dataset contains the data. 
  #    names(dataset) == c("Appliction","OMP", "opttype", "Gflops")
  
  dataset <- data.frame(Applications = character(0), OMP=character(0), 
                        opttype=character(0), Gflops=numeric(0), stringsAsFactors=F)
  benchmarkinfo  <-  data.frame(id=12699,name="FDTD",stringsAsFactors=F)
  benchmarkinfo[nrow(benchmarkinfo)+1,] <- c(12735,"HEAT")
  benchmarkinfo[nrow(benchmarkinfo)+1,] <- c(12736,"WAVE")
  benchmarkinfo[nrow(benchmarkinfo)+1,] <- c(12738,"POISSON")
  benchmarkinfo[nrow(benchmarkinfo)+1,] <- c(12739,"JACOBI")
  
  for(i in 1:nrow(benchmarkinfo))
  {
    info <- benchmarkinfo[i,]
    nt_str <- "OptConfig not like '%omp%'"
    nt_str <- append(nt_str,"OptConfig like '%omp@2%'")
    nt_str <- append(nt_str,"OptConfig like '%omp@4%'")
    nt_str <- append(nt_str,"OptConfig like '%omp@8%'")
    nt_str <- append(nt_str,"OptConfig like '%omp@16%'")
    n <- 1
    for (nt in nt_str)
    { 
      #browser()
      #origin
      opt_str <- "OptType like '%Origin%'"
      condit <- sprintf("SpecificsId = %s and %s and %s order by Gflops desc limit 1", info$id,nt,opt_str )
      tmp <-  performanceDB.SQL.select(selectcondition = condit,dbname = "hps", tbname = "optVariant")
      if(nrow(tmp)==1)
        dataset[nrow(dataset)+1,] <- c(info$name, n,"Origin", tmp$Gflops) #tiling best
      #simd+avx
      opt_str <- "OptType like '%DoSIMD-cflag%'"
      condit <- sprintf("SpecificsId = %s and %s and %s order by Gflops desc limit 1", info$id,nt,opt_str )
      tmp <-  performanceDB.SQL.select(selectcondition = condit,dbname = "hps", tbname = "optVariant")
      if(nrow(tmp)==1) 
        dataset[nrow(dataset)+1,] <- c(info$name, n,"+SIMD+Cflag", tmp$Gflops) #tiling best
      
      #tiling
      opt_str <- "OptType like '%Tiling%' and OptType not like '%Unrolling%'"
      condit <- sprintf("SpecificsId = %s and %s and %s order by Gflops desc limit 1", info$id,nt,opt_str )
      tmp <-  performanceDB.SQL.select(selectcondition = condit,dbname = "hps", tbname = "optVariant")
      if(nrow(tmp)==1)
        dataset[nrow(dataset)+1,] <- c(info$name, n,"+Tiling", tmp$Gflops) #tiling best
      #unrolling
      opt_str <- "OptType like '%Unrolling%'"
      condit <- sprintf("SpecificsId = %s and %s and %s order by Gflops desc limit 1", info$id,nt,opt_str )
      tmp <-  performanceDB.SQL.select(selectcondition = condit,dbname = "hps", tbname = "optVariant")
      if(nrow(tmp)==1)
        dataset[nrow(dataset)+1,] <- c(info$name, n,"+Unrolling", tmp$Gflops) #tiling best 
      
      
      n <- n*2 
    }
  }
  print(dataset)
  
  #best performance achieve finished 
  return(dataset)
  
}


theme_my <- function() {
  require(grid)
  theme_bw() + theme(axis.title.x = element_text(face="bold", size=12),
                     axis.title.y = element_text(face="bold", size=12, angle=90),
                     panel.grid.major = element_blank(), # switch off major gridlines
                     panel.grid.minor = element_blank(), # switch off minor gridlines
                     legend.position = c(0.2,0.8), # manually position the legend (numbers being from 0,0 at bottom left ofwhole plot to 1,1 at top right)
                     legend.title = element_blank(), # switch off the legend title
                     legend.text = element_text(size=12),
                     legend.key.size = unit(1.5, "lines"),
                     legend.key = element_blank()) # switch off the rectangle around symbols in the legend) 
}


###################################################################################################
#####  
#####               The Test
#####
###################################################################################################




if (FALSE) {
  ########## CompareTuning ###############
  function_name  <-  c("FDTD", "heat_3D", "JACOBI", "POISSON", "WAVE")
  cpu_targetId  <-  c(12699, 12735, 12739, 12738, 12736)
  cpu_predictId  <- c(12792, 12792, 12787, 12786, 12793)
  
  opar <- par(no.readonly=TRUE)
  print("======Start cpu==============")
  if (! file.exists("experiment")) {
    dir.create("experiment")
  }
  setwd("experiment")
  pdf("experiment_cpu.pdf",width=35,height=7) 
  par(mfrow=c(1,5))
  par(mar=c(4,6,4,2) + 0.1)
  for (ind in 1:length(function_name)) {
    print(sprintf("%s%s%s","==========",function_name[ind],"=========="))
    CompareTuning(cpu_targetId[ind], cpu_predictId[ind],"",0.95, 
                  function_name[ind])
  }
  dev.off()
  
  
  pdf("experiment_cuda.pdf",width=35,height=7) 
  par(mfrow=c(1,5))
  cuda_targetId  <-  c(12745, 12746, 12747, 12743, 12744)
  cuda_predictId  <- c(12760, 12755, 12746, 12753, 12768)
  
  print("======Start cuda==============")
  for (ind in 1:length(function_name)) {
    print(sprintf("%s%s%s","==========",function_name[ind],"=========="))
    CompareTuning(cuda_targetId[ind], cuda_predictId[ind], "",0.95,
                  function_name[ind])
  }
  
  dev.off()
  par(opar)
  setwd("..")
}

if (FALSE) {
  
  #############################################################################
  #    get the graph of the best gflops of each optimization methods on gpu
  #############################################################################
  
  # Test the comparisonValues
  benchmark_names  <-  c("FDTD", "heat_3D", "JACOBI", "POISSON", "WAVE")
  cuda_specificIds <- c(12745, 12746, 12747, 12743, 12744)
  
  get_item <- function(specificId, programmingModel) {
    # get the item data of the specific id
    # 
    # Args:
    #   specificsId: The specifics Id
    #   programmingModel: Such as cpu and cuda
    #
    # Returns:
    #   The dataframe contains the best gflops, If the best gflops is the combined,
    #   give every optimize methods gflops
    #
    
    item  <- c()
    origin_gflops <- performanceDB.GetVariantInfo(specificId, "origin", 1)$Gflops
    item["origin"]  <- origin_gflops
    
    best_gflops <- performanceDB.GetVariantInfo(specificId, "",1)$Gflops
    item["HPS"] <- best_gflops
    
    return(data.frame(item))
  }
  
  
  
  
  ######## CUDA #########################3
  data <- data.frame(row.names=c("origin","HPS","patus"))
  
  for (specificId in cuda_specificIds) {
    specificId_data <- get_item(specificId, "cuda")
    if (length(data)) {
      data <- cbind(data, specificId_data)
    } else {
      data <- specificId_data
    }
  } 
  patus <- c(69.3,87,49,52,43)
  data <- rbind(patus,data)
  names(data) <- benchmark_names
  row.names(data) <-c("patus","origin","+autotuning")
  
  print(data)
  pdf("optimize_compare_cuda.pdf",width=9,height=6)
  barplot(as.matrix(data),
          main="CUDA",
          #  width=0.5,
          ylab="Gflops",
          ylim = c(0, 200),
          xlab="Applications",
          axes = TRUE,
          cex.main=2,
          cex.lab=1.5,
          cex.axis=1.5,
          legend=rownames(data),
          args.legend=list(cex=1.0,yjust=0.8),
          beside=TRUE)
  
  dev.off()
  setwd("..")
  par(opar)
}






#paint the similar discovery
similar_discovery <- function()
{
  NSdata1 <- NOS_Similarty(12750,12778,"")
  NSdata2 <- NOS_Similarty(12781,12783,"")
  
  height <- 6
  width <- 5.5
  ca <- 1.5
  cl <-1.5
  cm <-2
  ################# group B #####################
  pdf("example_similar_number_50_78.pdf",height,width)
  plot((NSdata1$x),NSdata1$y,
       xlim=c(max(NSdata1$x),1),
       col="blue",pch=19,type="b",cex.main=cm,cex.axis=ca,cex.lab=cl,xlab="Number of best strategies",ylab="Number of same strategies",main="Group A: Same Strategies")
  abline(h=0,col="red")  
  dev.off()
  
  pdf("example_similar_ratio_50_78.pdf",height,width)
  plot((NSdata1$x),NSdata1$y/(NSdata1$x),
       xlim=c(max(NSdata1$x),1),
       col="blue",pch=19,type="b",cex.main=cm,cex.axis=ca,cex.lab=cl,xlab="Number of best strategies",ylab="Similarity ratio",main="Group A: Similarity Ratio")
  abline(h=0,col="red")  
  dev.off()
  
  ################# group B #####################
  pdf("example_nosimilar_number_81_83.pdf",height,width)
  plot((NSdata2$x),NSdata2$y,
       xlim=c(max(NSdata2$x),1),
       col="blue",pch=19,type="b",cex.main=cm,cex.axis=ca,cex.lab=cl,xlab="Number of best strategies",ylab="Number of same strategies",main="Group B: Same Strategies")
  abline(h=0,col="red")  
  dev.off()
  
  pdf("example_nosimilar_ratio_81_83.pdf",height,width)
  plot((NSdata2$x),NSdata2$y/(NSdata2$x),
       xlim=c(max(NSdata2$x),1),
       col="blue",pch=19,type="b",cex.main=cm,cex.axis=ca,cex.lab=cl,xlab="Number of best strategies",ylab="Similarity ratio",main="Group B: Similarity Ratio")
  abline(h=0,col="red")  
  dev.off()
  
  
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
  
  
  
  
  
  
  
  #(xlim=c(max(NSdata$x)+100,1),ylim=c(max(NSdata$y)+100,1))
  p <- ggplot(NSdata,aes(x,y)) +geom_point(col="red") 
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

if (FALSE) {
  ############################################################
  # PlotSimilary_exp_prd 
  ###########################################################
  a <- NA
  #a <- PlotSimilary_exp_prd("cpu",a)
  #plot(a$x,a$y,
  #     xlab="Similary of running instances",ylab="Similarity of Optimization space")
  a  <- PlotSimilary_exp_prd("cuda",a)
  
  setwd("experiment")
  pdf("Similarit.pdf")
  plot(a$x,a$y,
       xlab="Similary of running instances",ylab="Similarity of Optimization space")
  dev.off()
  setwd("..")
}


if (FALSE) {
  ############################################################
  # Find the best similarity model to meet the cpu 
  ###########################################################
  
  
}


if (FALSE) {
  ###########################################################
  # plot the graph of dsl,manual,generated code
  ##########################################################
  
  
  benchmark_names  <-  c("FDTD", "heat_3D", "JACOBI", "POISSON", "WAVE")
  cpu_specificIds <- c(12699, 12735, 12739, 12738, 12736)
  cuda_specificIds <- c(12745, 12746,12747, 12743,  12744)
  
  #####################CPU################################
  
  dataset <- data.frame(row.names=c("DSL", "Manual", "Generated"))
  dataset <- cbind(dataset, c(32,28,33))
  dataset <- cbind(dataset, c(10,45,93))
  dataset <- cbind(dataset, c(23,45,93))
  dunnintaset <- cbind(dataset, c(11,10,15))
  dataset <- cbind(dataset, c(11,11,16))
  
  colnames(dataset) <- benchmark_names
  setwd("experiment")
  opar <- par(no.readonly=TRUE)
  #par(cex.lab=1.2)
  #par(cex.main=10)
  pdf("code_amount_cpu.pdf",width=7,height=7)
  barplot(as.matrix(dataset),
          main="The Amount Of Code",
          ylab="Applications", xlab="Line Of Code",
          xlim=c(0,100),
          angle=45,
          density=20,
          horiz = TRUE,
          legend.text=rownames(dataset),
          args.legend = list(x = "topright",cex=1.5),
          beside=TRUE)
  
  dev.off()
  #####################CUDA################################
  dataset <- data.frame(row.names=c("DSL", "Manual", "Generated"))
  dataset <- cbind(dataset, c(32,168,174))
  dataset <- cbind(dataset, c(10,65,73))
  dataset <- cbind(dataset, c(23,92,100))
  dataset <- cbind(dataset, c(11,49,57))
  dataset <- cbind(dataset, c(11,52,65))
  
  colnames(dataset) <- benchmark_names
  pdf("code_amount_cuda.pdf")
  barplot(as.matrix(dataset),
          main="The Amount Of Code",
          ylab="Applications", xlab="Line Of Code",
          xlim=c(0,100),
          cex.names=2,
          horiz = TRUE,
          legend.text=rownames(dataset),
          args.legend = list(x = "topright",cex=1.5),
          beside=TRUE)
  
  par(opar)
  dev.off()
  setwd("..")
  
}





if (TRUE) {
  
  ####################################################################
  ##           Plot the contribution of each opttype
  ####################################################################
  #colnames(dataset) <- c("Applications", "OMP", "opttype", "Gflops")
  dataset <- GetEachOptBestGflops()
  library(ggplot2)
  
  dataset$Applications = factor(dataset$Applications)
  dataset$OMP = factor(dataset$OMP,levels=c("1","1.1","2","2.1","4","4.1","8","8.1","16","16.1"))
  dataset$opttype = factor(dataset$opttype)
  dataset$Gflops = as.numeric(dataset$Gflops)
  
  # Compute the contribution of each opttype
  library("plyr")
  dataset <- ddply(dataset, c("Applications", "OMP"), 
                   function(x){
                     stopifnot(nrow(x) <= 4)  # Origin ,SIMD, Tiling, Unrolling
                     GetGflops <- function(opttype) {
                       
                       gflops <- x[x$opttype==opttype,]$Gflops
                       if(length(gflops) == 0) {
                         gflops <- 0
                       }
                       return(gflops)
                     }
                     tmp_origin <- GetGflops("Origin")
                     tmp_simd <- GetGflops("SIMD")
                     tmp_tiling <- GetGflops("Tiling")
                     tmp_unrolling <- GetGflops("Unrolling")
                     
                     origin <- tmp_origin
                     simd <- ifelse(tmp_simd>tmp_origin, tmp_simd-tmp_origin, 0)
                     tiling <- ifelse(tmp_tiling>tmp_simd,tmp_tiling-tmp_simd, 0)
                     unrolling <- ifelse(tmp_unrolling>tmp_tiling, tmp_unrolling-tmp_tiling, 0)
                     
                     
                     data.frame(opttype=c("Origin","+SIMD+Cflag","+Tiling","+Unrolling"),
                                Gflops=c(origin, simd, tiling, unrolling))
                   })
  dataset$dsl="hps"
  # Add patus data
  levels(dataset$opttype) <- c(levels(dataset$opttype),"Patus")
  dataset[nrow(dataset)+1,] <- c("FDTD", "1","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("FDTD", "2","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("FDTD", "8","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("FDTD", "4","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("FDTD", "16","Patus", 12,"Patus")
  
  dataset[nrow(dataset)+1,] <- c("HEAT", "1","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("HEAT", "2","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("HEAT", "4","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("HEAT", "8","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("HEAT", "16","Patus", 12,"Patus")
  
  dataset[nrow(dataset)+1,] <- c("WAVE", "1","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("WAVE", "2","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("WAVE", "4","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("WAVE", "8","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("WAVE", "16","Patus", 12,"Patus")
  
  dataset[nrow(dataset)+1,] <- c("POISSON", "1","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("POISSON", "2","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("POISSON", "4","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("POISSON", "8","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("POISSON", "16","Patus", 12,"Patus")
  
  dataset[nrow(dataset)+1,] <- c("JACOBI", "1","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("JACOBI", "2","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("JACOBI", "4","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("JACOBI", "8","Patus", 12,"Patus")
  dataset[nrow(dataset)+1,] <- c("JACOBI", "16","Patus", 12,"Patus")
  
  # As the above set the Gflops as string, So we should change it back to numeric
  dataset$Gflops = as.numeric(dataset$Gflops)
  
  dataset_order <- dataset[dataset$opttype=="Patus",]
  dataset_order <- rbind(dataset_order,dataset[dataset$opttype=="Origin",])
  dataset_order <- rbind(dataset_order,dataset[dataset$opttype=="+SIMD+Cflag",])
  dataset_order <- rbind(dataset_order,dataset[dataset$opttype=="+Tiling",])
  dataset_order <- rbind(dataset_order,dataset[dataset$opttype=="+Unrolling",])
  levels(dataset$opttype) <- c("Patus","Origin","+SIMD+Cflag","+Tiling","+Unrolling")
  # ggplot(dataset[dataset$opttype!"Patus",], aes(OMP,Gflops, fill=opttype)) + 
  ggplot(dataset, aes(OMP,Gflops,fill=opttype)) + 
    geom_bar(stat="identity",position=position_dodge(),width=0.7) +
    theme(legend.position="top",legend.title=element_blank()) + 
    #geom_bar(data=dataset[dataset$opttype=="Patus",])+
    # stat="identity",width=0.3,position=position_dodge()) +
    facet_grid(. ~ Applications  )
  ggsave(file="experiment/openmp_cpu.pdf", height=4,width=8)
}


if(FALSE)
{
  ################WEAk Scaling########################
  
  dataset <- GetEachOptBestGflops()
  
  dataset$Applications = factor(dataset$Applications)
  dataset$OMP = as.numeric(dataset$OMP)
  dataset$opttype = factor(dataset$opttype)
  dataset$Gflops = as.numeric(dataset$Gflops)
  
  dataset <- ddply(dataset,c("Applications","OMP"),
                   function(x){
                     max_gflops <- max(x$Gflops)
                     origin_gflops  <- x[x$opttype=="Origin",]$Gflops
                     if (length(origin_gflops)==0) {
                       # stop()  # Current just set it to be 1
                       origin_gflops = 1
                     }
                     speedup <- max_gflops/origin_gflops
                     data.frame(Speedup=speedup)
                   })
  
  library(ggplot2)
  
  p <- ggplot(dataset,aes(x = OMP, y =Speedup,colour=factor(Applications) )) +
    geom_point() + geom_line() 
  
  ggsave(file="experiment/speedup.pdf",width=5,height=3) 
  
}

if (FALSE) {
  
  df <- NA 
  for(i in c(1,2,4,8,16))
  {
    j <- 0.1
    for(app in c("FDTD","WAVE","HIMENO","HEAT","POISSON"))
    { 
      tmp <- data.frame(application=app,omp=i+j,speedup=i*(1-0.01*i))
      if(is.na(df))
        df <- tmp
      else
        df <- rbind(df,tmp)
      j <- j + 0.1
    }
  }
  
  library(ggplot2)
  library(grid)
  p <- ggplot(df,aes(x = omp, y =speedup,colour=factor(df$application) )) +
    geom_point() + geom_line() + theme_bw() + theme(axis.title.x = element_text(face="bold", size=12),
                                                    axis.title.y = element_text(face="bold", size=12, angle=90),
                                                    panel.grid.major = element_blank(), # switch off major gridlines
                                                    panel.grid.minor = element_blank(), # switch off minor gridlines
                                                    legend.position = c(0.2,0.8), # manually position the legend (numbers being from 0,0 at bottom left of whole plot to 1,1 at top right)
                                                    legend.title = element_blank(), # switch off the legend title
                                                    legend.text = element_text(size=12),
                                                    legend.key.size = unit(1.5, "lines"),
                                                    legend.key = element_blank()) # switch off the rectangle around symbols in the legend) 
  print(p)
}
#close(conn)







######################## unUsed #####################################
if (FALSE) {
  # Test the comparisonValues
  benchmark_names <- c("JESMIN_MUPDATE1_3D", "JACOBI_PHYSIS_3D", "POISSON_19P",
                       "HEAT_3D", "WAVE_FD_3D")
  cpu_specificIds <- c(12699, 12739, 12738, 12735, 12736)
  cuda_specificIds <- c(12745, 12747, 12743, 12746, 12744)
  
  #########CPU###################
  opttypes <- c("DoSIMD", "Tiling", "Unrolling")
  opar <- par(no.readonly=TRUE)
  par(mfrow = c(3,2))
  ind <- 0
  for (specificId in cpu_specificIds) {
    ind <- ind + 1
    data <- data.frame(row.names=c("origin","Tiling", "Unrolling"))
    
    origin_gflops <- performanceDB.GetVariantInfo(specificId, "origin", 1)$Gflops
    item = c()
    item["DoSIMD"] <- origin_gflops
    item["Tiling"] <- 0
    item["Unrolling"] <- 0
    data <- cbind(data, item)
    
    item = c()
    item["DoSIMD"] <- origin_gflops
    all_configs <- performanceDB.GetVariantInfo(specificId, "",-1)
    best <- all_configs[1,]
    if (length(grep("@",best$OptConfig))) {
      # The combined 
      opts = strsplit(best$OptConfig, ";")
      for (opt in opts[[1]]) {
        fields = strsplit(opt, "@")[[1]]
        item[fields[1]] = all_configs[which(OptConfig==fields[2])]
      }
    } else if(length(grep(",",best$OptConfig))){
      item ["Tiling"] <- best$Gflops
      
    } else if (best$OptConfig != ""){
      item ["Unrolling"] <- best$Gflops
      
    } 
    item[setdiff(opttypes,names(item))] <- 0
    #browser()
    data <- cbind(data, item)
    names(data) <- c("origin", benchmark_names[ind])
    
    barplot(as.matrix(data),
            main="cpu",
            col=c("grey","pink"),
            xlab="compared", ylab="gflops",
            legend=names(data),
            args.legend = list(x = "topleft"))
    
  }
  par(opar)
  
}

if (FALSE) {
  # Test the comparisonValues
  benchmark_names <- c("JESMIN_MUPDATE1_3D", "JACOBI_PHYSIS_3D", "POISSON_19P",
                       "HEAT_3D", "WAVE_FD_3D")
  cpu_specificIds <- c(12699, 12739, 12738, 12735, 12736)
  cuda_specificIds <- c(12745, 12747, 12743, 12746, 12744)
  
  #########CPU###################
  opttypes <- c("DoSIMD", "Tiling", "Unrolling")
  ind <- 0
  data <- data.frame(row.names=c("origin","Tiling", "Unrolling"))
  names_data  <- c()
  for (specificId in cpu_specificIds) {
    ind <- ind + 1
    
    origin_gflops <- performanceDB.GetVariantInfo(specificId, "origin", 1)$Gflops
    item = c()
    item["DoSIMD"] <- origin_gflops
    item["Tiling"] <- 0
    item["Unrolling"] <- 0
    data <- cbind(data, item)
    
    item = c()
    item["DoSIMD"] <- origin_gflops
    all_configs <- performanceDB.GetVariantInfo(specificId, "",-1)
    best <- all_configs[1,]
    if (length(grep("@",best$OptConfig))) {
      # The combined 
      opts = strsplit(best$OptConfig, ";")
      for (opt in opts[[1]]) {
        fields = strsplit(opt, "@")[[1]]
        item[fields[1]] = all_configs[which(OptConfig==fields[2])]
      }
    } else if(length(grep(",",best$OptConfig))){
      item ["Tiling"] <- best$Gflops
      
    } else if (best$OptConfig != ""){
      item ["Unrolling"] <- best$Gflops
      
    } 
    item[setdiff(opttypes,names(item))] <- 0
    #browser()
    data <- cbind(data, item)
    
    item = c()
    item["DoSIMD"] <- 0
    item["Tiling"] <- 0
    item["Unrolling"] <- 0
    data <- cbind(data, item)
    names_data <- c(names_data, "origin", benchmark_names[ind],"")
  }    
  colnames(data) <- names_data
  barplot(as.matrix(data),
          main="cpu",
          col=c("grey","pink","red"),
          xlab="compared", ylab="gflops",
          legend=c("DoSIMD","Tiling", "Unrolling"),
          args.legend = list(x = "topleft"))
  
  
}




