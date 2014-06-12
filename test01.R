#!/usr/bin/env Rscript
#initialize env
library("foreign")
library("car")
library("effects")
library("plyr")
library("ggplot2")

if(grepl("/Users/luoyulong",getwd()))#In MacOS
{
  TOPDIR="/Users//luoyulong//workplace//program/R-script/"
} else if("/home/zengping", getwd())
{
   TOPDIR = "/home/zengping/work/graduate/stencilautotuning/RtrainingData" 
}
else{#In Windows  
  TOPDIR="C:/Users/luocean/Desktop/Mac-program/R-script/"
}
  setwd(TOPDIR) 
#DATADIR=c('512/*/*.arff', '256/*/*.arff','128/*/*.arff')
#DATADIR=c('unrolling-1-15/*/*.arff')
DATADIR=c("data/data_0523/*/*.arff")
source("input.R")
source("performanceDB.R")
TrainingSet<-inputTraningSet(DATADIR)
DO_UNROLLING=TRUE

#defalut 4X aveage variance
filterBadpoints<-function(C,att,threshold=4)
{ 
  rSet=subset(C,(C[[att]]-ave(C[[att]]))<threshold*ave((ave(C[[att]] )/C[[att]]-1)^2))
  return (rSet)
}

getExtremumsPoint<-function(data)
{
  C=data$Gflops
  Cleft=c(0,C[1:length(C)-1])
  Cright=c(C[2:length(C)],0)
  C1=C>Cleft
  C2=C>Cright
  extremums=C1 & C2
  return (data[extremums,])  
}


getTilingsize<-function(C)
{
  tmparray<-unlist(strsplit(C,","))
  return(as.numeric(tmparray[2]))
}
getUnrollingsize<-function(C)
{
  tmparray<-unlist(sub("Unrolling","",C))
  return(as.numeric(tmparray))
}

getImprovement<-function(x)
{
  x$improv=x$Gflops/x[which(x$config==1),]$Gflops
  return (x)
}









#for tiling set

if(FALSE)
{
  Tiling_data<-subset(TrainingSet,grepl("Tiling*",config),)
  Tiling_data$config<-lapply(Tiling_data$config,getTilingsize)
  
  
  
  plot(Tiling_data.128$config,Tiling_data.128$Gflops,
       ylim=c(min(Tiling_data$Gflops),max(Tiling_data$Gflops)),
       pch=2,lty=17,col="red",type="b",xlab="block size")
  
  
  #Tiling_data.256<-subset(Tiling_data,programsize==256,)
  #lines (Tiling_data.256$config,Tiling_data.256$Gflops,pch=1,lty=17,col="blue",type="b")
  
  
  Tiling_data.512<-subset(Tiling_data,programsize==512,)
  lines (Tiling_data.512$config,Tiling_data.512$Gflops,pch=3,lty=17,col="black",type="b")
}



if(DO_UNROLLING)
{ 
  Unrolling_data<-subset(TrainingSet,configtype=='Unrolling' & Fdensity!=32 & type=="float" &programsize==128,)
  Unrolling_data$config<-as.numeric(Unrolling_data$config)
  Unrolling_data$OptType="unrolling"
  Unrolling_data$nradius<-unlist(lapply(Unrolling_data$radius,getTilingsize))
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==8]<-5
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==15]<-9
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==30]<-9
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==13]<-5
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==32]<-24
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==22]<-13
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==29]<-17 
  
  Unrolling_data$realworkset=Unrolling_data$workset+(Unrolling_data$config-1)*Unrolling_data$unrolling_overhead*4*Unrolling_data$datatype
}









if(FALSE)
{
  #for unrolling 
  
  
  Unrolling_data<-subset(TrainingSet,configtype=='Unrolling' & Fdensity!=32 & type=="float" &programsize==128,)
  Unrolling_data$config<-as.numeric(Unrolling_data$config)
  Unrolling_data$nradius<-unlist(lapply(Unrolling_data$radius,getTilingsize))
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==8]<-5
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==15]<-9
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==30]<-9
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==13]<-5
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==32]<-24
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==22]<-13
  Unrolling_data$unrolling_overhead[Unrolling_data$Fdensity==29]<-17 
  
  
  Unrolling_data$realworkset=Unrolling_data$workset+(Unrolling_data$config-1)*Unrolling_data$unrolling_overhead*4*Unrolling_data$datatype
  
  attach(Unrolling_data)
  
  
  
  
  #Unrolling_impor<-ddply(Unrolling_data,c('programsize','workset','Fdensity'), getImprovement) 
  
  extremumsPoint<-ddply(Unrolling_data,c('programsize','workset','Fdensity'),getExtremumsPoint)
  OptimalPoint<-ddply(Unrolling_data,c('programsize','workset','Fdensity'), function(x) x[x$Gflops==max(x$Gflops),]) 
  
  rr=ddply(Unrolling_data,c('programsize','workset','Fdensity','config'),function(x) scale(x$Gflops)) 
  
  
  
  #  rr<-ddply(Unrolling_impor,c('programsize','workset','Fdensity','config'),function(x) filterBadpoints(x,"improv",1)) 
  
  rr=Unrolling_data 
  printResult(rr,extremumsPoint,OptimalPoint,"realworkset","Gflops","programsize")
  
  
  # fit<-glm(formula=
  #         Gflops~I(programsize^3)+I(programsize^2)+programsize+I(1/programsize)+
  #         Fdensity+I(1/Fdensity)+I(Fdensity^2)+I(Fdensity^3)+I(Fdensity^4)+
  #        realworkset+I(realworkset^2)+I(realworkset^3)+I(1/realworkset)+
  #       config+I(unrolling_overhead)+I(programsize/config)+nradius:programsize,
  #    data=Unrolling_data)
  
  
  fit<-lm(formula=
            Gflops~I(programsize^3)+I(programsize^2)+programsize+I(1/programsize)+programsize:+
            Fdensity+I(1/Fdensity)+I(Fdensity^2)+I(Fdensity^3)+I(Fdensity^4)+
            realworkset+I(realworkset^2)+I(realworkset^3)+I(realworkset^4)+I(realworkset^5)+I(1/realworkset)+
            config+I(unrolling_overhead)+I(programsize/config)+
            nradius:programsize+I((1/realworkset)^2)+num_unalign+num_align+
            config+I(config^2)+I(config^3)+I(config^4)+I(1/config)+I(1/config^2)+
            Fdensity:I(1/programsize),
          data=Unrolling_data,family=gaussian)
  summary(fit,correlation=TRUE)
  
  plot(fit)
  
  testdata=subset(Unrolling_data,programsize==512)
  #testdata$Gflops=0
  testdata$Gflops=predict(fit,testdata)
  testextremumsPoint<-ddply(testdata,c('programsize','workset','Fdensity'),getExtremumsPoint)
  testOptimalPoint<-ddply(testdata,c('programsize','workset','Fdensity'), function(x) x[x$Gflops==max(x$Gflops),]) 
  
  printResult(testdata,testextremumsPoint,testOptimalPoint,"realworkset","Gflops","programsize")
  
  #rr$realworkset
  
  detach(Unrolling_data)
}






tilingplot<-function(data,size,ws,c)
{
  tmp<-subset(data,data$programsize==size & data$workset==ws,)
  lines(tmp$tilingconfig,tmp$Gflops,col=c,type="b")
}


if(FALSE)
{
  #for tiling
  Tiling_data<-subset(TrainingSet,configtype=='Tiling',)
  Tiling_data$tilingconfig<-unlist(lapply(Tiling_data$config,getTilingsize))
  Tiling_data$nradius<-unlist(lapply(Tiling_data$radius,getTilingsize))
  
  
  str(Tiling_data)
  tmp=Tiling_data[c("tilingconfig","Gflops","programsize")]
  str(tmp)
  
  
  vdata=subset(Tiling_data,type=="float")
  
  
  extremumsPoint<-ddply(vdata,c('programsize','workset'),getExtremumsPoint)
  OptimalPoint<-ddply(vdata,c('programsize','workset'), function(x) x[x$Gflops==max(x$Gflops),])   
  
  vdata$score=0
  vdata$score[vdata[] %in% extremumsPoint ]<-1
  vdata$score[vdata %in% OptimalPoint]<-2
  vdata$row.names
  all.equal(vdata[,],extremumsPoint[,]) 
  
  z=vdata[,] %in% extremumsPoint
  
  
  p<-ggplot(vdata, aes(tilingconfig,Gflops,colour=factor(Fdensity/workset)) )
  p+geom_point()#+geom_line()+facet_wrap(~programsize,ncol=2)
  p+geom_point(aes(extremumsPoint$tilingconfig,extremumsPoint$Gflops,colour=factor(extremumsPoint$programsize) ) ) 
  
  
  
  par(lty=2)
  par(pch=17)
  tmp<-subset(Tiling_data,Tiling_data$programsize==256 & Tiling_data$workset==80,)
  plot(tmp$tilingconfig,tmp$P_WALL_CLOCK_TIME-ave(tmp$P_WALL_CLOCK_TIME),
       col="blue",type="b",ylim=c(min(tmp$P_WALL_CLOCK_TIME)-ave(tmp$P_WALL_CLOCK_TIME)[1],
                                  max(tmp$P_WALL_CLOCK_TIME)-ave(tmp$P_WALL_CLOCK_TIME)[1]))
  
  
  #old code 
  
  
  
  filter_names=c("n_add","n_sub","n_mul","n_div","L1CacheSize",
                 "L2CacheSize","config","type","configtype","CoreNumber","ThreadsPerCore","frequency","radius")
  
  filter_=names(Tiling_data) %in% filter_names
  numeric_data=Tiling_data[!filter_]
  
  
  
  
  fit<-lm(formula=I(programsize^3)+workset+tilingconfig~Gflops,data=numeric_data)
  
  
  data.workset80<-subset(numeric_data,numeric_data$workset==80,)
  
  data.sorted<-data.workset80[order(data.workset80$Gflops),]
  OptimalPoint<-ddply(data.workset80, 'programsize', function(x) x[x$Gflops==max(x$Gflops),]) 
  
  
  fit2<-lm(Gflops~I(programsize^3)+I(tilingconfig^2)+Fdensity+workset+tilingconfig:programsize,data=numeric_data)
  summary(fit2)
  par(mfrow=c(2,2))
  plot(fit2)
  
  confint(fit2)
  
  helplot(effect("tilingconfig:programsize",fit2,list(programsize=c(128,256,512))),multiline=TRUE)
  
  
  
  
  tmp<-numeric_data[c("programsize","tilingconfig","workset","Gflops")]
  cor(tmp)
  scatterplotMatrix(tmp,spread=TRUE)
  
  plot(numeric_data$programsize,numeric_data$P_WALL_CLOCK_TIME,)
  lines(data.workset80$programsize,fitted(fit2),pch=2,lty=18,col="red")
  
  
  
  
  
  coefficients(fit)
  summary(fit)
  cor(numeric_data)
  
  
  OptimalPoint<-ddply(Tiling_data, c('workset',"programsize"), function(x) x[x$Gflops==max(x$Gflops),]) 
  
  
  Tiling_numdata=
    
    
    cov(OptimalPoint)
  
  
  op.128=subset(OptimalPoint,programsize=="128")
  op.128=op.128[order(op.128$tilingconfig),]
  op.256=subset(OptimalPoint,programsize=="256")
  op.256=op.256[order(op.256$tilingconfig),]
  op.512=subset(OptimalPoint,programsize=="512")
  op.512=op.512[order(op.512$tilingconfig),]
  
  plot(OptimalPoint$workset,OptimalPoint$config,pch=1,lty=17,col="blue",type="b",xlab="workset",ylab="factor") 
}




#detach(TrainingSet)
