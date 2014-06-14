odbcConnect("hpsDB","hps","hps")
inputTraningSet<-function(dirs)
{
  #read all arff in DATADIR
  for(dir in dirs){
    fns = Sys.glob(dir)
    begin=1
    if(dir==dirs[1]){
      TrainingSet<-read.arff(fns[1])
      TrainingSet$kernelname=rep(gsub("\\([a-z_, ).*]*|void |/","",fns[1]),length(TrainingSet$workset))
      begin=2 
    }
    for (fn in fns[begin:length(fns)]) {
      ##对每个表格操作
      readdata<-read.arff(fn) 
      readdata$kernelname=rep(gsub("\\([a-z_, ).*]*|void |/","_",fn),length(readdata$workset))
      TrainingSet<-rbind(readdata,TrainingSet)
    }
  }
  
  TrainingSet$L1CacheSize<-as.numeric(TrainingSet$L1CacheSize)
  TrainingSet$L2CacheSize<-as.numeric(TrainingSet$L2CacheSize)
  TrainingSet$programsize<-as.numeric(TrainingSet$programsize)
  TrainingSet$CoreNumber<-as.numeric(TrainingSet$CoreNumber)
  TrainingSet$num_align<-as.numeric(TrainingSet$num_align)
  TrainingSet$num_unalign<-as.numeric(TrainingSet$num_unalign)
  TrainingSet$n_add=as.numeric(TrainingSet$n_add)
  TrainingSet$n_sub=as.numeric(TrainingSet$n_sub)
  TrainingSet$n_mul=as.numeric(TrainingSet$n_mul)
  TrainingSet$n_div=as.numeric(TrainingSet$n_div)
  TrainingSet$num_array<-as.numeric(TrainingSet$num_array)
  TrainingSet$num_readcachelines<-as.numeric(TrainingSet$num_readcachelines)
  TrainingSet$workset<-as.numeric(TrainingSet$workset)
  TrainingSet$P_WALL_CLOCK_TIME<-as.numeric(TrainingSet$P_WALL_CLOCK_TIME)
  TrainingSet$Fdensity<-as.numeric(TrainingSet$Fdensity)
  TrainingSet$datatype[TrainingSet$type=="float" ]<- 1
  TrainingSet$datatype[TrainingSet$type=="double" ]<- 2
  TrainingSet$steps[TrainingSet$programsize==128 ]<- 100
  TrainingSet$steps[TrainingSet$programsize>128 ]<-10
  #steps is the number of iteration times
  TrainingSet$Gflops<-(TrainingSet$programsize^3*TrainingSet$Fdensity*TrainingSet$steps)/(TrainingSet$P_WALL_CLOCK_TIME*1000)
  TrainingSet$Gstencil<-(TrainingSet$programsize^3)/(TrainingSet$P_WALL_CLOCK_TIME)
  
  return (TrainingSet)
}


getmodel=function()
{
  
}

