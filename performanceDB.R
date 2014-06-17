globalSQL=NA
globalModel=list(Tiling=NA,CUDABlocking=NA,Unrolling=NA,Dosimd=NA,OMP=NA)
rightmodel.att=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
                 #specifics
                 "ProblemSize","DataType","Fdensity","workset","n_add","n_sub","n_mul","n_div","loop_radius","num_align","num_unalign","num_array","num_readcachelines", 
                 #OptVariant
                 "OptType","Gflops")
getVariant.att=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
                 #specifics
                 "ProblemSize_x","ProblemSize_y","ProblemSize_z","DataType","Fdensity","workset",
                 "n_add","n_sub","n_mul","n_div","loop_radius_x","loop_radius_y","loop_radius_z",
                 "num_align","num_unalign","num_array","num_readcachelines" 
                 #OptVariant
                 #"OptType"
)

#link value in dataframe using string
linkvalues=function(df,linker=",")
{
  linked_str=""
  for(value in df[1:length(df)])
  { 
    if(is.character(value))
      strs_=sprintf("'%s'",value)
    else if(is.factor(value))
      strs_=sprintf("'%s'",as.character(value))
    else if(is.na(value))
      strs_=sprintf("%d",-1)
    else if(is.integer(value))
      strs_=sprintf("%d",value)
    else if(is.numeric(value))
      strs_=sprintf("%f",value)
    
    if(linked_str=="")
      linked_str=strs_
    else
      linked_str=paste(linked_str,strs_,sep=linker)
  }
  return (linked_str)
}



performanceDB.SQL.dbopen=function()
{
  if(is.na(globalSQL))
    channel=odbcConnect("myhps","hps","hps")
  else
    channel=globalSQL
  return (channel)
}

performanceDB.SQL.dbclose=function(channel)
{
  if(channel!=globalSQL)
    close(channel)
}
performanceDB.SQL.select=function(selectcondition="true",dbname="hps",tbname="testinsert")
{
  selcmd=sprintf('select * from %s.%s where %s;',dbname,tbname,selectcondition)
  channel=performanceDB.SQL.dbopen()
  result=sqlQuery(channel,selcmd)
  performanceDB.SQL.dbclose(channel)
  return (result)  
}

performanceDB.SQL.selectall=function(attnames)
{
  names_str=paste(attnames,collapse=",")
  selcmd=sprintf('select %s from specifics join platform on (specifics.PlatformId=platform.id) join optVariant on (specifics.id=optVariant.SpecificsId);',names_str)
  channel=performanceDB.SQL.dbopen()
  sqlQuery(channel,"use hps;")
  result=sqlQuery(channel,selcmd)
  performanceDB.SQL.dbclose(channel)
  return (result)  
}



#must have same OptType
performanceDB.SQL.selectspecificsWithSameOptType=function(attnames,opttype,condition="")
{
  attnames=append(attnames,"specifics.id")
  names_str=paste(attnames,collapse=",")
  if(condition!="")
    condition=paste(condition,"and")
  selcmd=sprintf('select %s from specifics join platform on ( specifics.PlatformId= platform.id) where %s specifics.id in (select SpecificsId from optVariant where OptType="%s");',names_str,condition,opttype)
  channel=performanceDB.SQL.dbopen()
  sqlQuery(channel,"use hps;")
  result=sqlQuery(channel,selcmd)
  performanceDB.SQL.dbclose(channel)
  return (result)  
}





#newdata 
#sname is the column name of specifics data
#vname is the column name of variant data
performanceDB.update=function(newdata,snames,vnames)
{
  conditions_str="" 
  for(sname in snames[1:length(snames)])
  {
    if(is.character(newdata[,sname]))
      condit=sprintf('%s="%s"',sname,newdata[,sname])
    else
      condit=sprintf('%s=%s',sname,newdata[,sname])
    
    if(sname==snames[1])
      conditions_str=condit
    else
      conditions_str=paste(conditions_str,condit,sep=' and ')
  }
  #search in performance database
  targetInDB=performanceDB.SQL.select(conditions_str,"hps","specifics")
  
  nrow(targetInDB)
  if(nrow(targetInDB)==0)
  {
    tmp=performanceDB.SQL.insert(newdata[snames],dbname="hps",tbname="specifics")
    sid=data.frame("SpecificsId"=tmp[1,])
    vdata=cbind(newdata[vnames],sid)
    performanceDB.SQL.insert(vdata,dbname="hps",tbname="optVariant")
  }
  else if(nrow(targetInDB)==1)
  { 
    specificId=targetInDB$id
    conditions_str=sprintf('%s=%d and %s="%s" and %s="%s"',
                           "SpecificsId",specificId,
                           "OptType",newdata[,"OptType"],
                           "OptConfig",newdata[,"OptConfig"])
    targetInDB2=performanceDB.SQL.select(conditions_str,"hps","optVariant")
    print(targetInDB2)
    if(nrow(targetInDB2)==0)
    { 
      
      variantdata=cbind(newdata[vnames],data.frame("SpecificsId"=specificId))
      performanceDB.SQL.insert(variantdata,dbname="hps",tbname="optVariant")
    }
    else if(nrow(targetInDB2)==1)
    {
      factor=1.2
      variantdata=cbind(newdata[vnames],data.frame("SpecificsId"=specificId))
      variantdata$Gflops=(targetInDB2$Gflops+variantdata$Gflops*factor)/(1+factor)
      variantdata$id=targetInDB2$id
      performanceDB.SQL.update(variantdata,dbname="hps",tbname="optVariant")
    }
    #else
  }
  #else 
  #  stopifnot(FALSE) 
}

 
#undate test
if(FALSE)
{
  globalSQL=performanceDB.SQL.dbopen()
  rawdata=performanceDB.SQL.select("TRUE",dbname="hps",tbname="experiment");
  rawdata=performanceDB.SQL.rawdatapreprocess(rawdata);
  allnames=names(rawdata)
  vnames=c("OptType","OptConfig","Gflops")
  vnames2=allnames %in% vnames
  snames=allnames[!vnames2]
  for(i in 1:nrow(rawdata))
    performanceDB.update(newdata=rawdata[i,],snames=snames,vnames=vnames)
  close(globalSQL)
}
performanceDB.SQL.rawdatapreprocess=function(trainingset)
{
  tnames=names(trainingset)
  for(tn in tnames)
  {
    if(is.factor(trainingset[1,tn]))
      trainingset[,tn]=as.character(trainingset[,tn])
  }
  
  #count Gflops
  trainingset$ProblemSizeSum=unlist(lapply(trainingset$ProblemSize,getProblemSizeSum))
  trainingset$Gflops=trainingset$Steps*trainingset$ProblemSizeSum*trainingset$Fdensity/trainingset$P_WALL_CLOCK_TIME/1000
  
  #filter attribution
  trainingset$id=NULL
  trainingset$ProblemSizeSum=NULL
  trainingset$P_WALL_CLOCK_TIME=NULL
  trainingset$PAPI_FP_INS=NULL
  trainingset$PAPI_LD_INS=NULL
  trainingset$PAPI_L1_DCM=NULL
  trainingset$PAPI_L2_TCM=NULL
  
  trainingset$OptType[trainingset$OptType==""]="Origin"
  return (trainingset);
}


getTilingsize<-function(C)
{
  tmparray<-unlist(strsplit(C,","))
  return(as.numeric(tmparray[2]))
}


getProblemSizeSum<-function(C)
{
  tmparray<-unlist(strsplit(C,","))
  
  tmparray=as.integer(tmparray)
  pss=1;
  for(tmp in tmparray)
  {
    pss=pss*tmp
  }
  return(pss)
}



#preprocss the 
performanceDB.SQL.preprocessByOptType=function(trainingset)
{
  trainingset=performanceDB.preprocess2num(trainingset)
  
  #Unrolling training
  unrolling_data=subset(trainingset,OptType=="Unrolling",)
  #unrolling_data$OptConfig=as.integer(unrolling_data$OptConfig)
  
  #OMP training
  omp_data=subset(trainingset,OptType=="OMP",)
  #omp_data$OptConfig=as.integer(omp_data$OptConfig)
  
  #Tiling training
  tiling_data=subset(trainingset,OptType=="Tiling",)
  # tiling_data$OptConfig<-unlist(lapply(tiling_data$OptConfig,getTilingsize))
  
  #CUDABlocking tranining
  cudablock_data=subset(trainingset,OptType=="CUDABlocking",)
  #cudablock_data$OptConfig<-unlist(lapply(cudablock_data$OptConfig,getTilingsize))
  
  #SIMD training
  dosimd_data=subset(trainingset,OptType=="DoSIMD",)
  #  dosimd_data$OptConfig=as.integer(dosimd_data$OptConfig)
  
  
  preprocessed=list(Unrolling=unrolling_data,Tiling=tiling_data,OMP=omp_data,CUDABlocking=cudablock_data,DoSIMD=dosimd_data)
  return (preprocessed);
}









#e.g:deletecmd="steps=10,problemsize='128,128,128'"
performanceDB.SQL.delete=function(deletecondition,dbname="hps",tbname="testinsert")
{
  delcmd=sprintf('delete from %s.%s where %s',dbname,tbname,deletecondition)
  channel=performanceDB.SQL.dbopen()
  result=sqlQuery(channel,delcmd)
  performanceDB.SQL.dbclose(channel)
}
performanceDB.SQL.insert=function(data,dbname="hps",tbname="testinsert")
{
  stopifnot(nrow(data)==1)
  db_tb=sprintf('%s.%s',dbname,tbname)
  filter1=names(data) %in% c("id")
  data=data[!filter1]
  s1=paste(names(data),collapse=",")
  #s2=paste(data,collapse=",")
  s2=linkvalues(data,",")
  inscmd=sprintf('insert into %s(%s) values(%s);',db_tb,s1,s2)
  channel=performanceDB.SQL.dbopen()
  sqlQuery(channel,inscmd)
  tableid=sqlQuery(channel,"select last_insert_id()");
  performanceDB.SQL.dbclose(channel)
  return (tableid)
}


performanceDB.SQL.insertmany=function(data,dbname="hps",tbname="testinsert")
{
  db_tb=sprintf('%s.%s',dbname,tbname)
  channel=performanceDB.SQL.dbopen()
  sqlSave(channel,data,append=TRUE,tablename =db_tb);
  tableid=sqlQuery(channel,"select last_insert_id()");
  performanceDB.SQL.dbclose(channel)
  return (tableid)
}


performanceDB.SQL.update=function(data,dbname="hps",tbname="testinsert")
{
  db_tb=sprintf('%s.%s',dbname,tbname)
  channel=performanceDB.SQL.dbopen()
  
  tmpdata=data
  tmpdata$id=NULL
  dnames=names(tmpdata)
  
  updcmd=""
  for(dn in dnames[1:length(dnames)])
  {
    if(is.character(data[,dn]))
      upd_str=sprintf('%s="%s"',dn,data[,dn])
    else
      upd_str=sprintf('%s=%s',dn,data[,dn])
    
    if(updcmd=="")
      updcmd=upd_str
    else
      updcmd=paste(updcmd,upd_str,sep=',')
  }
  updcmd=sprintf("UPDATE %s.%s set %s where id = %d",dbname,tbname,updcmd,data$id)
  sqlQuery(channel,updcmd)
  performanceDB.SQL.dbclose(channel)
}

#the function return the similarity of vector a and b based on optimizing technology "opttype"
#the input a,b is two specifics vector, the opttype determine the optimizing technology.
performanceDB.distance=function(a,b,opttype)
{
  rightmodel=performanceDB.rightmodel(opttype)
  dif=abs(a-b)
  #subtract the intercept
  r=predict(rightmodel,dif)-rightmodel$coefficients[[1]]
  return (r);
}

#this function will return a right model for opttype.
#especally, if the model is not trained before, the new model will be tranined and be stored in a global variable "globalModel" and return;
#or it will return the model in the global variale "globalModel"
performanceDB.rightmodel=function(opttype="Tiling")
{
  if(anyNA(globalModel[[opttype]]))
  {
    trainingset=performanceDB.SQL.selectall(rightmodel.att)
    trainingset=performanceDB.SQL.preprocessByOptType(trainingset)
    Optset=trainingset[[opttype]] 
    Optset$OptType=NULL
    allnames=names(Optset)
    thegflops=allnames %in% c("Gflops")
    trainingname=allnames[!thegflops]
    model_str=formula.generate(trainingname,2)
    print(sprintf("=========== generate %s model=========",opttype))
    rightmodel=eval(parse(text=sprintf("rightmodel<-lm(formula=Gflops~%s,data=Optset) ",model_str)))
    rightmodel$coefficients=abs(rightmodel$coefficients)
    globalModel[[opttype]]<<-rightmodel
  }
  else
    rightmodel=globalModel[[opttype]]
  return(rightmodel)
} 


#the C is a string that contains multiple numbers like "a,b,c,d", this function return the idx'th number in C
getNumberFromStr<-function(C,idx)
{
  tmparray<-unlist(strsplit(C,","))
  tmparray=as.integer(tmparray)
  return(tmparray[idx])
}



performanceDB.preprocess2num=function(trainingset)
{
  tnames=names(trainingset)
  for(tn in tnames)
  {
    if(is.factor(trainingset[1,tn]))
      trainingset[,tn]=as.character(trainingset[,tn])
  }
  
  if("DataType" %in% tnames)
  {
    trainingset$DataType[trainingset$DataType=="float"]=4
    trainingset$DataType[trainingset$DataType=="double"]=8
    trainingset$DataType=as.integer(trainingset$DataType)
  }
  
  if("loop_radius" %in% tnames)
  {
    trainingset$loop_radius_x=unlist(lapply(trainingset$ProblemSize,function(x) getNumberFromStr(x,1)))
    trainingset$loop_radius_y=unlist(lapply(trainingset$ProblemSize,function(x) getNumberFromStr(x,2)))
    trainingset$loop_radius_z=unlist(lapply(trainingset$ProblemSize,function(x) getNumberFromStr(x,3)))
    trainingset$loop_radius=NULL
  }
  
  if("ProblemSize" %in% tnames)
  {
    #trainingset$ProblemSizeSum=unlist(lapply(trainingset$ProblemSize,getProblemSizeSum))
    trainingset$ProblemSize_x=unlist(lapply(trainingset$ProblemSize,function(x) getNumberFromStr(x,1)))
    trainingset$ProblemSize_y=unlist(lapply(trainingset$ProblemSize,function(x) getNumberFromStr(x,2)))
    trainingset$ProblemSize_z=unlist(lapply(trainingset$ProblemSize,function(x) getNumberFromStr(x,3)))
    trainingset$ProblemSize=NULL
  } 
  return (trainingset);
}



#get n (number=n) best optimizing variants
#data: contains the specific of target running instance
#snames:determin the key attribution names of specifics in data,used for finding same/similar optimizion variant
#number:determin the number of return variant
#opttye:the optimizing technology that running instance want perform
performanceDB.getVariants=function(data,snames,number,opttype="Tiling")
{
  conditions_str=""
  for(sname in snames[1:length(snames)])
  {
    if(is.character(data[,sname]))
      condit=sprintf('%s="%s"',sname,data[,sname])
    else
      condit=sprintf('%s=%s',sname,data[,sname])
    if(sname==snames[1])
      conditions_str=condit
    else
      conditions_str=paste(conditions_str,condit,sep=' and ')
  }
  #search in performance database
  SameSpecInDB=performanceDB.SQL.selectspecificsWithSameOptType(snames,opttype,conditions_str)
  if(nrow(SameSpecInDB)==0)
  {
    #find simliar point
    specDB=performanceDB.SQL.selectspecificsWithSameOptType(snames,opttype)
    specDB=performanceDB.preprocess2num(specDB)
    data=performanceDB.preprocess2num(data)
    #because the name of specDB and data have been change after function preprocess2num
    snames=names(data)
    min_d=performanceDB.distance(specDB[1,snames],data[snames],opttype)
    similarSpec=specDB[1,] 
    for(i in 2:nrow(specDB))
    {
      d=performanceDB.distance(specDB[i,snames],data[snames],opttype)
      if(d<min_d)
      {
        min_d=d
        similarSpec=specDB[i,]
      }
    }
    specId=similarSpec$id
    selectcondition=sprintf("SpecificsId=%d order by Gflops desc limit %d",specId,number)
    variants=performanceDB.SQL.select(selectcondition,dbname="hps",tbname="optVariant")
    return(variants)
  }
  else if(nrow(SameSpecInDB)>=1)
  {
    specId=SameSpecInDB$id
    selectcondition=sprintf("SpecificsId=%d order by Gflops desc limit %d",specId,number)
    variants=performanceDB.SQL.select(selectcondition,dbname="hps",tbname="optVariant")
    return(variants)
  }
}


formula.generate <- function(factors, times) {
  
  # generate the formula
  # Args:
  #      factors: the factors of the formula,such as Fdencity,frequency
  #      times: the time range of each factor such as 1:5
  # returns: the generated factor like I(1/Fedencity) + Fencity
  # 
  
  stopifnot(is.character(factors))
  stopifnot(is.numeric(times))
  
  fomula_str = c()
  for (factor_item in factors) {
    for (time_item in times) {
      if(time_item == 1) {
        tmp = factor_item
      } else if(time_item == 0){
        next
      } else {
        tmp = paste("I(",factor_item,"^",time_item,")")
      }
      fomula_str = c(fomula_str, tmp)
    }
  }
  
  return(paste(fomula_str,collapse="+"))
}



#getVariant test
if(TRUE)
{
  globalSQL=performanceDB.SQL.dbopen() 
  tmp=rightmodel.att
  
  testdata0=performanceDB.SQL.selectall(tmp)
  testdata1=performanceDB.SQL.preprocessByOptType(testdata0)
  testdata2=testdata1[["Tiling"]]
  testdata3=testdata2[1,]
  
  testdata3$OptType=NULL
  testdata3$Gflops=NULL
  testdata3$ProblemSizeSum=NULL
  testdata3$ProblemSize="128,128,128"
  testdata3$DataType="float"
  testdata3$loop_radius="1,1,1"
  testdata3$ProblemSizeSum=NULL
  
  snames=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
           #specifics
           "ProblemSize","DataType","Fdensity","workset","n_add","n_sub","n_mul","n_div",
           "loop_radius","num_align","num_unalign","num_array","num_readcachelines")
  performanceDB.rightmodel("Tiling")
  bestvariants=performanceDB.getVariants(testdata3,snames,3,"Tiling")
  print("get best variants")
  print(bestvariants)
}





