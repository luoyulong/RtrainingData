argument_right="no init"
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
    else
      strs_=sprintf("%d",value)
    
    if(linked_str=="")
      linked_str=strs_
    else
      linked_str=paste(linked_str,strs_,sep=linker)
  }
  return (linked_str)
}



performanceDB.SQL.dbopen=function()
{
  channel=odbcConnect("myhps","hps","hps")
  return (channel)
}

performanceDB.SQL.dbclose=function(channel)
{ 
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
r=performanceDB.SQL.select(selectcondition="true",dbname="hps",tbname="optVariant")
names(r[1,])
#platform
tmp=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
      #specifics
      "ProblemSize","DataType","Fdensity","workset","n_add","n_sub","n_mul","n_div","loop_radius","num_align","num_unalign","num_array","num_readcachelines", 
      #OptVariant
      "SpecificsId","OptType","OptConfig","Gflops"
)
trainingset=performanceDB.SQL.selectall(tmp)


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
  print(pss)
  return(pss)
}


performanceDB.SQL.preprocess=function(trainingset)
{
  tnames=names(trainingset)
  for(tn in tnames)
  {
    if(is.factor(trainingset[1,tn]))
      trainingset[,tn]=as.character(trainingset[,tn])
  }
  trainingset$DataType[trainingset$DataType=="float"]=4
  trainingset$DataType[trainingset$DataType=="double"]=8
  trainingset$DataType=as.integer(trainingset$DataType)
  
  trainingset$ProblemSizeSum=unlist(lapply(trainingset$ProblemSize,getProblemSizeSum))
  trainingset$ProblemSize=NULL
  
  
  #Unrolling training
  unrolling_data=subset(trainingset,OptType=="Unrolling",)
  unrolling_data$OptConfig=as.integer(unrolling_data$OptConfig)
  
  #Tiling training
  tiling_data=subset(trainingset,OptType=="Tiling",)
  tiling_data$OptConfig<-unlist(lapply(tiling_data$OptConfig,getTilingsize))
  
  preprocessed=list(unrolling=unrolling_data,tiling=tiling_data)
  return (preprocessed);
}


trainingset=performanceDB.SQL.preprocess(trainingset)

unrollingdata=trainingset$tiling
 


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
  print(inscmd)
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
newdata=r[1,]
newdata=newdata[-"id"]
rid=performanceDB.SQL.insert(r[1,])



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

performanceDB.SQL.insert(r)
performanceDB.SQL.update(r) 
performanceDB.SQL.delete("type='float'")


r=performanceDB.SQL.select("SELECT * FROM hps.experiment where OptType='Tiling_CUDA'  ;")




performanceDB.rightmodel=function(trainingset)
{
  rightmodel<-lm(formula=
                   Gflops~I(ProblemSize^3)+I(ProblemSize^2)+ProblemSize+I(1/ProblemSize)+ProblemSize:+
                   Fdensity+I(1/Fdensity)+I(Fdensity^2)+I(Fdensity^3)+I(Fdensity^4)+
                   #  realworkset+I(realworkset^2)+I(realworkset^3)+I(realworkset^4)+I(realworkset^5)+I(1/realworkset)+I((1/realworkset)^2)+
                   OptConfig+I(unrolling_overhead)+I(ProblemSize/OptConfig)+
                   nradius:ProblemSize+num_unalign+num_align+
                   OptConfig+I(OptConfig^2)+I(OptConfig^3)+I(OptConfig^4)+I(1/OptConfig)+I(1/OptConfig^2)+
                   Fdensity:I(1/ProblemSize),
                 data=trainingset) 
  rightmodel$coefficients=abs(rightmodel$coefficients)
  return(rightmodel)
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


rightmodel.performanceDB=function(trainingset)
{
  rightmodel<-lm(formula=
                   Gflops~I(programsize^3)+I(programsize^2)+programsize+I(1/programsize)+programsize:+
                   Fdensity+I(1/Fdensity)+I(Fdensity^2)+I(Fdensity^3)+I(Fdensity^4)+
                   realworkset+I(realworkset^2)+I(realworkset^3)+I(realworkset^4)+I(realworkset^5)+I(1/realworkset)+
                   config+I(unrolling_overhead)+I(programsize/config)+
                   nradius:programsize+I((1/realworkset)^2)+num_unalign+num_align+
                   config+I(config^2)+I(config^3)+I(config^4)+I(1/config)+I(1/config^2)+
                   Fdensity:I(1/programsize),
                 data=trainingset) 
  rightmodel$coefficients=abs(rightmodel$coefficients)
  return(rightmodel)
} 







distance.performanceDB=function(a,b)
{
  if(argument_right=="no init")
    argument_right=rightmodel.performanceDB(thetrainingset)
  dif=abs(a-b)
  return (predict(argument_right,dif));
}


if(FALSE)
{
  newdata=data.frame("PlatformId"=1)
  newdata$FunctionName="testfunction"
  newdata$Steps=111
  newdata$ProgramingModel="testmodel"
  newdata$ProblemSize="128,128,128"
  snames=c("FunctionName","Steps","ProgramingModel","ProblemSize")
  
  newdata$OptType="testopt"
  newdata$OptConfig="321"
  newdata$Gflops=30
  vnames=c("OptType","OptConfig","Gflops")
  performanceDB.update(newdata,snames,vnames)
  performanceDB.getconfig(newdata,snames,2)
}
performanceDB.distance=function(a,b)
{
  if(argument_right=="no init")
  {
    SpecDB=performanceDB.SQL.select("TRUE","hps","specifics")
    
    for(Spec in Spec)
      
      
      
      
      argument_right=performanceDB.rightmodel(thetrainingset)
  }
  dif=abs(a-b)
  return (predict(argument_right,dif));
}

sqlcondition_cmd.performanceDB=function(condition_data)
{
  arguments=names(condition_data)
  cmd_str="" 
  for(arg in arguments[1:length(arguments)])
  {
    if(is.character(condition_data[,arg]))
      condit=sprintf('%s="%s"',arg,condition_data[,arg])
    else
      condit=sprintf('%s=%s',arg,condition_data[,arg])
    
    if(arg==arguments[1])
      cmd_str=condit
    else
      cmd_str=paste(cmd_str,condit,sep=' and ')
  }
  return (cmd_str)
}

search.performanceDB=function(db,search.condition)
{
  search.result=eval(parse(text=sprintf("subset(db,%s,)",search.condition)) )  
  return(search.result)
}
search2.performanceDB=function(db,condition_df)
{
  conditions_str="" 
  for(arg in names(condition_df))
  {
    if(is.character(condition_df[,arg]))
      condit=sprintf('%s=="%s"',arg,condition_df[,arg])
    else
      condit=sprintf('%s==%s',arg,condition_df[,arg])
    
    if(arg==names(condition_df)[1])
      conditions_str=condit
    else
      conditions_str=paste(conditions_str,condit,sep=' and ')
  }
  result=eval(parse(text=sprintf("subset(db,%s,)",conditions_str)) )  
  return(result)
}

sqlsearch.performanceDB=function(db,condition_data,orderby="Gflops")
{
  condition_cmd=sqlcondition_cmd.performanceDB(condition_data)
  select_cmd=sprintf("select * from db where %s order by %s",condition_cmd,orderby)
  result=sqldf(select_cmd)
  return (result)
}




updateGflops.performanceDB=function(db,update.condition,update.data,right=1.1)
{ 
  attach(db)  
  instance=search.performanceDB(db,update.condition)
  instance.updateGflops.performanceDB_lenghtnot_1=function(x) return (x==1)
  stopifnot(instance.updateGflops.performanceDB_lenghtnot_1(nrow(instance) ))
  eval(parse(text=sprintf("db$Gflops[which(%s)]=(Gflops+update.data*right)/(right+1)",update.condition)) ) 
  detach(db)
  return(db)
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
    print(vdata)
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




getconfig.performanceDB=function(condition_df,number,performanceDB)
{
  condition_names=names(condition_df)
  
  
  SameInDB=search2.performanceDB(performanceDB,condition_df)
  if(nrow(SameInDB)==0)
  {
    #find simliar point
    similarpoint=performanceDB[distance.performanceDB(performanceDB[condition_names],condition_df)
                               ==min(distance.performanceDB(performanceDB[condition_names],condition_df))]
    if(nrow(similarpoint)>=number)
      return (similarpoint[1:number,])
    else
      return (similarpoint)
  }
  else if(nrow(SameInDB)>=number)
  {
    SameInDB=SameInDB[order(SameInDB$Gflops),]
    return(SameInDB[1:number,])
  }
  else
  {
    SameInDB=SameInDB[order(SameInDB$Gflops),]
    return(SameInDB)
  }
}



performanceDB.getVariants=function(data,snames,number)
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
  SameSpecInDB=performanceDB.SQL.select(conditions_str,"hps","specifics")
  if(nrow(SameSpecInDB)==0)
  {
    #find simliar point
    SpecDB=performanceDB.SQL.select("TRUE",dbname="hps",tbname="specifics")
    similarSpec=SpecDB[performanceDB.distance(SpecDB[sname],data[sname])
                       ==min(performanceDB.distance(SpecDB[sname],data[sname]))]
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










rightmodel2.performanceDB=function(trainingset)
{
  rightmodel<-lm(formula=
                   Gflops~Fdensity+I(1/Fdensity)+I(Fdensity^2)+I(Fdensity^3)+I(Fdensity^4)+
                   realworkset+I(realworkset^2)+I(realworkset^3)+I(realworkset^4)+I(realworkset^5)+I(1/realworkset),
                 data=trainingset)
  return(rightmodel)
}








if(FALSE)
{
  target_arguments=c("OptType","programsize","Fdensity","workset","unrolling_overhead","config","L1CacheSize")
  perf_arguments=append(target_arguments,"Gflops")
  
  PDB=Unrolling_data[100,perf_arguments]
  
  for(i in 1:nrow(Unrolling_data))
  {
    instance=Unrolling_data[i,]
    PDB=update.performanceDB("unrolling",instance,target_arguments,PDB)
  }
  
}














