argument_right="not init"
thetrainingset=Unrolling_data

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
      conditions_str=paste(conditions_str,condit,sep=' & ')
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



update.performanceDB=function(opttype,newdata,arguments,performanceDB)
{
  conditions_str="" 
  for(arg in arguments[1:length(arguments)])
  {
    if(is.character(newdata[,arg]))
      condit=sprintf('%s=="%s"',arg,newdata[,arg])
    else
      condit=sprintf('%s==%s',arg,newdata[,arg])
    
    if(arg==arguments[1])
      conditions_str=condit
    else
      conditions_str=paste(conditions_str,condit,sep=' & ')
  }
  newdata$OptType=opttype
  
  targetInDB=search.performanceDB(performanceDB,conditions_str)
  nrow(targetInDB)
  if(nrow(targetInDB)==0)
  {
    arguments=append(arguments,"Gflops")
    performanceDB=rbind(performanceDB,newdata[arguments])
  }
  else if(nrow(targetInDB)==1)
    updateGflops.performanceDB(performanceDB,conditions_str,newdata$Gflops,1.2)
  #else 
  #  stopifnot(FALSE) 
  return (performanceDB)
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

 












