
source("../performanceDB.R")
#getVariant test
if(FALSE)
{
  global.conn=performanceDB.SQL.dbopen() 
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


#undate test
if(TRUE)
{
  global.conn=performanceDB.SQL.dbopen()
  rawdata=performanceDB.SQL.select("TRUE",dbname="hps",tbname="experiment");
  rawdata=performanceDB.SQL.rawdatapreprocess(rawdata);
  allnames=names(rawdata)
  vnames=c("OptType","OptConfig","Gflops")
  vnames2=allnames %in% vnames
  snames=allnames[!vnames2]
  for(i in 1:nrow(rawdata))
    performanceDB.update(newdata=rawdata[i,],snames=snames,vnames=vnames)
  close(global.conn)
}
