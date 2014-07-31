library(RODBC)
global.conn <- NA  # hold the global connection of the mysql dababase
rightmodel.att <- c("L1CacheSize", "L2CacheSize", "L3CacheSize",
                    "CoreNumber", "ThreadsPerCore", "frequency",  # Specifics attributes
                    "ProblemSize", "DataType", "Fdensity", 
                    "workset", "n_add", "n_sub", "n_mul", "n_div",
                    "loop_radius", "num_align", "num_unalign", 
                    "num_array", "num_readcachelines", "OptType",  # OptVariant attributes
                    "Gflops")

getVariant.att <- c("L1CacheSize", "L2CacheSize", "L3CacheSize",
                    "CoreNumber", "ThreadsPerCore", "frequency",  # Specifics attributes
                    "ProblemSize_x", "ProblemSize_y", "ProblemSize_z",
                    "DataType", "Fdensity", "workset",
                    "n_add", "n_sub", "n_mul", "n_div", 
                    "loop_radius_x", "loop_radius_y", "loop_radius_z",
                    "num_align", "num_unalign", "num_array", 
                    "num_readcachelines" # OptVariant attributes
)

#global.model <- list(Tiling = NA, CUDABlocking = NA,
#     Unrolling = NA, DoSIMD = NA, OMP = NA) # The right model


global.model <- list(cuda = NA, cpu = NA )# the similar model 
#Unrolling = NA, DoSIMD = NA, OMP = NA) # The right model


linkvalues <- function(df, linker = ",") {
  # Link value in dataframe with the linker and get a string
  # 
  # Args:
  #   df: Dataframe need to be linked
  #   linker: The linker string
  # 
  # Returns:
  #   The linked string
  linked_str <- ""
  for(value in df[1:length(df)]) { 
    if(is.character(value)) {
      strs_ <- sprintf("'%s'", value)
    }
    else if(is.factor(value)) {
      strs_ <- sprintf("'%s'", as.character(value))
    }
    else if(is.na(value)) {
      strs_ <- sprintf("%d", -1)
    }
    else if(is.integer(value)) {
      strs_ <- sprintf("%d", value)
    }
    else if(is.numeric(value)) {
      strs_ <- sprintf("%f",value)
    }
    
    if(linked_str == "") {
      linked_str <- strs_
    }
    else {
      linked_str <- paste(linked_str, strs_, sep = linker)
    }
  }
  return(linked_str)
}

performanceDB.SQL.dbopen <- function() {
  # Open a SQL conn if the global.conn has not been initialized,
  # and save the conn to global.conn, or just return global.conn
  #
  # Returns:
  #   The conn opened or geted from the global
  if(is.na(global.conn)) {
    conn <- odbcConnect("myhps","hps","hps")
  }
  else {
    conn <- global.conn
  }
  return(conn)
}

performanceDB.SQL.dbclose <- function(conn) {
  # Close the connection 
  #
  # Args:
  #   conn: The conn opened by performanceDB.SQL.dbopen
  close(conn)
}

performanceDB.SQL.select <- function(selectcondition = "true", 
                                     dbname = "hps", tbname = "testinsert") {
  # Do searching operation in database 'dbname' table  'tbname' 
  # using condition 'selectcondition'
  # 
  # Args:
  #   selectcondition: The condiction use to filter the result
  #   dbname: The database name
  #   tbname: The table name
  #
  # Returns:
  #   The dataframe which has been selected
  selcmd <- sprintf('select * from %s.%s where %s;', 
                    dbname, tbname, selectcondition)
  conn <- performanceDB.SQL.dbopen()
  result <- sqlQuery(conn,selcmd)
  
  return(result)  
}

performanceDB.SQL.selectall <- function(attnames, condition="true") {
  # Select all the data with the given attnames in three joined table:
  # platform, specifics, optVariants
  #
  # Args:
  #   attnames: The attribute need to select
  # 
  # Returns:
  #   The dataframe which has been selected
  names_str <- paste(attnames, collapse = ",")
  selcmd <- sprintf('select %s from specifics join platform on(specifics.PlatformId=platform.id) join optVariant on (specifics.id=optVariant.SpecificsId) where %s ;', names_str ,condition)
  conn <- performanceDB.SQL.dbopen()
  sqlQuery(conn,"use hps")
  result <- sqlQuery(conn,selcmd)
  return(result)  
}


performanceDB.SQL.selectSpecifics <- function(attnames, condition="true") {
  # Select all the speifics data with the given attnames in three joined table:
  # platform, specifics, optVariants
  #
  # Args:
  #   attnames: The attribute need to select
  # 
  # Returns:
  #   The dataframe which has been selected
  names_str <- paste(attnames, collapse = ",")
  selcmd <- sprintf('select %s from specifics join platform on(specifics.PlatformId=platform.id) where %s ;', names_str ,condition)
  conn <- performanceDB.SQL.dbopen()
  sqlQuery(conn,"use hps")
  result <- sqlQuery(conn,selcmd)
  return(result)  
}





performanceDB.SQL.selectspecificsWithSameOptType <- function(attnames, 
                                                             opttype, 
                                                             condition = "") {
  # Select all attribute that given by 'attnames' using select condition 'condition'
  # in two joined table: platform and specifics. Especially the attribute optType of
  # result should equal to 'opttype'
  # 
  # Args:
  #   attnames: The attribute should be selected
  #   opttype: the opttype condition, such as Unrolling, Tiling
  #   condition: the condition area in sql statement
  # 
  # Returns:
  #   The dataframe which has been selected
  attnames <- append(attnames, "specifics.id")
  names_str <- paste(attnames, collapse = ",")
  if(condition != "") {
    condition <- paste(condition, "and")
  }
  
  selcmd <- sprintf('select %s from specifics join platform on (specifics.PlatformId = platform.id) where %s specifics.id in (select SpecificsId from optVariant where OptType = "%s");',
                    names_str, condition, opttype)
  conn <- performanceDB.SQL.dbopen()
  sqlQuery(conn,"use hps;")
  result <- sqlQuery(conn,selcmd)
  return(result)  
}

performanceDB.getPlatformId <- function(platform)
{
  pnames=names(platform)
  conditions_str <- "" 
  for(pname in pnames[1:length(pnames)]) {
    if(is.character(platform[,pname])) {
      condit <- sprintf('%s="%s"', pname, platform[,pname])
    }
    else {
      condit <- sprintf('%s=%s', pname, platform[,pname])
    }
    
    if(pname == pnames[1]) {
      conditions_str <- condit
    }
    else {
      conditions_str <- paste(conditions_str, condit, sep =' and ')
    }
  }
  # Search in performance database
  targetInDB <- performanceDB.SQL.select(conditions_str, "hps", "platform")
  if(nrow(targetInDB) == 0) 
  {
    tmp <- performanceDB.SQL.insert(platform,dbname = "hps", tbname = "platform")
    pid <- tmp[1,]
    return (pid)
  }
  if(nrow(targetInDB) >=1)
  { 
    pid <- targetInDB$id
    return (pid)
  }
}


performanceDB.update <- function(newdata, pnames, snames, vnames) {
  # update the database
  #
  # Args:
  #   newdata: A dataframe that contains the needed data
  #   snames: The column name need to insert to 'specifics' table
  #   vname: The column name need to insert to 'variant' table
  newdata$PlatformId <- performanceDB.getPlatformId(newdata[pnames])
  snames <- append(snames,"PlatformId")
  conditions_str <- "" 
  for(sname in snames[1:length(snames)]) {
    if(is.character(newdata[,sname])) {
      condit <- sprintf('%s="%s"', sname, newdata[,sname])
    }
    else {
      condit <- sprintf('%s=%s', sname, newdata[,sname])
    }
    
    if(sname == snames[1]) {
      conditions_str <- condit
    }
    else {
      conditions_str <- paste(conditions_str, condit, sep =' and ')
    }
  }
  # Search in performance database
  targetInDB <- performanceDB.SQL.select(conditions_str, "hps", "specifics")
  
  if(nrow(targetInDB) == 0) {
    tmp <- performanceDB.SQL.insert(newdata[snames],dbname = "hps", tbname = "specifics")
    sid <- data.frame("SpecificsId" = tmp[1,])
    vdata <- cbind(newdata[vnames], sid)
    performanceDB.SQL.insert(vdata, dbname="hps", tbname="optVariant")
  }
  else if(nrow(targetInDB) == 1) { 
    specificId <- targetInDB$id
    conditions_str <- sprintf('%s=%d and %s="%s" and %s="%s"',
                              "SpecificsId", specificId,
                              "OptType", newdata[,"OptType"],
                              "OptConfig", newdata[,"OptConfig"])
    targetInDB2 <- performanceDB.SQL.select(conditions_str, "hps", "optVariant")
    if(nrow(targetInDB2) == 0) { 
      variantdata <- cbind(newdata[vnames], data.frame("SpecificsId" = specificId))
      performanceDB.SQL.insert(variantdata,dbname = "hps",tbname = "optVariant")
    }
    else if(nrow(targetInDB2) == 1) {
      factor <- 1.2
      variantdata <- cbind(newdata[vnames], data.frame("SpecificsId" = specificId))
      variantdata$Gflops <- (targetInDB2$Gflops + 
                               variantdata$Gflops * factor) / (1 + factor)
      variantdata$id <- targetInDB2$id
      performanceDB.SQL.update(variantdata,dbname = "hps", tbname = "optVariant")
    }
  }
}

performanceDB.SQL.rawdatapreprocess <- function(trainingset) {
  # Perform preprocess on raw data during performing update process
  # 
  # Args: 
  #   trainingset: The raw data, it is the data from experiment data 
  # 
  # Returns:
  #   The preprocessed data
  
  tnames <- names(trainingset)
  for(tn in tnames) {
    if(is.factor(trainingset[1,tn])) {
      trainingset[,tn] <- as.character(trainingset[,tn])
    }
  }
  
  # Count Gflops
  trainingset$ProblemSizeSum <- unlist(lapply(trainingset$ProblemSize, GetProblemSizeSum))
  trainingset$Gflops <- trainingset$Steps * trainingset$ProblemSizeSum * 
    trainingset$Fdensity / trainingset$P_WALL_CLOCK_TIME/1000
  
  # Filter attribution
  trainingset$id <- NULL
  
  
  trainingset$ProblemSizeSum <- NULL 
  trainingset$P_WALL_CLOCK_TIME <- NULL
  trainingset$PAPI_FP_INS <- NULL
  trainingset$PAPI_LD_INS <- NULL
  trainingset$PAPI_L1_DCM <- NULL
  trainingset$PAPI_L2_TCM <- NULL
  trainingset$OptType[trainingset$OptType == ""] <- "Origin"
  
  
  return(trainingset);
}


GetProblemSizeSum <- function(strs) {
  # Get the total size of the problem
  #
  # Args:
  #   strs: The problemSize strs like '128,128,128'
  # 
  # Returns:
  #   The product of fields of the strs split by ',', such as 128*128*128
  tmparray <- unlist(strsplit(strs, ","))
  tmparray <- as.integer(tmparray)
  pss <- 1;
  
  for(tmp in tmparray) {
    pss <- pss*tmp
  }
  
  return(pss)
}

performanceDB.SQL.preprocessByOptType <- function(trainingset) {
  # Preprocess the data in trainingset and split the trainingset into 
  # multiple parts by 'opttype' and return the collection
  # 
  # Args:
  #   trainingset: The trainingset data
  #
  # Returns:
  #   The collection of each opttype data
  trainingset <- performanceDB.preprocess2num(trainingset)
  
  # Unrolling training
  unrolling_data <- subset(trainingset, OptType == "Unrolling",)
  
  # OMP training
  omp_data <- subset(trainingset, OptType == "OMP",)
  
  # Tiling training
  tiling_data <- subset(trainingset, OptType == "Tiling",)
  
  # CUDABlocking tranining
  cudablock_data <- subset(trainingset, OptType == "CUDABlocking",)
  
  # SIMD training
  dosimd_data <- subset(trainingset, OptType == "DoSIMD",)
  
  preprocessed <- list(Unrolling = unrolling_data, Tiling = tiling_data,
                       OMP = omp_data, CUDABlocking = cudablock_data,
                       DoSIMD = dosimd_data)
  return(preprocessed);
}

performanceDB.SQL.delete <- function(deletecondition,
                                     dbname = "hps", tbname = "testinsert") {
  # Delete the data from database
  # 
  # Args:
  #   deletecondition: The condition string in the where block of sql
  #   dbname: The database name
  #   tbname: The table name
  
  delcmd <- sprintf('delete from %s.%s where %s',
                    dbname, tbname, deletecondition)
  conn <- performanceDB.SQL.dbopen()
  result <- sqlQuery(conn,delcmd)
}

performanceDB.SQL.insert <- function(data, dbname = "hps", 
                                     tbname = "testinsert") {
  # Perform a insert operation using 'data' in table 'tbname' of database 'dbname'
  #
  # Args:
  #   data: The data need to insert to database
  #   dbname: The database name
  #   tbname: The table name
  #
  # Returns:
  #   The last insert it
  stopifnot(nrow(data) == 1)
  db_tb <- sprintf('%s.%s', dbname, tbname)
  filter1 <- names(data) %in% c("id")
  data <- data[!filter1]
  s1 <- paste(names(data), collapse = ",")
  s2 <- linkvalues(data,",")
  inscmd <- sprintf('insert into %s(%s) values(%s);', db_tb, s1, s2)
  conn <- performanceDB.SQL.dbopen()
  sqlQuery(conn, inscmd)
  tableid <- sqlQuery(conn, "select last_insert_id()")
  return(tableid)
}

performanceDB.SQL.insertmany <- function(data, dbname = "hps",
                                         tbname = "testinsert") {
  # Insert some data to the database
  # 
  # Args:
  #   data: Some data row
  #   dbname: The database name
  #   tbname: The table name
  #
  # Returns:
  #   The last insert it
  db_tb <- sprintf('%s.%s', dbname, tbname)
  conn <- performanceDB.SQL.dbopen()
  sqlSave(conn, data, append = TRUE, tablename = db_tb);
  tableid <- sqlQuery(conn,"select last_insert_id()");
  return(tableid)
}

performanceDB.SQL.update <- function(data, dbname = "hps",
                                     tbname = "testinsert"){
  # Perform a update operation using 'data' in table 'tbname' of database 'dbname'
  #
  # Args:
  #   data: The data need to insert to database
  #   dbname: The database name
  #   tbname: The table name
  
  db_tb <- sprintf('%s.%s', dbname, tbname)
  conn <- performanceDB.SQL.dbopen()
  
  tmpdata <- data
  tmpdata$id <- NULL
  dnames <- names(tmpdata)
  
  updcmd <- ""
  for(dn in dnames[1:length(dnames)]) {
    if(is.character(data[,dn])) {
      upd_str <- sprintf('%s="%s"', dn, data[,dn])
    }
    else {
      upd_str <- sprintf('%s=%s', dn, data[,dn])
    }
    
    if(updcmd == "") {
      updcmd <- upd_str
    }
    else {
      updcmd <- paste(updcmd, upd_str, sep = ',')
    }
  }
  updcmd <- sprintf("UPDATE %s.%s set %s where id = %d", 
                    dbname, tbname, updcmd, data$id)
  sqlQuery(conn, updcmd)
}

performanceDB.distance <- function(a, b, pm) {
  # Calculate the similarity of two specific vector which based on optimizing
  # technology 'opttype'
  #
  # Args: 
  #   a: Specifics vector a
  #   b: Specifics vector b
  #   pm: The programming model of the two Specifics.
  # 
  # Returns:
  #    The distance of the two Specifics vector
  
  rightmodel <- performanceDB.BuildSimilarModel(pm)
  dif <- abs(a - b)
  r <- predict(rightmodel, dif)# - rightmodel$coefficients[[1]]
  return(r);
}

performanceDB.similarity <- function(a, b, pm,opttype="") {
  # Calculate the similarity of two specific vector which written in specific programming model 
  #
  # Args: 
  #   a: Specifics vector a
  #   b: Specifics vector b
  #   pm: The programming model of the two Specifics.
  # 
  # Returns:
  #    The similarity of the two Specifics vector
  
  rightmodel <- performanceDB.BuildSimilarModel(pm,opttype)
  dif <- abs(a - b)
  r <- predict(rightmodel, dif)# - rightmodel$coefficients[[1]]
  return(r);
}



performanceDB.rightmodel <- function(opttype) {
  # Build the right model for opttype, especially, if the model has not been
  # trained before, the new model will be trained and stored in the global 
  # variable global.model, or it just return the model in the global variable
  #
  # Args:
  #   opttype: The opttype if the righe model, such as Tiling, Unrolling
  #
  # Returns:
  #   The model of opttype
  if(anyNA(global.model[[opttype]])) {
    trainingset <- performanceDB.SQL.selectall(rightmodel.att)
    trainingset <- performanceDB.SQL.preprocessByOptType(trainingset)
    Optset <- trainingset[[opttype]] 
    Optset$OptType <- NULL
    allnames <- names(Optset)
    thegflops <- allnames %in% c("Gflops")
    trainingname <- allnames[!thegflops]
    model_str <- formula.generate(trainingname,2)
    print(sprintf("=========== generate %s model=========",opttype))
    rightmodel <- eval(parse(text=sprintf("rightmodel<-lm(formula=Gflops~%s,data=Optset) ",model_str)))
    rightmodel$coefficients <- abs(rightmodel$coefficients)
    global.model[[opttype]]<<-rightmodel
  }
  else {
    rightmodel <- global.model[[opttype]]
  }
  return(rightmodel)
} 

performanceDB.GetVariantInfo <- function(sid,opttype="",number=-1, omp=TRUE){
  conn <- odbcConnect("myhps","hps","hps")
  if(opttype=="") {
    if (omp) {
      selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d order by Gflops desc;',sid)
    } else {
      selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d and instr(OptType,"omp")=0 order by Gflops desc;',sid)
    }
  }
  else {
    selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d and OptType="%s" order by Gflops desc;',sid,opttype)
  }
  sqlQuery(conn,"use hps")
  result <- sqlQuery(conn,selcmd,stringsAsFactors = FALSE)
  close(conn)
  # print(nrow(result))
  if(number==-1)
    return (result)
  else 
    return (result[1:number,])
}

performanceDB.GetVariantInfo_noOrder <- function(sid,opttype="",number=-1,omp=TRUE){
  conn <- odbcConnect("myhps","hps","hps")
  if(opttype=="") {
    if (omp) {
      selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d;',sid)
    } else {
      selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d and instr(OptType,"omp")=0;',sid)
    }
  }
  else {
    selcmd <- sprintf('select Gflops, OptConfig from optVariant where SpecificsId=%d and OptType="%s" ;',sid,opttype)
  }
  sqlQuery(conn,"use hps")
  result <- sqlQuery(conn,selcmd,stringsAsFactors = FALSE)
  close(conn)
  # print(nrow(result))
  if(number==-1)
    return (result)
  else 
    return (result[1:number,])
}


performanceDB.GetSimilarNumber <- function(A,B)
{
  A <- as.data.frame(A,stringsAsFactors = FALSE)[!is.na(A),]
  B <- as.data.frame(B,stringsAsFactors = FALSE)[!is.na(B),]
  both <- union(A,B)
  equals <- length(A)+length(B)-length(both)
  return (equals)
}
performanceDB.GetSimilarFactor <- function(a_id,b_id,size,opttype,ifprint=TRUE)
{
  
  result1  <- performanceDB.GetVariantInfo(a_id,opttype,size)
  result2 <- performanceDB.GetVariantInfo(b_id,opttype,size)
  similarnumber <- performanceDB.GetSimilarNumber(result1$OptConfig,result2$OptConfig)
  if(ifprint)
    print(sprintf("Similar Rate: %d/%d",similarnumber,nrow(result1)))
  return (similarnumber)
}


performanceDB.BuildSimilarModel  <- function(pm,opttype="") {
  # Build the right model for opttype, especially, if the model has not been
  # trained before, the new model will be trained and stored in the global 
  # variable global.model, or it just return the model in the global variable
  #
  # Args:
  #   opttype: The opttype if the righe model, such as Tiling, Unrolling
  #   pm: This argument determine the type of programming model  
  # Returns:
  #   The model of opttype
  if(anyNA(global.model[[pm]])) {
    print("======begin to build similar model======")
    similarModel.att <- rightmodel.att[1:(length(rightmodel.att)-2)]
    similarModel.att <- append(similarModel.att, "specifics.id")
    SpecificsSets <- performanceDB.SQL.selectSpecifics (attnames=similarModel.att,sprintf(' num_array<16 and ProgrammingModel="%s"',pm))
    SpecificsSets <- performanceDB.preprocess2num(SpecificsSets)
    trainingSet <- NA
    for(i in 1:(nrow(SpecificsSets)-1))
      for(j in (i+1):nrow(SpecificsSets))
      {
        dif <- abs(SpecificsSets[i,]-SpecificsSets[j,])
        dif$similar <- performanceDB.GetSimilarFactor(SpecificsSets[i,]$id,SpecificsSets[j,]$id,100,opttype,TRUE)
        
        if(is.na(trainingSet))
          trainingSet <- dif
        else
          trainingSet <- rbind(trainingSet, dif)
      } 
    
    trainingSet$id=NULL
    times <- -1:3
    #tmpTrainingSet <<- trainingSet
    #trainingSet<-tmpTrainingSet
    model_str <- formula.generate(names(trainingSet[1:(length(trainingSet)-1)]),times)
    print(model_str)
    similarmodel <- eval(parse(text=sprintf("rightmodel<-glm(formula=similar~%s,family=poisson,data=trainingSet) ",model_str)))
    # rightmodel$coefficients <- abs(rightmodel$coefficients)
    global.model[[pm]]<<-similarmodel
  }
  else {
    similarmodel <- global.model[[pm]]
  }
  return(similarmodel)
} 













performanceDB.preprocess2num <- function(trainingset) {
  # Preprocess the trainingset, Change some string area to number,
  # Such as: 
  #   Datatype: set float to 4 and double to 8
  #   loop_radius: get each area of loop_radius,for example '3,4,5' to 
  #                loop_radius_x(3), loop_radius_y(4), loop_radius_z(5),
  #                And then delete the loop_radius field.
  #   ProblemSize: The same as loop_radius
  #
  # Args:
  #   The trainingset that need to be preprocess
  # 
  # Returns:
  #   The preprocessed trainingset
  tnames <- names(trainingset)
  for(tn in tnames) {
    if(is.factor(trainingset[1, tn])) {
      trainingset[,tn] <- as.character(trainingset[,tn])
    }
  }
  
  if("DataType" %in% tnames) {
    trainingset$DataType[trainingset$DataType == "float"] <- 4
    trainingset$DataType[trainingset$DataType == "double"] <- 8
    trainingset$DataType <- as.integer(trainingset$DataType)
  }
  
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
  
  if("loop_radius" %in% tnames) {
    trainingset$loop_radius_x <- unlist(lapply(trainingset$loop_radius,
                                               function(x) GetNumberFromStr(x, 1)))
    trainingset$loop_radius_y <- unlist(lapply(trainingset$loop_radius,
                                               function(x) GetNumberFromStr(x, 2)))
    trainingset$loop_radius_z <- unlist(lapply(trainingset$loop_radius,
                                               function(x) GetNumberFromStr(x, 3)))
    trainingset$loop_radius <- NULL
  }
  
  if("ProblemSize" %in% tnames) {
    trainingset$ProblemSize_x <- unlist(lapply(trainingset$ProblemSize,
                                               function(x) GetNumberFromStr(x, 1)))
    trainingset$ProblemSize_y <- unlist(lapply(trainingset$ProblemSize,
                                               function(x) GetNumberFromStr(x, 2)))
    trainingset$ProblemSize_z <- unlist(lapply(trainingset$ProblemSize,
                                               function(x) GetNumberFromStr(x, 3)))
    trainingset$ProblemSize <- NULL
  }
  
  if("ProgrammingModel" %in% tnames) {
    trainingset$ProgrammingModel <- NULL
  }
  if("FunctionName" %in% tnames) {
    trainingset$FunctionName <- NULL
  }
  if("PlatformName" %in% tnames) {
    trainingset$PlatformName <- NULL
  }
  return (trainingset);
}

performanceDB.getVariants <- function(data, snames, number, opttype="" ) {
  # Get n best optimizing variants
  # 
  # Args:
  #   data: The data contains the specific of target running instance
  #   snames: Determine the  attribute name of specifics in data, used for finding 
  #           same/similar optimizion variant
  #   number: Determine the number of varaints need to return
  #   opttype: The optimizing technology that running instance would perform
  #
  # Returns:
  #   Some variants which is number size
  for(sname in snames[1:length(snames)]) {
    if(is.character(data[,sname])) {
      condit <- sprintf('%s="%s"', sname, data[,sname])
    }
    else {
      condit <- sprintf('%s=%s', sname, data[,sname])
    }
    if(sname == snames[1]) {
      conditions_str <- condit
    }
    else {
      conditions_str <- paste(conditions_str, condit, sep=' and ')
    }
  } 
  #search in performance database
  SameSpecInDB <- performanceDB.SQL.selectspecificsWithSameOptType(snames,
                                                                   opttype, 
                                                                   conditions_str)
  
  if(nrow(SameSpecInDB) == 0) {
    print("begin to find similar")
    pm <- data$ProgrammingModel
    #find simliar point
    specDB <- performanceDB.SQL.selectSpecifics (append(snames,"specifics.id"),sprintf('ProgrammingModel="%s"',pm))
    specDB <- performanceDB.preprocess2num(specDB)
    data <- performanceDB.preprocess2num(data)
    #because the name of specDB and data have been change after function preprocess2num
    snames <- names(data)
    max_similarity <- performanceDB.similarity(specDB[1, snames], data[snames], pm, opttype)
    similarSpec <- specDB[1,] 
    for(i in 2:nrow(specDB)) {
      d <- performanceDB.similarity(specDB[i, snames], data[snames], pm, opttype)
      if(d > max_similarity) {
        max_similarity <- d
        similarSpec <- specDB[i,]
      }
    }
    specId <- similarSpec$id
    print(sprintf("specifics-%d is selected with similarity %f/50 ",specId,max_similarity))
    if(opttype!="")
      selectcondition <- sprintf("SpecificsId=%d and OptType=\"%s\" order by Gflops desc limit %d", 
                                 specId,opttype, number)
    else
      selectcondition <- sprintf("SpecificsId=%d order by Gflops desc limit %d", 
                                 specId,opttype, number)
    variants  <- performanceDB.SQL.select(selectcondition,
                                          dbname="hps", tbname="optVariant")
    return(variants)
  }
  else if(nrow(SameSpecInDB) >= 1) {
    print("There exist equal record!")
    specId <- SameSpecInDB$id
    if(opttype!="")
      selectcondition <- sprintf("SpecificsId=%d and OptType=\"%s\" order by Gflops desc limit %d", 
                                 specId,opttype, number)
    else
      selectcondition <- sprintf("SpecificsId=%d order by Gflops desc limit %d", 
                                 specId,opttype, number)
    variants <- performanceDB.SQL.select(selectcondition,
                                         dbname="hps", tbname="optVariant")
    return(variants)
  }
}

formula.generate <- function(factors, times) {
  # Generate the formula
  # Args:
  #   factors: The factors of the formula,such as Fdencity,frequency
  #   times: The time range of each factor such as 1:5
  #
  # Returns: 
  #   The generated factor like I(1/Fedencity) + Fencity
  # 
  
  stopifnot(is.character(factors))
  stopifnot(is.numeric(times))
  
  fomula_str <- c()
  for (factor_item in factors) {
    for (time_item in times) {
      if(time_item == 0){
        next
      } else {
        if(time_item>0){
          tmp <- paste("I(", factor_item, "^", time_item, ")")
        } else {
          tmp <- paste("I((", factor_item, "+0.001)^", time_item, ")")}
      }
      fomula_str <- c(fomula_str, tmp)
    }
  }
  return(paste(fomula_str, collapse = "+"))
}



#getVariant test
getVariantTest<-function()
{
  global.conn <<- odbcConnect("myhps","hps","hps") 
  if(file.exists("globalmodels.saved"))
  {
    print("detect model file, loading------")
    load("globalmodels.saved")
    global.model <<- global.model
    print("model file is loaded------------")
  }
  tmp=rightmodel.att
  
  testdata0=performanceDB.SQL.selectall(tmp)
  testdata1=performanceDB.SQL.preprocessByOptType(testdata0)
  testdata2=testdata1[["Tiling"]]
  testdata3=testdata2[1,]
  
  testdata3$OptType=NULL
  testdata3$Gflops=NULL
  testdata3$ProblemSizeSum=NULL
  testdata3$ProblemSize="128,128,256"
  testdata3$DataType="float"
  testdata3$loop_radius="1,1,1"
  testdata3$ProblemSizeSum=NULL
  
  
  testdata3$ProblemSize_x=NULL
  testdata3$ProblemSize_y=NULL
  testdata3$ProblemSize_z=NULL
  testdata3$loop_radius_x=NULL
  testdata3$loop_radius_y=NULL
  testdata3$loop_radius_z=NULL
  testdata3$ProgrammingModel="cuda"
  
  snames=c("L1CacheSize","L2CacheSize","L3CacheSize","CoreNumber","ThreadsPerCore","frequency",
           #specifics
           "ProblemSize","DataType","Fdensity","workset","n_add","n_sub","n_mul","n_div",
           "loop_radius","num_align","num_unalign","num_array","num_readcachelines")
  
  # print(testdata3)
  bestvariants=performanceDB.getVariants(testdata3,snames,3,"CUDABlocking")
  print("get best variants")
  print(bestvariants)
  save(global.model,file="globalmodels.saved")
  close(global.conn)
}





Commandline.getVariants <- function(data_str, opttype, nbegin, n, outputname="theoutput")
{ 
  dataItems <- unlist(strsplit(data_str, ":"))
  datadf_str <- ""
  for(item in dataItems)
  {
    itempair <- unlist(strsplit(item, "="))
    if(datadf_str!="")
      datadf_str <- paste(datadf_str,sprintf("%s=%s",itempair[1],itempair[2]),sep=",")
    else
      datadf_str <- sprintf("%s=%s",itempair[1],itempair[2])
  }
  datadf <- eval(parse(text=sprintf("data.frame(%s,stringsAsFactors = FALSE)",datadf_str)))
  bestvariants <- performanceDB.getVariants( datadf, names(datadf), n, opttype)
  bestconfigs <- as.character(bestvariants$OptConfig)
  print(bestvariants)
  sink(outputname)
  for(i in 1:length(bestconfigs))
    cat(sprintf("%s\n",bestconfigs[i]))
  sink()
  
  print("Commandline.getVariants running successfully!")
}





if(FALSE)
{
  platform_str="L1CacheSize=32:L2CacheSize=256:L3CacheSize=256:CoreNumber=16:ThreadsPerCore=1:frequency=2.6"
  specific_str="ProgramingModel='cpu':Fdensity=8:workset=72:workset_inc=5:n_add=5:n_sub=1:n_mul=2:n_div=0:loop_radius='1,1,1':num_align=6:num_unalign=2:num_array=2:ProblemSize='512,512,512':DataType='double':FunctionName='STENCIL_3D_7P':Steps=10"
  variant_str="OptType='Unrolling':OptConfig='100':P_WALL_CLOCK_TIME=0."
  Commandline.Update(platform_str,specific_str,variant_str)
  
  Commandline.getVariants("Fdensity=8:workset=72:workset_inc=5:n_add=5:
                          n_sub=1:n_mul=2:n_div=0:loop_radius='1,1,1':
                          num_align=6:num_unalign=2:num_array=2:
                          ProblemSize='512,512,512':DataType='double':
                          Steps=10:L1CacheSize=32:L2CacheSize=256:
                          L3CacheSize=256:CoreNumber=16:num_readcachelines=2:
                          ThreadsPerCore=1:frequency=2.6:ProgrammingModel='cpu'", "DoSIMD",1,3,"opt")
  
}




Commandline.Update <- function(platform_str,specific_str,variant_str)
{ 
  allstr=list(p=platform_str,s=specific_str,v=variant_str)
  for( i in 1:3)
  {
    dataItems <- unlist(strsplit(allstr[[i]], ":"))
    datadf_str=""
    for(item in dataItems)
    {
      itempair <- unlist(strsplit(item, "="))
      if(datadf_str!="")
        datadf_str=paste(datadf_str,sprintf("%s=%s",itempair[1],itempair[2]),sep=",")
      else
        datadf_str=sprintf("%s=%s",itempair[1],itempair[2])
    }
    df=eval(parse(text=sprintf("data.frame(%s,stringsAsFactors = FALSE)",datadf_str)))
    if(i==1)
      platformdf=df
    if(i==2)
      specificdf=df
    if(i==3)
      variantdf=df
  }
  newdata=cbind(platformdf,specificdf,variantdf)
  newdata=performanceDB.SQL.rawdatapreprocess(newdata)
  
  performanceDB.update(newdata,names(platformdf), names(specificdf), c("OptType","OptConfig","Gflops")) 
}


#undate test
if(FALSE)
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
