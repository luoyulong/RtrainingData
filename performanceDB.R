#!/usr/bin/env Rscript
# 
# Author:        luoyulong(luoyulong@ncic.ac.cn)
# Modification:  zengping(zengping@ncic.ac.cn)
# Date:          2014-06-18
#
# Desc: This file is for build model and apply some function to operate the database.

library(RODBC)

global.conn <- NA  # hold the global connection of the mysql dababase
global.model <- list(Tiling = NA, CUDABlocking = NA,
                     Unrolling = NA, Dosimd = NA, OMP = NA) # The right model

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

performanceDB.SQL.selectall <- function(attnames) {
  # Select all the data with the given attnames in three joined table:
  # platform, specifics, optVariants
  #
  # Args:
  #   attnames: The attribute need to select
  # 
  # Returns:
  #   The dataframe which has been selected
  names_str <- paste(attnames, collapse = ",")
  selcmd <- sprintf('select %s from specifics join platform on 
                    (specifics.PlatformId=platform.id) join optVariant 
                    on (specifics.id=optVariant.SpecificsId);', names_str)
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

  selcmd <- sprintf('select %s from specifics join platform on 
                    (specifics.PlatformId = platform.id) where %s specifics.id in 
                    (select SpecificsId from optVariant where OptType = "%s");',
                    names_str, condition, opttype)
  conn <- performanceDB.SQL.dbopen()
  sqlQuery(conn,"use hps;")
  result <- sqlQuery(conn,selcmd)
  return(result)  
}

performanceDB.update <- function(newdata, snames, vnames) {
  # update the database
  #
  # Args:
  #   newdata: A dataframe that contains the needed data
  #   snames: The column name need to insert to 'specifics' table
  #   vname: The column name need to insert to 'variant' table
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

performanceDB.distance <- function(a, b, opttype) {
  # Calculate the similarity of two specific vector which based on optimizing
  # technology 'opttype'
  #
  # Args: 
  #   a: Specifics vector a
  #   b: Specifics vector b
  #   opttype: The opttype of the two Specifics.
  # 
  # Returns:
  #    The distance of the two Specifics vector

  rightmodel <- performanceDB.rightmodel(opttype)
  dif <- abs(a - b)
  #subtract the intercept
  r <- predict(rightmodel, dif) - rightmodel$coefficients[[1]]
  return(r);
}

performanceDB.rightmodel <- function(opttype="Tiling") {
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
    trainingset$loop_radius_x <- unlist(lapply(trainingset$ProblemSize,
                                               function(x) GetNumberFromStr(x, 1)))
    trainingset$loop_radius_y <- unlist(lapply(trainingset$ProblemSize,
                                               function(x) GetNumberFromStr(x, 2)))
    trainingset$loop_radius_z <- unlist(lapply(trainingset$ProblemSize,
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
  return (trainingset);
}

performanceDB.getVariants <- function(data, snames, number, opttype = "Tiling") {
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
  conditions_str <- ""
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
    #find simliar point
    specDB <- performanceDB.SQL.selectspecificsWithSameOptType(snames, opttype)
    specDB <- performanceDB.preprocess2num(specDB)
    data <- performanceDB.preprocess2num(data)
    #because the name of specDB and data have been change after function preprocess2num
    snames <- names(data)
    min_d <- performanceDB.distance(specDB[1, snames], data[snames], opttype)
    similarSpec <- specDB[1,] 
    for(i in 2:nrow(specDB)) {
      d <- performanceDB.distance(specDB[i, snames], data[snames], opttype)
      if(d < min_d) {
        min_d <- d
        similarSpec <- specDB[i,]
      }
    }
    specId <- similarSpec$id
    selectcondition <- sprintf("SpecificsId=%d order by Gflops desc limit %d",
                               specId, number)
    variants  <- performanceDB.SQL.select(selectcondition,
                                          dbname="hps", tbname="optVariant")
    return(variants)
  }
  else if(nrow(SameSpecInDB) >= 1) {
    specId <- SameSpecInDB$id
    selectcondition <- sprintf("SpecificsId=%d order by Gflops desc limit %d", 
                               specId, number)
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
      if(time_item == 1) {
        tmp = factor_item
      } else if(time_item == 0){
        next
      } else {
        tmp <- paste("I(", factor_item, "^", time_item, ")")
      }
      fomula_str <- c(fomula_str, tmp)
    }
  }
  
  return(paste(fomula_str, collapse = "+"))
}
