#!/usr/bin/env Rscript
# 
# Author:        luoyulong(luoyulong@ncic.ac.cn)
# Modification:  zengping(zengping@ncic.ac.cn)
# Date:          2014-06-18
#
# Desc: This file is for build model and apply some function to operate the database.

library(RODBC)
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
TOPDIR <- dirname(frame_files[[length(frame_files)]])
setwd(TOPDIR)
source("pDBfunctions.R")



global.conn <- odbcConnect("myhps","hps","hps") 
if(file.exists("globalmodels.saved"))
{
  print("detect model file, loading------")
  load("globalmodels.saved")
  print("model file is loaded------------")
}
args<-commandArgs(TRUE)
cmdtype<-as.character(args[1])
if(!is.na(cmdtype))
  if(cmdtype=="exec")#execute a function
  {
    execfunc<-as.character(args[2])
    args_number<-as.integer(args[3])
    print(sprintf ("the function %s is executed with %d arguments",execfunc,args_number))
    
    args_str=""
    if(args_number>0)
      for(i in 4:(4+args_number-1))
      { 
        if(i==4)
          args_str<-as.character(args[i])
        else
          args_str<-paste(args_str,as.character(args[i]),sep=",")
      }
    
    exec_str <- sprintf("%s(%s)",execfunc,args_str)
    
    #todo: check if the function is in current env
    result <- eval(parse(text=sprintf("%s",exec_str)))
    #print(result)
  }

save(global.model,file="globalmodels.saved")
close(global.conn)
