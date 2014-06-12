printResult<-function(data,extremum,optimal,xdata,ydata,byline,bysubgraph)
{  
  p<-ggplot(data, aes_string(x=xdata,y=ydata,group="paste(kernelname,programsize)",colour=paste('factor(',byline,')',sep='' ) ) )
  p+geom_point()+geom_line()+facet_wrap(~Fdensity*programsize,ncol=1,scales="free_y")+
    geom_text(aes_string(x=xdata,y=ydata,label =paste('sprintf("%.2f",',ydata,')',sep='') ) ,data = extremum,vjust=-1.5,size=3)+
    geom_point(aes_string(x=xdata,y=ydata),data = extremum,size =5,colour='yellow',alpha=0.3)+
    geom_text(aes_string(x=xdata,y=ydata,label =paste('sprintf("%d,%d,%.2f",config,',xdata,',',ydata,')',sep='') )  ,col="red",data = optimal,vjust=4,size=4)+
    geom_point(aes_string(x=xdata,y=ydata),data = optimal,size =8,alpha=1)
  
}

 