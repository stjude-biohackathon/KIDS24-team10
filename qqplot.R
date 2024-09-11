qq.plot=function(form,           # formula or name of variable in data
                  data,           # data.frame
                  clr="sbp.clrs", # color scheme
                  txt=1,          # level of detail in narrative text
                  mda=1,          # missing data action
                  rpt=F,          # include in report T/F
                  fig.type="pdf", # R function to produce figure file
                  ...)          
  
{
  ########################################
  # convert data to a data.frame and process the form argument
  data=as.data.frame(data)
  
  # process the form argument
  try.form=try(as.formula(form),silent=T)         # determine whether form is a formula                   
  if (class(try.form)=="try-error")               # if not, form is a column of data
  {
    #form=as.character(deparse(substitute(form)))
    if (!(form%in%colnames(data)))
      stop(paste0(form," is not a column of input data."))
  }
  
  #####################################################
  # get caption and colors for the q-q plot
  fig.cap1=temp=resp=NULL
  
  {
    
    #temp=ggpubr::ggqqplot(test[,form])
    fig.cap1=paste0("A Quantile-Quantile plot of ",form,".  ")
    resp=form
    clrs=define.colors(1,clr)
    
  }
  
  
  ################################################
  # add more details to the caption as requested by the user
  fig.cap2=NULL
  if (txt>1)
  {
    fig.cap2=paste0("If all the points fall approximately along this reference line, 
                    we can assume normality.")

  }
  
  
  #################################################
  # add missing data alert as requested by the user
  fig.cap3=NULL
  nmiss=nrow(data)-length(which(is.na(data[,form])))
  if ((mda>0)&&(nmiss>0))
  {
    fig.cap3=paste0("The plot does not show ",nmiss,
                    " observation",c("","s")[1+(nmiss>1)]," with missing data.")
  }
  if ((mda>0)&&(nmiss==0))
  {
    fig.cap3=paste0("There was no missing data.  The plot shows all data.  ")
  }
  fig.cap=paste(fig.cap1,fig.cap2,fig.cap3,collapse="")
  
  
  ################################
  # Generate the figure and add it to the report if requested
  
  if (rpt) report.figure(fig.cap,fig.type,...)
  
  par(mar=c(5,10,1,1))              # set wide left margin for group names
  
  {
    # boxplot(data[,form],horizontal=T,
    #         las=1,ylab="",xlab=form,
    #         col=clrs)
    
    qqnorm(data[,form], pch = 1, frame = FALSE)
    qqline(data[,form], col = clrs, lwd = 2)
    
  }
  
  
  if (rpt) dev.off()
  
  res=list(fig.cap=fig.cap)
  
  return(res)
  
}