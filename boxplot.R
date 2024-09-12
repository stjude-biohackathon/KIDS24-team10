box.plot=function(form,           # formula or name of variable in data
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
  # get caption and colors for the boxplot
  fig.cap1=temp=resp=NULL
  
  if (class(form)=="formula") 
  {
    temp=boxplot(form,data,plot=F)    # get info about boxplot to be produced
    vrs=get.form.vars(form,data)      # get the variables from formula 
    resp=vrs$resp                     # get the response variable
    grp.clm=vrs$pred                  # the group variable is second 
    clrs=get.clrs(data,grp.clm,clr)   # get the colors for the boxplot
    clrs=clrs[temp$names]             # order them by group names
    fig.cap1=paste0("A box plot of ",resp," by ",grp.clm,".  ") # text of figure caption
  } else
  {
    if (!form%in%colnames(data))
      stop(paste0(form," is not a column of input data."))
    
    temp=boxplot(data[,form],plot=F)
    fig.cap1=paste0("A box plot of ",form,".  ")
    resp=form
    clrs=define.colors(1,clr)
  }

    
  ################################################
  # add more details to the caption as requested by the user
  fig.cap2=NULL
  if (txt>1)
  {
    fig.cap2=paste0("For each box, the left edge, center vertical bar, and right edge ",
                    "represent the lower quartile, median, and upper quartile of ",resp,
                    ", respectively.  ")
    fig.cap2=paste0(fig.cap2,"The dotted horizontal lines are called whiskers.  ")
    fig.cap2=paste0(fig.cap2,"The left and right edges of the whiskers are the minimum ",
                    "and maximum values of ",resp," that do not meet the Tukey definition ",
                    "of outliers.  ")
    nout=length(temp$out)
    if (nout>0)
      fig.cap2=paste0(fig.cap2,"  The ",nout," individual point",c("","s")[1+(nout>1)],
                      " appearing beyond ",
                      "the edges of the whiskers ",
                      "are outlier",c("","s")[1+(nout>1)]," according to the Tukey definition.  ")
  }
  
  
  #################################################
  # add missing data alert as requested by the user
  fig.cap3=NULL
  nmiss=nrow(data)-sum(temp$n)
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
  if (class(form)=="formula")
  {
    boxplot(form,data,horizontal=T,   # produce the boxplot
            las=1,ylab="",          
            xlab=resp,
            col=clrs)
  } else
  {
    boxplot(data[,form],horizontal=T,
            las=1,ylab="",xlab=form,
            col=clrs)
  }

  
  if (rpt) dev.off()
  
  res=list(fig.cap=fig.cap)
  
  return(res)
  
}
