#############################################
# report a scatter plot

library(ggplot2)
library(EnvStats)

scatter.plot=function(form,           # formula or name of variable in data
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
      stop(paste0(form," is not a valid formula."))
  }

  #####################################################
  # get caption and colors for the scatter plot
  fig.cap1=temp=resp=NULL
  form.names=get.form.names(form,data)
  y.name=form.names$y
  x.name=form.names$x   

  if (length(x.name)!=1) 
    stop("The formula must have exactly one x predictor.")
    
  clrs=define.colors(1,clr)  # get the colors for the boxplot

  fig.cap1=paste0("A scatter plot of ",y.name," vs ",x.name,".  ") # text of figure caption
 

    
  ################################################
  # add more details to the caption as requested by the user
  # fig.cap2=NULL
  # if (txt>1)
  # {
  #   fig.cap2=paste0("For each box, the left edge, center vertical bar, and right edge ",
  #                   "represent the lower quartile, median, and upper quartile of ",resp,
  #                   ", respectively.  ")
  #   fig.cap2=paste0(fig.cap2,"The dotted horizontal lines are called whiskers.  ")
  #   fig.cap2=paste0(fig.cap2,"The left and right edges of the whiskers are the minimum ",
  #                   "and maximum values of ",resp," that do not meet the Tukey definition ",
  #                   "of outliers.  ")
  #   nout=length(temp$out)
  #   if (nout>0)
  #     fig.cap2=paste0(fig.cap2,"  The ",nout," individual point",c("","s")[1+(nout>1)],
  #                     " appearing beyond ",
  #                     "the edges of the whiskers ",
  #                     "are outlier",c("","s")[1+(nout>1)]," according to the Tukey definition.  ")
  # }
  
  
  #################################################
  # add missing data alert as requested by the user
  fig.cap2=fig.cap3=NULL
  
  glm.temp=glm(form,data=data)
  
  nmiss=nrow(data)-nrow(glm.temp$model)
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

  #par(mar=c(5,10,1,1))        # set wide left margin for group names
  # plot(form,data,             # produce the boxplot
  #       las=1,
  #      ylab=y.name,          
  #           xlab=x.name,
  #           col=clrs,...)
  if (class(form)=="formula")
  {
    form.names=get.form.names(form,data)
    vars=get.form.vars(form,data)
    
    y.name=form.names$y
    x.name=form.names$x 
    sp = ggplot(data,aes(x=data[,x.name],y=data[,y.name]),fill=data[,x.name])+
      labs(x = vars$pred, y = vars$resp, fill=vars$pred, title = "Scatter Plot")+
      geom_point()+theme_bw()+geom_abline()
    
    print(sp)
    
  }
    
    
  if (rpt) dev.off()
  
  res=list(fig.cap=fig.cap)
  
  return(res)
  
}