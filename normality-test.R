norm.test=function(form,           # formula or name of variable in data
                 data,           # data.frame
                 alpha=0.05,     # significance threshold
                 clr="sbp.clrs", # color scheme
                 txt=1,          # level of detail in narrative text
                 fig=1,          # level of detail to provide in figures
                 tbl=1,          # level of detail to provide in tables
                 mda=1,          # missing data alert
                 dgt=4,          # number of digits
                 hdr=4,          # 0=no header, 1-6 = html header level (1=largest text,6=smallest text)
                 rpt=F,          # include in report T/F
                 rxv=0,          # 0 = no archive, 1 = archive up to a certain size, 2 = archive no matter what
                 fig.type="pdf", # R function to produce figure file
                 ...)            # options for R figure function
  
{
  #########################################
  # Bibliographic Reference
  ref='Shapiro, S.S.; Wilk, M.B.(1965). An analysis of variance test for normality (complete samples). 
  Biometrika. 52 (3–4): 591–611.'
  id="doi:10.1093/biomet/52.3-4.591"
  bib.data=cbind.data.frame(id=id,ref=ref)
  ref.num="[1]"
  
  if (rpt) ref.num=cite.ref(id,ref)
  ##################################
  
  try.form=try(class(form)=="character",silent=T)   
  
  if (!try.form){
    stop(paste0(form,'is not a character'))
  }
  
  if (class(form)=="character"){ 
    
    y.name=form
    res.tbl=summarize(form,data,mda=mda)   # summary table
    shapiro.res=shapiro.test(data[,y.name]) # Shapiro-Wilk test result
    
    ########################################
    # Produce Q-Q plot if requested
    
    if (fig>0)
      
    {
      qqpt=qq.plot(form,data,clr=clr,txt=txt,
              mda=mda,rpt=rpt,
              fig.type=fig.type)
      
    }
    
    ########################################
    # Add table to report if requested by user
    tbl.cap=NULL
    if (tbl>0)  tbl.cap=paste0("Summary statistics for ",y.name)
    if (rpt&&(tbl>0))
    {
      df.tbl=cbind.data.frame(stat=rownames(res.tbl),res.tbl)
      colnames(df.tbl)[1]=y.name
      report.table(df.tbl,tbl.cap)
    }
    
    ##################################
    # Prepare narrative text
    test.name=shapiro.res$method
    
    if (shapiro.res$p.value>alpha){
      
      res.txt=paste0(test.name," shows the p-value of ",
                     rpt.num(shapiro.res$p.value,dgt),' is greater than significance level=', alpha,
                     ' implying that the distribution of the data are not significantly 
                     different from normal distribution. In other words, we can assume the normality') 
      
      
    } else {
      
      res.txt=paste0(test.name," shows the p-value of ",
                     rpt.num(shapiro.res$p.value,dgt),' is less than or equal to significance level=', alpha,
                     ' implying that the distribution of the data are significantly 
                     different from normal distribution. In other words, we cannot assume the normality')
    }
    
    
    ##############################
    # Add header as requested
    
    res.hdr=NULL
    if ((hdr>0)&&(rpt)) 
    {
      res.hdr=paste0("<h",hdr,"> Normality test of ",y.name,"</h",hdr,">")
      res.txt=paste0(res.hdr,res.txt)
    }
   
    ################################
    # add table and figure reference as requested
    tbl.num=fig.num=1
    if (rpt)
    {
      tbl.num=get.tbl.num()
      fig.num=get.fig.num()
    }
    if (tbl>0) res.txt=paste(res.txt,paste0("Table ",tbl.num," provides summary statistics of ", y.name, ". "), sep="<br>")
    if (fig>0) res.txt=paste(res.txt,paste0("Figure ",fig.num," provides Q-Q plot of ", y.name, ". "), sep="<br>")
    
    #####################################
    # return result
    
    res.txt=paste0(res.txt,collapse="")
    fig.cap=NULL
    if(fig>0) fig.cap=qqpt$fig.cap
    if(tbl>0) tbl.cap=tbl.cap
    if(tbl==0) res.tbl=NULL
    if(txt==0) res.txt=NULL
    
    if (rpt) 
    {
      report.text(res.txt)
      
      report.method(method=paste0(test.name,ref.num),
                    purpose=paste0("test the normality of ", y.name,'. 
                                   Note that, normality test is sensitive to sample size. 
                                   Small samples most often pass normality tests. 
                                   Therefore, it’s important to combine visual inspection and significance test in order to take the right decision'))
    }
    
    ###############################################
    # archive data if requested
    
    if (rxv>0)
    {
      dset=data[,c(y.name)]
      archive.report.data(dset,raw=F)
      m=get.ads.num()
      res.txt=paste0(res.txt,
                     "The data for this result is archived in analysis data set ",
                     m,".  ")
    }
    
    ############################################
    # Methods sentence
    ugrp=colnames(res.tbl)
    mtd.txt=paste0(test.name,ref.num," was used test the normality of ", y.name)
    
  } 
  
  ####################################
  # Add missing data alert
  n.miss=sum(res.tbl["missing",])
  if ((mda>0)&&(n.miss>0))
  {
    res.txt2=paste0("This result ignores ",n.miss,
                    " observation",c("","s")[1+(n.miss>1)]," with missing data.  ")
    res.txt=paste0(res.txt,res.txt2,collapse="")
  }

  
  ###############################################
  # Package and return result object
  
  res=list(txt=res.txt,
           tbl=res.tbl,
           tbl.cap=tbl.cap,
           fig.cap=fig.cap,
           mtd=mtd.txt,
           ref=bib.data)
  
  return(res)
  
}