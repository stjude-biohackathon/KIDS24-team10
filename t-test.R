student.test=function(form,           # formula or name of variable in data
                 data,           # data.frame
                 paired=FALSE,  # if compare two paired samples, proceed with paired=T, otherwise, paired=F;
                 alternative='two.side',    # options: two.sided, less, greater
                 var.equal=FALSE,    #a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch test is used.
                 mu=0,           # a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
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
  ref='Student (1908) The Probable Error of a Mean. Biometrika. 6 (1): 1–25'
  id="doi:10.1093/biomet/6.1.1"
  bib.data=cbind.data.frame(id=id,ref=ref)
  ref.num="[1]"
  
  if (rpt) ref.num=cite.ref(id,ref)
  ##################################
  
  if (class(form)=="character"){ 
    ##one-sample T test
    y.name=form
    res.tbl=summarize(form,data,mda=mda)   # summary table
    t.res=t.test(data[,y.name], mu=mu, conf.level=1-alpha) # t test result
    
    ########################################
    # Produce boxplot if requested
    
    if (fig>0)
      
    {
      bxpt=box.plot(form,data,clr=clr,       # boxplot
                    txt=txt,mda=mda,rpt=rpt,
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
    test.name="One-sample Student's t-Test"
    res.txt1=paste0("The mean of ",y.name," was ",
                   txt.list(paste0(rpt.num(res.tbl["mean",],dgt)," among the ",
                                   res.tbl["available",]," ",
                                   colnames(res.tbl),
                                   " observations")),
                   " with available data (p = ",rpt.num(t.res$p.value,dgt),paste0(";",test.name,').'))
    
    if (t.res$p.value<=alpha){
      
      res.txt2=paste0("As the p-value is less than or equal to the significance level=", alpha,
                      ". We can conclude that the mean of ", y.name, " is significantly different from the theoretical mean of ", mu ,".")
      
    } else {
      
      res.txt2=paste0("As the p-value is greater than the significance level=", alpha,
                      "We can conclude that the mean of ", y.name, "is not significantly different from the theoretical mean of ", mu,".")
    }
    
    res.txt=paste(res.txt1,res.txt2,sep="<br>")
    
    ##############################
    # Add header as requested
    
    res.hdr=NULL
    if ((hdr>0)&&(rpt)) 
    {
      res.hdr=paste0("<h",hdr,"> Compare mean of ",y.name," with theoretical mean","</h",hdr,">")
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
    if (fig>0) res.txt=paste(res.txt,paste0("Figure ",fig.num," provides box plot of ", y.name, ". "), sep="<br>")
    
    #####################################
    # return result
    
    res.txt=paste0(res.txt,collapse="")
    fig.cap=NULL
    if(fig>0) fig.cap=bxpt$fig.cap
    if(tbl>0) tbl.cap=tbl.cap
    if(tbl==0) res.tbl=NULL
    if(txt==0) res.txt=NULL
    
    if (rpt) 
    {
      report.text(res.txt)
      
      test.name <- paste0(test.name,' (a parametric test that assume normality of the data)')
      report.method(method=paste0(test.name,ref.num),
                    purpose=paste0("compare the mean of ", y.name))
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
    mtd.txt=paste0(test.name,ref.num," was used to compare the mean of ", y.name)
    
    
  } else if (class(form)=="formula"){ 
    
    ##two-sample T test
    # Get variable names
    form.names=get.form.names(form,data)
    y.name=form.names$y
    grp.name=form.names$x[1]
    # Compute summary statistics and t test results
    res.tbl=summarize(form,data,mda=mda)   # summary table
    t.res=t.test(form,data,
                 paired=paired,
                 alternative=alternative,
                 var.equal=var.equal) # t test result
    
    ########################################
    # Produce boxplot if requested
    
    if (fig>0)
      
    {
      bxpt=box.plot(form,data,clr=clr,       # boxplot
                    txt=txt,mda=mda,rpt=rpt,
                    fig.type=fig.type)
    }
    
    ########################################
    # Add table to report if requested by user
    tbl.cap=NULL
    if (tbl>0)  tbl.cap=paste0("Summary statistics for ",y.name," by ",grp.name,".  ")
    if (rpt&&(tbl>0))
    {
      df.tbl=cbind.data.frame(stat=rownames(res.tbl),res.tbl)
      colnames(df.tbl)[1]=y.name
      report.table(df.tbl,tbl.cap)
    }
    
    
    ##################################
    # Prepare narrative text
    test.name=ifelse(paired==F,"Unpaired two-sample Student's t-Test","Paired two-sample Student's t-Test")
    res.txt11=paste0("The mean of ",y.name," was ",
                   txt.list(paste0(rpt.num(res.tbl["mean",],dgt)," among the ",
                                   res.tbl["available",]," ",
                                   colnames(res.tbl),
                                   " observations")),
                   " with available data (p = ",rpt.num(t.res$p.value,dgt),paste0(";",test.name,').'))
    grp=colnames(res.tbl)
    if (t.res$p.value<=alpha){
      
      res.txt22=paste0("As the p-value is less than or equal to the significance level=", alpha,
                      ". We can conclude that the mean of ",y.name[1]," for ", grp[1]," is significantly different from the mean of ", y.name, " for ", grp[2],". ")
    
      } else {
      
        res.txt22=paste0("As the p-value is greater to the significance level=", alpha,
                        ". We can conclude that the mean of ",y.name[1]," for ", grp[1]," is not significantly different from the mean of ", y.name, " for ", grp[2],". ")
        
    }
    
    res.txt=paste(res.txt11,res.txt22,sep="<br>")
    
    ##############################
    # Add header as requested
    
    res.hdr=NULL
    if ((hdr>0)&&(rpt)) 
    {
      res.hdr=paste0("<h",hdr,">Compare mean of ",y.name," by ",grp.name,"</h",hdr,">")
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
    if (tbl>0) res.txt=paste(res.txt,paste0("Table ",tbl.num," provides summary statistics of ",
                                             y.name, " by ", grp.name,". "), sep="<br>")
    if (fig>0) res.txt=paste(res.txt,paste0("Figure ",fig.num," provides box plot of ",
                                             y.name, " by ", grp.name,". "), sep="<br>")
    
    
    #####################################
    # return result
    
    res.txt=paste0(res.txt,collapse="")
    fig.cap=NULL
    if(fig>0) fig.cap=bxpt$fig.cap
    if(tbl>0) tbl.cap=tbl.cap
    if(tbl==0) res.tbl=NULL
    if(txt==0) res.txt=NULL
    
    if (rpt) 
    {
      report.text(res.txt)
      
      test.name <- paste0(test.name,' (a parametric test that assume normality of the data)')
      report.method(method=paste0(test.name,ref.num),
                    purpose=paste0("compare the mean of ",y.name,
                                   " across the ", grp.name, ": ", txt.list(colnames(res.tbl))))
    }
    
    ###############################################
    # archive data if requested
    
    if (rxv>0)
    {
      dset=data[,c(y.name,grp.name)]
      archive.report.data(dset,raw=F)
      m=get.ads.num()
      res.txt=paste0(res.txt,
                     "The data for this result is archived in analysis data set ",
                     m,".  ")
    }
    
    ############################################
    # Methods sentence
    ugrp=colnames(res.tbl)
    mtd.txt=paste0(test.name,ref.num,
                   " was used to compare the mean of ",y.name,
                   " across the ",grp.name, ":",
                   txt.list(colnames(res.tbl)),"<br>")
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