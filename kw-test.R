kw.test=function(form,           # formula or name of variable in data
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
  ##################################
  # Get variable names
  form.names=get.form.names(form,data)
  y.name=form.names$y
  grp.name=form.names$x[1]
  
  ##################################
  # Compute summary statistics and KW test results
  res.tbl=summarize(form,data,mda=mda)   # summary table
  kw.res=kruskal.test(form,data)         # results of the Kruskal-Wallis test
  
  
  ##################################
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
  res.txt=paste0("The median of ",y.name," was ",
                 txt.list(paste0(rpt.num(res.tbl["median",],dgt)," among the ",
                                 res.tbl["available",]," ",
                                 grp.name," group ",colnames(res.tbl),
                                 " observations")),
                 " with available data (p = ",rpt.num(kw.res$p.value,dgt),"; Kruskal-Wallis test).  ")
  
  ##############################
  # Add header as requested
  
  res.hdr=NULL
  if ((hdr>0)&&(rpt)) 
  {
    res.hdr=paste0("<h",hdr,">Association of ",y.name," with ",grp.name,"</h",hdr,">")
    res.txt=paste0(res.hdr,res.txt,collapse="")
  }
  
  ################################
  # add table and figure reference as requested
  tbl.num=fig.num=1
  if (rpt)
  {
    tbl.num=get.tbl.num()
    fig.num=get.fig.num()
  }
  if (tbl>0) res.txt=paste0(res.txt,paste0("Table ",tbl.num," provides summary statistics of ",
                                      y.name," by ",grp.name,".  "),collapse="")
  if (fig>0) res.txt=paste0(res.txt,paste0("Figure ",fig.num," provides box plots of ",y.name,
                                      " by ",grp.name,".  "),collapse="")
  
  
  ####################################
  # Add missing data alert
  n.miss=sum(res.tbl["missing",])
  if ((mda>0)&&(n.miss>0))
  {
    res.txt2=paste0("This result ignores ",n.miss,
                    " observation",c("","s")[1+(n.miss>1)]," with missing data.  ")
    res.txt=paste0(res.txt,res.txt2,collapse="")
  }
  
  #########################################
  # Bibliographic Reference
  kw.ref='Kruskal WH, Wallis WA (1952). "Use of ranks in one-criterion variance analysis". Journal of the American Statistical Association. 47 (260): 583–621.'
  kw.id="doi:10.1080/01621459.1952.1048344"
  bib.data=cbind.data.frame(id=kw.id,ref=kw.ref)
  ref.num="[1]"
  
  if (rpt) ref.num=cite.ref(kw.id,kw.ref)
  
  ############################################
  # Methods sentence
  ugrp=colnames(res.tbl)
  mtd.txt=paste0("The Kruskal-Wallis test ",ref.num,
             " was used to compare the median of ",y.name,
             " across the ",grp.name," groups ",
             txt.list(colnames(res.tbl)),".")
  

  
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
    report.method(method=paste0("Kruskal-Wallis test ",ref.num),
                  purpose=paste0("compare the median of ",y.name,
                                 " across the ",grp.name," groups"))
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