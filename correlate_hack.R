
 #define function
correlate.test=function(form,           # formula or name of variable in data
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



  # Get variable names
  form.names=get.form.names(form,data)
  y.name=form.names$y
  x.name=form.names$x[1]

  paste0(y.name," ", x.name)
  # compute correlation statistics and summary results
  y=data[ ,y.name]
  x=data[ ,x.name]

  ry=rank(y)
  rx=rank(x)

  rr.fit=lm(ry~rx,x=T,y=T)

  lm.fit=lm(form,data)
  r=residuals(lm.fit)
  sw.res=shapiro.test(r)

  sw.sig=(sw.res$p.value<=0.05)

  sp.corr=cor.test(x,y,method="spearman",use="pairwise.complete.obs")
  pr.corr=cor.test(x,y,method="pearson",use="pairwise.complete.obs")

  corr.method=c("Pearson","Spearman")[1+sw.sig]
  corr.stat=c(pr.corr$estimate,sp.corr$estimate)[1+sw.sig]
  corr.pvalue=c(pr.corr$p.value,sp.corr$p.value)[1+sw.sig]
  corr.sig=(corr.pvalue<=0.05)


  # compute scatter plot if asked

  # if (fig>0)
  # {
  # add peter scatterplot here
  # }
  #



  # get text result from analysis

  if (txt>0)
  {
    res.txt=paste0("The correlation of ",x.name," and ",
                   y.name," is ",c("not","")[1+corr.sig],
                   " statistically significant (",
                   corr.method," r = ",round(corr.stat,3),
                   "; p = ",corr.pvalue,").  ")
  }

  # Add header as requested

  res.hdr=NULL
  if ((hdr>0)&&(rpt))
  {
    res.hdr=paste0("<h",hdr,">Correlation of ",y.name," with ",x.name,"</h",hdr,">")
    res.txt=paste0(res.hdr,res.txt,collapse="")
  }


  mtd.txt=paste0("To evaluate the association of ",x.name," and ",y.name,
                 ", a simple linear regression model with ",x.name," as the sole predictor of ",
                 y.name," was fit to the data.  ",
                 "The distribution of residuals of this model did",c(" not","")[1+sw.sig],
                 " differ significantly from the normal distribution ",
                 "(Shapiro-Wilk p = ",sw.res$p.value,").  ",
                 "Therefore, ",
                 corr.method,"'s correlation was used to evaluate the association of ",
                 x.name," and ",y.name,".  ")
  #####################################

  # return result

  res.txt=paste0(res.txt,collapse="")

  fig.cap=NULL
  #if(fig>0) fig.cap=bxpt$fig.cap
  #if(tbl>0) tbl.cap=tbl.cap
  if(tbl==0) res.tbl=NULL
  if(txt==0) res.txt=NULL

  if (rpt)
  {
    report.text(res.txt)
    report.method(method=paste0("simple Linear Regression model with ",x.name," as the sole predictor of ",
                                y.name),
                  purpose=paste0("evaluate the association of ",x.name," and ",y.name,".",
                                 " The distribution of residuals of this model did",c(" not","")[1+sw.sig],
                                 " differ significantly from the normal distribution ",
                                 "(Shapiro-Wilk p = ",sw.res$p.value,").  ",
                                 "Therefore, ",
                                 corr.method,"'s correlation was used to evaluate the association of ",
                                 x.name," and ",y.name,".  "))
  }


  # Bibliographic Reference

  kw.ref="ADD REFERENCE PAPER"
  kw.id="ADD DOI"
  bib.data=cbind.data.frame(id=kw.id,ref=kw.ref)
  ref.num="[1]"

  if (rpt) ref.num=cite.ref(kw.id,kw.ref)


  # Package and return result object

  res=list(txt=res.txt,
           tbl=NULL,
           tbl.cap=NULL,
           fig.cap=NULL,
           mtd=mtd.txt,
           ref=NULL)

  return (res)

}





