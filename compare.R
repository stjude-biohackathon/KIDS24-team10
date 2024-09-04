#####################################
# Compare two variables

#' Compare two Variables
#'
#' @description
#' The `compare()` function compares two variables/features of a data set. Given two variables as
#' a formula (y~x), it compares the variables based on the data type of `y`.
#'  * If y variable is of `survival` class, it compares the events.
#'  * If y variable is of `numeric` class, it compares the centers.
#'  * If y variable is of `factor` class, it compares the proportions.
#'
#' The comparison results of the two variables are presented as text narrative, plots, tables, references.
#'
#'
#'
#' @param form The formula which is used to define the relationship of the two variables.
#' @param data The data set from which the two variables are taken.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param y.name The 'y' variable name in the formula. Default value is NULL. #ask_Stan
#' @param grp.name The 'group' name of the variable in the formula. Default value is NULL. #ask_Stan
#' @param clr The color(s) used in the plot(s).
#'
#' @return returns the relationship between the variables with a narrative,plots and references.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' compare(supp~len, data_frame)
#'
compare=function(form,data,
                 txt=1,tbl=1,fig=1,
                 y.name=NULL,
                 grp.name=NULL,
                 clr="rainbow")

{
  #library('stats')
  data=data.frame(data)

  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  y=data[,y.clm]



  if (any(class(y)%in%c("Surv","competing.events")))
  {
    res=compare.events(form,data,txt,tbl,fig,y.name,grp.name,clr)
    return(res)
  }

  if (any(class(y)%in%c("numeric","integer","double")))
  {
    res=compare.centers(form,data,txt,tbl,fig,y.name,grp.name,clr)
    return(res)
  }

  if (any(class(y)%in%c("factor","character","ordered")))
  {
    res=compare.proportions(form,data,txt,tbl,fig,y.name,grp.name,clr)
    return(res)
  }

  stop(paste0("The compare function doesn't handle y-variables of class ",class(y),".  "))
}


##################################################
#

#' Compare events between 2 variables if the 'y' variable is of survival class.
#'
#'
#' @param form The formula which is used to define the relationship of the two variables.
#' @param data The data set from which the two variables are taken.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param name.y The 'y' variable name in the formula. Default value is NULL. #ask_Stan
#' @param name.grp The 'group' variable name in the formula. Default value is NULL. #ask_Stan
#' @param clr The color(s) used in the plot(s).
#'
#' @return describes the relationship between the variables with a narrative,plots and references.
#' @export
#'
#' @examples #ask_Stan
#'
#'
compare.events=function(form,
                        data,
                        txt=1,tbl=1,fig=1,
                        name.y=NULL,
                        name.grp=NULL,
                        clr="rainbow")

{
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  y=data[,y.clm]
  y.cls=class(y)

  grp.clm=form.vars$x.var


  if (is.null(name.y)) name.y=y.clm
  if (is.null(name.grp)) name.grp=grp.clm

  if (any(y.cls=="Surv"))
  {
    sfit=survival::survfit(form,data=data)
    log.rank=survdiff(form,data)
    df=length(log.rank$n)-1
    p.value=pchisq(log.rank$chisq,df,lower.tail=F)
    sig=p.value<0.05
    grp.list=names(log.rank$n)
    eq.pos=regexpr("=",grp.list,fixed=T)
    grp.list=substring(grp.list,eq.pos+1)
    stbl=summary(sfit,times=pretty(c(0,max(y[,1]))))
    res.tbl=cbind.data.frame(group=as.character(stbl$strata),
                             time=stbl$time,
                             n.risk=stbl$n.risk,
                             n.event=stbl$n.event,
                             n.censor=stbl$n.censor,
                             surv=stbl$surv,
                             LB95=stbl$lower,
                             LB95=stbl$upper)
    n.grp=length(levels(stbl$strata))
    clrs=define.colors(n.grp,clr)
    plot(sfit,col=clrs,lwd=2,las=1,
         xlab=paste0("Time"),
         ylab=y.clm)
    res.txt=paste0("There is ",c("not","")[1+sig]," statistically compelling evidence ",
                   "that the ",name.y," distribution differs across the ",name.grp,
                   " groups ",text.list(grp.list)," (p = ",p.value,").  ")
    ref="Harrington, D. P. and Fleming, T. R. (1982). A class of rank test procedures for censored survival data. Biometrika 69, 553-566."
    method=paste0("The log-rank test (Harrington and Fleming 1982) was used to compare the ",
                  "distribution of ",name.y," across the ",name.grp," groups ",text.list(grp.list),".  ")
    res=list(tbl=res.tbl,
             txt=res.txt,
             method=method,
             ref=ref)
    return(res)
  }

  if (any(y.cls=="competing.events"))
  {
    grp.clm=form.vars$x.var
    grp=data[,grp.clm]
    ev.key=attr(y,"ev.key")
    evnt=ev.key[as.character(y[,2])]
    ci.res=cmprsk::cuminc(y[,1],evnt,grp,cencode=ev.key[as.character(0)])

    res.tbl=cmprsk::timepoints(ci.res,times=pretty(c(0,max(y[,1]))))$est
    res.tbl=t(res.tbl)

    res.tbl2=ci.res$Tests

    crv.names=colnames(res.tbl)
    grp.names=crv.names
    evt.names=crv.names
    for (i in 1:length(ev.key)) grp.names=gsub(paste0(" ",ev.key[i]),"",grp.names,fixed=T)
    uniq.grps=unique(grp.names)
    for (i in 1:length(uniq.grps)) evt.names=gsub(paste0(uniq.grps[i]," "),"",evt.names,fixed=T)
    uniq.evnts=unique(evt.names)


    n.grps=length(uniq.grps)
    clrs=define.colors(n.grps,clr)
    names(clrs)=uniq.grps
    ci.clrs=clrs[grp.names]

    n.types=length(uniq.evnts)
    lty=1:n.types
    names(lty)=uniq.evnts
    ci.lty=lty[evt.names]


    plot(ci.res,col=ci.clrs,lty=ci.lty,las=1,xlab="Time",lwd=2)

    res.txt=NULL
    for (i in 1:ncol(res.tbl))
    {
      clm.txt=paste0("Based on this data, it is estimated that ",
                     text.list(paste0(round(100*res.tbl[,i],2),"%")),
                     " of subjects in the ",grp.names[i]," population ",
                     "have experiened ",evt.names[i]," by times ",
                     text.list(rownames(res.tbl)),", respectively.  ")
      res.txt=c(res.txt,clm.txt)
    }

    for (i in 1:nrow(res.tbl2))
    {
      row.txt=paste0("There is ",c("not ","")[1+(res.tbl2[i,"pv"]<0.05)],
                     "statistically compelling evidence that the cumulative incidence of ",
                     rownames(res.tbl2)[i]," differs across the ",
                     text.list(uniq.grps)," populations (p = ",res.tbl2[i,"pv"],").  ")
      res.txt=c(res.txt,row.txt)
    }




    res.tbl=cbind(time=as.numeric(rownames(res.tbl)),res.tbl)
    res.tbl2=cbind.data.frame(event=rownames(res.tbl2),
                              p.value=res.tbl2[,"pv"])


    method=paste0("Gray's (1988) was used to estimate the cumulative incidence of ",
                  text.list(uniq.evnts)," and compare those estimates across the ",
                  text.list(uniq.grps)," groups.  ")

    ref="Gray RJ (1988) A class of K-sample tests for comparing the cumulative incidence of a competing risk, ANNALS OF STATISTICS, 16:1141-1154"



    res=list(tbl=list(estimates=res.tbl,
                      comparisons=res.tbl2),
             txt=res.txt,
             method=method,
             ref=ref)

    class(res)="SBP.result"

    return(res)
  }
}

##################################################
#
#' Compare centers of a numeric variable across groups
#'
#' @param form The formula which is used to define the relationship between the two variables.
#' @param data The data set from which the two variables are taken.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param name.y The 'y' variable name in the formula. Default value is NULL. #ask_Stan
#' @param name.grp The 'group' variable name in the formula. Default value is NULL. #ask_Stan
#' @param clr The color(s) used in the plot(s).
#'
#' @return describes the relationship between the variables with a  narrative,plots and references .
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' compare(len~supp,data_frame)
#'
compare.centers=function(form,
                         data,
                         txt=1,tbl=1,fig=1,
                         name.y=NULL,
                         name.grp=NULL,
                         clr="rainbow")

{
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  y=data[,y.clm]

  grp.clm=form.vars$x.var[1]
  y=data[,y.clm]
  grp=data[,grp.clm]

  if (!is.numeric(y))
    stop("y.clm must be the name of a column of a numeric data variable in the dset data.frame.")

  nmy=name.y
  nm.grp=name.grp

  if (is.null(nmy)) nmy=y.clm
  if (is.null(nm.grp)) nm.grp=grp.clm

  tbl1=aggregate(y,by=list(grp),numeric.descriptive.table)
  tbl1b=cbind.data.frame(tbl1[,1],tbl1$x)
  colnames(tbl1b)=c(nm.grp,colnames(tbl1$x))
  tbl1=tbl1b


  lm.res=lm(y~grp)
  anova.res=anova(lm.res)
  aov.res=aov(lm.res)
  tukey.res=TukeyHSD(aov.res)
  welch.res=stats::oneway.test(y~grp,var.equal = F)
  kw.res=kruskal.test(y~grp)
  pwt.res=pairwise.t.test(y,grp,p.adjust.method="holm",pool.sd=F)
  pww.res=pairwise.wilcox.test(y,grp,plev.adjust.method="holm",paired=F)

  r=residuals(lm.res)
  sw.res=shapiro.test(r)
  bartlett.res=bartlett.test(y,grp)




  setting=1+(bartlett.res$p.value<0.05)+2*(sw.res$p.value<0.05)
  p.value=c(anova.res$`Pr(>F)`[1],
            welch.res$p.value,
            kw.res$p.value,
            kw.res$p.value)[setting]


  method=c("Equal-Variance One-Way ANOVA",
           "Welch Unequal-Variance One-Way test",
           "Kruskal-Wallis test",
           "Kruskal-Wallis test")[setting]

  primary.ref=c("",
                'B. L. Welch (1951). On the comparison of several mean values: an alternative approach. Biometrika, 38, 330-336. doi: 10.2307/2332579.',
                'Kruskal; Wallis (1952). "Use of ranks in one-criterion variance analysis". Journal of the American Statistical Association. 47 (260): 583-621. doi:10.1080/01621459.1952.10483441.',
                'Kruskal; Wallis (1952). "Use of ranks in one-criterion variance analysis". Journal of the American Statistical Association. 47 (260): 583-621. doi:10.1080/01621459.1952.10483441.')[setting]

  if (nrow(tbl1)==2)
  {
    t.eqv.res=t.test(y~grp,var.equal=T)
    t.welch.res=t.test(y~grp,var.equal=F)
    wilcox.res=wilcox.test(y~grp)

    method=gsub("Equal-Variance One-Way ANOVA","Equal-Variance Two-Samples t-test",method)
    method=gsub("Welch Unequal-Variance One-Way test","Unequal-variance Two-Samples t-test",method)
    method=gsub("Kruskal-Wallis test","Wilcoxon rank-sum test",method)

    primary.ref=c("",
                  "",
                  'Wilcoxon, Frank (1945). "Individual comparisons by ranking methods". Biometrics Bulletin. 1 (6): 80-83. doi:10.2307/3001968. hdl:10338.dmlcz/135688. JSTOR 3001968.',
                  'Wilcoxon, Frank (1945). "Individual comparisons by ranking methods". Biometrics Bulletin. 1 (6): 80-83. doi:10.2307/3001968. hdl:10338.dmlcz/135688. JSTOR 3001968.')[setting]


    p.value=c(t.eqv.res$p.value,
              t.welch.res$p.value,
              wilcox.res$p.value,
              wilcox.res$p.value)[setting]

  }

  param=c("mean","mean","median","median")[setting]

  txt1=paste0("This data ",
              c("provides","does not provide")[1+(p.value>0.05)],
              " statistically compelling evidence that the population ",
              param," of ",
              nmy," differs across ",nm.grp,
              " (p = ",p.value,"; ",method,").  ")



  tbl2=NULL
  txt2=NULL
  pwc.ref=NULL
  pwc.mtd=NULL
  if (p.value<0.05)
  {
    if (setting==1)
    {
      tbl2=tukey.res$grp
      for (i in 1:nrow(tbl2))
      {
        grps=unlist(strsplit(rownames(tbl2)[i],split="-",fixed=T))
        pwc.txt=paste0("The population means of groups ",grps[1]," and ",grps[2],
                       c(""," do not")[1+(tbl2[i,4]>0.05)]," differ significantly ",
                       "(",rownames(tbl2)[i]," mean difference = ",tbl2[i,"diff"],
                       "; 95% CI: ",tbl2[i,"lwr"],", ",tbl2[i,"upr"],
                       "; Tukey's HSD adjusted p = ",tbl2[i,"p adj"],").  ")
        txt2=c(txt2,pwc.txt)
        pwc.ref='Tukey, John (1949). "Comparing Individual Means in the Analysis of Variance". Biometrics. 5 (2): 99-114. JSTOR 3001913'
        pwc.mtd=paste0("The Tukey (1949) honest significant difference procedure was used to follow-up the significant ",
                       "differences in the mean of ",nmy," across the ",nm.grp," groups ",text.list(tbl1[,1])," found by equal-variance one-way ANOVA.  ")
      }
    }
    if (setting==2)
    {
      tbl2=pwt.res$p.value
      for (i in 1:ncol(tbl2))
      {
        for (j in i:nrow(tbl2))
        {
          pwc.txt=paste0("The means of groups ",colnames(tbl2)[i]," and ",
                         rownames(tbl2)[j],c(""," do not")[1+(tbl2[j,i]>0.05)],
                         " differ significantly ","(Holm's adjusted p = ",tbl2[j,i],"; pairwise t-test).  ")
          txt2=c(txt2,pwc.txt)
          pwc.ref='Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65-70. https://www.jstor.org/stable/4615733.'
          pwc.mtd=paste0("Pairwise two-sample unequal-variance t-tests with Holm's multiplicity adjustment were performed to follow-up on the significant ",
                         "differences in the mean of ",nmy," across the ",nm.grp," groups ",text.list(tbl1[,1])," found by Welch's unequal-variance one-way ANOVA-like test.")
        }
      }
    }
    if (setting%in%(3:4))
    {
      tbl2=pww.res$p.value
      for (i in 1:ncol(tbl2))
      {
        for (j in i:nrow(tbl2))
        {
          pwc.txt=paste0("The medians of groups ",colnames(tbl2)[i]," and ",
                         rownames(tbl2)[j],c(""," do not")[1+(tbl2[j,i]>0.05)],
                         " differ significantly ","(Holm's adjusted p = ",tbl2[j,i],"; pairwise Wilcoxon test).  ")
          txt2=c(txt2,pwc.txt)
          pwc.ref='Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65-70. https://www.jstor.org/stable/4615733.'
          pwc.mtd=paste0("Pairwise Wilcoxon rank-sum tests with Holm's multiplicity adjustment were performed to follow-up on the significant ",
                         "differences in the median of ",nmy," across the ",nm.grp," groups ",text.list(tbl1[,1])," found by the Kruskal-Wallis test.")
        }
      }
    }

  }

  if (fig>0)
  {
    clrs=define.colors(length(unique(grp)),clr)
    par(mai=c(1,2,1,1))
    boxplot(y~grp,horizontal=T,col=clrs,xlab=nmy,las=1,ylab="")
  }
  if (fig>1)
  {
    par(mai=rep(1,4))
    qqnorm(r,xlab="Standard Normal Quantile",
           ylab=paste0("Residual of ",nmy),main="")
    qqline(r)
  }

  res.txt=c(txt1,txt2)

  mthd1=paste0("To select the best procedure for comparing the center of ",nmy,
               " across the ",nm.grp," groups ",text.list(tbl1[,1]),", ",
               "Bartlett's test was used to test whether the variability of ",nmy,
               " was equal across groups and the Shapiro-Wilk test was used to ",
               "evaluate the normality of the differences of the ",nmy," from their ",
               "respective group means.  ")
  mthd2=paste0("Based on these evaluations, the ",method," was used to compare the ",
               param," of ",nmy," across the ",nm.grp," groups ",text.list(tbl1[,1]),".  ")

  ref1=c("Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. Proceedings of the Royal Society of London Series A 160, 268-282. doi: 10.1098/rspa.1937.0109.",
         'Shapiro, S. S.; Wilk, M. B. (1965). "An analysis of variance test for normality (complete samples)". Biometrika. 52 (3-4): 591-611. doi:10.1093/biomet/52.3-4.591. JSTOR 2333709. MR 0205384.')




  ref=c(primary.ref,ref1,pwc.ref)
  ref=ref[ref!=""]

  res=list(txt=res.txt,
           tbl=list(desc.stats=tbl1,
                    pw.comps=tbl2),
           method=c(mthd1,mthd2,pwc.mtd),
           ref=ref)

  class(res)="SBP.result"

  return(res)


}



########################################
#

#' Compare Proportions between variables when the 'y' variable is of factor class.
#'
#' @param form The formula which is used to define the relationship between the two variables.
#' @param data The data set from which the two variables are taken.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param y.name The 'y' variable name in the formula. Default value is NULL. #ask_Stan
#' @param grp.name The 'group' variable name in the formula. Default value is NULL. #ask_Stan
#' @param clr The color(s) used in the plot(s).
#'
#' @return describes the relationship between the variables with a narrative,plots and references.
#'
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' compare(supp~len,data_frame)
#'
compare.proportions=function(form,
                             data,
                             tbl=1,fig=1,txt=1,
                             y.name=NULL,
                             grp.name=NULL,
                             clr="rainbow")

{

  form.vars=get.vars(form)
  cty.clm=form.vars$y.var
  grp.clm=form.vars$x.var[1]

  cty=data[,cty.clm]
  grp=data[,grp.clm]

  full.tbl=table(grp,cty,exclude=NULL)
  avl.tbl=table(grp,cty)

  nm.cty=cty.clm
  nm.grp=grp.clm

  if (is.null(y.name)) y.name=nm.cty
  if (is.null(grp.name)) grp.name=nm.grp

  nm.cty=y.name
  nm.grp=grp.name

  names(dimnames(full.tbl))=c(nm.grp,nm.cty)
  names(dimnames(avl.tbl))=c(nm.grp,nm.cty)


  uniq.cty=unique(cty)
  clrs=define.colors(length(uniq.cty),clr)

  if (fig>0)
  {
    mosaic.plot(form,data,clr,y.name,grp.name)
  }


  full.test=cx.test(full.tbl)
  avl.test=cx.test(avl.tbl)

  res.txt=NULL

  if (txt>0)
  {
    res.txt1=paste0("An analysis including missing data as a separate category ",
                    c("finds","does not find")[1+(full.test$p.value>0.05)],
                    " statistically compelling evidence that ",
                    cty.clm," and ",grp.clm," are informatively associated ",
                    "(p = ",full.test$p.value,"; ",full.test$method,").  ")
    res.txt2=paste0("An analysis that includes only subjects with complete data ",
                    c("finds","does not find")[1+(avl.test$p.value>0.05)],
                    " statistically compelling evidence that ",
                    cty.clm," and ",grp.clm," are informatively associated ",
                    "(p = ",avl.test$p.value,"; ",avl.test$method,").  ")

    res.txt=c(res.txt1,res.txt2)
  }

  res.tbl=NULL
  if (tbl>0)
  {
    res.tbl=list(n.full=full.tbl,
                 n.available=avl.tbl,
                 prop.full=full.tbl/rowSums(full.tbl),
                 prop.available=avl.tbl/rowSums(avl.tbl))
  }

  method=paste0(full.test$method," was used to evaluate the association of ",
                y.name," with ",grp.name, " while considering missing data as a separate category.  ",
                avl.test$method," was used to evaluate the association of ",
                y.name," with ",grp.name," after exclusion of missing data.  ")



  fisher.used=any(c(substring(full.test$method,1,6),
                    substring(avl.test$method,1,6))%in%"Fisher")
  chisq.used=any(c(substring(full.test$method,1,7),
                   substring(avl.test$method,1,7))%in%"Pearson")



  fisher.ref="Fisher, R. A. (1962). Confidence limits for a cross-product ratio. Australian Journal of Statistics, 4, 41. doi: 10.1111/j.1467-842X.1962.tb00285.x."
  chisq.ref="Hope, A. C. A. (1968). A simplified Monte Carlo significance test procedure. Journal of the Royal Statistical Society Series B, 30, 582-598. doi: 10.1111/j.2517-6161.1968.tb00759.x. https://www.jstor.org/stable/2984263."

  ref=NULL
  if (fisher.used) ref=c(ref,fisher.ref)
  if (chisq.used) ref=c(ref,chisq.ref)


  res=list(tbl=res.tbl,
           txt=res.txt,
           method=method,
           ref=ref)

  class(res)="SBP.result"

  return(res)


}


#####################################
# Test of independence on a cross-tabulation

#' Test of independence on a cross-tabulation
#'
#' `cx.test()` tests the independence on a cross-tabulation result.
#'
#' @param cx.tbl #ask_Stan
#' @param B #ask_Stan
#'
#' @return #ask_Stan
#'
#' @export
#'
#' @examples
#' #ask_Stan
#'
cx.test=function(cx.tbl,B=9999)
{
  res=try(fisher.test(cx.tbl),silent=T)
  # if (class(res)=="try-error") #TODO: delete if below code works
  if( is( res, "try-error"))
    res=chisq.test(cx.tbl,simulate.p.value=T,B=B)

  res$method=gsub("\n\t","",res$method)
  res$method=gsub("(","",res$method,fixed=T)
  res$method=gsub(")","",res$method,fixed=T)

  return(res)
}



######################################
# get variables from a formula

#' Get the dependent(y) and independent(x) variables from a formula
#'
#' @param form The formula which defines the relationship of the variables.
#'
#' @return returns the x and y variables from the formula.
#' @export
#'
#' @examples
#' get.vars(points~team)
#'
get.vars=function(form)
{
  x.vars=NULL
  if(is.character(form))
  {
    form.str=try(as.formula(form),silent=T)
    # if (class(form.str)=="try-error") #TODO: delete if below code works
    if( is( form.str, "try-error"))
    {
      y.var=form
      res=list(y.var=y.var,
               x.var=NULL)
      return(res)
    }
  }

  # if (class(form)=="formula") #TODO: delete if below code works
  if( is( form, "formula"))
    form.str=deparse(form)


  tilde.split=unlist(strsplit(form.str,split=" ~ ",fixed=T))
  y.var=tilde.split[1]
  x.vars=NULL

  if (length(tilde.split)>1)
  {
    plus.split=unlist(strsplit(tilde.split[2],split=" + ",fixed=T))
    x.vars=plus.split
  }

  res=list(y.var=y.var,
           x.vars=x.vars)

  return(res)

}

###################################
# get y-variable from a formula

#' Get y-variable from a formula
#'
#' @param form The formula which defines the relationship of the variables.
#'
#' @return returns the y variable of the formula.
#' @export
#'
#' @examples
#' get.yvar(height~age)
#'
get.yvar=function(form)

{
  form.str=deparse(form)
  y.var=unlist(strsplit(form.str,split=" ~ ",fixed=T))[1]
  return(y.var)
}
