

################################
# SBP box plot

#' Create a SBP box plot
#'
#' The function `box.plot()` creates a box plot based on the input data. The input data can be both a formula(y~x)
#' or a variable/feature in a data set.
#'
#' @param input the input as a column name in quotation marks or a formula.
#' @param data the data set which contains the input column(s).
#' @param y.name the name of the "y" column of the data set. Default value is set to NULL.
#' @param clr The color of the graph(s) in the plot(s).Default value is set to "rainbow".
#'
#' @return shows a box plot based on the input type(single column data or formula).
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' box.plot("len", data_frame)
#' box.plot(len~supp,  data_frame)
#'
box.plot=function(input,data,y.name=NULL,clr="rainbow")

{
  data=data.frame(data)
  if (is.character(input))
  {
    if (is.null(y.name)) y.name=input
    xlbl=y.name

    x=data[,input]

    clr=define.colors(1,clr)

    x.rng=range(x,na.rm=T)
    rng.diff=diff(x.rng)
    x.rng[1]=x.rng[1]-0.05*rng.diff
    x.rng[2]=x.rng[2]+0.05*rng.diff
    par.opts=graphics::par()
    graphics::par(mar=c(6,4,4,1))
    graphics::boxplot(x,horizontal=T,pch=19,col=clr[1],
            ylim=x.rng,
            cex.lab=1.5,las=1,cex.axis=1.5,
            xlab=xlbl,cex=as.numeric(length(x)>=100))
    graphics::par(mar=par.opts$mai)
  }

  #if (class(input)=="formula") #TODO: delete if below code works
  if( is(input,"formula") )
  {
    form.vars=get.vars(input)
    y.clm=form.vars$y.var
    y=data[,y.clm]

    grp.clm=form.vars$x.var[1]

    if (is.null(y.name)) y.name=input
    xlbl=y.clm

    y=data[,y.clm]
    grp=data[,grp.clm]

    clrs=define.colors(length(unique(grp)),clr)
    par.opts=graphics::par()
    graphics::par(mar=c(6,12,4,1))
    graphics::boxplot(y~grp,horizontal=T,col=clrs,xlab=xlbl,ylab="",
            cex.lab=1.5,las=1,cex.axis=1.5)
    graphics::par(mar=par.opts$mar)
  }
}

###########################################
# SBP pie plot

#' Create a SBP pie plot
#'
#' The function `pie.plot()` creates a pie plot of a given variable/feature of a data set.
#'
#' @param y.clm the numerical variable in quotations from data set
#' @param data name of the data set
#' @param y.name the name of the 'y' variable in the formula. Default value is NULL. #ask_Stan
#' @param all #ask_Stan
#' @param clr The color(s) of the graph in the plot.Default value is set to NULL.
#'
#' @return shows a pie plot in different ways(missing data,no missing data and missing data in a distinct category)
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' pie.plot("len", data_frame)
pie.plot=function(y.clm,data,y.name=NULL,all=F,clr=NULL)

{
  data=data.frame(data)
  x=data[,y.clm]

  avl.tbl=table(x)
  all.tbl=table(x,exclude=NULL)
  res.tbl=avl.tbl
  if (all) res.tbl=all.tbl

  n.miss=sum(all.tbl)-sum(avl.tbl)

  sub.txt=paste0("excludes ",n.miss," missing observations")
  if (all&&(n.miss>0)) sub.txt="includes missing data as a distinct category"
  if (n.miss==0) sub.txt="no missing observations"

  if (is.null(y.name)) y.name=y.clm

  clrs=define.colors(length(res.tbl),clr)

  pct.tbl=100*res.tbl/sum(res.tbl)

  par.opts=graphics::par()
  graphics::par(mar=rep(6,4)+0.1)
  graphics::pie(pct.tbl,col=clrs,
      main=y.name,cex.main=1.5,
      labels=paste0(names(res.tbl),
                    "\n (n = ",res.tbl,
                    "; ",round(pct.tbl,2),"%)"),
      sub=sub.txt)
  graphics::par(mar=par.opts$mar)
}


###############################################
# SBP bar plot

#' Create a SBP bar plot
#'
#' The function `bar.plot()` creates a bar plot of a given variable of a data set.
#'
#' @param y.clm the "y" column of the data set.
#' @param data the data set from which the data is taken.
#' @param all #ask_Stan
#' @param y.name the name of the "y" column.Default value is set to NULL.
#' @param clr The color(s)  in the plot(s). Default value is set to NULL.
#'
#' @return shows a bar plot of the "y" column data.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' bar.plot("len", data_frame)
#'
bar.plot=function(y.clm,data,all=F,y.name=NULL,clr=NULL)

{
  data=data.frame(data)
  if (is.null(y.name)) y.name=y.clm

  x=data[,y.clm]


  #if (class(x)[1]%in%c("ordered","factor","character")) #TODO: delete if below code works
  if( is(x,"ordered") || is(x,"factor") || is(x,"character"))
  {
    avl.tbl=table(x)
    all.tbl=table(x,exclude=NULL)
    res.tbl=avl.tbl
    if (all) res.tbl=all.tbl

    n.miss=sum(all.tbl)-sum(avl.tbl)

    sub.txt=paste0("excludes ",n.miss," missing observations")
    if (all&&(n.miss>0)) sub.txt="includes missing data as a distinct category"
    if (n.miss==0) sub.txt="no missing observations"

    clrs=define.colors(length(res.tbl),clr)
    pct.tbl=100*res.tbl/sum(res.tbl)
    par.opts=graphics::par()
    graphics::par(mar=c(6,12,4,8)+0.1)
    bp.res=graphics::barplot(res.tbl,horiz=T,cex.names=1.5,
                   xlab=paste0(y.name," (n)"),col=clrs,las=1,
                   cex.axis=1.25,cex.lab=1.5,xlim=c(0,1.3)*max(res.tbl),
                   sub=sub.txt)
    graphics::text(res.tbl,bp.res,paste0("n = ",res.tbl,"\n (",round(pct.tbl,2)," %)"),cex=1,pos=4)
    graphics::par(mar=par.opts$mar)
  }

  #if (class(x)[1]%in%c("numeric","integer","double"))#TODO: delete if below code works
  if( is(x,"numeric") || is(x,"double") || is(x,"integer"))
  {
    if (is.null(clr)) clr="gray"
    clrs=define.colors(1,clr)
    hst=graphics::hist(x,plot=F)
    par.opts=graphics::par()
    graphics::par(mar=c(6,6,2,1))
    hst=graphics::hist(x,col=clrs[1],cex.lab=1.5,prob=F,main="",cex.axis=1.5,
             xlab=y.name,las=1,ylim=c(0,1.1)*max(hst$counts),ylab="Number")
    graphics::text(hst$mids,hst$counts,hst$counts,pos=3)
    graphics::par(mar=par.opts$mar)
  }
}

##########################################################
# normal quantile-quantile plot

#' Create a SBP normal quantile-quantile plot
#'
#' The function `nqq.plot()` creates a quantile quantile plot of a data set variable/feature.
#'
#' @param y.clm the "y" column of the data set.
#' @param data the data set from which the "y" column is taken.
#' @param y.name the name of the "y" column. Default value is set to NULL.
#' @param clr the color(s) of the plot. Default value is set to 'black'.
#'
#' @return shows a quantile-quantile plot of the "y" column.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' nqq.plot("len", data_frame)
#'
nqq.plot=function(y.clm,data,y.name=NULL,clr="black")

{
  data=data.frame(data)
  if (is.null(y.name)) y.name=y.clm
  x=data[,y.clm]
  clrs=define.colors(1,clr)
  par.opts=graphics::par()
  graphics::par(mar=c(6,6,2,1))
  stats::qqnorm(x,xlab="Standard Normal Quantile",main="",
         ylab=paste0("Actual Value of ",y.name),
         cex.lab=1.5,cex.axis=1.5)
  stats::qqline(x)
  graphics::par(mar=par.opts$mar)

}

#########################################################
# Event plot

#' Create a SBP event plot
#'
#' #Does it show any graphs? #ask_Stan
#'
#' @param input the input can be both a column with quatation mark or a formula.
#' @param data the name of the data set that contains the input column(s).
#' @param y.name name of the "y" column.
#' @param clr the color of the plot.
#'
#' @return shows an event plot based on the input type(formula or column data).
#' @export
#'
#' @examples #ask_Stan
#'
event.plot=function(input,data,y.name=NULL,clr=NULL)

{
  data=data.frame(data)
  # if nmx not provided then extract it from the input
  if (is.null(y.name)) y.name=input

  #if (class(input)=="character") #TODO: delete if below code works
  if( is(input,"character") )
  {
    x=data[,input]
    cls=class(x)
    if (!(cls%in%c("Surv","competing.events")))
      stop("x must be of class Surv or competing.events.")




    # Kaplan-Meier curves
    if(cls=="Surv")
    {
      clr=define.colors(1,clr)
      km=survival::survfit(x~1)

      ylbl=paste0("Pr(",y.name,")")
      plot(km,las=1,conf.int=T,mark.time=T,lwd=2,
           cex.axis=1.5,cex.lab=1.5,ylab=ylbl,
           xlab="Time",col=clr,conf.type="log-log")
    }

    if (cls=="competing.events")
    {
      evnt.types=unique(x[,2])
      evnt.types=evnt.types[evnt.types!=0]
      clrs=define.colors(length(evnt.types),clr)
      ci=cmprsk::cuminc(x[,1],x[,2])

      # label the output
      names(ci)=substring(names(ci),3)
      ev.key=attr(x,"ev.key")
      for (i in 1:length(ev.key))
        names(ci)=gsub(names(ev.key)[i],ev.key[i],names(ci),fixed=T)

      plot(ci,las=1,col=clrs,lty=1,lwd=2,
           xlab="Time",ylab="Cumulative Incidence",
           cex.lab=1.5,cex.axis=1.5)
    }
  }

  #if (class(input)=="formula") #TODO: delete if below code works
  if( is(input,"formula") )
  {
    form.vars=get.vars(input)
    y.clm=form.vars$y.var
    y=data[,y.clm]

    #if (class(y)=="Surv") #TODO: delete if below code works
    if( is(y,"Surv") )
    {
      grp.clm=form.vars$x.var
      grp=data[,grp.clm]

      #if (class(grp)[1]%in%c("numeric","integer")) #TODO: delete if below code works
      if( is(grp,"numeric") || is(grp,"integer"))
      {
        grp=stats::quantile(grp,3)
        Rcode=paste0('data[,"',grp.clm,'"]=grp')
        eval(parse(text=Rcode))
      }

      sfit=survival::survfit(input,data=data)
      stbl=summary(sfit,times=pretty(c(0,max(y[,1]))))
      n.grp=length(levels(stbl$strata))
      clrs=define.colors(n.grp,clr)
      xlim=c(0,1+0.3*(length(sfit$strata)>1))*max(y[,1],na.rm=T)
      par.opts=graphics::par()
      graphics::par(mar=c(6,6,1,1))
      plot(sfit,col=clrs,lwd=2,las=1,
           xlab=paste0("Time"),
           ylab=y.name,xlim=xlim,
           cex.axis=1.5,cex.lab=1.5)
      if (length(sfit$strata)>1)
      {
        graphics::legend(1.05*max(y[,1],na.rm=T),1,
               col=clrs,lty=1,lwd=2,
               names(sfit$strata),ncol=1,
               cex=0.60)
        graphics::par(mar=par.opts$mar)
      }
    }

    #if (class(y)=="competing.events") #TODO: delete if below code works
    if( is(y,"competing.events") )
    {
      grp.clm=form.vars$x.var
      grp=data[,grp.clm]

      #if (class(grp)[1]%in%c("numeric","integer")) #TODO: delete if below code works
      if( is(grp,"numeric") || is(grp,"integer"))
      {
        grp=stats::quantile(grp,3)
      }

      ev.key=attr(y,"ev.key")
      evnt=ev.key[as.character(y[,2])]
      ci.res=cmprsk::cuminc(y[,1],evnt,grp,cencode=ev.key[as.character(0)])

      res.tbl=cmprsk::timepoints(ci.res,times=pretty(c(0,max(y[,1]))))$est
      res.tbl=t(res.tbl)

      res.tbl2=ci.res$Tests

      crv.names=colnames(res.tbl)
      grp.names=as.character(sort(unique(grp)))
      evt.names=as.character(sort(as.numeric(ev.key[setdiff(names(ev.key),as.character(0))])))


      crv.evts=crv.names
      crv.grps=crv.names

      for (i in 1:length(grp.names))
      {
        grp.mtch=which(substring(crv.names,1,nchar(grp.names[i]))==grp.names[i])
        crv.grps[grp.mtch]=grp.names[i]
      }

      for (i in 1:length(evt.names))
      {
        evt.mtch=which(substring(crv.names,nchar(crv.names)-nchar(evt.names[i])+1)==evt.names[i])
        crv.evts[evt.mtch]=evt.names[i]
      }



      n.grps=length(unique(crv.grps))

      clrs=define.colors(n.grps,clr)
      names(clrs)=unique(crv.grps)

      ci.clrs=clrs[crv.grps]


      n.types=length(unique(crv.evts))
      lty=1:n.types
      names(lty)=unique(crv.evts)

      ci.lty=lty[crv.evts]



      plot(ci.res,col=ci.clrs,lty=ci.lty,las=1,lwd=2,
           xlab="Time",ylab="Cumulative Incidence",
           cex.axis=1.5,cex.lab=1.5)
    }


  }
}

################################################
#

#' Create a SBP scatter plot
#'
#' The function `scatter.plot()` creates a scatter plot from a formula(y~x).
#'
#' @param form the formula the takes two numerical variables.
#' @param data The data frame that contains the 2 variables.
#' @param clr the color(s) of the plot. Default value is set to "black" and "red".
#' @param x.name the name of the "x" column. Default value is set to NULL.
#' @param y.name the name of the "y" column. Default value is set to NULL.
#' @param line the line to draw in the plot. Default value is set to NA.
#' @param txt A flag that indicates to display text. Default value is set to 0.
#'
#' @return shows a scatter plot displaying the relationship between two numeric variables.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' scatter.plot(len~dose, data_frame)
#'
scatter.plot=function(form,data,
                      clr=c("black","red"),
                      x.name=NULL,
                      y.name=NULL,
                      line=NA,txt=0)

{
  data=data.frame(data)
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  x.clm=form.vars$x.var
  y=data[,y.clm]
  x=data[,x.clm]


  if(is.null(x.name)) x.name=x.clm
  if(is.null(y.name)) y.name=y.clm

  lm.fit=stats::lm(y~x)
  r=stats::residuals(lm.fit)
  sw.res=normality.test(r)
  sw.sig=(sw.res$p.value<=0.05)

  ry=rank(y)
  rx=rank(x)
  rr.fit=stats::lm(ry~rx,x=T,y=T)

  sp.corr=stats::cor.test(x,y,method="spearman",use="pairwise.complete.obs")
  pr.corr=stats::cor.test(x,y,method="pearson",use="pairwise.complete.obs")

  corr.method=c("Pearson","Spearman")[1+sw.sig]
  corr.stat=c(pr.corr$estimate,sp.corr$estimate)[1+sw.sig]
  corr.pvalue=c(pr.corr$p.value,sp.corr$p.value)[1+sw.sig]


  sub.txt=""

  if ((line%in%1)&&(txt>0))
    sub.txt=paste0(corr.method," r = ",round(corr.stat,3),
                   "; p = ",corr.pvalue)

  clrs=define.colors(1+(!is.na(line)),clr)
  par.opts=graphics::par()
  graphics::par(mar=c(6,6,6,2))
  plot(x,y,xlab=x.name,ylab=y.name,
       main="",cex.axis=1.5,cex.lab=1.5,
       col=clrs[1],sub=sub.txt)

  if (line%in%1)
  {

    if (!sw.sig) graphics::abline(lm.fit,col=clrs)
    if (sw.sig)
    {
      mrm.res=monotone.rank.model(x,y)
      mrmx.ord=order(mrm.res$x)
      graphics::lines(mrm.res$x[mrmx.ord],mrm.res$y.hat[mrmx.ord],col=clrs[2])
    }

  }

  if (line%in%0)
  {
    y.mn=mean(y)
    graphics::abline(y.mn,0,col=clrs[2])
  }
  graphics::par(mar=par.opts$mar)

}

############################################
# mosaic plot

#' Create a SBP mosaic plot
#'
#' The function `mosaic.plot()` creates a mosaic graph from a formula(y~x).
#'
#' @param form The formula which is used to compare categorical data with a numeric variable.
#' @param data name of the data set where the variables are taken.
#' @param clr the color of the graphs in the plot. Default value is set to "rainbow".
#' @param y.name The name of the 'y' variable in the formula. Default value is set to NULL.
#' @param grp.name The group variable name in the formula. Default value is set to NULL.
#'
#' @return displays a mosaic graph in different ways(with missing data,missing data as a distinct category or without missing data).
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' mosaic.plot(len~supp, data_frame)
#'
mosaic.plot=function(form,data,clr="rainbow",
                     y.name=NULL,grp.name=NULL)

{
  data=data.frame(data)
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

  n.full=sum(full.tbl)
  n.avl=sum(avl.tbl)

  if (n.full==n.avl)
  {
    graphics::mosaicplot(full.tbl,col=clrs,las=1,xlab=nm.grp,ylab=nm.cty,main="",
               cex.axis=1,
               sub="No Missing Data")
  } else {

    graphics::mosaicplot(full.tbl,col=clrs,las=1,xlab=nm.grp,ylab=nm.cty,main="",
               cex.axis=1,
               sub="Includes Missing Data as a Distinct Category")
    graphics::mosaicplot(avl.tbl,col=clrs,las=1,xlab=nm.grp,ylab=nm.cty,main="",
               cex.axis=1,
               sub="Excludes Missing Data")
  }


}


###########################################
# categorize a numeric variable by quantile

#' Categorize a numeric variable by quantile
#'
#' The function `cut.quantile()` divides a variables/feature into a given number of sub-groups. By default, it
#' divides the variable into 4 subgroups(quartiles).
#'
#' @param x name of the numeric variable
#' @param ... other parameters passed to quantile method
#' @param n the number of subgroup to create.By default, the division value is 4, meaning it will create quartile division.
#'
#' @return returns the variable data as quantile/subgroups.
#'
#' @export cut.quantile
#' @export
#'
#' @examples
#'
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#'
#' cut.quantile(data_frame$len)
#'
cut.quantile=function(x, n=4, ...)
{
  qntl=stats::quantile(x,(1:(n-1))/n,na.rm=T)
  qntl=c(-Inf,qntl,Inf)
  grp=cut(x,qntl)
  levels(grp)=paste0("Ranked subgroup ",1:n," of ",n)
  return(grp)

}

#############################################################

