#Source Code Libraries 

# source("/Users/kendao/Desktop/KIDS24-team10-main/base.R")
# source("/Users/kendao/Desktop/KIDS24-team10-main/boxplot.R")
# source("/Users/kendao/Desktop/KIDS24-team10-main/colors.R")
# source("/Users/kendao/Desktop/KIDS24-team10-main/report.R")
# source("/Users/kendao/Desktop/KIDS24-team10-main/summarize.R")
# source("D:/Biohackathon24/base.R")
# source("D:/Biohackathon24/boxplot.R")
# source("D:/Biohackathon24/colors.R")
# source("D:/Biohackathon24/report.R")
# source("D:/Biohackathon24/summarize.R")
# source("H:/Stanley/SBP2-Code/2024-09-06-Biohackathon/Abdel_Peter/violinplot-final.R")

#glm.tbl function to return odds ratio, LB, UB and pvalue of logistic regression model
glm.tbl=function(glm.result)
  
{
  ci.tbl=confint(glm.result)
  smry.tbl=coef(summary(glm.result))
  res.tbl=cbind.data.frame(coefficient=smry.tbl[,1],
                           LB95=ci.tbl[,1],
                           UB95=ci.tbl[,2],
                           p=smry.tbl[,4])
  
  if ((glm.result$family$family=="gaussian")&&(glm.result$family$link=="identity"))
  {
    return(res.tbl)
  }
  
  if ((glm.result$family$family=="binomial")&&(glm.result$family$link=="logit"))
  {
    res.tbl[,1:3]=exp(res.tbl[,1:3])
    colnames(res.tbl)[1]="odds.ratio"
    return(res.tbl)
  }
  
}


## Logistic regression function
model.binary=function(form,
                      data,
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
  form.names=get.form.names(form,data) #form.names <- get.form.names(event ~ ER+Grade+Age,data=nki70_NA)
  y.name=form.names$y   
  grp.name=form.names$x  
  
  #reformat grp.name if there are >1 covariates
  if(length(grp.name) > 2) { 
    ref.grp.name <- paste(paste(grp.name[1:length(grp.name) - 1], collapse = ", "), "and", grp.name[length(grp.name)])
  } else if(length(grp.name) == 2) {
    ref.grp.name <- paste(grp.name[1], "and", grp.name[2])
  } else {
    ref.grp.name <- grp.name
  }
  
  
  #Logistic Regression Model + Results table
  glm.res=glm(formula=form,data=data,family=binomial)     #glm.res <- glm(event ~ ER+Grade+Age,data=nki70_NA, family = binomial)
  res.tbl=glm.tbl(glm.res)
  res.tbl=data.frame(cbind(rownames(res.tbl),res.tbl))
  colnames(res.tbl)<-c(" ","odds.ratio","LB95","UB95","p")
  
  
  class<-rep(NA,length(grp.name))
  for (i in 1:length(grp.name)){
    if (sum(class(data[,grp.name[i]]) %in% c("numeric","double","integer"))>=1){
      class[i]="Numeric"
    }else if(sum(class(data[,grp.name[i]]) %in% c("character","factor","ordered"))>=1){
      class[i]="Categorical"
    }
  }
  
  grp.name.class<-data.frame(cbind(grp.name,class))
  grp.name.class$rep<-NA
  for (i in 1:nrow(grp.name.class)){
    if (grp.name.class$class[i]=="Numeric"){
      grp.name.class$rep[i]=1
    }else if (grp.name.class$class[i]=="Categorical"){
      lvl<-unique(data[,grp.name[i]])
      lvl<-lvl[complete.cases(lvl)]
      grp.name.class$rep[i]=length(lvl)-1
    }
  }
  
  res.tbl$variable<-c("NA",rep(grp.name.class$grp.name,times=grp.name.class$rep))
  res.tbl$cls<-c("NA",rep(grp.name.class$class,times=grp.name.class$rep))
  

  r=residuals(glm.res)
  y.hat=predict(glm.res,newdata=data)
  p.hat=exp(y.hat)/(1+exp(y.hat))
  y.obs=glm.res$y
  pred=data.frame(p.hat=exp(y.hat)/(1+exp(y.hat)),
                  y.obs=as.factor(data[,y.name]))
  pred$y.obs_clr=data[,paste0(y.name,"_clr")]
  colnames(pred)<-c("Predicted_probability",y.name,
                    paste0(y.name,"_clr"))

  
  ##################################=
  # Produce boxplot if requested
  fig.num0=1
  if (rpt) fig.num0=get.fig.num()+1
  
  if (fig==1)
  {
    bxpt=box.plot(as.formula(paste0("Predicted_probability","~",y.name)),data=pred,clr=clr,       # boxplot
                  txt=txt,mda=0,rpt=rpt,
                  fig.type=fig.type)
    #xlab="Model Probability Prediction",
    #ylab="Observed Outcome",
    #las=1)
  }
  
  if (fig>1)
  {
    bxpt=box.plot(as.formula(paste0("Predicted_probability","~",y.name)),data=pred,clr=clr,       # boxplot
                  txt=txt,mda=mda,rpt=rpt,
                  fig.type=fig.type)
    #bxpt$fig.cap=paste0("A box plot of predicted probability of ",y.name," by modeling on ",ref.grp.name,".  ")
    
    vlpt=violin.plot(as.formula(paste0("Predicted_probability","~",y.name)),data=pred,clr=clr,       # violinplot
                     txt=txt,mda=mda,rpt=rpt,
                     fig.type=fig.type)
    #vlpt$fig.cap=paste0("A violin plot of predicted probability of ",y.name," by modeling on ",ref.grp.name,".  ")
                       
  }
  
  # ######################################
  # Add table to report if requested by user
  tbl.cap=NULL
  if (tbl>0) tbl.cap=paste0("Odds ratio estimates for logistic model fitted on ",y.name," to ",ref.grp.name,".")
    
  if (rpt&&(tbl>0))
  {
    # df.tbl=cbind.data.frame(stat=rownames(res.tbl),res.tbl)
    # colnames(df.tbl)[1]=y.name
    # report.table(df.tbl,tbl.cap)
    report.table(res.tbl[,-c(6,7)],tbl.cap)
  }

  
  ##################################
  # Prepare narrative text
  
  res.tbl$text<-NA
  for (i in 2: nrow(res.tbl)){
    if (res.tbl$cls[i]=="Numeric"){
      res.tbl$text[i]=paste0("Increasing ", rownames(res.tbl)[i]," by one unit changes the odds of ",y.name," by ",rpt.num(res.tbl$odds.ratio[i],dgt),
                             " (p = ",rpt.num(res.tbl$p[i],dgt),") when other varaibles are constant if have. ")
      
    } else if(res.tbl$cls[i]=="Categorical"){
      res.tbl$text[i]=paste0("The odds of ",y.name, " of ", rownames(res.tbl)[i]," versus its reference group is by ",rpt.num(res.tbl$odds.ratio[i],dgt),
                             " (p = ",rpt.num(res.tbl$p[i],dgt),") when other variables are constant if have. ")
    }
  }
  
  res.txt<-paste0(res.tbl$text[-1],collapse=" ")
  
  
  
  ##############################
  # Add header as requested
  
  res.hdr=NULL
  if ((hdr>0)&&(rpt)) 
  {
    res.hdr=paste0("<h",hdr,">Logistic regression of ",y.name," with ",ref.grp.name,"</h",hdr,">")
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
  
  
  if (tbl>0) res.txt=paste0(res.txt,paste0("Table ",tbl.num," provides output for logistic model of ",
                                           y.name," and ",ref.grp.name, ".  "),collapse="")
  
  # if (fig>0) res.txt=paste0(res.txt,paste0("Figure ",fig.num," provides box plots of ",y.name, " probability prediction from logistic model with "
  #                                          ,ref.grp.name,".  "),collapse="")
  
  if (fig==1) res.txt=paste0(res.txt,paste0("Figure ",fig.num," provides box plots of ",y.name, " probability prediction from logistic model with "
                                            ,ref.grp.name,".  "),collapse="")
  
  if (fig>1) res.txt=paste0(res.txt,paste0("Figures ",fig.num0,"-",fig.num," provides box plots and violin plots of ", y.name,
                                           " probability prediction from logistic model with " ,ref.grp.name,".  "),collapse="")
  ####################################
  #Add missing data alert
  #na.rows <- is.na(data[,y.name]) #Find the rows that are TRUE for any 1 NA value within the columns used as coeff for the model
  n.miss <- nrow(data)-nrow(glm.res$model) #Sum the rows that are TRUE for NA value
   if ((mda>0)&&(n.miss>0)) 
   {
     res.txt2=paste0("This result ignores ",n.miss,
                     " observation",c("","s")[1+(n.miss>1)]," with missing data.  ")
     res.txt=paste0(res.txt,res.txt2,collapse="")
   }
  
  
  #########################################
  # Bibliographic Reference
  kw.ref='Agresti, Alan. (2002). Categorical Data Analysis. New York: Wiley-Interscience.'
  kw.id="ISBN 978-0-471-36093-3"
  bib.data=cbind.data.frame(id=kw.id,ref=kw.ref)
  ref.num="[1]"

  if (rpt) ref.num=cite.ref(kw.id,kw.ref)
  
  
  ############################################
  # Methods sentence
  #ugrp=colnames(res.tbl)
  mtd.txt=paste0("The logistic regression model ",ref.num,
                 " was used to model the log-odds of an ",y.name,
                 " as a linear combination of ",ref.grp.name, collapse="")
  
  
  #####################################
  # return result
  
  res.txt=paste0(res.txt,collapse="")
  fig.cap=NULL
  if(fig==1) fig.cap=bxpt$fig.cap
  if(fig>1) fig.cap=bxpt$fig.cap
  if(fig>1) fig.cap=vlpt$fig.cap
  # if(fig==1) fig.cap=paste0("A box plot of predicted probability of ",y.name," by modeling on ",ref.grp.name,".  ")
  # if(fig>1) fig.cap=paste0("A box plot of predicted probability of ",y.name," by modeling on ",ref.grp.name,".  ")
  # if(fig>1) fig.cap=paste0("A violin plot of predicted probability of ",y.name," by modeling on ",ref.grp.name,".  ")
  if(tbl>0) tbl.cap=tbl.cap
  if(tbl==0) res.tbl=NULL
  if(txt==0) res.txt=NULL
  
  if (rpt) 
  {
    report.text(res.txt)
    report.method(method=paste0("Logistic regression model ",ref.num),
                  purpose=paste0("model the log-odds of an ",y.name,
                                 " as a linear combination of ",ref.grp.name,"."))
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
           tbl=res.tbl[,-c(6,7)],
           tbl.cap=tbl.cap,
           fig.cap=fig.cap,
           mtd=mtd.txt,
           ref=bib.data
           )
  
  return(res)
  
}


