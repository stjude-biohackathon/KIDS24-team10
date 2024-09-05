summarize=function(form,
                   data,
                   mda=1)
{
  ########################################
  # convert data to a data.frame and process the form argument
  data=as.data.frame(data)
  
  # process the form argument
  try.form=try(as.formula(form),silent=T)         # determine whether form is a formula                   
  if (class(try.form)=="try-error")               # if not, form is a column of data
  {
    form=as.character(deparse(substitute(form)))
    if (!(form%in%colnames(data)))
      stop(paste0(form," is not a column of input data."))
  }
  
  #######################################
  # extract y-variable and group variable
  if(class(form)=="character")
  {
    y=dset[,form]
    grp=rep(1,nrow(data))
  } else
  {
    vrs=get.form.vars(form,data)      # get the variables from formula 
    resp=vrs$resp                     # get the response column name
    grp.clm=vrs$pred                  # get the group column name
    y=data[,resp]                     # get the y-variable
    grp=data[,grp.clm]                # get the group variable
  }
  
  ######################################
  # summarize a quantitative y
  if (class(y)%in%c("numeric","double","integer"))
  {
    grp=as.character(grp)
    ugrp=sort(unique(grp))
    temp=smry(y)
    mtx=matrix(NA,length(temp),length(ugrp))
    for (g in 1:length(ugrp))
    {
      in.grp=(grp%in%ugrp[g])
      if (any(in.grp))  mtx[,g]=smry(y[in.grp])
    }
    colnames(mtx)=ugrp
    rownames(mtx)=names(temp)
    if (ncol(mtx)==1)  colnames(mtx)=form
    return(mtx)
  }
  
  ####################################
  # summarize a qualitative y
  if (class(y)%in%c("ordinal","factor","character"))
  {
    if (mda>0)  res=base::table(y,grp,exclude=NULL)
    else res=base::table(y,grp)
    
    names(dimnames(res))=c(resp,grp.clm)
    
    return(res)
    
  }
  
  
}



###########################################
# compute summary statistics for numeric data

smry=function(x,mda=1)
{
  
  if(class(x)%in%c("numeric","double","integer"))
  {
    n=length(x)
    na=sum(is.na(x))
    av=n-na
    ave=mean(x,na.rm=T)
    std=sd(x,na.rm=T)
    mdn=median(x,na.rm=T)
    q1=quantile(x,1/4,na.rm=T)
    q3=quantile(x,3/4,na.rm=T)
    min=min(x,na.rm=T)
    max=max(x,na.rm=T)
    res=c(n=n,
          missing=na,
          available=av,
          mean=ave,
          stdev=std,
          median=mdn,
          q1=q1,
          q3=q3,
          min=min,
          max=max)
    res=unlist(res)
    names(res)=c("n","missing","available",
                 "mean","standard deviation",
                 "median","lower quartile",
                 "upper quartile",
                 "minimum",
                 "maximum")
    if (mda==0)
    {
      res["n"]=n-na
      res=res[-2]
    }
    return(res)
  }
}
