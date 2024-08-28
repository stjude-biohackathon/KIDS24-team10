
####################################
# get y-variable and x-varibles name from a form argument

get.form.names=function(form,data=NULL)
{
  
  yvar=NULL
  xvar=NULL
  try.form=try(as.formula(form),silent=T)
  if (class(try.form)=="try-error")
  {
    try.form=as.character(try.form)
    try.form=gsub("Error : object '","",try.form,fixed=T)
    try.form=gsub("' not found\n","",try.form,fixed=T)
    yvar=try.form
  }
  
  if (class(try.form)=="formula")
  {
    form.terms=terms(try.form)
    ynum=attr(form.terms,"response")
    if (ynum==0) stop("formula does not specify a y-variable to the left of ~.")
    
    try.form=deparse(try.form)
    spl.form=unlist(strsplit(try.form," ~ "))
    yvar=spl.form[1]
    
    xvar=rownames(attr(form.terms,"factors"))
    xvar=setdiff(xvar,yvar)
  }
  
  if (!is.null(data))
  {
    in.data=(yvar%in%colnames(data))
    if(!in.data) stop(paste0(yvar," is not a column of input data."))
    
    if (!is.null(xvar))
    {
      not.in.data=setdiff(xvar,colnames(data))
      if (length(not.in.data)>0)
        stop(paste0())
    }
  }
  
  res=list(y=yvar,
           x=xvar)
  return(res)
}


######################################
# concatenate a vector of character strings by commas and "and"

txt.list=function(txt.str)
{
  txt.str=as.character(txt.str)
  res=NULL
  if (length(txt.str)==1) res=txt.str
  if (length(txt.str)==2) res=paste0(txt.str,collapse=" and ")
  if (length(txt.str)>2)  res=paste(paste0(txt.str[-length(txt.str)],collapse=", "),
                                     "and",txt.str[length(txt.str)])
  return(res)
}


#################################
# get variables from a formula

get.form.vars=function(form,data=NULL)
  
{
  trms=rownames(attr(terms(form),"factors")) # get terms as character strings
  rsp=attr(terms(form),"response")           # get the index of the response term
  resp=trms[rsp]                             # get the response term
  pred=trms                                  # other terms are predictors
  if (rsp==1)  pred=trms[-rsp]
  
  if (!is.null(data))                        # compare against the data set to see if the terms are there  
  {
    clms=colnames(data)
    if (!all(trms%in%clms))
    {
      miss=setdiff(trms,clms)
      stop(paste("Variable(s)",
                 paste0(miss,collapse=","),
                 "not found in data."))
    }
  }
  res=list(resp=resp,
           pred=pred)
  return(res)
  
}




#################################
# Represent input argument as a character

arg.as.char=function(x)
{
  temp=try(x,silent=T)
  if (class(temp)=="character")
    return(temp)
  
  temp=deparse(match.call())
  res=get.arg(temp,"x")
  return(res)
}


#######################################
# get the argument value


get.arg=function(call.string, # obtain with deparse(match.call())
                 arg.string)  # character string with argument name
{
  
  x.pos=regexpr(paste0(arg.string," = "),call.string,fixed=T)
  x.name=substring(call.string,x.pos+nchar(arg.string)+3)
  x.name=x.name[x.pos>0]
  
  
  comma.pos=regexpr(",",x.name,fixed=T)
  close.pos=regexpr(")",x.name,fixed=T)
  end.pos=close.pos
  
  if (length(comma.pos)>0)
  {
    if (comma.pos>0) end.pos=comma.pos
  }
  
  x.name=substring(x.name,1,end.pos-1)
  
  
  x.name=gsub('\"','',x.name)
  
  return(x.name)
}
