##############################################
# TO DO LIST
# fix bugs in data archiving functions
# add functions for capturing code to regenerate results



##########################################
# Set up directory structure and background 
# data objects to produce a statistical report

begin.report=function(dir.name=NULL,            # report directory
                      title=NULL,               # report title (character string)
                      author=NULL,              # author list (character string)
                      color.scheme=rainbow,     # color scheme for figures
                      fig=1,txt=1,tbl=1,rxv=1,  # level of detail options
                      max.rxv=10000000)         # maximum size of file to archive
  
{
  # ignore call if rpt.env already exists
  if (exists("rpt.env")&&is.environment(rpt.env)) return()
  
  # Create report directory 
  if (is.null(dir.name)) dir.name=getwd() # default working directory
  time.stamp=Sys.time()
  rpt.time=time.stamp
  time.stamp=gsub(":","-",time.stamp,fixed=T)
  time.stamp=gsub(" ","-",time.stamp,fixed=T)
  rpt.dir=paste0(dir.name,"/stat-report-",time.stamp)
  dir.create(rpt.dir)
  rpt.dir=paste0(rpt.dir,"/")
  
  # initialize report information in report background environment
  #tch.inf=list(session.info=sessionInfo(),   # obtain technical information
  #             system.info=Sys.info())
  
  #print(tch.inf$system.info)
  
  if (is.null(author)) author=tch.inf$system.info["effective_user"]
  if (is.null(title)) title="Statistical Report"
  
  # Create report subdirectories
  fig.dir=paste0(rpt.dir,"figures/")    # figure subdirectory
  tbl.dir=paste0(rpt.dir,"tables/")     # table subdirectory
  rxv.dir=paste0(rpt.dir,"data/")       # data subdirectory
  raw.dir=paste0(rxv.dir,"raw/")        # raw data subdirectory 
  ads.dir=paste0(rxv.dir,"analysis/")   # analysis data subdirectory
  cod.dir=paste0(rpt.dir,"code/")       # code subdirectory
  tch.dir=paste0(rpt.dir,"technical/")  # technical information subdirectory
  
  dir.create(fig.dir)
  dir.create(tbl.dir)
  dir.create(rxv.dir)
  dir.create(raw.dir)
  dir.create(ads.dir)
  dir.create(cod.dir)
  dir.create(tch.dir)
  
  # create background environment with directory & subdirectory names
  assign("rpt.env",new.env(),           # create the rpt.env background information environment
         envir=.GlobalEnv)   
  assign("title",title,pos=rpt.env)           # report title
  assign("author",author,pos=rpt.env)         # report author
  assign("rpt.time",rpt.time,pos=rpt.env)     # report date and time
  assign("rpt.dir",rpt.dir,pos=rpt.env)       # report directory
  assign("fig.dir",fig.dir,pos=rpt.env)       # figure subdirectory
  assign("tbl.dir",tbl.dir,pos=rpt.env)       # table subdirectory
  assign("rxv.dir",rxv.dir,pos=rpt.env)       # data archive subdirectory 
  assign("raw.dir",raw.dir,pos=rpt.env)       # raw data archive subdirectory
  assign("ads.dir",ads.dir,pos=rpt.env)       # analysis data archive subdirectory
  assign("cod.dir",cod.dir,pos=rpt.env)       # code subdirectory
  assign("tch.dir",tch.dir,pos=rpt.env)       # technical information subdirectory
  assign("time.stamp",time.stamp,pos=rpt.env) # time stamp for version indexing
  assign("max.rxv",max.rxv,pos=rpt.env)       # maximum size data file to copy into archive
  
  # capture values for level of detail options
  assign("txt",txt,pos=rpt.env) # level of text option for SBP
  assign("fig",fig,pos=rpt.env) # level of figure option
  assign("tbl",fig,pos=rpt.env) # level of details for tables option
  assign("rxv",rxv,pos=rpt.env) # level of details for archiving option
  
  assign("clr.sch",color.scheme,pos=rpt.env) # color scheme for figures
  assign("res.txt",NULL,pos=rpt.env)         # results text 
  assign("mtd.txt",NULL,pos=rpt.env)         # methods text
  assign("mtd.tbl",NULL,pos=rpt.env)         # methods table
  assign("fig.cap",NULL,pos=rpt.env)         # figure captions
  assign("rpt.tbl",NULL,pos=rpt.env)         # list of report tables
  assign("tbl.cap",NULL,pos=rpt.env)         # table captions
  assign("bib.inf",NULL,pos=rpt.env)         # bibliography
  assign("fil.inf",NULL,pos=rpt.env)         # file information
  assign("raw.data",NULL,pos=rpt.env)        # raw data obtained through read.report.data
  assign("ads.data",NULL,pos=rpt.env)        # analysis data sets
  
  # lock the report background down to prevent inadvertent changes
  lockEnvironment(rpt.env)
  lockBinding(objects(pos=rpt.env),rpt.env)
}


##################################
# Generate the report

complete.report=function()
  
{
  # write the narrative file
  rpt.dir=get("rpt.dir",pos=rpt.env)
  txt.file=paste0(rpt.dir,"/report-narrative.html")
  
  # Write title, author, date & time
  rpt.txt=get("res.txt",pos=rpt.env)
  rpt.title=get("title",pos=rpt.env)
  rpt.author=get("author",pos=rpt.env)
  rpt.time=get("rpt.time",pos=rpt.env)
  rpt.time=as.character(rpt.time)
  rpt.time=substring(rpt.time,1,19)
  
  write(paste0("<h1>",as.character(rpt.title),"</h1>"),txt.file)
  #write("<br><br>",txt.file,append=T)
  write(paste0("<h3>",rpt.author,"</h3>"),txt.file,append=T)
  #write("<br><br>",txt.file,append=T)
  write(paste0("<h3>",rpt.time,"</h3>"),txt.file,append=T)
  #write("<br><br>",txt.file,append=T)
  write("<h3>Results<h3>",txt.file,append=T)
  write(as.character(rpt.txt),txt.file,sep="\n",append=T)
  
  # write the methods paragrph
  mtd.txt=get.methods.text()
  if (!is.null(mtd.txt))
  {
    write("<br><h3>Methods</h3>",txt.file,append=T)
    write(as.character(mtd.txt),txt.file,append=T)
  }
  
  # write the bibliogrpahy
  write.report.bib()

  
  # write the tables and their captions
  tbl.dir=get("tbl.dir",pos=rpt.env)
  tbl.file=paste0(tbl.dir,"/report-tables.xlsx")
  rpt.tbl=get("rpt.tbl",pos=rpt.env)
  tbl.cap=get("tbl.cap",pos=rpt.env)
  tbl.cap.file=paste0(tbl.dir,"/table-captions.txt")
  
  n.tbl=length(rpt.tbl)
  if (n.tbl>0)
  {
    code=paste0("out.tbl=list(",
                paste0("list.of.tables=cbind.data.frame(table=tbl.cap),",
                       paste0("Table_",1:n.tbl,
                              "=as.data.frame(rpt.tbl[[",1:n.tbl,"]])",collapse=","),")"))
    eval(parse(text=code))
    write_xlsx(out.tbl,tbl.file)
    
    write("<h3>List of Tables</h3>",txt.file,append=T)
    write(paste0("Tables are availble in the file ",tbl.file,".<br><br>"),
          txt.file,append=T)
    write(paste0(tbl.cap,"<br><br>"),txt.file,append=T)
    
  }
  

  
  
  # write the figure captions
  fig.cap=get("fig.cap",pos=rpt.env)
  fig.dir=get("fig.dir",pos=rpt.env)
  fig.cap.file=paste0(fig.dir,"/figure-captions.txt")
  
  if (length(fig.cap)>0)
  {
    write(as.character(fig.cap),fig.cap.file,sep="\n \n \n")
    write("<h3>List of Figures</h3>",txt.file,append=T)
    write(paste0("Figures are available as separate files in the directory ",fig.dir,".<br><br>"),
          txt.file,append=T)
    write(paste0(fig.cap,"<br><br>"),txt.file,append=T)
  }
    
  
  
  
  # write the technical information
  get.rpt.file.info(txt.file,"report.narrative")
  get.rpt.file.info(tbl.file,"report.tables")
  #get.rpt.file.info(tbl.cap.file,"table.captions")
  get.rpt.file.info(fig.cap.file,"figure.captions")
  write.tech.info()

  # save archive data sets
  save.rxv.data()
  
  rm(envir=rpt.env)

  
}

#################################
# Add text to the report

report.text=function(...) # a set of inputs that can be represented as character strings
  
{
  rpt.text=paste.text(...)            # put inputs together into one character string
  rpt.txt=get("res.txt",pos=rpt.env)  # extract previously reported text
  rpt.txt=c(rpt.txt,rpt.text)         # add input text to previous text
  update.rpt.env("res.txt",rpt.txt)   # update this item in rpt.env
}


###########################
# Generate a figure for the report

report.figure=function(cap=NULL,    # text of the caption (minus figure index)
                       type="pdf",  # type of graphics file for the figure
                       ...)         # options for R function generating that type of figure file

{
  # extract and increment figure index number
  fig.caps=get("fig.cap",pos=rpt.env) 
  fig.num=length(fig.caps)+1
  
  # constrct indexed figure caption
  caption=paste.text("Figure",fig.num,". ",cap)
  fig.caps=c(fig.caps,caption)
  
  # name graphics file for figure
  fig.dir=get("fig.dir",pos=rpt.env)
  file.name=paste0("Figure_",fig.num,".",type)
  full.name=paste0(fig.dir,"/",file.name)
  
  # open graphics file for figure
  fig.func=eval(parse(text=type))
  fig.func(full.name,...)
  
  # update file info and figure captions in rpt.env
  fig.info=cbind.data.frame(file.name=file.name,
                            file.type="report.figure",
                            md5=NA,
                            size=NA,
                            original.location=NA,
                            report.location=full.name)
  
  
  fil.inf=get("fil.inf",rpt.env)
  fil.inf=rbind.data.frame(fil.inf,fig.info)
  
  
  update.rpt.env("fil.inf",fil.inf)
  update.rpt.env("fig.cap",fig.caps)

}

#####################################
# Write the bibliography for the report

write.report.bib=function()
  
{
  bib.inf=get("bib.inf",rpt.env)
  n.ref=nrow(bib.inf)
  bib.txt=paste0("[",1:n.ref,"] ",
                 bib.inf$ref," ",
                 bib.inf$id,"<br><br>")
  
  rpt.dir=get("rpt.dir",pos=rpt.env)
  txt.file=paste0(rpt.dir,"/report-narrative.html")
  
  write("<h3>Works Cited</h3>",file=txt.file,append=T)
  write(bib.txt,file=txt.file,sep="\n",append=T)
  
}


#####################################
# Archive a data set

archive.report.data=function(data,raw=F)
  
{
  # get directory to write
  if (raw) dtype="raw.data"
  else dtype="ads.data"
  
  rxv.data=get(dtype,rpt.env)
  
  # convert data set to a data.frame
  data=as.data.frame(data)
  
  dtype2=gsub("ads.data","analysis.data",dtype,fixed=T)
  
  if (is.null(rxv.data))
  {
    code=paste0("rxv.data=list(",dtype2,".set1=data)")
    eval(parse(text=code))
    update.rpt.env(dtype,rxv.data)
    return()
  }
  
  m=length(rxv.data)
  code=paste0("rxv.data=list(",
              paste0(paste0(dtype2,".set",1:m,"=as.data.frame(rxv.data[[",1:m,"]])"),collapse=","),
              ",data.set",m+1,"=data)")
  eval(parse(text=code))
  
  update.rpt.env(dtype,rxv.data)
  return()
}

############################################
# save archived data sets

save.rxv.data=function()
  
{
  raw.data=get("raw.data",rpt.env)
  raw.dir=get("raw.dir",rpt.env)
  if (!is.null(raw.data))
  {
    raw.file=paste0(raw.dir,"/raw-data.xlsx")
    write_xlsx(raw.data,raw.file)
  }
  
  ads.data=get("ads.data",rpt.env)
  ads.dir=get("ads.dir",rpt.env)
  if (!is.null(ads.data))
  {
    ads.file=paste0(ads.dir,"/analysis-data.xlsx")
    write_xlsx(ads.data,ads.file)
  }
  
}


#####################################
# Add an indexed table to the report

report.table=function(tbl,      # table to add to report
                      cap=NULL) # table caption
{
  tbls=get("rpt.tbl",rpt.env)
  caps=get("tbl.cap",rpt.env)
  tbl.num=length(caps)+1
  
  cap=paste.text("Table ",tbl.num,". ",
                 cap)
  tbl=as.data.frame(tbl)
  if (is.null(tbls)) tbls=list(rpt.tbl1=tbl)
  else  {
    code=paste0("tbls=list(",
                 paste0("rpt.tbl",1:(tbl.num-1),
                           "=as.data.frame(tbls[[",1:(tbl.num-1),"]]),",collapse=" "),
                 paste0("rpt.tbl",tbl.num,"=as.data.frame(tbl))"),
                 collapse="")
    eval(parse(text=code))
  }
    
  caps=c(caps,cap)
  
  update.rpt.env("tbl.cap",caps)
  update.rpt.env("rpt.tbl",tbls)
}

###################################
# add a method to the report

report.method=function(method,purpose)
  
{
  row.mtd=cbind.data.frame(method=method,
                           purpose=purpose)
  mtd.tbl=get("mtd.tbl",rpt.env)
  mtd.tbl=rbind.data.frame(mtd.tbl,
                           row.mtd)
  update.rpt.env("mtd.tbl",mtd.tbl)
}

#####################################
# get the methods text for the report file

get.methods.text=function()
  
{
  mtd.txt=NULL
  mtd.tbl=get("mtd.tbl",rpt.env)
  if (!is.null(mtd.tbl))
  {
    uniq.methods=unique(mtd.tbl$method)
    for (m in uniq.methods)
    {
      mtch=which(mtd.tbl$method%in%m)
      temp=paste0("The ",m," was used to ",
                  txt.list(mtd.tbl$purpose[mtch]),".  ")
      mtd.txt=paste0(mtd.txt,temp)
    }
  }
  return(mtd.txt)
}

##################################
# write a number into the report

rpt.num=function(x,dgt=4,style="html")
{
  res=format(x,digits=dgt)
  epos=regexpr("e",res)
  has.e=(epos>0)

  if ((style=="html"))
  {
    res[has.e]=gsub("e","<sup>",res[has.e])
    res[has.e]=paste0(res[has.e],"</sup>")
  }
  
  return(res)
}


###################################
# write technical details

write.tech.info=function()
  
{
  # get the relevant session information
  session.info=sessionInfo()
  R.version=session.info$R.version$version.string
  R.platform=session.info$platform
  RNG.kind=paste(session.info$RNGkind,collapse=" ")
  R.running=paste0(session.info$running)
  sess.info=cbind.data.frame(item=c("R.version",
                                    "R.platform",
                                    "RNG.kind",
                                    "R.running"),
                             value=c(R.version,
                                     R.platform,
                                     RNG.kind,
                                     R.running))
  
  sys.info=Sys.info()
  sys.info=cbind.data.frame(item=names(sys.info),
                            value=sys.info)
  
  pack.info=NULL
  pack.list=c(session.info$basePkgs,
              names(session.info$otherPkgs),
              names(session.info$loadedOnly))
  pack.info=cbind.data.frame(package=pack.list,
                             version=NA)
  

  
  
  
  for (i in 1:length(pack.list))
  {
    pack.desc=packageDescription(pack.info$package[i])
    pack.version=try(pack.desc$Version)
    if (pack.version!="try-error")
      pack.info$version[i]=pack.version
  }
  
  file.data=get("fil.inf",pos=rpt.env)
  
  tech.info=list(file.data=file.data,
                 session.info=sess.info,
                 system.info=sys.info,
                 package.info=pack.info)
  
  

  
  tech.dir=get("tch.dir",pos=rpt.env)
  tech.file=paste0(tech.dir,"/technical-details.xlsx")
  write_xlsx(tech.info,tech.file)
  

}


##################################
# Get information stored in the rpt.env report detail environment

get.rpt.details=function()
{
  res=as.list(rpt.env)
  return(res)
}

############################################
# cite a reference

cite.ref=function(id,bib.ref)
{
  add.bib.ref(id,bib.ref)
  bib.data=get("bib.inf",pos=rpt.env)
  ref.num=which(id==bib.data$id)
  ref.str=paste0("[",paste0(ref.num,collapse=","),"]")
  return(ref.str)
}


######################################
# add a reference to the bibliography

add.bib.ref=function(id,bib.ref)
{
  bib.data=get("bib.inf",pos=rpt.env)
  if (is.null(bib.data))
  {
    bib.data=cbind.data.frame(id=id,
                              ref=bib.ref)
    update.rpt.env("bib.inf",bib.data)
    return()
  }
  
  in.bib=id%in%bib.data$id
  if (in.bib) return()
  
  
  new.ref=cbind.data.frame(id=id,
                           ref=bib.ref)
  bib.data=rbind.data.frame(bib.data,
                            ref=new.ref)
  update.rpt.env("bib.inf",bib.data)
  return()
  
}

#############################
# read a data file into R

read.report.data=function(file.name=NULL)
  
{
  ##########################
  # Extract the value of the archive options
  
  rxv=get("rxv",pos=rpt.env)
  

  ##############################################
  # Determine whether the file exists
  real.file=file.exists(file.name)
  
  if (!real.file)
  {
    stop.alert(file.name,"does not exist.  ")
    file.name=file.choose()
  }
  
  ######################################
  # Determine the type of data
  file.type=file.extension(file.name)
  alert(basename(file.name)," is a ",file.type," file.")
  
  ######################################
  # Read a csv or txt file
  if (file.type%in%c("csv","txt"))
  {
    dset=try(fread(file.name))
    dset=as.data.frame(dset)
    get.rpt.file.info(file.name,"input.data")
    return(dset)
  }
  
  ####################################
  # load an R data file
  if (file.type=="Rdata")
  {
    loaded.objects=load(file.name,verbose=T)
    get.rpt.file.info(file.name,"input.data")
    return(invisible())
  }
  
  
  ###################################
  # read an xlsx file
  if (file.type=="xlsx")
  {
      dset=read_xlsx(file.name,sheet=sheet.choice)
      dset=as.data.frame(dset)
      get.rpt.file.info(file.name,"input.data")
      return(dset)
  }
    
  #############################
  # read a SAS data file
  if (file.type=="sas7bdat")
  {
    dset=read_sas(sas.file)
    dset=as.data.frame(sas.data)
    get.rpt.file.info(file.name,"input.data")
    return(dset)
  }

}

#################################
# Get file information for the report

get.rpt.file.info=function(file.name,
                           file.type)
{
  # Now get the information about the file
  if (!file.exists(file.name))
  {
    warning(paste(file.name,"does not exist.  No file info retrieved for report."))
    invisible()
  }
  
  dfile.info=file.info(file.name)
  dfile.md5=md5sum(file.name)
  
  file.info2=cbind.data.frame(file.name=basename(file.name),
                              file.type=file.type,
                              md5=dfile.md5,
                              size=dfile.info$size,
                              original.location=dirname(file.name),
                              report.location=NA)
  
  fil.inf=get("fil.inf",rpt.env)
  fil.inf=rbind.data.frame(fil.inf,file.info2)
  update.rpt.env("fil.inf",fil.inf)
  
  invisible()
}

#################################
# Get current table number

get.tbl.num=function()
{
  tbl.num=length(get("tbl.cap",rpt.env))
  return(tbl.num)
}

###################################
# Get current figure number

get.fig.num=function()
{
  fig.num=length(get("fig.cap",rpt.env))
  return(fig.num)
}

##################################
# Get analysis data set number

get.ads.num=function()
{
  ads.data=get("ads.data",rpt.env)
  m=length(ads.data)
  return(m)
}


##################################
# Extract a file extension

file.extension=function(file.name)
{
  split.name=unlist(strsplit(file.name,split=".",fixed=T))
  return(split.name[length(split.name)])
}


####################################
# Alert the user with a message

alert=function(...)
{
  alert.message=paste0(...)
  message(alert.message)
}

####################################
# Alert the user and stop the calculation

stop.alert=function(...)
{
  alert.message=paste0(...)
  stop(alert.message)
}

############################
# paste.text

paste.text=function(...)
  
{
  rpt.text=list(...)                        # put all input together in a list
  rpt.text=as.character(unlist(rpt.text))   # unlist it and convert to character
  rpt.text=paste(rpt.text,collapse=" ")     # put it all together in one character string
  rpt.text=gsub("  "," ",rpt.text)          # remove double spaces
  rpt.text=gsub(" .",".",rpt.text,fixed=T)  # remove space before period
  return(rpt.text)
}

##########################
# update an item in rpt.env

update.rpt.env=function(item.name,  # character string name of object to update
                        new.value)  # new value of the object to update
  
{
  unlockBinding(item.name,rpt.env)         # unlock the item in rpt.env
  assign(item.name,new.value,pos=rpt.env)  # update the item in rpt.env
  lockBinding(item.name,rpt.env)           # relock the item in rpt.env
}

######################
# Some needed libraries
library(writexl)  # write excel file for tables and data sets
library(haven)    # read a SAS data file
library(tools)    # md5sum