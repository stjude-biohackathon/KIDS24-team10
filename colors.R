######################################
# Assign colors to a data set
# define an attribute "painted" as TRUE

paint.data=function(data,clr="sbp.clrs")
{
  # initially assign all elements color 1 of the palette
  one.clr=define.colors(1,clr)
  data.clrs=matrix(one.clr,nrow(data),ncol(data))
  
  # assign color for each column of data
  k=ncol(data)
  for (i in 1:k) # loop over data columns
  {
    col.class=class(data[,i])
    if (col.class%in%c("ordered","factor","character"))
    {
      u=unique(data[,i])
      m=length(u)
      clrs=define.colors(m,clr)
      for (j in 1:m)
      {
        mtch=(data[,i]%in%u[j])
        data.clrs[mtch,i]=clrs[j]
      }
    }
  }
  colnames(data.clrs)=paste0(colnames(data),"_clr")
  data=cbind.data.frame(data,data.clrs)
  attr(data,"painted")=T
  return(data)
}

#######################
# determine whether data is painted

is.painted=function(data)
{
  res=attr(data,"painted")
  if (is.null(res)) return(FALSE)
  return(res)
}

################################
# get colors for a plot

get.clrs=function(data,clm,clr)
{
  clr.clm=paste0(clm,"_clr")
  if(clr.clm%in%colnames(data))
  {
    clrs=data[,clr.clm]
    dup=duplicated(clrs)
    clrs=clrs[!dup]
    names(clrs)=data[!dup,clm]
    return(clrs)
  }
  
  y=data[,clm]
  u=unique(y)
  n=length(u)
  clrs=define.colors(n,clr)
  names(clrs)=u
  return(clrs)
  
}

#################################
# Define colors

define.colors=function(n,
                       clr.palette="sbp.clrs")
{
  
  clr.funcs=c("sbp.clrs","rainbow","heat.colors",
              "terrain.colors","topo.colors",
              "cm.colors")
  
  clr.names=colors()
  if (all(clr.palette%in%clr.names))
  {
    if (length(clr.names)>=n)
    {
      res=clr.palette[1:n]
      return(res)
    }
  }
  
  if (all(areColors(clr.palette)))
  {
    if (length(clr.names)>=n)
    {
      res=clr.palette[1:n]
      return(res)
    }
  }
  
  if (clr.palette%in%clr.funcs)
  {
    n.str=max(n,2)
    clr.code=paste0(clr.palette,"(",n,")")
    res=eval(parse(text=clr.code))
    return(res)
  }
  
  clr.palettes=hcl.pals()
  if (clr.palette%in%clr.palettes)
  {
    res=hcl.colors(max(n,2),palette=clr.palette)
    if (n<=1) res=res[1]
    return(res)
  }
  
  stop("clr.palette must be a vector of color names, the name of a color function, or the name of a color palette: see help(hcl.colors).")
  
}

###################################
# show color palettes

show.palettes=function(n.colors=8)
{
  plot(c(0,15),c(0,-25),type="n",axes=F,
       xlab="",ylab="",
       main="Color Palettes in R")
  
  available.palletes=c("sbp.clrs",
                       "rainbow",
                       "heat.colors",
                       "terrain.colors",
                       "topo.colors",
                       "cm.colors",
                       hcl.pals())
  
  for (i in 1:length(available.palletes))
  {
    x=3*((i-1)%/%24)
    y=-((i-1)%%24)
    
    
    rect(x+(0:(n.colors-1))/n.colors,y-0.2,
         x+(1:n.colors)/n.colors,y-0.8,
         col=define.colors(n.colors,available.palletes[i]))
    text(x+1,y-0.5,available.palletes[i],pos=4,cex=0.75)
  }
  
}


######################################
# show colors

show.colors=function()
  
{
  clrs=colors()
  
  for (i in 0:9)
    clrs=gsub(i,"",clrs)
  
  clrs=unique(clrs)
  
  plot(c(0,30),c(0,-25),type="n",axes=F,
       xlab="",ylab="",
       main="Named Colors in R")
  
  for (i in 1:length(clrs))
  {
    x=5*(i-1)%/%24
    y=-((i-1)%%24)
    
    rect(x,y-0.2,x+0.25,y-0.8,
         col=clrs[i])
    text(x+0.25,y-0.5,clrs[i],cex=0.70,pos=4)
    
    
  }
}

areColors <- function(x) 
  {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}


#######################################
# sbp colors

sbp.clrs=function(n)
  
{
  clr.names=c("steelblue",
              "gold",
              "forestgreen",
              "darksalmon",
              "orchid",
              "turquoise",
              "sienna",
              "skyblue",
              "slategray",
              "firebrick",
              "rosybrown",
              "navyblue",
              "thistle",
              "sandybrown",
              "moccasin",
              "blueviolet",
              "darkmagenta",
              "paleturquoise",
              "tan",
              "olivedrab",
              "hotpink",
              "coral",
              "mistyrose")
  
  n.clrs=length(clr.names)
  if (n>n.clrs)
    stop("Too many colors requested for sbp.clrs")
  
  return(clr.names[1:n])
  
}

#################################
# Get name of color column for a given data column
# in a painted data set

sbp.clr.clm=function(clm.name)
  
{
  res=paste0(clm.name,"_clr")
  return(res)
}
