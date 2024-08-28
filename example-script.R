#######################################
# Start with clean slate in R
rm(list=ls())


########################################
# Generate an example data set

set.seed(1234)  # random seed
n=50            # sample size

treatment=sample(LETTERS[1:4],n,T) # group variable
subgroup=sample(letters[1:4],n,T) # class variable
x=rnorm(n)                   # normal variable 1
y=rnorm(n)                   # normal variable 2
tm=rexp(n)                   # event time
ev=rbinom(n,1,0.5)           # event indicator

dset=cbind.data.frame(id=1:n,
                      treatment=treatment,
                      subgroup=subgroup,
                      x=x,
                      y=y,
                      tm=tm,
                      ev=ev)

rm(grp,cls,x,y,tm,ev)

dset$y[13]=NA
dset$treatment[c(11,17)]=NA


###########################################
# Source the code libraries

source("C:/Users/spounds/Box/SBP2-Code/2024-08-26/base.R")
source("C:/Users/spounds/Box/SBP2-Code/2024-08-26/boxplot.R")
source("C:/Users/spounds/Box/SBP2-Code/2024-08-26/colors.R")
source("C:/Users/spounds/Box/SBP2-Code/2024-08-26/report.R")
source("C:/Users/spounds/Box/SBP2-Code/2024-08-26/summarize.R")
source("C:/Users/spounds/Box/SBP2-Code/2024-08-26/kw-test.R")


######################################
# generate an example report

rpt.dir="C:/Users/spounds/Box/SBP2-Code/2024-08-26/test-reports/"

begin.report(dir.name=rpt.dir,
             title="Test Report",
             author="Stan")

kw.res=kw.test(y~treatment,data=dset,rpt=T,rxv=1)

kw.res2=kw.test(y~subgroup,dset,rpt=T)

complete.report()
rm(rpt.env)
