# KIDS24-team10
## ranksum-test.r
Function ranksum.test performs Wilcoxon test
* If paired=TRUE : paired two-sample Wilcoxon test (also known as Wilcoxon Wilcoxon Signed-Rank test)
* If paired=FALSE (default): unpaired two-samples Wilcoxon test (also known as Wilcoxon rank sum test or Mann-Whitney test).
* The argument “alternative =” can be used to specify the alternative hypothesis. It must be one of “two.side” (default), “greater” or “less”


## two.variance-test.r
Function F.test compare two sample variances
* The argument “alternative =” can be used to specify the alternative hypothesis. It must be one of “two.side” (2 tailed F test, default), “greater” or “less” (one sided F test)

## normality-test.r
Function norm.test performs Shapiro-Wilk normality test
* form='variable name'
* p-value > 0.05 implies normality

## qqplot.r
Function qq.plot performs a quantile-quantile plot for normality test 

## t-test.r
Function student.test performs one and two sample t-tests. 
* For one sample t-test, form="variable name", mu=a theoretical mean
* For two sample t-test, form=x~y
* The function will automatically check whether variance is equal or not
* User can set paired=TRUE perform paired student's t-test. Default is FALSE
* The argument “alternative =” can be used to specify the alternative hypothesis. It must be one of “two.side” (2 tailed t test, default), “greater” or “less” (one sided t test)
