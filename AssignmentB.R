#ISCG 8025 Introduction to Data Science Assignment B
rm(list=ls())
cat("\14")

library(gridExtra)
library(grid)
library(gtable)

# Part1
cleanData <- function(data)
{
    levels(data$City)<-c(levels(data$City),'Auckland')
    data$City[data$City=='Ackland'] <- 'Auckland'

    data <- subset(data, ((City == "Auckland") | (City == "Wellington")))

    data <- subset(data, Area >= 0)
    
    data <- subset(data, P.Winter < 10000 & P.Winter >= 0)
    data <- subset(data, P.Summer < 10000 & P.Summer >= 0)
    
    return(data)
}

# Annual average power consumption
AddAnnualAverage <-function(data)
{
  data$P.Annual = (data$P.Summer + data$P.Winter)/2
  return(data)
}

# Part2
PrintMeanSd <- function(data, title="")
{
  tableResult <- matrix(c(format(mean(data$P.Winter), nsmall=1), format(sd(data$P.Winter), nsmall=1),
                          format(mean(data$P.Summer), nsmall=1), format(sd(data$P.Summer), nsmall=1),
                          format(mean(data$P.Annual), nsmall=1), format(sd(data$P.Annual), nsmall=1)),
                          nrow=3, ncol=2)
  
  rownames(tableResult) <- c("P.Winter", "P.Summer", "P.Annual")
  colnames(tableResult) <- c("Mean", "Standard Dev")
  
  #grid.newpage()
  #d <- head(tableResult)
  #grid.table(d)
  d <- head(tableResult)
  table <- tableGrob(d)
  
  title <- textGrob(title, gp = gpar(fontsize = 14, fontface="bold"))
  padding <- unit(0.5,"line")
  table <- gtable_add_rows(
    table, heights = grobHeight(title) + padding, pos = 0
  )
  table <- gtable_add_grob(
    table, list(title),
    t = 1, l = 1, r = ncol(table)
  )
  grid.newpage()
  grid.draw(table)
}

PlotDensity <- function(data, title="")
{
  par(mfrow=c(1,3))
  ylim <- range(c(density(dataSet$P.Winter)$y,density(dataSet$P.Summer)$y, density(dataSet$P.Annual)$y))
  xlim <- range(c(density(dataSet$P.Winter)$x,density(dataSet$P.Summer)$x, density(dataSet$P.Annual)$x))
  
  plot(density(dataSet$P.Winter), col="blue", xlim=xlim, ylim=ylim, sub="P.Winter", main="")
  plot(density(dataSet$P.Summer), col="red", xlim=xlim, ylim=ylim, sub="P.Summer", main=title)
  plot(density(dataSet$P.Annual), col="green", xlim=xlim, ylim=ylim, sub="P.Annual", main="")
  par(mfrow=c(1,1))
}

PlotBox <- function(data, title="")
{
  boxplot(dataSet$P.Winter, dataSet$P.Summer,dataSet$P.Annual,
         col=c("blue", "red", "green"), 
         names=c("P.Winter", "P.Summer", "P.Annual"),
         boxwex=0.2,
         main=title)
}


PlotArea <- function(data, title="")
{
  PrintMeanSd(data, title)
  readline()
  PlotDensity(data, title)
  readline()
  PlotBox(data, title)
  readline()
}

# Part3
ScatterPlot <- function(powData, areaData, title="", xlab="")
{
  plot(areaData, powData, main=title, xlab=xlab, ylab="Power")
  par(new=TRUE)
  
  model1 <- lm(powData~poly(areaData,1))
  lines(sort(areaData),model1$fitted.values[order(areaData)])
  error1 = model1$fitted.values - powData
  MSE1 = mean(error1^2) 
  
  model2 <- lm(powData~poly(areaData,2))
  lines(sort(areaData),model2$fitted.values[order(areaData)], col="blue")
  error2 = model2$fitted.values - powData
  MSE2 = mean(error2^2) 

  model3 <- lm(powData~poly(areaData,3))
  lines(sort(areaData),model3$fitted.values[order(areaData)], col="red")
  error3 = model3$fitted.values - powData
  MSE3 = mean(error3^2) 
  
  legend("topleft", pch="-", col=c("black","blue","red"), 
         legend=c(paste("linear: MSE =", format(MSE1, nsmall=1)),
                  paste("2nd order poly: MSE =", format(MSE2, nsmall=1)),
                  paste("3rd order poly: MSE =", format(MSE3, nsmall=1))))
  
}

ScatterPlotArea <- function(data, title="")
{
  par(mfrow=c(1,3))
  
  ScatterPlot(data$P.Winter, data$Area, title="P.Winter", xlab=title)
  ScatterPlot(data$P.Summer, data$Area, title="P.Summer", xlab=title)
  ScatterPlot(data$P.Annual, data$Area, title="P.Annual", xlab=title)
  
  par(mfrow=c(1,1))
  
  readline()
}

# Main Program

# read data
dataSet <- read.csv("Data Set 6.csv", strip.white=TRUE)

# Part	1	– Data	Cleaning	and	Transformation
dataSet <- cleanData(dataSet)
dataSet <- AddAnnualAverage(dataSet)

# Part	2 – Univariate	Analysis
PlotArea(dataSet, "Auckland & Wellington")
PlotArea(subset(dataSet, City=="Auckland"), "Auckland")
PlotArea(subset(dataSet, City=="Wellington"), "Wellington")

# Part	3 – Bivariate	Analysis
ScatterPlotArea(dataSet, title="Auckland & Wellington")
ScatterPlotArea(subset(dataSet, City=="Auckland"), title="Auckland")
ScatterPlotArea(subset(dataSet, City=="Wellington"), title="Wellington")

# Main Program END
