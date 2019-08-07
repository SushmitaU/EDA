
#this functions gives the overview of the data:
overview <- function(data)
{
  x = as.data.frame(x=c(nrow(data)
                        ,ncol(data)
                        ,sum(sapply(data, is.numeric)) #counts numeric variables
                        ,sum(sapply(data,is.factor)) #counts factor/categorical variables
                        ,sum(sapply(data,is.character)) #counts character variables
                        ,sum(sapply(data,is.logical)) #counts logical variable such as T/F
                        ,sum(table(which(sapply(data, var)==0)))) #counts variables with single value in all its rows
                    ,row.names = c('Sample Size','No. of Variables','No. of Numeric Variables'
                                   ,'No. of factor Variables','No. of Character Variables'
                                   ,'No. of Logical Variables','No. of Zero variance Variables (Uniform)'))

  colnames(x) <- c('#Observations') #adding column name to the dataframe
  return(x)
}
#-----------------------------------------------

#this function gives the structure of the data:
structure <- function(data)
{
  x <- data.frame(#"Variables" = names(data)
    "Variable Type" = sapply(data, class) #checks the variable type
    ,"Percentage of Missing" = colSums(as.matrix(round(apply(data,2,is.na)/nrow(data) *100))) #missing values
    ,"No. of Unique Values" = (sapply(sapply(data, unique), length)))
  return(x)
}

#------------------------------------
#this function gives the graphs for features:
Graphs <- function(data,name=names(data),dirc = getwd(),index=NULL)
{

  initial = getwd()
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  if(is.null(index))
  {
    index = ifelse( name %in% names(data),match(name, names(data)), 1:ncol(data) )
  }

  #creating df for numerics variables
  df = data.frame(data[index])
  nums = unlist(lapply(df, is.numeric))
  data_num = df[ , nums]

  for(i in index)
  {
    #for numeric/continuos variables - univariate analysis
    if(is.numeric(data[,i]))
    {
      setwd(dirc)
      png(paste(names(data)[i], ".png", sep=""))

      par(mfrow=c(2,1),font.lab=2)
      color <- brewer.pal(8, "Set2")

      #boxplot
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),
              xlab = names(data)[i], col = color, border = "grey5",
              horizontal = T)
      #histogram
      hist(data[,i], main = paste("Histogram of", names(data)[i]),
           xlab = names(data)[i], ylab = "Frequency", col = color, border = "grey5")

      #summary
      summ = summary(data[,i])
      sum_det = paste(names(summ),format(summ,digits = 2),collapse = "; ")

      title(sub = sum_det, line = 3.75,font.sub=4)

      dev.off()
    }

    #for categorical variables - univariate analysis
    if(is.factor(data[,i]))
    {
      setwd(dirc)
      png(paste(names(data)[i], ".png", sep=""))

      par(mfrow=c(2,1),font.lab=2)
      color <- brewer.pal(8, "Set2")

      #barplot
      barplot(table(data[,i]), main = paste("Bar Graph of", names(data)[i]),xlab = names(data)[i],ylab = 'Frequency',col = color )

      #pie chart
      pct <- round(table(data[,i])/sum(table(data[,i]))*100)
      lbls <- paste(names(table(data[,i])),':',pct,"%",sep="") # add % to labels
      pie(table(data[,i]),main = paste("Pie Chart of", names(data)[i]),labels = lbls,col = color)

      #summary at the end of each plot
      summcat = summary(data[,i])
      sum_cat = paste(names(summcat),format(summcat,digits = 2),collapse = "; ")

      title(sub = sum_cat, line = 3.5, col = "red", font = 3.5, font.sub=4)

      dev.off()
    }
  }

  #---------Numeric data multivariate Analysis---------------------

  png(paste("Scatter Plot", ".png", sep=""))

  #scatter matrix
  plot(data_num , main='Scatter Plot', pch=20 , cex=1.5 , col=rgb(0.5, 0.8, 0.9, 0.7))
  dev.off()

  #heatmap
  png(paste("Heat Map", ".png", sep=""))
  cormat<-signif(cor(data_num),2)
  col<- colorRampPalette(c("#54B948", "white", "red"))(20)
  heatmap(cormat, main='Heat Map',col=col, symm=TRUE,cexRow = 1.5,cexCol = 1.5)

  dev.off()
  #-----------------------------------------------------------------
  setwd(initial)
}
