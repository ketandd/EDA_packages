setwd("C:\\Users\\HP\\R Assignments\\Classroom sessions")

data = read.csv("Attr.csv")

View(data)

Graphs <- function(data, dataname)
{
  cat_index = c()
  num_index = c()
  j = 1
  k = 1
  for(i in 1:ncol(data))
  {
    if (is.numeric(data[,i]) & length(unique(data[,i])) > 10) # to perform generation of graphs only if the variablis numeric
    { 
      num_index[j] = i
      j = j+1
      File = paste(getwd(),'/',dataname,'_Univariate_num_continuous/',names(data)[i], ".png", sep = "")
      if(!file.exists(File))
      {
        dir.create(dirname(File),showWarnings = F)
      }
      
      png(File)
      
      par(mfrow = c(2,1))
      
      hist(data[,i],main= paste("Histogram of", names(data)[i]),
           xlab = names(data)[i],ylab= 'frequency', col = "green")
      boxplot(data[,i],main= paste("Boxplot of", names(data)[i]),
              xlab = names(data)[i],col = "Orange", horizontal = T)
      
      dev.off()
    }
    else if(length(unique(data[,i])) <= 10)
    { 
      cat_index[k] = i
      k = k+1
      File = paste(getwd(),'/',dataname,'_Univariate_Categorical/',names(data)[i], ".png", sep = "")
      if(!file.exists(File))
      {
        dir.create(dirname(File),showWarnings = F)
      }
      
      png(File)
      par(mfrow = c(2,1))
      
      barplot(table(data[,i]),main= paste("Barplot of", names(data)[i]),
              xlab = names(data)[i],ylab = 'count', col = "Orange")
       
      pie(table(data[,i]),main= paste("Pie Chart of", names(data)[i]), 
          col=c("red","orange","yellow","blue","green", "pink"),
          border="black")
      
      
      dev.off()
    }
    
    
  }

}

?pie
Bivariate <- function(data, dataname)
{
  cat_index = c()
  num_index = c()
  j = 1
  k = 1
  for(i in 1:ncol(data))
  {
    if (is.numeric(data[,i]) & length(unique(data[,i])) > 10) # to perform generation of graphs only if the variablis numeric
    { 
      num_index[j] = i
      j= j+1
      
    }
    else if(length(unique(data[,i])) <= 5)
    { 
      cat_index[k] = i
      k = k+1
    }
    
  }
  
  for (numeric in num_index)
  {
    for (cat in cat_index)
    {
      File = paste(getwd(),'/',dataname,'Bivariate_Boxplot/',names(data)[numeric],'_',names(data)[cat], ".png", sep = "")
      if(!file.exists(File))
      {
        dir.create(dirname(File),showWarnings = F)
      }
      
      png(File)
      boxplot(data[,numeric]~data[,cat],main= paste("Boxplot of", names(data)[numeric],'and',names(data)[cat]),
              xlab = names(data)[cat],ylab = names(data)[numeric],col = "Orange")
      dev.off()
    }
  }
  
}

data2 = read.csv('cars.csv')

data = read.csv('framingham.csv')
Bivariate(data2, 'cars')

Graphs(data, 'framingham')

colnames(data)


