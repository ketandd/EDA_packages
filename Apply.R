#Q1 ======

data = read.csv('cars.csv')

pmiss <- function(x)
{return(sum(is.na(x))/length(x)*100)}  # p miss function


pmissing <- function(data)
{
  #creating a local function
  pmiss <- function(x)
  {
    return(sum(is.na(x))/length(x)*100)
  }
  
  miss <- apply(data, 2, pmiss)
  miss <- as.matrix(miss)
  colnames(miss) <- "% Missing"
  
  return(miss)
}

pmissingfor <- function(data)
{
  #creating a local function
  pmiss <- function(x)
  {
    return(sum(is.na(x))/length(x)*100)
  }
  
  miss <- c()
  
  for(i in 1:ncol(data))
  {
    m = (round((sum(is.na((data)[i]))/nrow(data)*100),3))
    miss <- c(miss,m)
  }
  
  
  miss <- as.matrix(miss)
  colnames(miss) <- "% Missing"
  rownames(miss) <- c(names(data))
  return(miss)
}


#Q1 ---- ends here-------------------------------------------------------------------------------

# Q2====

Outlier_det <- function (data)
{
  if (!is.data.frame(data))
  {
    print('Please input data frame')
    return(NULL)
  }
  outlier = c()
  name = c()
  data = Filter(is.numeric,data)
  j=1
  for(i in 1:ncol(data))
  {
    
    
    if (length(unique(data[,i])) > 10)
    {
      name[j] = names(data[i])
      x = data[i][!is.na(data[i])]
      
      upperbound = quantile(data[,i],0.75,na.rm = T) + 1.5 * IQR(data[,i],na.rm = T)
      lowerbound = quantile(data[,i],0.25,na.rm = T) - 1.5 * IQR(data[,i],na.rm = T)
      
      if (sum(x>upperbound) + sum(x<lowerbound) != 0)
      {
        outlier[j]= 'Outlier Detected'
      }
      else
      {
        outlier[j]= '_'
      }
      j = j+1
      
    }
    
  }
  
  outlier = matrix(data = outlier, nrow = length(name), dimnames = list(name,c('Outlier')))
  
  print(outlier,quote = F)
  
}

#Q2 ---- ends here-------------------------------------------------------------------------------

# Q3 ===

Outlier <- function (data)
{
  if (!is.data.frame(data))
  {
    print('Please input data frame')
    return(NULL)
  }
  uppercount = c()
  lowercount = c()
  name = c()
  data = Filter(is.numeric,data)
  j=1
  for(i in 1:ncol(data))
  {
    
    
    if (length(unique(data[,i])) > 10)
    {
      name[j] = names(data[i])
      x = data[i][!is.na(data[i])]
      
      upperbound = quantile(data[,i],0.75,na.rm = T) + 1.5 * IQR(data[,i],na.rm = T)
      lowerbound = quantile(data[,i],0.25,na.rm = T) - 1.5 * IQR(data[,i],na.rm = T)
      
      uppercount[j] = sum(x>upperbound)
      lowercount[j] = sum(x<lowerbound)
      j = j+1
      
    }
    
  }
  
  outlier = matrix(data = c(uppercount,lowercount), nrow = length(name), dimnames = list(name,c(' upper outlier','lower outlier')))
  
  return(outlier)
  
}



#Q3 ----- ends here------------------------------------------------------------------------------

#"Q4"+++======

Outlier_log <- function (data)
{
  if (!is.data.frame(data))
  {
    print('Please input data frame')
    return(NULL)
  }
  uppercount = c()
  lowercount = c()
  name = c()
  data = Filter(is.numeric,data)
  j=1
  for(i in 1:ncol(data))
  {
    
    
    if (length(unique(data[,i])) > 10)
    {
      name[j] = names(data[i])
      x = log(data[i][!is.na(data[i])])
      
      
      
      upperbound = quantile(x,0.75,na.rm = T) + 1.5 * IQR(x,na.rm = T)
      lowerbound = quantile(x,0.25,na.rm = T) - 1.5 * IQR(x,na.rm = T)
      
      uppercount[j] = sum(x>upperbound)
      lowercount[j] = sum(x<lowerbound)
      j = j+1
      
    }
    
  }
  
  outlier = matrix(data = c(uppercount,lowercount), nrow = length(name), dimnames = list(name,c(' upper outlier','lower outlier')))
  
  return(outlier)
  
}



#===================================================end of try area===================================================

##HOMEWORK
##1. Try to create the same function using the for loop.


##2. Create a function that can detect the presence of outliers in the variables of a dataset
##   The function will accept the data as an argument and return you a matrix indicating
##   which variable has outliers in it. The output is expected as follows:
##
##   OUTPUT:
##                Outlier
##   Variable1    -
##   Variable2    Outlier Detected
##   Variable3    Outlier Detected
##   Variable4    -
##   Variable5    Outlier Detected
##   
##   etc.


##3. Create the function that will accept a data set as an argument and return the outlier
##   counts for each variables in the data set. The output is expected as follows
##
##   OUTPUT:
##                Upper Count     Lower Count
##   Variable1    0               0
##   Variable2    12              0
##   Variable3    1               21
##   Variable4    0               44
##   Variable5    0               1
##   
##   etc.



##4. Add an extra feature to the function in 3. The function should return an object with
##   two extra columns that gives the count of the outliers when log transformation of the
##   variable is taken.








