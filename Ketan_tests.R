hypo_tests <-function(data)
  
{
  binary <- c()   # segregated data(indexes of the columns of the data) in lists
  multi = c()
  numer_cont = c()
  numer_cat = c()
  for(i in 1:ncol(data))
  {
    
    if (length(unique(data[,i])) == 2) # find binary variables
    { 
      binary = c(binary,i)
      
      if (is.numeric(data[,i]))
      {
        
        numer_cat = c(numer_cat,i)
      }
      
    }
    
    else if (length(unique(data[,i])) >2 & length(unique(data[,i])) < 15) # find binary variables
    {multi = c(multi,i)
    
    if (is.numeric(data[,i]))
    {
      
      numer_cat = c(numer_cat,i)
    }
    }
    
    else if (is.numeric(data[,i]) & length(unique(data[,i])) >= 15)
    {
      numer_cont = c(numer_cont,i)
    }
    
    
  }
  
  PtestT = c()
  fir_var_t = c()
  sec_var_t = c()
  fir_var_chi = c()
  sec_var_chi = c()
  Ptestchi = c()
  Ptestaov = c()
  fir_var_aov = c()
  sec_var_aov = c()
  for (j in binary)
  {
    for (k in numer_cont)
    {
      fir_var_t = c(fir_var_t,names(data)[j])
      sec_var_t = c(sec_var_t,names(data)[k])
      PtestT = c(PtestT, (t.test(data[,k][data[,j] == unique(data[,j])[1]], data[,k][data[,j] == unique(data[,j])[2]]))$p.value)
      
      #print(paste('T-test for binary variable', names(data)[j], 'and','continuous variable',names(data)[k]))
      #print(t.test(data[,k][data[,j] == unique(data[,j])[1]], data[,k][data[,j] == unique(data[,j])[2]]))
      
    }
    for (a in numer_cat)
    {
      fir_var_chi = c(fir_var_chi,names(data)[j])
      sec_var_chi = c(sec_var_chi,names(data)[a])
      Ptestchi = c(Ptestchi, (chisq.test(data[,a], data[,j]))$p.value)
      #print(paste('Chi-squared test for binary variable', names(data)[j], 'and','categorical variable',names(data)[a]))
      #print(chisq.test(data[,a], data[,j]))
    }
  }  
  
  for (j in multi)
  {
    for (k in numer_cont)
    {
      fir_var_aov = c(fir_var_aov,names(data)[j])
      sec_var_aov = c(sec_var_aov,names(data)[k])
      Ptestaov = c(Ptestaov, (summary(aov(data[,k]~ data[,j])))[[1]][["Pr(>F)"]][1])
      #print(paste('Anova for multi variable', names(data)[j], 'and','continuous variable',names(data)[k]))
      #print(summary(aov(data[,k]~ data[,j])))
      
    }
    
  } 
  
  
  print("T-test results with binary variables: ", quote = F)
  print(data.frame(fir_var_t,sec_var_t,PtestT))
  print("Chi-square Test results with binary variables: ", quote = F)
  print(data.frame(fir_var_chi,sec_var_chi,Ptestchi))
  print("ANOVA results: ", quote = F)
  print(data.frame(fir_var_aov,sec_var_aov,Ptestaov))
  #print(length(fir_var_aov))
  #print(sec_var_aov)
  #print(Ptestaov)
  
  
}



data = read.csv("framingham.csv")

hypo_tests(data)


summary(aov(data$totChol~ data$TenYearCHD))#[[1]][["Pr(>F)"]][1]






