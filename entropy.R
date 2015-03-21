#-------------------START-FUNCTION-entropy----------------
# This function calculates the entropy of a given dataset

entropy <- function (dv_name, ent_data)
  
{
  # Get distinct values in dependent variable and find its probaility
  dv_output_class <- unique(ent_data[dv_name])
  
  # total no of observation
  nobs <- nrow(ent_data)
  
  # number of distinct result class
  noutput_class <- nrow(dv_output_class)
  
  # calculate the entropy of dataset
  # h(data) = - plog(p) - nlog(n) where (p = positive fraction, n = negtive)
  
  ent <- 0
  
  for(i in 1:noutput_class)
  {
    count <- sum(ent_data[dv_name]== toString(dv_output_class[i,1]))
    ent <- ent - 1*(count/nobs)*log(count/nobs, base = 2)
  }
  
  ent
  
}

#-------------------END-FUNCTION-entropy------------
