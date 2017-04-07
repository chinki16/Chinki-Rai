#Simple steps to write a package in R.
#To creat a package in R, You should have package devtools and roxygen2.
#step 1 
install.packages("devtools")
library(devtools)
install.packages("roxygen2")
library(roxygen2)

#Step 2
#Either set your directory or you can continue with current directory
create("chinki ")

#step3
chinki_profile=function(input)
{
  if (input=="Facebook")
    {
        print("https://www.facebook.com/chinki.rai.75")
    }
    else
       {
           if (input=="GitHub")
            {
              Print ("https://github.com/Chinkirai/Chinki-Rai")
            }
             else 
                {
                  if(input=="Linkdine")
                   {
                     print ("https://www.linkedin.com/in/chinki-rai-4053b911a/")
        
                   }
                   else
                      print ("Contact to chinki at chinkichinki.rai@gmail.com")
                }
            }
        }
# Now its time to use package. Name of the package chinki
# To use the package 
install.packages("chinki")
library(chinki)
