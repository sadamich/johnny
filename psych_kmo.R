https://cran.r-project.org/web/packages/psych/refman/psych.html#KMO

library(psych)

str(Thurstone)
 num [1:9, 1:9] 1 0.828 0.776 0.439 0.432 0.447 0.447 0.541 0.38 0.828 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:9] "Sentences" "Vocabulary" "Sent.Completion" "First.Letters" ...
  ..$ : chr [1:9] "Sentences" "Vocabulary" "Sent.Completion" "First.Letters" ...

KMO(Thurstone)
Kaiser-Meyer-Olkin factor adequacy
Call: KMO(r = Thurstone)
Overall MSA =  0.88
MSA for each item = 
        Sentences        Vocabulary   Sent.Completion     First.Letters 
             0.86              0.86              0.90              0.86 
Four.Letter.Words          Suffixes     Letter.Series         Pedigrees 
             0.88              0.92              0.85              0.93 
     Letter.Group 
             0.87 
k.m <- KMO(Harman.political)   #compare to the results in Dziuban and Shirkey (1974)
k.m
Kaiser-Meyer-Olkin factor adequacy
Call: KMO(r = Harman.political)
Overall MSA =  0.81
MSA for each item = 
        Lewis     Roosevelt  Party Voting Median Rental Homeownership 
         0.73          0.76          0.84          0.87          0.53 
 Unemployment      Mobility     Education 
         0.93          0.78          0.86 

lowerMat(k.m$Image)
              Lewis Rsvlt PrtyV MdnRn Hmwnr Unmpl Mblty Edctn
Lewis          1.00                                          
Roosevelt     -0.76  1.00                                    
Party Voting   0.32 -0.52  1.00                              
Median Rental -0.15  0.08 -0.04  1.00                        
Homeownership -0.01  0.15  0.24 -0.23  1.00                  
Unemployment   0.22 -0.32 -0.13  0.16 -0.11  1.00            
Mobility       0.10 -0.13  0.26 -0.22  0.69  0.03  1.00      
Education      0.27 -0.08  0.29 -0.57  0.26  0.24 -0.04  1.00

lowerMat(partial.r(Harman.political))   #identical to image, except for sign
              Lewis Rsvlt PrtyV MdnRn Hmwnr Unmpl Mblty Edctn
Lewis          1.00                                          
Roosevelt      0.76  1.00                                    
Party Voting  -0.32  0.52  1.00                              
Median Rental  0.15 -0.08  0.04  1.00                        
Homeownership  0.01 -0.15 -0.24  0.23  1.00                  
Unemployment  -0.22  0.32  0.13 -0.16  0.11  1.00            
Mobility      -0.10  0.13 -0.26  0.22 -0.69 -0.03  1.00      
Education     -0.27  0.08 -0.29  0.57 -0.26 -0.24  0.04  1.00