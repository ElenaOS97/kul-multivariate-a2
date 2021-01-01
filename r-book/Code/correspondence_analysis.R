#######################################################################
#  SETTING DIRECTORY AND UPLOADING DATA
#######################################################################
setwd ("C:/Users/Elena/Desktop/LEUVEN/Multivariate/2nd Assignment")
load("C:/Users/Elena/Desktop/LEUVEN/Multivariate/2nd Assignment/datacar.Rdata")

#######################################################################
# LIBRARIES NEEDED
#######################################################################
install.packages("ca")
library(ca)

install.packages("openxlsx")
library(openxlsx)

#######################################################################
# Are X and Y statistically dependent? --> CHI-SQUARE TEST
#######################################################################
chisq.test(datacar)

# Create row profiles table:

row_p = prop.table(data.matrix (datacar), margin=1)

mean = apply(row_p,2,mean)
row_profiles = rbind(row_p,mean)

write.xlsx(row_profiles,file="rows.xlsx")

# Create column profiles table:

col_p = prop.table(data.matrix (datacar), margin=2)

mean = apply(col_p,1,mean)
col_profiles = cbind(col_p,mean)

write.xlsx(col_profiles,file="columns.xlsx")


# P-value < alpha at all significant levels --> reject H0 
# where H0: X and Y are not statistically dependent 

# Consequently, it makes sense to use Correspondence Analysis
                                                    
#######################################################################
# CORRESPONDENCE ANALYSIS
#######################################################################

ca.out = ca(datacar)
summary(ca.out)

# First two dimensions FINE
# Rows: every car but s900 has qlt>890
# Columns: pwrf (qlt=781)

install.packages("factoextra")
library(ggplot2)
library(factoextra)
library(ggplot2)

# COLPRINCIPAL
fviz_ca_biplot(ca.out, repel=TRUE, 
               title = "Biplot, Correspondence Analysis",
               arrows=c(TRUE,TRUE), map="colprincipal",
               labelsize=4, xlim=c(-1.5,0.5))

#ROWPRINCIPAL
fviz_ca_biplot(ca.out, repel=TRUE, 
               title = "Biplot, Correspondence Analysis",
               arrows=c(TRUE,TRUE), map="rowprincipal",
               labelsize=4, xlim=c(-1.5,0.5))

# SYMBIPLOT
fviz_ca_biplot(ca.out, repel=TRUE, 
               title = "Biplot, Correspondence Analysis",
               arrows=c(TRUE,TRUE), map="symbiplot",
               labelsize=4, xlim=c(-1,1))

