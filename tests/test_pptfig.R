# PURPOSE:
#   Example code utilizing integraltemplates package for creating a PowerPoint
#
# NOTES:
#
# PROJECT INFORMATION:
#   Name: All projects
#   Number: OH01
#
# HISTORY:
# Date		    Author         Remarks
# ----------- ------------   --------------------------------------------------
#	2022-04-12  Eben Pendleton Created. Added header and switched to local
#                                     data source
# 20220725    K Heal updated with better ic_ calls
#===============================================================================
# Packages
library(ggplot2)
library(integral)

###Figure
p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  integral::ic_ppttheme()

### pptfig Minimal example
my_pres <- ic_pptfig(
  fig = p,
  fignum = 1,
  author = "J. Doe",
  doctitle1 = "Discharge at USGS Stream Gage 01114500")

# saves out the pptx file
print(my_pres, "Output_default.pptx")
