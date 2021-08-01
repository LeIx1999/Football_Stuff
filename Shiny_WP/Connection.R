######################################################################################################################################
# Shiny App auf shinyapps.io deployen
# Verbindung mit der Seite herstellen
#####################################################################################################################################
# packages laden
library(rsconnect)

# Account authorisieren
setAccountInfo(
  name = 'jniesen',
  token='9F06C46B1D81ECB087FFCD5DA60DDA6D',
  secret = 'qcWI0s6wwXDpEco4tHBKVbD6sGzxf7aGuvgCQaTV'
)

# Script deployen
deployApp("C:/Users/janni/OneDrive - EUFH GmbH/R-Uebungen/Football Stuff/Football/Shiny_WP")





