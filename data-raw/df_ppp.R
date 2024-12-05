# Download new dataset with years
df_ppp <- read.csv("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,1.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...PPP_B1GQ.......?dimensionAtObservation=AllDimensions&format=csvfilewithlabels") %>%
  filter(Reference.area == "Netherlands") %>%
  select("TIME_PERIOD", "OBS_VALUE")  %>%
  rename("Year" = "TIME_PERIOD",
         "PPP" = "OBS_VALUE") %>%
  arrange(desc("Year"))

# save the dataset
usethis::use_data(df_ppp, overwrite = TRUE)
