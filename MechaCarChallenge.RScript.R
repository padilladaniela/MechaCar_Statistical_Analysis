library(dplyr)
mechacar_mpg <- read.csv('MechaCar_mpg.csv',check.names = F, stringsAsFactors = F) #read as data frame
head(mechacar_mpg)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, 
   data=mechacar_mpg) # create multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,
           data=mechacar_mpg)) # summarize the model

suspension_coil <-read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F) # read as data frame
head(suspension_coil)
total_summary <- suspension_coil %>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), 
                                               SD = sd(PSI), .groups = "drop") #create a total summary table for the PSI column

lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot)%>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI),
                                                                            SD = sd(PSI), .groups = "keep") #create a summary table by Manufacturing Lot for the PSI column

all_lots <- t.test(suspension_coil$PSI, mu = 1500) # Generate the t.test for all Manufacturing Lots compared to the population mean of 1,500 pounds per square inch
lot_1 = subset(suspension_coil, Manufacturing_Lot == 'Lot1') # Get the data for Manufacturing Lot #1 only
lot_2 = subset(suspension_coil, Manufacturing_Lot == 'Lot2') # Get the data for Manufacturing Lot #2 only
lot_3 = subset(suspension_coil, Manufacturing_Lot == 'Lot3') # Get the data for Manufacturing Lot #3 only

lot_1_t.test <- t.test(lot_1[['PSI']], mu = 1500) # Generate the t.test for Manufacturing Lot 1 compared to the population mean of 1,500 pounds per square inch
lot_2_t.test <- t.test(lot_2[['PSI']], mu = 1500) # Generate the t.test for Manufacturing Lot 2 compared to the population mean of 1,500 pounds per square inch
lot_3_t.test <- t.test(lot_3[['PSI']], mu = 1500) # Generate the t.test for Manufacturing Lot 3 compared to the population mean of 1,500 pounds per square inch

