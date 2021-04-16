#the input has to be a csv file obtained by the conversion of an excel file

qpcr_plate <- read.table("test.csv", header = TRUE, sep = ";")

rows <- qpcr_plate[,2]
rfu <- qpcr_plate[,7]

rfu <- as.numeric(gsub(",", ".", gsub("\\.", "", rfu))) # the commas in the rfu vector have to be replace by dot and then transformed in numeric

df <- data.frame(matrix(ncol = length(rows), nrow = 0))
df <- rbind(df, rfu)
colnames(df) <- rows

hist(t(df)) 

qpcr_plate_t <- cbind(t(df), row.names(t(df))) # transpose the df (columns become rows and viceversa). Attach a new column with the well name
qpcr_plate_t <- cbind(qpcr_plate_t, gsub("^[[:upper:]]","" , qpcr_plate_t[,2])) # add new column with the well number (from 1 to 24)
qpcr_plate_t <- cbind(qpcr_plate_t, gsub("\\d","" , qpcr_plate_t[,2])) # Add new column with row letter (from A to I) 


qpcr_plate_t <- qpcr_plate_t[order(as.numeric(qpcr_plate_t[,3])),] # order the rows according to the numeric value (from 1 to 24)
qpcr_plate_t <- cbind(qpcr_plate_t, max(as.numeric(qpcr_plate_t[,1]))/as.numeric(qpcr_plate_t[,1])) #create new column and insert the result of the following calculation: maximum value of the column (rfu)/rfu value of that row
qpcr_plate_t[,5] <- as.numeric(qpcr_plate_t[,5])*2
qpcr_plate_t[,5] <- round(as.numeric(qpcr_plate_t[,5]), digits = 2)
qpcr_plate_t <- qpcr_plate_t[,c(2,5)]

volumes <- qpcr_plate_t[,2]
volumes[ as.numeric(volumes)>9 ] <- 0 #volume threshold. samples with more than 9 ul have assigned 0
volumes[ as.numeric(volumes)<0 ] <- 0 #volume threshold. samples with less than 0 ul have assigned 0
wells <- qpcr_plate_t[,1]

output_table <- data.frame(matrix(ncol = 0, nrow = length(volumes)))
output_table <- cbind(output_table, wells)
output_table <- cbind(output_table, volumes)

write.csv(output_table[,1:2], file = "D:/work/haru/HK099_4/admin_2021-04-09 15-09-46_CC008578_bQ2_pooling_threshold.csv", row.names = FALSE )

table(volumes) #Check for each volume the number of corresponding samples
sum(as.numeric(output_table$volumes)) #print total volume of the pooled samples

