

Translocations <- function() {
  filename <-readline(prompt="Enter document name (saved VCF output from 10X LONGRANGER as CSV):")

  mydata <- read.csv(filename, header = TRUE, skip = 53)

  View(mydata)

  endcomp <- data.frame("eight" = (as.integer(substr(mydata$INFO, 5, 8))),"nine" = (as.integer(substr(mydata$INFO, 5, 9))),"ten" = (as.integer(substr(mydata$INFO, 5, 10))), "eleven" = (as.integer(substr(mydata$INFO, 5, 11))), "twelve"= (as.integer(substr(mydata$INFO, 5, 12))), "digit40" = (as.integer(substr(mydata$INFO,37,40))),"digit41" = (as.integer(substr(mydata$INFO,37,41))), "digit42" = (as.integer(substr(mydata$INFO,37,42))), "digit43" = (as.integer(substr(mydata$INFO,37,43))), "digit44" = (as.integer(substr(mydata$INFO,37,44))))

  endcomp[is.na(endcomp)] <- 0

  endmax <- pmax(endcomp$eight,endcomp$nine,endcomp$ten,endcomp$eleven,endcomp$twelve,endcomp$digit40, endcomp$digit41, endcomp$digit42, endcomp$digit43, endcomp$digit44)

  mydata$temp<- endmax

  mydata <- mydata[!grepl('DUP', mydata$ALT),]
  mydata <- mydata[!grepl('INV', mydata$ALT),]
  mydata <- mydata[!grepl('UNK', mydata$ALT),]
  mydata <- mydata[!grepl('DEL', mydata$ALT),]
  mydata <- mydata[!grepl('INV', mydata$INFO),]
  mydata <- mydata[!grepl('DEL', mydata$INFO),]
  mydata <- mydata[!grepl('DUP', mydata$INFO),]

  mydata <- mydata[!grepl('LOWQ', mydata$FILTER),]

  mydata <- mydata[order(mydata$ID),]

  callOdd <- mydata[ c(TRUE,FALSE),]

  callEven <- mydata[ !c(TRUE,FALSE),]

  circosData <- data.frame("Chromosome" = callOdd$X.CHROM, "chromStart" = callOdd$temp, "chromEnd" = callOdd$POS, "Chromosome.1" = callEven$X.CHROM, "chromStart.1" = callEven$temp, "chromEnd.1" = callEven$POS)


  circosData$Chromosome <- sub("^", "chr", circosData$Chromosome)
  circosData$Chromosome.1 <- sub("^", "chr", circosData$Chromosome.1)

  write.csv(circosData, file = "TranslocationsOutput.csv", row.names = FALSE)


  View(circosData)
}
