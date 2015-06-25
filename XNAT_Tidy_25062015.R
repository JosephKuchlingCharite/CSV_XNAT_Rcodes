##Processing XNAT-DATABASE-CHART into TIDY - DATA - FORM
##according to H. Wickhams "Tidy Data" framework

## mother function of all XNAT Tidy processing (XNAT_Tidy) which needs an input
## csv-file (converted from excel file into csv with ; as seperators) and an output
## name for saving the final dataframe into .R and .csv

XNAT_Tidy <- function(inputcsv, output) {

        ## reads in dataframe of CSV-file and seperates fields with ";" . NAs all strings that
        ## are empty!
        dataframe <- read.csv(inputcsv, sep=";", na.strings="")
        
        # dataframe2 <- read.csv("CSV_XNAT_02.csv", sep=";", na.strings="", nrows=30)
        # library("splitstackshape")
        # this would split the one column into different columns if the newest version
        # of R would have been installed
        # dataframe3 <- dataframe2
        # dataframe3 <- cSplit(dataframe2, "Scans", sep=" ", type.convert=FALSE)
        
        
        ## load Hadley Wickhams Tidy Data libraries
        library("reshape")
        library("reshape2")
        library("ggplot2")
        
        ## removes column header SCANS and replaces it with "X0"
        dataframe_allx <- dataframe
        names(dataframe_allx)[9]<- "X0"


        ## Melt data to get all sequence names within the field as variable parameters. All
        ## NA values within the sequence fields will not be included into the finally melted
        ## table.

        XNAT_molten1 <- melt(dataframe_allx, id=c("Label", "Project", "Date", "Subject", "M.F", "Age", "Type", "Scanner"), na.rm = TRUE)


        ## Replace all X1, X2... within the variable column with 1 (for positive) 
        XNAT_einser1 <- XNAT_molten1
        XNAT_einser1[,"variable"]=1

        ## swap columns 
        XNAT_einser_sw <- XNAT_einser1
        XNAT_einser_sw <- XNAT_einser_sw[, c("Label", "Project", "Date", "Subject", "M.F", "Age", "Type", "Scanner", "value", "variable")]

        ## exchange column headers variable and value
        XNAT_einser_ex <- XNAT_einser_sw
        names(XNAT_einser_ex) <- c("Label", "Project", "Date", "Subject", "M.F", "Age", "Type", "Scanner", "variable", "value")


        ## cast all sequences into columnheaders
        XNAT_cast1 <- XNAT_einser_ex
        XNAT_cast1 <- cast(XNAT_cast1, Label + Project + Date + Subject + M.F + Age + Type + Scanner ~ variable, length)
        

        ## save dataframe into temporary CSV - file (for rebooting the data properties which
        ## must have changed somehow as a result of melting and casting (another melt will
        ## give  different results...))

        write.csv(XNAT_cast1, file="CSV_XNAT_temporary_save.csv")

        ## load temporarily saved XNAT file for rebooting the data frame
        
        newXNAT_frame <- read.csv("CSV_XNAT_temporary_save.csv")

        ## melt newly loaded dataframe to get the desired data frame BUT DON'T FORGET THE
        ## X column header and variables which have been created while loading the reboot
        ## file (otherwise the data structure will once again be confusing)

        newXNAT_molten <- melt(newXNAT_frame, id = c("X", "Label", "Project", "Date", "Subject", "M.F", "Age", "Type", "Scanner"), rm.na = TRUE)

        # Delete X - column
        
        tidy_data_frame_01 <- newXNAT_molten
        tidy_data_frame_01$X <- NULL
        

        # Switch columns into the desired order
        
        tidy_data_frame_02 <- tidy_data_frame_01
        tidy_data_frame_02 <- tidy_data_frame_02[, c("Project", "Subject", "M.F", "Label", "Date", "Age", "Type", "Scanner", "variable", "value")]
        

        # Zero out all NAs in value column
        
        #tidy_data_frame_03 <- tidy_data_frame_02
        #tidy_data_frame_03$value[is.na(tidy_data_frame_03$value)] <- 0
        #View(tidy_data_frame_03)
        
        # NAing all Zeros in value column
        # just for practicing and for possible removal if required
        
        #tidy_data_frame_04 <- tidy_data_frame_03
        #tidy_data_frame_04$value[tidy_data_frame_03$value == 0] <- NA
        #View(tidy_data_frame_04)
        
        XNAT_Tidy_Frame <<- tidy_data_frame_02
        View(XNAT_Tidy_Frame)
        
        #Save tidy data with dataframeCSVsaver
        
        dataframeCSVsaver(output, XNAT_Tidy_Frame)

        ## Now data is hopefully ready for further visualisation with ggplot2
}