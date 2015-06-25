## Der XNAT-Sorter öffnet eine XNAT-CSV-Tabelle, überprüft anhand der in die Funktion
## eingegebenen Argumente welche Akquisitionen er herausfiltern soll und gibt diese
## in einer output-File (als R - bzw. XLS - File) aus.

## XNAT_sorter("Name der Input-CSV", "Name der Output - XLS/R", Sortierungsargumente)

XNAT_sorter <- function(input, output, ...) {
        
        ##LOads XNAT-CSV-File
        rohtabelle <- read.csv(file = input)
        
        argumentslist <- eval(substitute(alist(...)))
        ## Lists all additional arguments (sorting factors), that have been typed into
        ## the function brackets
        
        columnpositions <- positionfinder(argumentslist)
        ## positionfinder finds all arguments "Project", "Subject", "Gender", "Age" and 
        ## "Scans" and summarizes them in a "position" - vector where TRUE means "factor found"
        ## and FALSE means "no factor found" (oder andersherum?)
        
        #factorlist <- argumentslist[!columnpositions]        
        ## now all relevant factor names are extracted from the argumentslist to create
        ## a list with only the relevant factors
        
        dataframeextr <- rohtabelle
        
        for (i in 1:length(columnpositions)) {
                ## if columnposition = FALSE: GO THROUGH THE SORTER ALGORITHM
                ## (search for column and name etc.)
                if (!columnpositions[i]) {
                        
                        #posstring <- toString(columnpositions[i])
                        #posstring2 <- toString(columnpositions[i+1])
                        dataframeextr <- table_extractor(dataframeextr, toString(argumentslist[i]), toString(argumentslist[i+1]))
                        print(dataframeextr)
                        
                        #rm(posstring)
                        #rm(posstring2)
                        
                }
                ## if columnposition = TRUE: DON'T DO ANYTHING
                else {
                        halten <- 1
                }
        }
        
        ## now the filtered data frame is going to be stored as an xls and and .R - file
        ## by using dataframesaver function. The Output - Name is going to be 
        ## the outputname indicated by the output argument in the function argument
        ## list
        
        dataframesaver(output, dataframeextr)
}

## positionfinder finds all arguments "Project", "Subject", "Gender", "Age" and 
## "Scans" and summarizes them in a "position" - vector where 1 means "factor found"
## and 0 means "no factor found"

positionfinder <- function(Faktorenliste) {
        
        Integerliste <- lapply(Faktorenliste, grep, ignore.case = TRUE, "project | subject | gender | age | scans")
        ## searches for words project, subject etc. within Faktorenliste and stores integer list
        ## in Integerliste with 1 for found and 0 for not found
        
        NAIntegerliste <- Integerliste > 0
        ## creates list with NA if Integerliste value = 0 and True if Integerliste - value = 1
        
        TFIntegerliste <- is.na(NAIntegerliste)
        ## creates list with TRUE if NAInterliste value is NA and FALSE if NAIntegerliste
        ## is FALSE;
        ## ALL HITS ARE NOW STORED IN THE FALSE - INFORMATION OF TFIntegerliste!
        
        #Integervektor <- vector('integer')
        ## creates empty vector for conversion of list into an integer vector (for further
        ## processing)
        
        
        ## The Integervektor gets value 0 if TFIntegervektor was TRUE and value 1 if 
        ## TFIntegervektor was FALSE
        #for(i in 1:length(TFIntegerliste)) {
                        
         #    if (TFIntegerliste[i]) {
         #                       Integervektor[i] <- 0
          #              }
           #  else {
            #                    Integervektor[i] <- 1
             #           }
                
        #
        
        print(TFIntegerliste)
        ## prints out the TFIntegervektor with False for found and TRUE for not found within a vector        

        }

## Der Table Extractor nimmt ein Dataframe, sucht anhand eines parameterheaders nach dem 
## gewünschten Kriterium (parametervalue) und reduziert dann die Tabelle (extracting) bis
## nur die gewünschten Akquisitionen übrigbleiben

table_extractor <- function(frametable, parameterheader, parametervalue) {
        
        parameterheader2 <- grep(parameterheader, colnames(frametable), ignore.case = TRUE, value = TRUE)
        #sucht die Headerspalte heraus (case insensitive)
        
        headercolumn <- frametable[, parameterheader2]
        ## creates a data frame of the parameterheader column (headercolumn)
        
        parameterrows <- grep(parametervalue, headercolumn, ignore.case=TRUE)
        ## creates a vector of all row numbers where parametervalue is part of the Sequence names
        
        allparvalue <- frametable[parameterrows, ]
        print(allparvalue)
        ## calculates a dataframe out of the raw table which only consists of acquisitions
        ## which consist of DTI (allDTI) and displays dataframe of all 
        ## acquisitions containing DTI acquisitions
        
        #numofpar <- table(allparvalue$headercolumn)
        ## displays all subjects with corresponding number of acquisitions
        
        #acq1 <- numofpar > 0
        ## finds out which subjects have more than or equal to one DTI acquisition - DTI 
        ## patients
        
        #acquisitionsubj <- numofpar[acq1]
        #print(acquisitionsubj)
        ## displays all subjects with at least 1 DTI acquisition (all DTI patients
        ## with number of acquisitions are displayed) 
}
        
        
dataframesaver <- function(output, dataframeextr2) {
        
        ## Function to save as xls open data frames (tables) in excel
        ## source: modified from stackoverflow.com: Viewing tables of data in R
        
        ## puts output and .xls together for the data name string
        
        xlsstring <- ".xls"
        Rstring <- ".R"
        xlsfilename <- paste(output, xlsstring, sep="")
        Rfilename <- paste(output, Rstring, sep=)
        
        write.table(dataframeextr2, file = xlsfilename, col.names=NA)
        ## writes a table (saved as .xls file) which is supposed to be opened by 
        ## Microsoft Excel - includes all DTI-Follow-up- Patients
        
        #system(paste('open -a \"/Applications//Microsoft Office 2011/Microsoft Excel.app\"', xlsfilename)
        ## automatically opens the DTI follow up patients table 
        ## Cave: Many warning messages (could be optimized further in the future)
        
        ## Saving data frames as .R - files by using dput (analogous to JHU R Programming
        ## lectures on coursera) and all frames in dump (analogous to coursera)
        
        dput(dataframeextr2, file = Rfilename)
        ## Saving DTI follow up framework in an R file which can be easily redisplayed
        ## by using: dget("DTI_Follow_up_patients.R")
        
        
}
