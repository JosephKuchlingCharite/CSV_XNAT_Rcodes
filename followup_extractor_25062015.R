## follow up extractor takes an original frame and arguments columnheader and factorvalue
## and creates a dataframe of all subjects with more than 1 acquisition regarding the
## parameter listed in the list of arguments

followup_extractor <- function(originalframe) {
        
        
        numofacq <- table(originalframe$Subject)
        ## displays all subjects with corresponding number of acquisitions
        
        fup1 <- numofacq > 1
        ## finds out which subjects have more than 1 acquisition
        
        followupsubjs <- numofacq[fup1]
        ## displays all subjects with more than 1 DTI acquisition (all follow - up 
        ## patients with number of acquisitions are displayed)
        
        fupnames <- dimnames(followupsubjs)
        ## drags out the names of all follow-up patients from followups
        
        subjcolumn <- originalframe[, "Subject"]
        ## creates a list of all Subjects from raw data table
        
        subjcolumchar <- as.character(subjcolumn)
        ## converts data frame of subjcolumn into character variable (for furhter 
        ## string processing)
        
        fupindex <- lapply(subjcolumchar, grep, fupnames)
        ## gives out 0 if no intersection between raw list of subjects and follow-up
        ## list of subjects is detected and gives out 1 if an element of raw data list
        ## of subjects is an element of follow-up list of subjects
        
        fupindexnaf <- fupindex > 0
        ## gives out NA if fupindex value is 0 and gives out TRUE if fupindex is > 0 
        ## (in this case 1). If patient is follow up than row number is TRUE, if not, 
        ## row- number is NA
        
        fupindextf <- is.na(fupindexnaf)
        ## gives out TRUE if fupindexnaf value is NA and gives out FALSE if fupindexnaf
        ## value was TRUE. If patient is follow up than row number is FALSE, if not,
        ## row-number is TRUE
        
        followuptable <- originalframe[!fupindextf, ]
        print(followuptable)
        ## creates the follow up patients data frame from the original table by "deleting" all 
        ## non-follow-up patients
        
     
}

