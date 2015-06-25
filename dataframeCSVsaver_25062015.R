## now the filtered data frame is going to be stored as an xls and and .R - file
## by using dataframesaver function. The Output - Name is going to be 
## the outputname indicated by the output argument in the function argument
## list


dataframeCSVsaver <- function(output, dataframeextr2) {
        
        ## Function to save as xls open data frames (tables) in excel
        ## source: modified from stackoverflow.com: Viewing tables of data in R
        
        ## puts output and .xls together for the data name string
        
        csvstring <- ".csv"
        Rstring <- ".R"
        csvfilename <- paste(output, csvstring, sep="")
        Rfilename <- paste(output, Rstring, sep="")
        
        write.table(dataframeextr2, file = csvfilename)
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
