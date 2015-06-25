## this function shows all followup patients of one chosen sequence within a
## scatter plot and a connection line between followup subjects with reference
## to respective date

longitudinal_visualisation <- function(tidydataframe, sequencename) {

subdataframe <- subset(tidydataframe[grep(sequencename, tidydataframe[, "variable"]), ], value == 1)
sequencedataframe <- subdataframe

## Subset to all patients with one DTI sequence (without zeros) + 
## only longitudinal data!!!

followupframe <- followup_extractor(subdataframe)
followups <<- followupframe


## extracts all follow up patients (patients who appear more than once within the data frame)

ggplot(followupframe, aes(x = Subject, y = Date, color= Subject, group = Subject)) + geom_point() + facet_wrap(~ Project, scale="free_x") + geom_line(aes(group = Subject))
## boxplot of longitudinal DTI patients (x axis) and the different dates
## of acquisition + connection of follow- up dates by a line per subject
## facetting is made by different projects!

}


