
library(tidyverse)

####### Global Vars ############
cit.path = 'Citation/PaperCitation/'
subj.path = paste(cit.path, "SubjectArea/", sep = "")
min.citations = 4

#############################################################
#####       Data loading 1/3 Scopus search              #####
#############################################################
# set working directory

ScopusOriginal <- read.csv("scopusGunshotResidueAllyear2018.csv", sep=",", header=TRUE)

# Select the Year and EID of each record
TotalPublication <- ScopusOriginal %>%
  select(Year,EID)
#############################################################
#####       Data loading 2/3 Individual citation list   #####
#############################################################

# set extension and Citation
extension <- ".csv"
Citation <- Sys.glob(paste(cit.path, "*", extension, sep = ""))

# create a Reference column and add individual file name to it 
CitationData <- tibble(Reference = Citation) %>% # create a data frame
              # holding the file names
  mutate(file_contents = map(Reference,          # read files into
                             ~ read_csv(file.path("./", .))) # a new data column
  )
 
# remove the extension and path of the file in column reference
CitationData$Reference <- gsub(extension, "", Citation)
CitationData$Reference <- gsub(cit.path, "", CitationData$Reference)

# unnest the data
CitationDataUnnest <- unnest(CitationData)

# remove the citation occuring after 2018
CitationData2018 <- subset(CitationDataUnnest,Year<2019)

# aggregate the data and rename the column to Reference and Citation
CitationDataCountAll <- aggregate(CitationData2018$Reference, list(CitationData2018$Reference), FUN=length)
names(CitationDataCountAll) <- c("Reference","Citation")

# kept records with more than 5 citation 
CitationDataCount <- subset(CitationDataCountAll,Citation>-1)

CountPublicationsScopus <-subset(CitationData2018,EID %in% TotalPublication$EID)
CitationDataCountScopus <- aggregate(CountPublicationsScopus$Reference, list(CountPublicationsScopus$Reference), FUN=length)
names(CitationDataCountScopus) <- c("Reference","Scopus")

TopCitation <- merge(CitationDataCount, CitationDataCountScopus, by="Reference", all = T)
TopCitation$Scopus[is.na(TopCitation$Scopus)] <- 0

# Add the subject Area to the publication 
Subject <- Sys.glob(paste(subj.path, "*", extension, sep = ""))
SubjectDataNumber <- seq(Subject)

SubjectData <- tibble(Reference = Subject) %>% # create a data frame
# Holding the file names
  mutate(file_contents = map(Reference,          # read files into
                             ~ read_csv(file.path("./", .))) # a new data column
  )
SubjectData
SubjectData$Reference <- gsub(extension, "", Subject)
SubjectData$Reference <- gsub(subj.path, "", SubjectData$Reference)
SubjectDataUnnest <- unnest(SubjectData)
SubjectDataUnnest <- data.frame(SubjectDataUnnest)

# Generate collapse list of "Subject Area" for each Publications
SubjectAreaData <- SubjectDataUnnest %>%  group_by(EID) %>% summarise(SubjectArea = paste(Reference, collapse=", "))

SubjectAreaData <- as.data.frame(SubjectAreaData)

SubjectAreaDataCount <- aggregate(SubjectAreaData$EID, list(SubjectAreaData$SubjectArea), FUN=length)
sum(SubjectAreaDataCount$x)

SubjectArea <-subset(SubjectAreaData,EID %in% TopCitation$Reference)
names(SubjectArea) <- c("Reference","SubjectArea")

# Merge tables
TopCitationSubjectArea <- merge(TopCitation, SubjectArea, by="Reference", all = T)
TopCitationSubjectArea$Percentage <- round(TopCitationSubjectArea$Scopus/TopCitationSubjectArea$Citation*100, 1)

#############################################################
#####                      GRAPH                        #####
#############################################################

temp3 <- subset(TopCitationSubjectArea, Citation > min.citations)
temp1 <- data.frame(table(temp3$SubjectArea))
names(temp1) <- c("SubjectArea", "Freq");temp1
temp1 <-temp1[temp1$Freq > 4,];temp1

SubjA <- as.character(temp1$SubjectArea, use.names = FALSE); SubjA
sum(temp1$Freq)

ReduceTopCitationSujectArea <-subset(temp3, SubjectArea %in% SubjA);ReduceTopCitationSujectArea
ReduceTopCitationSujectArea$SubjectArea <- str_wrap(ReduceTopCitationSujectArea$SubjectArea,16)

dotplot_1 = ggplot(ReduceTopCitationSujectArea, aes(x=SubjectArea, y=Percentage)) + 
  geom_boxplot() + xlab("Subject Area") + ylab("Percentage /%") +
  geom_dotplot(aes(fill=SubjectArea), binaxis="y", show.legend = F,
               stackdir="center", binwidth=1.5) +
  theme(text = element_text(family = "Palatino"),
        axis.text.x = element_text(angle=0, vjust=0.6))
  
ggsave("Fig4_SubjectBoxplot.png",dotplot_1, width = 9, height = 6, units = "in", dpi=150)

