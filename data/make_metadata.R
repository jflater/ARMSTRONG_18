library(readxl)
library(tidyverse)
# These two sheets aren't 100% done yet, update them. Standards to DNA Well #s
metadata_Jared_Armstrong_Rainfall <- read_excel("metadata_Jared_Armstrong_Rainfall.xlsx", 
                                                sheet = "DNA")
metadata_Concentration <- read_excel("metadata_Jared_Armstrong_Rainfall.xlsx", 
                                                sheet = "DNA_Concentration_2")
  
#View(metadata_Jared_Armstrong_Rainfall)
#Parse collumn to make new column for seqeuncing ID
df <- data.frame(do.call('rbind', strsplit(as.character(metadata_Jared_Armstrong_Rainfall$`label (treatment_plot_sample_depth_time)`),'_',fixed=TRUE)))
df

head(df[1,])
new_columns <- c("Treatment", "Plot", "Location_in_plot", "Depth", "Day")
colnames(df) <- new_columns
colnames(df) 
df$Sample_ID <- "NA"
levels(df$Treatment)

# Treatment info
# A is for Armstrong, W is for WORLE
df$Sample_ID[df$Treatment == "crop + manure without STRIP"] <- "ACM_" 
df$Sample_ID[df$Treatment == "crop + STRIP with manure"] <- "ACSM_" 
df$Sample_ID[df$Treatment == "crop + STRIP without manure"] <- "ACS_"
#unique(df$Sample_ID)

## should be no "NA" if uncoment above
df$Sample_ID[df$Treatment == "Armstrong B1 Manure"] <- "AM_"
df$Sample_ID[df$Treatment == "Armstrong B2 Manure"] <- "AM_"
df$Sample_ID[df$Treatment == "Armstrong B3 Manure"] <- "AM_"
df$Sample_ID[df$Treatment == "Worle B1 Manure"] <- "WM_"
df$Sample_ID[df$Treatment == "Worle B2 Manure"] <- "WM_"
df$Sample_ID[df$Treatment == "Worle B3 Manure"] <- "WM_"

new <- df %>%
  mutate(Plot = recode(Plot, p1 = "P1",
                             p2 = "P2",
                             p3 = "P3",
                             p4 = "P4",
                             p5 = "P5",
                             p6 = "P6",
                             p7 = "P7",
                             p8 = "P8",
                             p9 = "P9",
                       `Worle B1 Manure` = "NA",
                       `Worle B2 Manure` = "NA",
                       `Worle B3 Manure` = "NA",
                       `Armstrong B1 Manure` = "NA",
                       `Armstrong B2 Manure` = "NA",
                       `Armstrong B3 Manure` = "NA"),
         Depth = recode(Depth, d1 = "D1",
                        d2 = "D2"),
         Location_in_plot = recode(Location_in_plot, s1 = "S1", 
                                                     s2 = "S2",
                                                     s3 = "S3",
                                                     s4 = "S4",
                                                     s5 = "S5",
                                                     s6 = "S6",
                                                     s7 = "S7",
                                                     s8 = "S8",
                                                     s9 = "S9",
         `Worle B1 Manure` = "NA",
         `Worle B2 Manure` = "NA",
         `Worle B3 Manure` = "NA",
         `Armstrong B1 Manure` = "NA",
         `Armstrong B2 Manure` = "NA",
         `Armstrong B3 Manure` = "NA"),
         Depth = recode(Depth, d1 = "D1",
                               d2 = "D2",
                        `Worle B1 Manure` = "NA",
                        `Worle B2 Manure` = "NA",
                        `Worle B3 Manure` = "NA",
                        `Armstrong B1 Manure` = "NA",
                        `Armstrong B2 Manure` = "NA",
                        `Armstrong B3 Manure` = "NA"),
         Day = recode(Day, baseline = "TB",
                           t0 = "T000",
                           t0 = "T000",
                           t0 = "T000",
                           t0 = "T000",
                           t0 = "T000",
                           t0 = "T000",
                           t2 = "T002",
                          t14 = "T014",
                         t153 = "T153",
                      `Worle B1 Manure` = "T000",
                      `Worle B2 Manure` = "T000",
                      `Worle B3 Manure` = "T000",
                      `Armstrong B1 Manure` = "T000",
                      `Armstrong B2 Manure` = "T000",
                      `Armstrong B3 Manure` = "T000",
                      ))
  
unique(new$Day)

new$Sample_ID <- paste(new$Sample_ID, new$Day, "_", new$Plot, "_", new$Location_in_plot, "_", new$Depth)
unique(new$Sample_ID)

new <- new %>%
  mutate(Sample_ID = gsub(" ", "", Sample_ID))

metadata_Jared_Armstrong_Rainfall$sample_id <- new$Sample_ID
