suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))

read_file <- function(filename) {
  start<-  str_locate(filename, "Results1_GROUPED - ")[,"end"]+1
  stop <- str_locate(filename, "Columns1.txt")[,"start"]-2
  groupid<- gsub(pattern = " ", replacement = "", x=unlist(strsplit(substr(filename, start, stop), ";")))
  myfile<- read.delim(filename)
  myfile <- myfile %>% select(CalcYear, CalcMonth, PolWeighted_E, PolWeighted_A, PolWeighted_Exposure)
  if(any(grepl("DUR_M", groupid))){
    # there is a DUR_M in the identifier
    starting<- as.integer(unlist(strsplit(groupid[grep("DUR_M", groupid)], split="="))[2])
    myfile[["POL_DUR_M"]] = seq(from=starting, length.out=nrow(myfile))
    #    print(filename)
    myfile <- myfile %>% mutate(POL_DUR = ifelse(POL_DUR_M <= 12, sprintf('M%02d', POL_DUR_M), sprintf('Y%02d', POL_DUR_M %/% 12 + 1))) 
    #%>% mutate(DUR_M = starting)
    groupid <- groupid[-grep("DUR_M", groupid)]
  }
  for(grp in groupid){
    var_name<-unlist(strsplit(grp, split="="))[1]
    var_value<- unlist(strsplit(grp, split="="))[2]
    myfile[[var_name]] <- rep(var_value, nrow(myfile))
  }
  myfile <- mutate_all(myfile, ~replace(., is.na(.), 0))    # replace any NA values with 0 so it doesn't affect the summation
#  myfile <- myfile %>% mutate(filename = basename(filename))
  return(myfile %>% filter(POL_DUR_M >0) )
}


if(interactive()){
  args = c('D:/Documents/Mo.net demo projects/ExperienceMonitoring/Results_SmokerSex', "Consolidated_SmokerSex.csv")
} else {
  args <- commandArgs(trailingOnly = TRUE)
}

base_path = gsub("\\", "/", args[1], fixed=TRUE)
if(str_sub(base_path, -1, -1) == "/") {base_path <- str_sub(base_path, 1, -2)}
targetfilename = paste0(base_path, "/", args[2])

#setwd(base_path)
print(paste0(Sys.time(), " Gathering filenames for consolidation."))
target_files <- list.files(path = base_path, pattern="GROUPED", full.names = TRUE)

record_max = length(target_files)*1 + 200*0   # change this when you want to debug something
print(paste0(Sys.time(), " Starting file consolidation."))
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = record_max, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

output <- data.table::rbindlist(lapply(target_files[1:record_max], function(x) 
                                                                    {setTxtProgressBar(pb, match(x, target_files)) 
                                                                      read_file(x)
                                                                      }
                                ))
close(pb)
print(paste0(Sys.time(), " File consolidation completed."))
compressed <- output  %>% select(-POL_DUR_M, -CalcMonth) %>% group_by(CalcYear, POL_DUR, Smoker, SEX) %>% summarise_all(sum)
compressed_2 <- output %>% select(-POL_DUR, -CalcMonth) %>% group_by(CalcYear, POL_DUR_M, Smoker, SEX)  %>% summarise_all(sum) 
#compressed <- output %>% select(-POL_DUR)

write.csv(compressed, file = targetfilename, row.names = F)
write.csv(compressed_2, file = gsub(".csv", "2.csv", targetfilename), row.names = F)
print(paste0(Sys.time(), " ", targetfilename, " has been written to disk."))
