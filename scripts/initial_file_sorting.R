##########################################################################
# THIS CODE MAKES SOME QUALITY CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#

library(tidyverse)

# List binary and command files to be removed from repository if also data file exists

f <- c(list.files("data", pattern = "binary_", recursive = T, full.names = T),
       list.files("data", pattern = "command_", recursive = T, full.names = T))

for(i in f){ if(file.exists(gsub("binary_","data_",i)) | file.exists(gsub("command_","data_",i))){
  unlink(i)
} else {
  print(paste0("DATA FILE MISSING!!! ", i))
} 
}
# If no printed messages then no problems

# Haxo

f <- list.files("data", pattern = "-2021.ltd$", recursive = T, full.names = T)

for(i in f){ if(file.exists(gsub("-2021.ltd","-2021.csv",i))){
  unlink(i)
} else {
  print(paste0("DATA FILE MISSING!!! ", i))
} 
}

###########################################################################
# Check Tomst ID-numbers from last year data
maxdt <- read_csv("data/reading_times_2020.csv") %>% 
  mutate(site = site)

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none

# Site 38 has two files with different tomst ids
maxdt %>% filter(tomst_id == 94194223) # Actually 55
maxdt %>% filter(tomst_id == 94194231) # True 38
# Based on 2020 data tomst id 94194231 is real site 38
# And 94194223 belongs to site 55
fi %>% filter(site == 55) # And there is no 2021 data for site 55
# Thus I copy the 94184843 data to 71 folder
file.copy(paste0(getwd(), "/data/Hyytiälä21/TOMST/38/data_94194223_0.csv"),
          paste0(getwd(), "/data/Hyytiälä21/TOMST/55/data_94194223_0.csv"))
unlink("data/Hyytiälä21/TOMST/38/data_94194223_0.csv")

###########################################################################
# Update the file list

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none

#######################################################################
# Check if missing sites in 2021 data
all <- full_join(fi, maxdt %>% rename(tomst_id_20 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # No, Good!

# Non-matching sites
all %>% filter(!complete.cases(.))

# No sites that occur only in 2021 data

#sites 8, 9, 24, 31, 34, 49 in 2020 data but not in 2021
all %>% filter(tomst_id == 94194220) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194218) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194215) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194211) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194217) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194352) # No such tomst_id in 2021 data, so it is fine


# For sites 8, 9, 24, 31, 34, 49 find 2020 data and copy to repository
f2 <- list.files("C:/Users/OMISTAJA/OneDrive - University of Helsinki/R_Projects/microclim_suomi/raw_field_data",
                 pattern = "data_", recursive = T, full.names = T)

# Copy site 8 data from last year data
f2[grepl("94194220", f2)]
dir.create(paste0(getwd(), "/data/hyytiala_2021/8"))
file.copy(f2[grepl("94194220", f2)],
          paste0(getwd(), "/data/hyytiala_2021/8/data_94194220_0.csv"))

# Copy site 9 data from last year data
f2[grepl("94194218", f2)]
dir.create(paste0(getwd(), "/data/hyytiala_2021/9"))
file.copy(f2[grepl("94194218", f2)][1],
          paste0(getwd(), "/data/hyytiala_2021/9/data_94194218_0.csv"))
file.copy(f2[grepl("94194218", f2)][2],
          paste0(getwd(), "/data/hyytiala_2021/9/data_94194218_1.csv"))

# Copy site 24 data from last year data
f2[grepl("94194215", f2)]
dir.create(paste0(getwd(), "/data/hyytiala_2021/24"))
file.copy(f2[grepl("94194215", f2)],
          paste0(getwd(), "/data/hyytiala_2021/24/data_94194215_0.csv"))

# Copy site 31 data from last year data
f2[grepl("94194211", f2)]
dir.create(paste0(getwd(), "/data/hyytiala_2021/31"))
file.copy(f2[grepl("94194211", f2)],
          paste0(getwd(), "/data/hyytiala_2021/31/data_94194211_0.csv"))

# Copy site 34 data from last year data
f2[grepl("94194217", f2)]
dir.create(paste0(getwd(), "/data/hyytiala_2021/34"))
file.copy(f2[grepl("94194217", f2)],
          paste0(getwd(), "/data/hyytiala_2021/34/data_94194217_0.csv"))

# Copy site 49 data from last year data
f2[grepl("94194352", f2)]
dir.create(paste0(getwd(), "/data/hyytiala_2021/49"))
file.copy(f2[grepl("94194352", f2)],
          paste0(getwd(), "/data/hyytiala_2021/49/data_94194352_0.csv"))

########################################################################################
# Update file list

f <- list.files("data", pattern = "data_", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# Looks good still!!!

#######################################################################
# Check if Tomst ids match between years
all <- full_join(fi, maxdt %>% rename(tomst_id_20 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # These are fine

all %>% filter(tomst_id == tomst_id_20)
all %>% filter(tomst_id != tomst_id_20)
# All seems to match nicely!!!!!!!!!!


# Good to go and read the data


