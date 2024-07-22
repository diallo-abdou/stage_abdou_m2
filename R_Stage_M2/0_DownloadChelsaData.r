
rastlist_short = rastlist_short[indx_120]
rastlist_short_tasmin <- gsub("pr", "tasmin", rastlist_short)
rastlist_short_pet <- gsub("pr", "pet_penman", rastlist_short)



## pet ---------

# load the file containing all urls

# pet_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_PET.txt")
pet_urls <- read.table("C:/Users/diall/Downloads/datas/urls_pet.txt")
pet_urls = as.data.frame(pet_urls)
pet_urls$V1=as.character(pet_urls$V1)
pet_urls = pet_urls[1:nrow(pet_urls),]



indexes_pet <- sapply(rastlist_short_pet, function(file) {grep(file, pet_urls)})
pet_urls = pet_urls[indexes_pet]


# Can loop through and download all

# pet_urls <- pet_urls[-grep("198", pet_urls[,1]),] # probably won't need before then
# this also changes it to a vector
# should have also removed 197*


mainDir = "D:/CHELSEA"
subFolder <- "pet"

if (!dir.exists(file.path(mainDir, subFolder))) {
  dir.create(file.path(mainDir, subFolder), recursive = TRUE)
}


fileName = "CHELSA_pet_penman_12_2006_V.2.1.tif"

for(i in 82:length(pet_urls)){
  
  #  for(i in 297:nrow(pet_urls)){
  # i =471
  els <- strsplit(pet_urls[i],split='/', fixed=TRUE)
  fileName <- tail(els[[1]], n=1)
  
  download.file(pet_urls[i], destfile = file.path(mainDir, subFolder, fileName), 
                method = "curl")
  
  cat(i,"/",length(pet_urls),"\n")
}


library(beepr)
beep()
# 6  11



## tasmin ---------

# load the file containing all urls

# tasmin_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_PET.txt")
tasmin_urls <- read.table("C:/Users/diall/Downloads/datas/urls_tasmin.txt")
tasmin_urls = as.data.frame(tasmin_urls)
tasmin_urls$V1=as.character(tasmin_urls$V1)
tasmin_urls = tasmin_urls[1:nrow(tasmin_urls),]



indexes_tasmin <- sapply(rastlist_short_tasmin, function(file) {grep(file, tasmin_urls)})
tasmin_urls = tasmin_urls[indexes_tasmin]


# Can loop through and download all

# tasmin_urls <- tasmin_urls[-grep("198", tasmin_urls[,1]),] # probably won't need before then
# this also changes it to a vector
# should have also removed 197*



mainDir = "E:/CHELSEA"
subFolder <- "tasmin"


if (!dir.exists(file.path(mainDir, subFolder))) {
  dir.create(file.path(mainDir, subFolder), recursive = TRUE)
}

for(i in 1:length(tasmin_urls)){
  
  
  els <- base::strsplit(tasmin_urls[i],split='/', fixed=TRUE)
  fileName <- tail(els[[1]], n=1)
  
  
  download.file(tasmin_urls[i], destfile = file.path(mainDir, subFolder, fileName), 
                method = "curl")
  
  cat(i,"/",length(tasmin_urls),"\n")
}


library(beepr)
beep()


## pr ---------

# load the file containing all urls

# pr_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_pr.txt")
pr_urls <- read.table("C:/Users/diall/Downloads/datas/urls_pr.txt")
pr_urls = as.data.frame(pr_urls)
pr_urls$V1=as.character(pr_urls$V1)
pr_urls = pr_urls[1:nrow(pr_urls),]

# Can loop through and download all

# pr_urls <- pr_urls[-grep("198", pr_urls[,1]),] # probably won't need before then
# this also changes it to a vector
# should have also removed 197*


mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # external harddrive
mainDir = "D:/CHELSEA"
subFolder <- "pr"

if (!dir.exists(file.path(mainDir, subFolder))) {
  dir.create(file.path(mainDir, subFolder), recursive = TRUE)
}
i =222
for(i in 456:length(pr_urls)){
  
  #  for(i in 297:nrow(pr_urls)){
  
  els <- strsplit(pr_urls[i],split='/', fixed=TRUE)
  fileName <- tail(els[[1]], n=1)
  
  
  download.file(pr_urls[i], destfile = file.path(mainDir, subFolder, fileName), 
                method = "curl")
  
  cat(i,"/",length(pr_urls),"\n")
}


library(beepr)
beep()
# 6  11




## tas ---------

# load the file containing all urls

# tas_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_tas.txt")
tas_urls <- read.table("C:/Users/diall/Downloads/datas/urls_tas.txt")
tas_urls = as.data.frame(tas_urls)
tas_urls$V1=as.character(tas_urls$V1)
tas_urls = tas_urls[1:nrow(tas_urls),]

# Can loop through and download all

# tas_urls <- tas_urls[-grep("198", tas_urls[,1]),] # probably won't need before then
# this also changes it to a vector
# should have also removed 197*


mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # external harddrive
mainDir = "F:/CHELSEA"
subFolder <- "tas"

if (!dir.exists(file.path(mainDir, subFolder))) {
  dir.create(file.path(mainDir, subFolder), recursive = TRUE)
}

for(i in 480:length(tas_urls)){
  
  #  for(i in 297:nrow(tas_urls)){
  
  els <- strsplit(tas_urls[i],split='/', fixed=TRUE)
  fileName <- tail(els[[1]], n=1)
  
  
  download.file(tas_urls[i], destfile = file.path(mainDir, subFolder, fileName), 
                method = "curl")
  
  cat(i,"/",length(tas_urls),"\n")
}


library(beepr)
beep()
# 6  11


## tasmax ---------

# load the file containing all urls

# tasmax_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_PET.txt")
tasmax_urls <- read.table("C:/Users/diall/Downloads/datas/urls_tasmax.txt")
tasmax_urls = as.data.frame(tasmax_urls)
tasmax_urls$V1=as.character(tasmax_urls$V1)
tasmax_urls = tasmax_urls[1:nrow(tasmax_urls),]

# Can loop through and download all

# tasmax_urls <- tasmax_urls[-grep("198", tasmax_urls[,1]),] # probably won't need before then
# this also changes it to a vector
# should have also removed 197*


mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # external harddrive
mainDir = "E:/CHELSEA"
subFolder <- "tasmax"

if (!dir.exists(file.path(mainDir, subFolder))) {
  dir.create(file.path(mainDir, subFolder), recursive = TRUE)
}

for(i in 1:length(tasmax_urls)){
  
  #  for(i in 297:nrow(tasmax_urls)){
  
  els <- strsplit(tasmax_urls[i],split='/', fixed=TRUE)
  fileName <- tail(els[[1]], n=1)
  
  
  download.file(tasmax_urls[i], destfile = file.path(mainDir, subFolder, fileName), 
                method = "curl")
  
  cat(i,"/",length(tasmax_urls),"\n")
  }


library(beepr)
beep()
# 6  11







# fin abdou


















#### Download the CHELSA data
# this script uses a list of URLS from this website:
# https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2Fmonthly%2F
# too then download all the files (between 1990 and 2019 - chosen to probably encapsulate everything)
# for the following variables
# PET
# PR
# TAS
# TAS MAX
# TAS MIN
library(raster)

## note, I did this on my personal laptop, and have not rerun it again.
## Have to leave it on my external hard drive as well

mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # external harddrive



## PET ---------

# load the file containing all urls

pet_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_PET.txt")

# Can loop through and download all

pet_urls <- pet_urls[-grep("198", pet_urls[,1]),] # probably won't need before then
# this also changes it to a vector
# should have also removed 197*


mainDir <- "D:/CHELSA_MONTHLY-1980-2019" # external harddrive

subFolder <- "PET"


for(i in 1:length(pet_urls)){
    
#  for(i in 297:nrow(pet_urls)){
    
  els <- strsplit(pet_urls[i],split='/', fixed=TRUE)
  fileName <- tail(els[[1]], n=1)
  
  
  download.file(pet_urls[i], destfile = file.path(mainDir, subFolder, fileName), 
                method = "curl")}


library(beepr)
beep()

##### PR - precipitation amount ------


# load the file containing all urls

pr_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_PR.txt")


# Remove some
pr_urls <- pr_urls[-grep("198", pr_urls[,1]),] # probably won't need before then # becomes a vector
pr_urls <- pr_urls[-grep("197", pr_urls)] # probably won't need before then
# 354

# Can loop through and download all


subFolder <- "PR"


for(i in 1:length(pr_urls)){
  
  els <- strsplit(pr_urls[i],split='/', fixed=TRUE) # split string 
  fileName <- tail(els[[1]], n=1) # to get the filename
  
  
  download.file(pr_urls[i], destfile = file.path(mainDir, subFolder, fileName),
                method = "curl")}

beep()





## TEMPERATURE --------

# load the file containing all urls

ta_urls <- read.table("G:/My Drive/Landworm/UrbanSubGroup/envidatS3paths_TA.txt")


# Remove some
ta_urls <- ta_urls[-grep("198", ta_urls[,1]),] # probably won't need before then # becomes a vector
ta_urls <- ta_urls[-grep("197", ta_urls)] # probably won't need before then
# 360

# Can loop through and download all


subFolder <- "TA"


for(i in 1:length(ta_urls)){
  
  els <- strsplit(ta_urls[i],split='/', fixed=TRUE) # split string 
  fileName <- tail(els[[1]], n=1) # to get the filename
  
  
  download.file(ta_urls[i], destfile = file.path(mainDir, subFolder, fileName),
                method = "curl")}

beep()

