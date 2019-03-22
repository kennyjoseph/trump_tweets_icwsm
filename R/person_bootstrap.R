library(googlesheets)
library(genderizeR)
library(reticulate)

my_sheets <- gs_ls()

trump_person <- gs_title("trump_person_map")

nytimes_mapping <- data.table(gs_read(trump_person,"nytimes_attacked"))
nytimes_people <- unique(tolower(nytimes_mapping$name[nytimes_mapping$type == "person"]))
nytimes_people <- nytimes_people[!is.na(nytimes_people)]

# already done, after nyt updated page
existing_people <- gs_read(trump_person, "people_info_latest")

people_info <-  data.table(name=nytimes_people[! nytimes_people %in% existing_people$name])
first_names <- sapply(people_info$name, function(l){strsplit(l," ")[[1]][1]})
people_info$first_name <- first_names



g <- findGivenNames(unique(first_names),textPrepare = F)
g <- g[count > 10]
setnames(g, "name","first_name")


cong <- fread("../data/legislators-current.csv")
cong[, name := paste(first_name,last_name)]
cong[, name := tolower(paste(first_name,last_name))]


people_info <- merge(people_info, g, by="first_name",all.x=T)
people_info <- merge(people_info, cong, by="name",all.x=T)



source_python("../python/wikifier.py")
write.table(people_info$name[people_info$first_name.x != people_info$name],"./people_names.txt",
            row.names=F,quote=F,col.names=F,fileEncoding = "utf8")
wikify("./people_names.txt")

wiki_data <- fread("../data/wiki_data.csv")

people_info <- merge(people_info, wiki_data, all.x=T,by="name")

gs_ws_new(trump_person,"people_info_latest_updated",input=people_info)


