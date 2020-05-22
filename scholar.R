library(scholar)
library(plyr)
library(dplyr)
library(stringr)
library(rorcid)
library(utils)

Sys.setenv(ORCID_TOKEN = "Bearer d8fddf71-359a-44b2-87b8-5c139bc8dd3b")


id = c(
  
  "-i6EN7kAAAAJ", # Claudia Buntrock
  "PyioS4kAAAAJ", # David Ebert
  "BgSmYnUAAAAJ", # Mathias Harrer
  "4aBH4MkAAAAJ" # Marvin Franke
  
)

# Loop through IDs
pubs = list()
for (i in 1:length(id)){
  
  pubs[[i]] = scholar::get_publications(id[i]) %>% 
    mutate(journal = as.character(journal),
           name = scholar::get_profile(id[i])$name) %>%
    filter(journal != "") %>% 
    select(name, title, author, journal, number, year, cites)
  
}
do.call(rbind, pubs) -> pubs
pubs$title = as.character(pubs$title)

# Source outlist
source("outlist.R")

pubs %>% 
  filter(!title %in% outlist) -> pubs


# Try to get a few DOIs from ORCID
pubs.orcid = works(orcid_id("0000-0001-6820-0146")) # David Ebert

pubs.orcid$doi = NA
for (i in 1:nrow(pubs.orcid)){
  
  suppressWarnings({
  #grab the list
  temp = pubs.orcid$`external-ids.external-id`[[i]]
  
  #see if the doi is there 
  if (length(temp) > 0 && length(temp[ temp$`external-id-type` == "doi" ,  "external-id-value" ] > 0)) {
    pubs.orcid$doi[i] = temp[ temp$`external-id-type` == "doi" ,  "external-id-value" ]
  } #close if, this handles no dois and multiple ids
  })
  
}

pubs.orcid = pubs.orcid[!duplicated(pubs.orcid$title.title.value), ]
pubs.orcid %>% 
  mutate(title = title.title.value) %>% 
  select(title, doi) -> pubs.orcid
pubs.orcid = pubs.orcid[!duplicated(pubs.orcid$doi), ]

merge(pubs, pubs.orcid, by = "title", all.x = T) -> pubs
pubs = pubs[!duplicated(pubs$title), ]

# Write md's
for (i in 1:nrow(pubs)){
  
  
  # Get Tidy study name
  name = str_replace_all(pubs$title[[i]], " ", "-") %>% 
    str_replace_all(., "[^[:alnum:]]", "-") 
  
  # Get Author Names
  authors = as.character(pubs$author[[i]]) %>% 
    str_replace_all(., "DD Ebert", "admin") %>% 
    str_replace_all(., "D Ebert", "admin") %>% 
    str_replace_all(., "C Buntrock", "buntrock") %>% 
    str_replace_all(., "M Harrer", "harrer") %>% 
    str_replace_all(., "AC Zarski", "zarski") %>% 
    str_replace_all(., "A Zarski", "zarski") %>%
    str_replace_all(., "S Schlicker", "schlicker") %>%
    str_replace_all(., "I Titzler", "titzler") %>%
    str_replace_all(., "K Saruhanjan", "saruhanjan") %>%
    str_replace_all(., "K Saruhanjan", "saruhanjan") %>%
    str_replace_all(., "J Laferton", "laferton") %>%
    str_replace_all(., "JAC Laferton", "laferton") %>%
    str_replace_all(., "F Kählke", "kaehlke") %>%
    str_replace_all(., "F Kaehlke", "kaehlke") %>%
    str_replace_all(., "K Weisel", "weisel") %>%
    str_replace_all(., "KK Weisel", "weisel") %>%
    str_replace_all(., "A Etzelmüller", "etzelmueller") %>%
    str_replace_all(., "A Etzelmueller", "etzelmueller") %>%
    str_replace_all(., "M Franke", "franke") %>%
    str_replace_all(., "J Thielecke", "thielecke") %>%
    str_replace_all(., "J Freund", "freund") %>%
    strsplit(., ",")
  authors = authors[[1]]
  
  
  # Get Publication
  publication = paste0("*",pubs$journal[i], "*, ", as.numeric(pubs$number[i]))
  
  # Get Year
  year = as.Date(ISOdate(pubs$year[i], 1, 1)) 
  
  # Get title
  title = pubs$title[i] %>% 
    str_replace_all(., ",", " ") %>% 
    str_replace_all(., "'", "") %>% 
    str_replace_all(., "…", "") %>% 
    str_replace_all(., ":", "-")
  
  # Get doi
  doi = pubs$doi[i]
  
  ##### GERMAN VERSION ######
  
  # Make path
  path = paste0("content/de/publication/", name)
  
  # Drop out of iteration if it already exists
  if (dir.exists(path)){
    next
  }
  
  # Create directory
  dir.create(path)
  
  sink(paste0(path,"/","index.md"))
  cat("--- \n")
  cat("abstract: '' \n")
  cat("authors: \n")
  for (x in 1:length(authors)){
    cat(paste0(" - ", authors[x], "\n"))
  }
  if (is.na(doi)){
    cat("doi: '' \n")
  } else {
    cat(paste0("doi: '", doi, "' \n"))
  }
  cat("featured: false \n")
  cat(paste0("publication: '", publication, "' \n"))
  cat("publication_short: '' \n")
  if (is.na(year)){
    cat("publishDate: '' \n")
  } else {
    cat(paste0("publishDate: '", year, "' \n"))
  }
  cat(paste0("title: '", title, "' \n"))
  cat("url_code: '' \n")
  cat("url_dataset: '' \n")
  cat("url_pdf: '' \n")
  cat("url_poster: '' \n")
  cat("url_project: '' \n")
  cat("url_slides: '' \n")
  cat("url_source: '' \n")
  cat("url_video: '' \n")
  cat("---")
  sink()
  
  
  ##### ENGLISH VERSION ######
  
  # Make path
  path = paste0("content/en/publication/", name)
  
  # Drop out of iteration if it already exists
  if (dir.exists(path)){
    next
  }
  
  # Create directory
  dir.create(path)
  
  
  sink(paste0(path,"/","index.md"))
  cat("--- \n")
  cat("abstract: '' \n")
  cat("authors: \n")
  for (x in 1:length(authors)){
    cat(paste0(" - ", authors[x], "\n"))
  }
  if (is.na(doi)){
    cat("doi: '' \n")
  } else {
    cat(paste0("doi: '", doi, "' \n"))
  }
  cat("featured: false \n")
  cat(paste0("publication: '", publication, "' \n"))
  cat("publication_short: '' \n")
  if (is.na(year)){
    cat("publishDate: '' \n")
  } else {
    cat(paste0("publishDate: '", year, "' \n"))
  }
  cat(paste0("title: '", title, "' \n"))
  cat("url_code: '' \n")
  cat("url_dataset: '' \n")
  cat("url_pdf: '' \n")
  cat("url_poster: '' \n")
  cat("url_project: '' \n")
  cat("url_slides: '' \n")
  cat("url_source: '' \n")
  cat("url_video: '' \n")
  cat("---")
  sink()

}

# # # # # # # # # # # # # # # #
#                             #
#     Write press pages       #
#                             #
# # # # # # # # # # # # # # # #

# German

press = read.csv("press.csv")
press = press[order(-press$Jahr),] # sort
press$Link = as.character(press$Link)
press$Static = as.character(press$Static)
press$Title = as.character(press$Title) %>% 
  trimws()

sink("content/de/press/text.md")
cat("+++", "\n")
cat("widget = 'blank'", "\n")
cat("weight = 120", "\n")
cat(" ", "\n")
cat("[design.spacing]", "\n")
cat("padding = ['0px', '0', '0px', '100']", "\n")
cat("+++", "\n")
cat("\n")

cat("**Über uns wurde unter Anderem berichtet in:**", "\n")

cat("![](presse_logo.jpg)", "\n")
cat("\n")

cat("&nbsp;", "\n")
cat("\n")

for (i in 1:nrow(press)){
  
  if (press$Link[i] != ""){
    link = press$Link[i]
  } else {
    link = paste0("/files/press/", press$Static[i])
  }
  
  paste0("**", press$Title[i], ".**", " ",
         str_replace_all(press$Beschreibung[i], "\\.", ""), ". ", 
         "*", press$Quelle_Datum[i], "* ") %>% cat()
  paste0("[↗](", link, ")") %>% cat()
  cat("\n")
  cat("\n")
  
}

sink()


# English

press = press %>% filter(Lang == "en")

sink("content/de/press/text.md")
cat("+++", "\n")
cat("widget = 'blank'", "\n")
cat("weight = 120", "\n")
cat(" ", "\n")
cat("[design.spacing]", "\n")
cat("padding = ['0px', '0', '0px', '100']", "\n")
cat("+++", "\n")
cat("\n")

cat("**Über uns wurde unter Anderem berichtet in:**", "\n")

cat("![](presse_logo.jpg)", "\n")
cat("\n")

cat("&nbsp;", "\n")
cat("\n")

for (i in 1:nrow(press)){
  
  if (press$Link[i] != ""){
    link = press$Link[i]
  } else {
    link = paste0("/files/press/", press$Static[i])
  }
  
  paste0("**", press$Title[i], ".**", " ",
         str_replace_all(press$Beschreibung[i], "\\.", ""), ". ", 
         "*", press$Quelle_Datum[i], "* ") %>% cat()
  paste0("[↗](", link, ")") %>% cat()
  cat("\n")
  cat("\n")
  
}

sink()




