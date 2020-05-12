library(scholar)
library(plyr)
library(dplyr)
library(stringr)

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



# Write md's
for (i in 1:nrow(pubs)){
  
  # Get Tidy study name
  name = str_replace_all(pubs$title[[i]], " ", "-") %>% 
    str_replace_all(., "[^[:alnum:]]", "-") 
  
  # Get Author Names
  authors = as.character(pubs$author[[i]]) %>% 
    str_replace_all(., "DD Ebert", "admin") %>% 
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
  
  # Make path
  path = paste0("content/publication/", name)
  
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
  cat("doi: '' \n")
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