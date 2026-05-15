#
library(revtools)
library(tidyverse)
library(readxl)
library(synthesisr)

# #dynamically set working directory
# home_wd = "C:/Users/Spencer/OneDrive/Desktop/UBC/MSc/Planetscope literature review/reference lists"
# ubc_wd = "C:/Users/spenshi/OneDrive/Desktop/UBC/MSc/Planetscope literature review/reference lists"
# 
# if(dir.exists(ubc_wd)){
#   wd <<- setwd(ubc_wd)
# } else {
#   wd <<- setwd(home_wd)
# }

#load reference lists to filter
# wos_scopus = read.csv('reference lists/wos&scopus_20240522.csv')
wos_scopus = read.csv("reference lists/combined wos scopus 20250818_2.csv", header = T)
review_list = read_excel("reference lists/review_list_8.xlsx")

#----filter combined wos/scopus reference list----
# #remove items from 2025
# end_year = 2024
# n_Inital = nrow(wos_scopus)
# wos_scopus = wos_scopus |> filter(Publication.Year <= end_year)
# n_ToEndYear = nrow(wos_scopus)
# cat("Records published later than",end_year,':',n_Inital,'-',n_ToEndYear,'=',n_Inital-n_ToEndYear)

# num_total = nrow(wos_scopus)
# cat('Number of publications in WOS or Scopus from',end_year,'or earler:' ,num_total)

#remove duplicate items
wos_scopus1 = wos_scopus %>% #make new "identifier" column based on Title
  mutate(identifier_title = gsub("the", "", #remove "the"
                                 gsub("\\s+", "", #no spaces
                                      gsub("[[:punct:]]", "", #no punctuation
                                           iconv(tolower(Title), to = "ASCII//TRANSLIT"))))) |> #no accents over letters
  #group_by(identifier) %>%
  distinct(identifier_title, .keep_all = T)

num_duplicates = nrow(wos_scopus) - nrow(wos_scopus1) 
cat('Number of duplicates between the two databases:',nrow(wos_scopus),'-',nrow(wos_scopus1),'=',num_duplicates)

#remove items without "forest" (not random forest) in the abstract or title
wos_scopus2 = wos_scopus1 %>%
  mutate(HasForest = (grepl("(?i)(?<!random )forest", wos_scopus1$Title, perl = TRUE) | grepl("(?i)(?<!random )forest", wos_scopus1$Abstract.Note, perl = TRUE))) |>
  filter(HasForest == T)


num_without_forest = nrow(wos_scopus1) - nrow(wos_scopus2)
cat('Number of papers where the only mention of "forest" is "random forest":',nrow(wos_scopus1),'-',nrow(wos_scopus2),'=',num_without_forest)

#----get items that don't appear in the current review list----
to_filter_df <- wos_scopus2 #the list that you want to filter
filtered_by_df <- review_list #the list of references that you want to filter it by

#add identifier_title column to filtered_by_df
filtered_by_df = filtered_by_df %>% #make new "identifier" column based on Title
  mutate(identifier_title = gsub("the", "", #remove "the"
                                 gsub(" ", "", #no spaces
                                      gsub("[[:punct:]]", "", #no punctuation
                                           iconv(tolower(Title), to = "ASCII//TRANSLIT"))))) #no accents over letters

#produce list of references that DO NOT appear in another
not_in_common <- anti_join(to_filter_df, filtered_by_df, by="identifier_title") # 'by="Title" means that you filter references by the "Title" column in the df

#produce list of references that appear in another
common = semi_join(to_filter_df, filtered_by_df, by = "identifier_title")

#produce list of columns that DO NOT appear in the other dataframe
to_filter_cols = tibble(cols = names(to_filter_df))
filtered_by_cols = tibble(cols = names(filtered_by_df))

uncommon_cols = anti_join(to_filter_cols, filtered_by_cols, by='cols')


#remove columns that were added prior to export
not_in_common = subset(not_in_common, select = -identifier_title)
not_in_common = subset(not_in_common, select = -HasForest)

# #write csv of output list
# write.csv(filtered, "filtered_refs.csv", row.names = FALSE)
write.csv(not_in_common, paste0('reference lists/','newrefs_20250818.csv'))

#write output RIS file
output_string = 'newrefs_20250818'
# write_bibliography(not_in_common, paste0(output_string,".ris"))

# write_bibliography(not_in_common, paste0('reference lists/', output_string,".bib"), format = 'bib')
# bib = read_bibliography(paste0('reference lists/', output_string,".bib"))
# write_bibliography(bib, paste0('reference lists/', output_string,".ris"), format = 'ris')

# parse_bibtex(bib)
# write_ris(not_in_common)

#----find publications in review list that don't appear in updated list----

review_list = review_list |>
  mutate(identifier_title = gsub("the", "", #remove "the"
                                                           gsub(" ", "", #no spaces
                                                                gsub("[[:punct:]]", "", #no punctuation
                                                                     iconv(tolower(Title), to = "ASCII//TRANSLIT"))))) #no accents over letters
wos_scopus2 = wos_scopus2 |>
  mutate(identifier_title = gsub("the", "", #remove "the"
                                                           gsub(" ", "", #no spaces
                                                                gsub("[[:punct:]]", "", #no punctuation
                                                                     iconv(tolower(Title), to = "ASCII//TRANSLIT"))))) #no accents over letters
not_in_dblists = anti_join(x = wos_scopus2 
                           # |> select(Publication.Year, Author, Title)
                           ,
                           y = review_list 
                           # |> select(`Publication Year`, Author, Title)
                           ,by='identifier_title'
                           )



