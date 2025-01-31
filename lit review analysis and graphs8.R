
#booter function loads data/packages and sets up the environment. "df" is the base dataframe that gets worked on in subsequent sections

{ #RUN THIS
  booter <- function() { #function that does all the initial loading, filtering etc. so I can quickly reset the environment if I change anything
  
  #----load packages, load reference list, set working directory----
  
  library(tidyverse)
  library(splitstackshape)
  library(viridis)
  library(RColorBrewer)
  library(scales)
  library(ggplot2)
  library(readxl)
  library(sf)
  library(terra)
  library(rnaturalearth)
  library(giscoR)
  library(countrycode)
  # library(ggtext)
  library(networkD3)
  library(ggbreak)
  library(gsubfn)
  library(ggrepel)
  library(car)
  # library(circlize)
  library(ggalluvial)
  library(Polychrome)
  library(ggpubr)
  library(GGally)
  library(cowplot)
  library(ggforce)
  
  home_wd <- "C:/Users/Spencer/OneDrive/Desktop/UBC/MSc/Planetscope literature review/reference lists"
  ubc_wd <- "C:/Users/spenshi/OneDrive/Desktop/UBC/MSc/Planetscope literature review/reference lists"
  
  if(dir.exists(ubc_wd)){ #automatically set working directory depending on if I'm working from my laptop or work desktop
    wd <<- setwd(ubc_wd)
  } else {
    wd <<- setwd(home_wd)
  }
  
  review_list <<- 'review_list_7.2.xlsx'
  
  review_list_temp = tempfile(fileext = '.xlsx')
  file.copy(review_list, review_list_temp, overwrite = T)
  
  df_big <<- read_excel(review_list_temp, sheet = "main table")
  
  #----clean applications----
  #clean applications (i.e. make terminology consistent) NOTE: MAKE SURE EACH LINE IS THE SAME AS THE NEXT CODE BLOCK
  df_big$Application_cleaned = df_big$Application_recleaned %>%
    # str_replace_all('Leaf phenology|Solar illumination|Flowering phenology', 'Phenology') %>%
    str_replace_all('Carbon flux|Aboveground carbon', 'Aboveground biomass') %>%
    str_replace_all('Forest and tree mapping|Vegetation detection', 'Forest or tree mapping') %>%
    str_replace_all('Forest succession', 'Successional stage') %>%
    str_replace_all('Invasive plant', 'Planet species') %>%
    str_replace_all('Tree species diversity', 'Biodiversity') %>%
    str_replace_all('Wildlife diversity', 'Biodiversity') %>%
    str_replace_all('Leaf phenology|Flowering phenology|Land surface phenology|Solar illumination', 'Phenology') %>%
    str_replace_all('Insect disturbance|Tree decline|Drought|Defoliation|Wildlife disturbance', 'Tree health') %>%
    str_replace_all('Unspecified disturbance', 'Deforestation') %>%
    str_replace_all('Tree species', 'Forest or tree classification') %>%
    str_replace_all('Disease', 'Tree health') %>%
    # str_replace_all('Forest or tree classification diversity', 'Forest or tree classification') %>%
    str_replace_all('Canopy cover|Canopy height|Canopy openness|Max gap|Mean gap|Plant area density', 'Canopy structural metrics') %>%
    str_replace_all('Radiometric normalization', 'Image quality') %>%
    str_replace_all('Image quality investigation', 'Image quality') %>%
    str_replace_all('Image gap filling', 'Image quality') %>%
    str_replace_all('Cloud and shadow detection', 'Image quality') %>%
    str_replace_all('Stem diameter|Stem density|Basal area', 'Stem structural metrics') %>%
    str_replace_all('Successional stage', 'Forest or tree classification') %>% 
    str_replace_all('Species diversity', 'Tree species diversity') %>%
    str_replace_all('Plant species', 'Understory plant') %>%
    #new aggregation scheme
    str_replace_all('Harvesting|Fire severity|Fire area mapping|Fire severity|Windthrow mapping|Windthrow impact|Landslide|Kiln scar detection', 'Harvesting or abiotic disturbance') %>%
    str_replace_all('Phenologic trend analysis|Illumination conditions', 'Phenology') %>%
    str_replace_all('Tree disease|Insect defoliation detection|Tree mortality|Snag detection', 'Tree health') %>%
    str_replace_all('Forest or tree mapping|Forest or tree classification|Planet species detection|Land cover change', 'Land cover classification') %>%
    str_replace_all('Canopy structural metrics|Stem structural metrics', 'Forest structural metrics') %>%
    str_replace_all('Image quality', 'Data quality')
  
  #change applications for model performance
  df_big$PS_model_performance_to_aggregate = df_big$PS_model_performance_to_aggregate %>%
    # str_replace_all('Leaf phenology|Solar illumination|Flowering phenology', 'Phenology') %>%
    str_replace_all('Carbon flux|Aboveground carbon', 'Aboveground biomass') %>%
    str_replace_all('Forest and tree mapping|Vegetation detection', 'Forest or tree mapping') %>%
    str_replace_all('Forest succession', 'Successional stage') %>%
    str_replace_all('Invasive plant', 'Planet species') %>%
    str_replace_all('Tree species diversity', 'Biodiversity') %>%
    str_replace_all('Wildlife diversity', 'Biodiversity') %>%
    str_replace_all('Leaf phenology|Flowering phenology|Land surface phenology|Solar illumination', 'Phenology') %>%
    str_replace_all('Insect disturbance|Tree decline|Drought|Defoliation|Wildlife disturbance', 'Tree health') %>%
    str_replace_all('Unspecified disturbance', 'Deforestation') %>%
    str_replace_all('Tree species', 'Forest or tree classification') %>%
    str_replace_all('Disease', 'Tree health') %>%
    # str_replace_all('Forest or tree classification diversity', 'Forest or tree classification') %>%
    str_replace_all('Canopy cover|Canopy height|Canopy openness|Max gap|Mean gap|Plant area density', 'Canopy structural metrics') %>%
    str_replace_all('Radiometric normalization', 'Image quality') %>%
    str_replace_all('Image quality investigation', 'Image quality') %>%
    str_replace_all('Image gap filling', 'Image quality') %>%
    str_replace_all('Cloud and shadow detection', 'Image quality') %>%
    str_replace_all('Stem diameter|Stem density|Basal area', 'Stem structural metrics') %>%
    str_replace_all('Successional stage', 'Forest or tree classification') %>% 
    str_replace_all('Species diversity', 'Tree species diversity') %>%
    str_replace_all('Plant species', 'Understory plant') %>%
    #new aggregation scheme
    str_replace_all('Harvesting|Fire severity|Fire area mapping|Fire severity|Windthrow mapping|Windthrow impact|Landslide|Kiln scar detection', 'Harvesting or abiotic disturbance') %>%
    str_replace_all('Phenologic trend analysis|Illumination conditions', 'Phenology') %>%
    str_replace_all('Tree disease|Insect defoliation detection|Tree mortality|Snag detection', 'Tree health') %>%
    str_replace_all('Forest or tree mapping|Forest or tree classification|Planet species detection|Land cover change', 'Land cover classification') %>%
    str_replace_all('Canopy structural metrics|Stem structural metrics', 'Forest structural metrics') %>%
    str_replace_all('Image quality', 'Data quality')
  
  #----subset monster excel table----
  
  #replace spaces in colnames with "."
  new_column_names <- gsub(" ", ".", 
                           gsub("-", "_", colnames(df_big)))
  
  df_big <- setNames(df_big, new_column_names)
  
  # #remove unncessary bibliographic info
  # last_col = 'Legislative Body' #name of the last column in the table with bibliographic info (i.e. the last column before the stuff i'm interested in)
  # df_sub = data.frame(df_big[4:8], df_big[(which(names(df_big) == last_col) + 1):ncol(df_big)]) #subset dataframe to only have the first few bibliographic columns then all the columns to the right of 'last_col'
  
  #remove references flagged 'Exclude', 'Maybe exclude', and 'No_access'
  df_sub = subset(df_big, (is.na(Exclude) & is.na(Maybe_exclude) & is.na(No_access)))
  
  
  #Add Column with in-text citation
  citer = function(aut, y = "n.d.", delimiter1 = ";", delimiter2 = ","){
    
    a = str_count(aut, delimiter1)
    
    b = if(a == 0){
      
      c = str_split(aut, delimiter2)
      c[[1]][1]
      
    } else {
      
      if(a == 1){
        
        c = str_replace_all(aut, delimiter2, delimiter1)
        d = str_split(c, delimiter1)
        paste0(d[[1]][1]," & ",d[[1]][3])
        
      } else {
        
        c = str_replace_all(aut, delimiter2, delimiter1)
        d = str_split(c, delimiter1)
        paste0(d[[1]][1]," et al.")
        
      }
    }
    e = paste0(b, ", ",y)
    return(e)
  } #define function for turning Publication.Year and Authors into in-text citations
  
  cites = c()
  
  for(i in 1:nrow(df_sub)){
    a = citer(aut = df_sub$Author[i], y = df_sub$Publication.Year[i])
    cites[i] = a
  }
  
  df_sub$Citation = cites
  
  
  #get names of columns representing individual RS systems
  start_col_ind = which(new_column_names=='RS_systems') + 1 #index number of first RS col
  end_col_ind = grep('SysConcat', new_column_names)[1] -1
  rs_col_names <<- new_column_names[start_col_ind:end_col_ind]
  
  columns_to_select <- c(
    "Publication.Year", 
    "Citation", 
    "Title", 
    "Publication.Title", 
    "Item.Type", 
    "PS_timeseries", 
    "Application_cleaned", 
    "PS_role", 
    "PS_data.product", 
    "Radiometric_normalization_Approach", 
    "Additional_radiometric_normalization", 
    "Revisit_cleaned", 
    "Timeseries_length", 
    "n_timeseries_dates", 
    "no_scenes", 
    "no_study_sites", 
    "Revisit_density_class", 
    "N_features", 
    "non_PS_model_features", 
    "Indices_or_bands_2", 
    "Country_", 
    "Continent", 
    "Total_area_imaged_km2_clean", 
    "Acquisition_years", 
    "feature_selection", 
    "optimal_feature_subset", 
    "PS_output_spatial_resolution", 
    "ecosystem_type_biome", 
    "Pixel_object", 
    "Object_segmentation_alg",
    "Object_segmentation_alg_type",
    "Approach_method", 
    "Approach_method1", 
    "Analysis_performance_3",
    'PS_model_performance_to_aggregate',
    "data_fusion", 
    "Four_or_eight_bands", 
    "Relative_alg_performance", 
    "Relative_RS_performance",
    "Relative_RS_performance_2",
    "Non_PS_bands_in_other_RS_models",
    "RS_performance_to_aggregate",
    "Alg_performance_comparison", 
    "Multi_vs_unitemporal", 
    "N_other_RS_systems", 
    "RS_systems",
    "Alg_performance_to_aggregate",
    "Performance_object_pixel",
    "Supervision",
    rs_col_names # include the variable storing column names here
  )
  
  # Apply the subset operation
  df_sub <<- df_sub[, columns_to_select]
  
  #---- Define functions----
  # define function that deconcatenates columns which contain multiple comma separated values, creates a new dataframe with just the column of interest, and tallies how many elements for each value appear in the df
  
  splitter_counter <<- function(a){
    b <- paste(a, collapse = ", ")
    c <- cSplit(data.frame(b), "b", ", ", direction = "long")
    d <- c %>% count(b)
    d %>% mutate(perc = 100*(n/nrow(df_sub)))
  }
  
  # define another splitter-counter function that deconcatenates columns with whatever list delimiter you want
  
  splitter_counter2 <<- function(a, x){ #a is a column, x is a list delimiter (e.g. ", ", "; ")
    b <- paste(a, collapse = x)
    c <- cSplit(data.frame(b), "b", x, direction = "long")
    d <- c %>% count(b)
    d %>% mutate(perc = 100*(n/nrow(df_sub)))
  }
  
  #function that removes brackets and spaces before brackets from text strings. operate on a single value or a column/list
  
  bracket_cleaner <<- function(col){ #col is a dataframe column, list, or single value
    gsub("\\s*\\([^)]+\\)", "", col)
  }
  
  #extend the bracket_cleaner function to handle symbols other than round brackets
  
  bracket_cleaner2 <<- function(col, open_sym = "\\[", close_sym = "\\]") { #default square brackets
    # Construct the dynamic regex pattern using the provided symbols
    # Match zero or more whitespace characters followed by the opening symbol,
    # any characters (non-greedy) until the closing symbol, and the closing symbol itself
    pattern <- paste0("\\s*", open_sym, ".*?", close_sym)
    
    # Apply gsub to remove the text between the symbols and the preceding space
    gsub(pattern, "", col)
    
  }
  
  # #function for replacing delimiters within a certain style of bracket
  # 
  # replace_delimiter <<- function(x, old_sep = ', ', new_sep = '; ', open_brac = '(', close_brac = ')'){
  #   gsub()
  # }
  
  #function for extracting text that falls between two symbols (e.g. getting text that falls within brackets). 
  
  extract_between_symbols <<- function(col, open_sym = "\\(", close_sym = "\\)") {
    # Construct the dynamic regex pattern using the provided symbols
    pattern <- paste0("(?<=", open_sym, ")[^", close_sym, "]+(?=", close_sym, ")")
    # Apply str_extract to extract the text between the symbols
    str_extract(col, pattern)
  }
  
  #---- Formatting variables ----
  
  theme_set( #set default theme characteristics for every ggplot
    theme_classic() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
  )
  
  line_size <<- 1
  point_size <<- 4
  
  axis_label_size <<- 20
  axis_text_size <<- 15
  y_axis_lim <<- 64
  
  continent_palette <<- "Set2"
  
  simple_plot_col <<- "dodgerblue4"
  
  bicolor_plot_col <<- c("magenta3", "dodgerblue4")
  
  tricolor_plot_col <<- c('navy', 'seagreen', 'plum4')
  
  application_color_map <<- tibble(
    
    Application_cleaned = splitter_counter(df_sub$Application_cleaned)[['b']],
    
    application_colors = 
      # glasbey.colors(length(unique(
      # splitter_counter(df_sub$Application_cleaned)[['b']]))),
      createPalette(
        N = nrow(splitter_counter(df_sub$Application_cleaned)) #get number of colors based on number of applications
        , seedcolors = c("#ff3333", "#339966", "#336699") #seed to generate color map
      ),
    
    application_count = splitter_counter(df_sub$Application_cleaned)[['n']]
  )
  
  application_colors <<- setNames(application_color_map$application_colors,
                                  application_color_map$Application_cleaned)
}

booter()

df = df_sub} #RUN THIS

#####---------------------------------- Publication info -------------------------------------
#---- Eligibility ----

#access
df_big %>% group_by(No_access) %>% summarise(count = n())

#exclusion
df_big %>% mutate(excluder = ifelse(is.na(Exclude), Maybe_exclude, Exclude)) %>% group_by(excluder) %>% summarise(count = n())
df_big %>% filter(str_detect(Exclude, 'glish')|str_detect(Maybe_exclude, 'glish')) %>% select(Author, Title, Exclude) #how many not in english

#---- Publication year----

year_df = splitter_counter(df$Publication.Year)

# year_plot = ggplot(year_df, aes(x = b, y = n)) +
#   geom_col() +
#   labs(
#     x = "Publication year",
#     y = "Number of references",
#     fill = "Type") +
#   theme_classic()
# ##
# year_plot

year_plot = ggplot(year_df[year_df$b != 2024], aes(x = b, y = n)) +
  geom_point(
    size = point_size
  ) +
  geom_line(
    size = line_size
  ) +
  labs(
    x = "Publication year",
    y = "Number of references",
    fill = "Type") +
  ylim(0, ceiling(max(year_df$n)/5) * 5) +
  theme_classic()
##
year_plot




#---- Publication type----

pub_type_summ = df %>%
  # filter((is.na(Exclude) & is.na(Maybe_exclude))) %>%
  group_by(Item.Type) %>%
  summarise(n = n())
pub_type_summ


#---- Journals ----

j_df = df_big %>%
  group_by(`Publication Title`) %>%
  summarise(n())

#####---------------------------------- WHAT TYPE OF RESEARCH IS BEING DONE WITH Planetscope -------------------------------------
#---- biome/forest type----

biome_codes = tibble(biome_code = c(1:14), biome_name = c( #link the name of each WWF biome with its numerical code
  'Tropical and subtropical moist broadleaf forests (tropical and subtropical, humid)',
  'Tropical and subtropical dry broadleaf forests (tropical and subtropical, semihumid)',
  'Tropical and subtropical coniferous forests (tropical and subtropical, semihumid)',
  'Temperate broadleaf and mixed forests (temperate, humid)',
  'Temperate coniferous forests (temperate, humid to semihumid)',
  'Boreal forests/taiga (subarctic, humid)',
  'Tropical and subtropical grasslands, savannas, and shrublands (tropical and subtropical, semiarid)',
  'Temperate grasslands, savannas, and shrublands (temperate, semiarid)',
  'Flooded grasslands and savannas (temperate to tropical, fresh or brackish water inundated)',
  'Montane grasslands and shrublands (alpine or montane climate)',
  'Tundra (Arctic)',
  'Mediterranean forests, woodlands, and scrub or sclerophyll forests (temperate warm, semihumid to semiarid with winter rainfall)',
  'Deserts and xeric shrublands (temperate to tropical, arid)',
  'Mangrove (subtropical and tropical, salt water inundated)'))

biome_codes$biome_name <- gsub("\\(.*?\\)", "", biome_codes$biome_name) #removes the text in ellipses from the biome codes

{tr = 'Tropical' #aggregate different biomes into biome "types"
  te = 'Temperate'
  bo = 'Boreal'
  step = 'Grassland, scrubland, desert, tundra'
  biome_codes$biome_type = c(tr,tr,tr,te,te,bo,step,step,step,step,step,te,step,tr)}


# Extract and process the numbers, filtering out those not in the range 1:14
df <- df %>%
  mutate(biome_nums = str_extract_all(ecosystem_type_biome, "\\((\\d+)\\)") %>%
           lapply(function(x) {
             numbers <- unique(as.numeric(gsub("\\D", "", x)))
             numbers <- numbers[numbers >= 1 & numbers <= 14]
             return(numbers)
           })) %>%
  mutate(biomes = lapply(biome_nums, function(nums) {
    biome_name <- biome_codes$biome_name[biome_codes$biome_code %in% nums]
    return(paste(biome_name, collapse = "; "))
  }))

df$biomes <- as.character(df$biomes) #convert each vector of biome codes to a string

for(i in 1:length(df$biomes)){ #add the plantations and city parks and other things that aren't wwf biomes back to the 'biomes' column
  if(df$biomes[i] == ""){
    df$biomes[i] = df$ecosystem_type_biome[i]
  }
}

biomes_df = cSplit(df, splitCols = 'biomes', sep = "; ", direction = 'long') #create dataframe where row only has one forest biome value

biomes_df = subset(biomes_df, select = -biome_nums) #remove biome_nums column

biomes_df = biomes_df %>%
  mutate(Ecosystem_type = ifelse(str_detect(biomes, "plantation|Urban"),
                                 "Artificial",
                                 "Natural"))

#aggregate biomes by "type" (e.g. tropical, temperate)
biomes_df_agg <- biome_codes %>%
  mutate(biome_name = trimws(biome_name)) %>%
  right_join(biomes_df, by = c('biome_name' = 'biomes')) %>%
  distinct(Citation, 
           Title, 
           biome_type,
           .keep_all = TRUE) %>%
  group_by(biome_type) %>%
  summarise(count = n(), .groups = 'drop')

# write.csv(biomes_df, file = 'biomes_out.csv') #write csv with each forest ecosystem for each paper on its own line

#biomes summary (filter out NA values in cases where forest biome is not provided)
biomes_summ = splitter_counter2(df$biomes, " ; ") %>% filter(!is.na(b))

biomes_summ = biomes_summ %>% #add whether ecosystems are artificial or natural
  mutate(Ecosystem_type = ifelse(str_detect(b, ("plantation|Urban")),
                                 "Artificial",
                                 "Natural"))

##
ggplot(biomes_summ, aes(x = reorder(b, desc(-n)), y = n, fill = Ecosystem_type)) +
  geom_col() +
  labs(
    x = "Terrestrial ecosystem",
    y = "Number of references",
    fill = "Ecosystem type"
  ) +
  theme_classic() +
  scale_fill_manual(values = bicolor_plot_col) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  coord_flip()  # Flip the coordinates to make y-axis horizontal

#---- Application ----

#summarize applications

apps_df = df %>%
  filter(PS_role != 'Validation') %>%
  cSplit(splitCols = 'Application_cleaned', sep = ', ', direction = 'long')

apps_summ = apps_df %>%
  distinct(Title, Application_cleaned) %>%
  group_by(Application_cleaned) %>%
  summarise(n = n()) %>%
  rename(b = Application_cleaned)
  

n_min_pubs = 3 #minimum number of publications that an application must appear in to receive its own entry in the graphs etc.

n_apps_min = sum(apps_summ$n[apps_summ$n < n_min_pubs]) 

other_label = paste0('Applications in <',n_min_pubs,' references')

apps_summ_trunc = rbind( #group applications that appear in <n_min_pubs references as being 'Other'
  apps_summ %>% 
    select(b,n) %>% 
    filter(n >=n_min_pubs)
  , tribble(~b, ~n, other_label, n_apps_min))

app_bargraph = ggplot(
  apps_summ_trunc
  # apps_summ
  , aes(x = reorder(b , desc(-n)), y = n
  )) +
  # geom_col(fill = simple_plot_col) +
  geom_col(fill = 'seagreen')+
  labs(
    x = "",
    y = "Number of references"
  ) +
  theme_classic() +
  # scale_fill_manual(values = simple_plot_col) +
  coord_flip()  # Flip the coordinates to make y-axis horizontal
##
app_bargraph

app_bargraph_long = ggplot(
  apps_summ
  # apps_summ
  , aes(x = reorder(b , desc(-n)), y = n
  )) +
  # geom_col(fill = simple_plot_col) +
  geom_col(fill = simple_plot_col)+
  labs(
    x = "",
    y = "Number of references"
  ) +
  theme_classic() +
  # scale_fill_manual(values = simple_plot_col) +
  coord_flip()  # Flip the coordinates to make y-axis horizontal

#---- country ----

#cleaning and data prep
{
countries_df = df$Country_ %>%
  bracket_cleaner() %>%
  splitter_counter()
# countries_df = splitter_counter(df$Country_) #split comma-separated countries
# 
# countries_df$b = bracket_cleaner(countries_df$b) #remove bracketed text 

world_tot = ne_countries(returnclass = 'sf')

world = world_tot%>%
  select(geounit) %>%
  rename(b = geounit)

world_merged = world %>% left_join(countries_df)


#check if all countries are present before plotting

check = c()

for(i in 1:length(countries_df$b)){
  ifelse(countries_df$b[i] %in% world_tot$geounit,
         check[i] <- "Good",
         check[i] <- countries_df$b[i])
}

check_sorted = sort(check)
wrld_sorted = sort(world_tot$geounit)

# Define the plot limits (adjust these if necessary)
xmin <- -180
xmax <- 180
ymin <- -90
ymax <- 90
}

#world map
{
  ggplot() +
    geom_sf(data = world_merged, aes(fill = n)) +
    scale_fill_viridis(option = 'H')+
    labs(
      fill = 'Number of references') +
    theme_minimal_grid()+
    coord_sf(crs= "+proj=robin")
    
}

#---- biome/forest type----

biome_codes = tibble(biome_code = c(1:14), biome_name = c( #link the name of each WWF biome with its numerical code
  'Tropical and subtropical moist broadleaf forests (tropical and subtropical, humid)',
  'Tropical and subtropical dry broadleaf forests (tropical and subtropical, semihumid)',
  'Tropical and subtropical coniferous forests (tropical and subtropical, semihumid)',
  'Temperate broadleaf and mixed forests (temperate, humid)',
  'Temperate coniferous forests (temperate, humid to semihumid)',
  'Boreal forests/taiga (subarctic, humid)',
  'Tropical and subtropical grasslands, savannas, and shrublands (tropical and subtropical, semiarid)',
  'Temperate grasslands, savannas, and shrublands (temperate, semiarid)',
  'Flooded grasslands and savannas (temperate to tropical, fresh or brackish water inundated)',
  'Montane grasslands and shrublands (alpine or montane climate)',
  'Tundra (Arctic)',
  'Mediterranean forests, woodlands, and scrub or sclerophyll forests (temperate warm, semihumid to semiarid with winter rainfall)',
  'Deserts and xeric shrublands (temperate to tropical, arid)',
  'Mangrove (subtropical and tropical, salt water inundated)'))

biome_codes$biome_name <- gsub("\\(.*?\\)", "", biome_codes$biome_name) #removes the text in ellipses from the biome codes


# Extract and process the numbers, filtering out those not in the range 1:14
df <- df %>%
  mutate(biome_nums = str_extract_all(ecosystem_type_biome, "\\((\\d+)\\)") %>%
           lapply(function(x) {
             numbers <- unique(as.numeric(gsub("\\D", "", x)))
             numbers <- numbers[numbers >= 1 & numbers <= 14]
             return(numbers)
           }))

df <- df %>%
  mutate(biomes = lapply(biome_nums, function(nums) {
    biome_names <- biome_codes$biome_name[biome_codes$biome_code %in% nums]
    return(paste(biome_names, collapse = "; "))
  }))

df$biomes <- as.character(df$biomes) #convert each vector of biome codes to a string

for(i in 1:length(df$biomes)){ #add the plantations and city parks and other things that aren't wwf biomes back to the 'biomes' column
  if(df$biomes[i] == ""){
    df$biomes[i] = df$ecosystem_type_biome[i]
  }
}

biomes_df = cSplit(df, splitCols = 'biomes', sep = "; ", direction = 'long') #create dataframe where row only has one forest biome value

biomes_df = subset(biomes_df, select = -biome_nums) #remove biome_nums column

biomes_df = biomes_df %>%
  mutate(Ecosystem_type = ifelse(str_detect(biomes, ("plantation|Urban")),
                                 "Artificial",
                                 "Natural"))

# write.csv(biomes_df, file = 'biomes_out.csv') #write csv with each forest ecosystem for each paper on its own line

#biomes summary
biomes_summ = splitter_counter2(df$biomes, " ; ")

biomes_summ = biomes_summ %>% #add whether ecosystems are artificial or natural
  mutate(Ecosystem_type = ifelse(str_detect(b, ("plantation|Urban")),
                                 "Artificial",
                                 "Natural")) %>%
  filter(!is.na(b))

##
ggplot(biomes_summ, aes(x = reorder(b, desc(-n)), y = n, fill = Ecosystem_type)) +
  geom_col() +
  labs(
    x = "Terrestrial ecosystem",
    y = "Number of references",
    fill = "Ecosystem type"
  ) +
  theme_classic() +
  scale_fill_manual(values = bicolor_plot_col) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  coord_flip()  # Flip the coordinates to make y-axis horizontal

#group artificial ecosystems into a single category, remove 'artificial vs natural' as an aesthetic
df = df %>% 
  mutate(biomes_2 = ifelse(str_detect(biomes,'rban|lantation'), 'Urban vegetation or plantation', biomes)) %>%
  filter(!is.na(biomes_2)|biomes_2 != 'NA')
biomes_summ_2 = splitter_counter2(df$biomes_2, ' ; ') %>% filter(!is.na(b))


ggplot(biomes_summ_2, aes(x = reorder(b, desc(-n)), y = n)) +
  geom_col(fill = simple_plot_col) +
  labs(
    # x = "Terrestrial ecosystem",
    y = "Number of references",
  ) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  # scale_fill_manual(values = simple_plot_col) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  coord_flip()  # Flip the coordinates to make y-axis horizontal


#---- biomes versus applications ----

biomes_apps_df = biomes_df %>%
  cSplit(splitCols = 'Application_cleaned', direction = 'long', sep = ', ') %>%
  filter(!is.na(biomes)|biomes == 'NA') %>%
  mutate(biomes_2 = ifelse(str_detect(biomes, 'rban|plantation'), 'Urban vegetation or plantation', biomes))

biomes_apps_summ = biomes_apps_df %>%
  group_by(Application_cleaned, biomes_2) %>%
  summarise(count = n()) %>%
  left_join(tibble(Application_cleaned = apps_summ$b, app_freq = apps_summ$n)) %>%
  left_join(tibble(biomes_2 = biomes_summ_2$b, biome_freq = biomes_summ_2$n)) %>%
  mutate(Application_label = paste0(Application_cleaned, ' (',app_freq,')'),
         biome_label = paste0(biomes_2, ' (',biome_freq,')')) %>%
  filter(!is.na(Application_cleaned))


#sankey and alluvial plots (don't work because node height is not proportional to numbers of publications)
{
  library(ggsankey)
  app_colors <- setNames(application_color_map$application_colors, application_color_map$Application_cleaned)

  ggplot(biomes_apps_summ, aes(y = count,
                               axis1 = reorder(Application_cleaned, -app_freq),
                               axis2 = reorder(biomes_2, -biome_freq))) +
    geom_alluvium(aes(fill = Application_cleaned)) +
    geom_stratum() +
    geom_label(stat = "stratum", aes(label = str_wrap(after_stat(stratum), width = 50)), size = 2.4) +
    scale_fill_manual(values = app_colors) + # Apply custom colors
    scale_x_discrete(limits = c("Application", "Terrestrial biomes"), expand = c(0.15, 0.05)) +
    theme_classic()+
    theme(
      legend.position = 'none'
    )

  # biomes_apps_df_long = biomes_apps_df %>%
  #   select(Title, Application_cleaned, biomes_2) %>%
  #   left_join(tibble(Application_cleaned = apps_summ$b, app_freq = apps_summ$n)) %>%
  #   left_join(tibble(biomes_2 = biomes_summ_2$b, biome_freq = biomes_summ_2$n)) %>%
  #   pivot_longer(cols = c('Application_cleaned', 'biomes_2'), names_to = 'node', values_to = 'stratum') %>%
  #   mutate(stratum_height = ifelse(node == 'Application_cleaned', app_freq, biome_freq))

  biomes_apps_df_ntitleapps = biomes_apps_df %>% group_by(Title, Application_cleaned) %>% summarize(ntitleapps = n())
  biomes_apps_df_ntitlebiomes = biomes_apps_df %>% group_by(Title, biomes_2) %>% summarize(ntitlebiomes = n())

  biomes_apps_weights_df = merge(biomes_apps_df_ntitleapps, biomes_apps_df_ntitlebiomes) %>%
    mutate(weight = 1/ntitleapps)

  biomes_apps_df_long = biomes_apps_df %>%
    merge(biomes_apps_weights_df) %>%
    make_long(Application_cleaned, biomes_2
              , value = weight
              ) %>%
    left_join(tibble(node = apps_summ$b, n_apps_pubs = apps_summ$n)) %>%
    left_join(tibble(node = biomes_summ_2$b, n_biomes_pubs = biomes_summ_2$n)) %>%
    mutate(ordering = ifelse(x == 'Application_cleaned', n_apps_pubs, n_biomes_pubs))

  app_colors <- setNames(application_color_map$application_colors, application_color_map$Application_cleaned)

  ggplot(biomes_apps_df_long, aes(x = x,
                                  next_x = next_x,
                                  node = reorder(node, ordering),
                                  next_node = next_node,
                                  label = str_wrap(node, width = 50),
                                  value = value,
                                  fill = factor(node))) +
    geom_sankey(
      node.fill = 'lightgrey'
      ,type = 'alluvial'
      , width = 0.45
      , alpha = 0.75
    ) +
    # geom_sankey_label(
    #   # aes(label = str_wrap(node, width = 0.5)),
    #   fill = 'white'
    #     ,size = 2.4)+
    # # scale_fill_manual(values = app_colors) + # Apply custom colors
    # scale_x_discrete(labels = c("Application"
    #                             , "Terrestrial biomes")
    #                  # , expand = c(0.15, 0.05)
    #                  ) +
    theme_classic()+
    theme(
      legend.position = 'none'
    )

  # a = biomes_apps_summ %>% make_long(Application_cleaned, biomes_2)
  #
  # ggplot(biomes_apps_df %>% make_long(Application_cleaned, biomes_2), aes(x = x,
  #                next_x = next_x,
  #                node = node,
  #                next_node = next_node,
  #                fill = factor(node))) +
  #   geom_sankey() +
  #   scale_fill_discrete(drop=FALSE)
  }

#balloon plot


# ggballoonplot(biomes_apps_summ, x = 'Application_cleaned', y = 'biomes_2', size = 'count')

ggplot(biomes_apps_summ %>% arrange(desc(biome_freq))
       , aes(x = reorder(Application_label, -app_freq)
             , y = 
               reorder(
                 biome_label
                 , biome_freq)
             ,
             , size = as.integer(count)
             , label = count))+
  # geom_label(
  #   shape = 21
  #   , fill = 'lightblue'
  # )+
  geom_point(
    color = simple_plot_col
  )+
  labs(x = 'Application', y = 'Terrestrial ecosystem'
       , size = 'Publications'
  )+
  scale_size(name = 'Number of publications', range = c(2,8), breaks = c(2,4,6,8,10))+
  theme_minimal() +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, 40))+
  # scale_size_continuous(breaks = c(2,4,6,8,10))+
  theme(axis.text.x = element_text(
    angle = 45
    #,vjust = 
    ,hjust = 1)
    # ,legend.position = 'none'
  )

#####---------------------------------- DATA CHARACTERISTICS -------------------------------------
#---- Spatial characteristics----

  #data cleaning and prep
  {
    #define breaks for the log scale
    spat_breaks = c(
      0,
      10
      ,10^2
      ,10^3
      ,10^4
      ,10^5
      ,10^6
      ,10^7
      ,Inf
    )
    
      spat_labels <- c(
        paste0("≤ ", trimws(format(spat_breaks[-1][spat_breaks[-1] <= 10^7], big.mark = ",", scientific = FALSE))), 
        "10,000,000 <"
      )
      
      size_class_order = tibble(size_class = spat_labels, size_class_order = 1:length(spat_labels))
      
            scene_size_class_order = tribble(
        ~scene_size_class, ~scene_size_class_order,
        '≤ Dove Classic scene' , 1,
        '≤ Dove-R scene' , 2,
        '≤ Super Dove scene' , 3,
        '> PlanetScope scene', 4
      )
      
      # Apply the breaks and labels to the dataframe
      area_df = df %>%
        mutate(Total_area_imaged_km2_clean = as.numeric(bracket_cleaner(Total_area_imaged_km2_clean))) %>%
        filter(!is.na(Total_area_imaged_km2_clean)) %>%
        mutate(
          #provide size classes saying size of study area vs a planetscope scene (for color)
          scene_size_class = case_when(
            Total_area_imaged_km2_clean <= 192 ~ '≤ Dove Classic scene',
            Total_area_imaged_km2_clean <= 384 ~ '≤ Dove-R scene',
            Total_area_imaged_km2_clean <= 637 ~ '≤ Super Dove scene',
            .default = '> PlanetScope scene'
          ),
          size_class = cut(Total_area_imaged_km2_clean, #get size class for each study area based on spat_breaks (for histogram bins)
                           breaks = spat_breaks, 
                           include.lowest = F
                           ,labels = spat_labels
                           )
        ) #%>%
        # mutate(scene_size_class_order = case_match(scene_size_class, #get ordering variable for arranging scene size classes in stacked bar graph
        #                                            '≤ Dove Classic scene' ~ 1,
        #                                            '≤ Dove-R scene' ~ 2,
        #                                            '≤ Super Dove scene' ~ 3,
        #                                            .default = 4
        #                                            ))
    
    
    area_summ = area_df %>% #get number of publications in each size class and scene size class
      group_by(size_class, scene_size_class) %>%
      summarise(count = n()) %>%
      merge(size_class_order) %>% #add size class ordering variable
      merge(scene_size_class_order) #add scene size class ordering variable
  }

#plot stacked bargraph
{
    ggplot(area_summ, aes(
      x = size_class
      , y = count
      , fill = reorder(scene_size_class, -scene_size_class_order)
    )) +
      geom_bar(position = 'stack', stat='identity') +
      labs(
        # title = "Counts in Each Class",
        x = expression("Area imaged with Planetscope (km"^2*")"),
        y = "Number of references",
        fill = 'Size class'
      ) +
      scale_fill_manual(values = c('powderblue', 'turquoise', 'turquoise2', 'steelblue2' ))+
      # scale_fill_brewer(palette = 'Purples', direction = -1)+
      # scale_x_continuous(labels = area_hist$class_lab, breaks = area_hist$class_no) +  # Customize x-axis labels
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
      }
#median size of area imaged
median(area_df$Total_area_imaged_km2_clean)

#---- Temporal resolution (revisit) ----

#cleaning and prep
{
  splitter_counter(tolower(bracket_cleaner(df$PS_timeseries))) #how many use time series?
  
  df = df %>% #if revisit interval is not provided, calculate using number of dates and timeseries length
    mutate(Revisit_cleaned = bracket_cleaner(Revisit_cleaned)) %>%
    mutate(Revisit_cleaned = ifelse(
      # is.na(Revisit_cleaned) & str_detect(tolower(PS_timeseries),'y')
      (is.na(Revisit_cleaned) | Revisit_cleaned == 'NA') &
        !is.na(as.numeric(bracket_cleaner(n_timeseries_dates))) &
        !is.na(as.numeric(bracket_cleaner(Timeseries_length)))
      # !(is.na(n_timeseries_dates)|n_timeseries_dates == 'NA') &
      # !(is.na(Timeseries_length)|Timeseries_length == 'NA')
      ,
      as.numeric(bracket_cleaner(Timeseries_length))/(as.numeric(bracket_cleaner(n_timeseries_dates)) - 1),
      Revisit_cleaned
    ))
  
  revisit_df = df %>% #add values for histogram based on Revisit_density_class
    filter(str_detect(tolower(PS_timeseries), "y")) %>%
    mutate(Revisit_density_class = bracket_cleaner(Revisit_density_class)) %>%
    cSplit(splitCols = c('Revisit_density_class', 'Revisit_cleaned'), sep = c(', ', '; '), direction = 'long') %>%
    mutate(Revisit_cleaned = ifelse(grepl('\\d', Revisit_cleaned), Revisit_cleaned,
                                    ifelse(Revisit_density_class == 'Annual', 365,
                                           ifelse(Revisit_density_class == 'Monthly', 30,
                                                  ifelse(Revisit_density_class == 'Quarterly', 91,
                                                         'Error'))))) %>%
    filter(Revisit_cleaned != 'Error') %>%
    mutate(Revisit_cleaned = as.numeric(bracket_cleaner(Revisit_cleaned))) %>%
    filter(Revisit_cleaned != 'NA') %>%
    filter(!is.na(Revisit_cleaned)) %>%
    mutate(Revisit_cleaned = round(Revisit_cleaned))
  
  sort(revisit_df$Revisit_cleaned)
  median(revisit_df$Revisit_cleaned)#median time between images
  
  
  median(as.numeric(bracket_cleaner(revisit_df$Timeseries_length)), na.rm = T) #median timeseries length
  
  median(as.numeric(bracket_cleaner(revisit_df$n_timeseries_dates)), na.rm=T) #median number of dates in each timeseries
}

#compute histogram

{
  ##
  ggplot(revisit_df[revisit_df$Revisit_cleaned < 80,], aes(x = Revisit_cleaned, y = 0)) + geom_point(alpha = 0.2)
}

{
  # revisit_breaks = c(0,2,5,10,16,30,40,100,200,300,400,2190)
  # revisit_breaks = c(0,2,3.5,7,14,35,100,200,400,Inf)
  
  # revisit_breaks = c(0,4,9,18,60,113,380,Inf)
  # revisit_labels = c('Semi-weekly', 'Weekly', 'Bimonthly', 'Monthly', 'Quarterly', 'Annual', 'Multi-year')
  
  # revisit_breaks = c(0,3,6,9,12,20,40,60,80,120,340,400,Inf)
  revisit_breaks = c(0,2,5,8,12,20,40,60,80,120,340,400,Inf)
  
  # revisit_labels = sapply(2:length(revisit_breaks), function(i) paste0(revisit_breaks[i-1]," to ", revisit_breaks[i]))
  
  revisit_labels = sapply(2:length(revisit_breaks), function(i) 
    paste0(revisit_breaks[i-1] + ifelse(i > 2, 1, 0), " to ", revisit_breaks[i]))
  
  revisit_labels[length(revisit_labels)] = paste(revisit_breaks[length(revisit_breaks)-1],"<")
  
  
  revisit_hist = hist(revisit_df$Revisit_cleaned, breaks = revisit_breaks, plot = F)
  revisit_hist_df = tibble(count = revisit_hist$counts,
                           label = revisit_labels,
                           # label = revisit_labels,
                           class_no = 1:(length(revisit_breaks)-1))
  
  revisit_hist_df$label[nrow(revisit_hist_df)] = paste0(revisit_breaks[length(revisit_breaks)-1]," <")
  
  revisit_classes_key = tibble(Revisit_density_class = revisit_labels,
                               low_bound = revisit_breaks[1:(length(revisit_breaks)-1)],
                               up_bound = revisit_breaks[2:length(revisit_breaks)])  
  
  ##
  ggplot(revisit_hist_df, aes(x = reorder(label, class_no), y = count)) +
    geom_bar(stat = "identity"
             # , fill = simple_plot_col
             , fill = 'lightsalmon3') +
    labs(
      x = expression("Average time between images (days)"),
      y = "Number of references"
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#plot temp resolution versus time-series length
{
  ##
  ggplot(revisit_df %>% #number of dates in timeseries versus timeseries length
           filter(!is.na(as.numeric(bracket_cleaner(Revisit_cleaned))) & !is.na(as.numeric(bracket_cleaner(Timeseries_length)))) %>%
           mutate(Timeseries_length = as.numeric(bracket_cleaner(Timeseries_length))) %>%
           mutate(n_timeseries_dates = as.numeric(bracket_cleaner(n_timeseries_dates)))
         , aes(x = Timeseries_length, y = n_timeseries_dates)) +
    geom_point(alpha = 0.4, color = simple_plot_col) +
    labs(
      x = expression("Length of time series (days)"),
      y = "Number of dates in time series"
    ) +
    scale_x_continuous(
      # trans='log10', guide = 'axis_logticks',
      breaks = seq(0, max(as.numeric(bracket_cleaner(revisit_df$Timeseries_length)), na.rm = TRUE), by = 365)
    )+
    # scale_y_continuous(trans='log10', guide = 'axis_logticks')+
    scale_x_break(c(1200,2000))+
    # scale_y_break(c(175,230))+
    theme_classic() +
    theme(
      axis.title.x.top = element_blank(),
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank()
    )
  
  {
    # ##
    # ggplot(revisit_df %>% #average interval between images versus timeseries length
    #          filter(!is.na(as.numeric(bracket_cleaner(Revisit_cleaned))) & !is.na(as.numeric(bracket_cleaner(Timeseries_length)))) %>%
    #          mutate(Timeseries_length = as.numeric(bracket_cleaner(Timeseries_length))) %>%
    #          mutate(n_timeseries_dates = as.numeric(bracket_cleaner(n_timeseries_dates)))
    #        , aes(x = Timeseries_length, y = Revisit_cleaned)) +
    #   geom_point(alpha = 0.4, color = simple_plot_col) +
    #   labs(
    #     x = expression("Length of time series (days)"),
    #     y = "Average time between images (days)"
    #   ) +
    #   scale_x_continuous(
    #     # trans='log10', guide = 'axis_logticks',
    #     breaks = seq(0, max(as.numeric(bracket_cleaner(revisit_df$Timeseries_length)), na.rm = TRUE), by = 365)
    #                      )+
    #   # scale_y_continuous(trans='log10', guide = 'axis_logticks')+
    #   scale_x_break(c(1200,2000)) +
    #   theme_classic()
    
    # #digging in
    # revisit_df <- revisit_df %>% #add the revisit classes in the revisit_classes_key back to the original dataframe
    #   mutate(Revisit_density_class = case_when(
    #     Revisit_cleaned > revisit_classes_key$low_bound[1] & Revisit_cleaned <= revisit_classes_key$up_bound[1] ~ revisit_classes_key$Revisit_density_class[1],
    #     Revisit_cleaned > revisit_classes_key$low_bound[2] & Revisit_cleaned <= revisit_classes_key$up_bound[2] ~ revisit_classes_key$Revisit_density_class[2],
    #     Revisit_cleaned > revisit_classes_key$low_bound[3] & Revisit_cleaned <= revisit_classes_key$up_bound[3] ~ revisit_classes_key$Revisit_density_class[3],
    #     Revisit_cleaned > revisit_classes_key$low_bound[4] & Revisit_cleaned <= revisit_classes_key$up_bound[4] ~ revisit_classes_key$Revisit_density_class[4],
    #     Revisit_cleaned > revisit_classes_key$low_bound[5] & Revisit_cleaned <= revisit_classes_key$up_bound[5] ~ revisit_classes_key$Revisit_density_class[5],
    #     Revisit_cleaned > revisit_classes_key$low_bound[6] & Revisit_cleaned <= revisit_classes_key$up_bound[6] ~ revisit_classes_key$Revisit_density_class[6],
    #     Revisit_cleaned > revisit_classes_key$low_bound[7] & Revisit_cleaned <= revisit_classes_key$up_bound[7] ~ revisit_classes_key$Revisit_density_class[7]
    #   ))
    # 
    }
}

#summarize temporal characteristics by application
{
  revisit_app_summ = revisit_df %>%
    mutate(
      n_timeseries_dates = bracket_cleaner(n_timeseries_dates)
      , Timeseries_length = bracket_cleaner(Timeseries_length)
    ) %>%
    cSplit(splitCols = 'Application_cleaned', direction = 'long') %>%
    group_by(Application_cleaned) %>%
    summarise(
      count = n()
      , median_revisit_interval = median(as.numeric(Revisit_cleaned), na.rm = T)
      , mean_revisit_interval = mean(as.numeric(Revisit_cleaned), na.rm = T)
      , min_revisit = min(as.numeric(Revisit_cleaned), na.rm=T)
      , max_revisit = max(as.numeric(Revisit_cleaned), na.rm = T)
      , median_timeseries_length = median(as.numeric(Timeseries_length), na.rm = T)
      # , mean_revisit_interval = mean(as.numeric(Revisit_cleaned), na.rm = T)
      , mean_timeseries_length = mean(as.numeric(Timeseries_length), na.rm=T)
      , min_revisit = min(as.numeric(Timeseries_length), na.rm=T)
      , max_revisit = max(as.numeric(Timeseries_length), na.rm = T)
      , Citations_concat = paste0("(", paste(Citation, collapse = "), ("), ")")
    ) %>%
    filter(count > 1)
  # view(revisit_app_summ)
}

#temporal characteristics by revisit interval bargraphs
{
  revisit_app_df_reshaped = revisit_df %>%
    cSplit(splitCols = 'Application_cleaned', direction = 'long', sep = ', ') %>%
    mutate(n_timeseries_dates = bracket_cleaner(n_timeseries_dates),
           Timeseries_length = bracket_cleaner(Timeseries_length),
           Revisit_cleaned = as.character(Revisit_cleaned)) %>%
    pivot_longer(
      cols = c('Revisit_cleaned', 'n_timeseries_dates', 'Timeseries_length'),
      values_to = 'days',
      names_to = 'temporal_attribute') %>%
    filter(days != 'NA') %>%
    mutate(days = as.numeric(days))
  
  ggplot(revisit_app_df_reshaped, aes(x = Application_cleaned, y = days)) +
    geom_boxplot()+
    geom_point(
      alpha = 0.5,
      position = position_dodge2(width = 0.1)
    )+
    facet_wrap(vars(temporal_attribute), 
               ncol = 1,
               scales = 'free_y') +
    theme_classic()+
    theme(
      axis.text.x = element_text(
        angle = 45
        #,vjust = 
        ,hjust = 1)
      ,legend.position = 'none')
}



#---- Temporal resolution 2 ----

#data cleaning
{
temp_res_df = df %>%
  #filter to only include publications with multitemporal data
  mutate(PS_timeseries_cleaned = bracket_cleaner(PS_timeseries)) %>%
  filter(tolower(PS_timeseries) != 'n') %>%
  #split column to handle publications where different scales of timeseries monitoring are used
  cSplit(splitCols = 'Revisit_cleaned', sep = '; ', direction = 'long') %>%
  #clean brackets, filter NA values from Timeseries_length and n_dates so that columns are numeric
  filter(Timeseries_length != 'NA') %>%
  filter(n_timeseries_dates != 'NA') %>%
  mutate(Revisit_cleaned = as.numeric(bracket_cleaner(Revisit_cleaned))) %>%
  mutate(n_timeseries_dates = as.numeric(bracket_cleaner(n_timeseries_dates))) %>%
  mutate(Timeseries_length = as.numeric(bracket_cleaner(Timeseries_length))) %>%
  #calculate average revisit for publicaitons where revisit time is not specified
  mutate(Revisit_cleaned = ifelse(is.na(Revisit_cleaned),
                                  (Timeseries_length-1)/(n_timeseries_dates),
                                  Revisit_cleaned))
}

#scatter plots
{
  #with facet zoom***
  {
    ggplot(temp_res_df,
           aes(x = Revisit_cleaned
               ,y = Timeseries_length
               ,size = n_timeseries_dates)) +
      geom_point(alpha = 0.5, color = simple_plot_col) +
      labs(
        x = 'Average revisit time (days)'
        ,y = 'Total length of timeseries (days)'
        ,size = 'Image days'
      )+
      scale_size(range = c(1.5,8), breaks = c(2,50,100,150,200))+
      facet_zoom(xy = Revisit_cleaned <=100
                 ,horizontal = F
      )+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(), # Remove major gridlines
        panel.grid.minor = element_blank()  # Remove minor gridlines
      )
  }
  
  #with patchwork
  {
    temp_all = ggplot(temp_res_df %>% mutate(zoomed = ifelse(Revisit_cleaned <= 100, T, F)),
                     aes(x = Revisit_cleaned
                         ,y = Timeseries_length
                         ,size = n_timeseries_dates
                         ,color = zoomed)) +
      geom_point(alpha = 0.5) +
      labs(
        x = 'Average revisit time (days)'
        ,y = 'Total length of timeseries (days)'
        ,size = 'Image days'
      )+
      scale_color_manual(values = c('tomato3', 'steelblue3'))+
      scale_size(range = c(1.7,8))+
      theme_classic() +
      theme(
        axis.title.x = element_blank()
      ) +
      guides(color = 'none')
    
    temp_zoomed = ggplot(temp_res_df %>% filter(Revisit_cleaned <= 100),
                     aes(x = Revisit_cleaned
                         ,y = Timeseries_length
                         ,size = n_timeseries_dates)) +
      geom_point(alpha = 0.5, color = 'steelblue3') +
      labs(
        x = 'Average revisit time (days)'
        ,y = 'Total length of timeseries (days)'
        ,size = 'Image days'
      )+
      scale_size(range = c(1.7,8))+
      theme_classic() +
      guides(size = 'none')
    
    temp_all/temp_zoomed
  }
}

#---- Output resolution ----

res_out = cSplit(df, 'PS_output_spatial_resolution', direction = 'long')
res_out_summ = splitter_counter(
  as.numeric(bracket_cleaner(res_out$PS_output_spatial_resolution)))
res_out_summ

#---- data product -----

#raw summary
dp_summ = df %>%
  group_by(PS_data.product) %>%
  summarise(count = n())

#extra clean summary
dp_summ_clean = df %>%
  mutate(PS_data.product = bracket_cleaner(PS_data.product)) %>%
  mutate(PS_data.product = str_replace_all(PS_data.product, 'SR.*', 'SR')) %>%
  mutate(PS_data.product = str_replace_all(PS_data.product, 'na', 'NA')) %>%
  group_by(PS_data.product) %>%
  summarise(count = n())
dp_summ_clean

#only for analysis publications
df %>% filter(PS_role != 'Validation') %>%
  mutate(PS_data.product = bracket_cleaner(PS_data.product)) %>%
  mutate(PS_data.product = str_replace_all(PS_data.product, 'SR.*', 'SR')) %>%
  mutate(PS_data.product = str_replace_all(PS_data.product, 'na', 'NA')) %>%
  group_by(PS_data.product) %>%
  summarise(count = n())

#####--------------------------------- How is PS being used in conjunction with other RS systems -------------------------------------
#---- PS role and publication year ----
df = df %>% #clean the PS_role column so that there is only one possible value for "validation"
  mutate(PS_role_1 = ifelse(PS_role == "Principal imaging", 
                            "Principal imaging",
                            "Validation")) 

PS_use <- splitter_counter(df$PS_role_1)
PS_use

ggplot(PS_use, aes(x=b, y=n, fill=b)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = n), position = position_stack(vjust=0.5)) +
  labs(x = "Role of PS imagery", y = "Number of references") +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = 'none')

#planetscope role by year
PS_use_year = df %>%
  mutate(PS_role = ifelse(PS_role == 'Principal imaging', 'Analysis', 'Validation')) %>%
  group_by(PS_role, Publication.Year) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(PS_role, Publication.Year = full_seq(Publication.Year, 1), fill = list(count = 0)) %>%
  rbind(
    # PS_use_year,
    tibble(Publication.Year = year_df$b, PS_role = 'Total', count = year_df$n)) %>%
  mutate(PS_role = factor(PS_role, levels = c('Total', 'Analysis', 'Validation'))) %>%
  distinct() %>%
  filter(Publication.Year < max(Publication.Year)) %>%
  filter(!is.na(PS_role))

##
ggplot(PS_use_year, aes(x = Publication.Year, y = count, color = PS_role, group = PS_role)) +
  geom_point(size = point_size) +
  geom_line(size = line_size) +
  # geom_area()+
  labs(
    x = "Publication year",
    y = "Number of references"
    # fill = "Type"  # Since you're using color, this can be removed
  ) +
  ylim(0, ceiling(max(PS_use_year$count) / 5) * 5) +
  scale_color_manual(values = tricolor_plot_col) +  # Set custom colors
  theme_classic() +
  theme(legend.title = element_blank())

#planetscope role by year of data acquisition
data_year_role_summ = df_sub %>%
  cSplit(splitCols = 'Acquisition_years', sep = ', ', direction = 'long') %>%
  filter(!is.na(as.numeric(Acquisition_years))) %>%
  filter(Publication.Year < max(Publication.Year)) %>% #only look at publications from years with complete data
  group_by(Acquisition_years, PS_role) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(Acquisition_years, PS_role, fill = list(count = 0))
data_year_role_summ

##
ggplot(data_year_role_summ, aes(x = Acquisition_years, y = count, color = PS_role, group = PS_role)) +
  geom_point(size = point_size) +
  geom_line(size = line_size) +  # Uncomment this if you want to add lines
  labs(
    x = "Publication year",
    y = "Number of references"
    # fill = "Type"  # Since you're using color, this can be removed
  ) +
  ylim(0, ceiling(max(data_year_role_summ$count) / 5) * 5) +
  scale_color_manual(values = bicolor_plot_col) +  # Set custom colors
  theme_classic()

# #stratify ps role by application category (NOTE: run the applicaiton module before this code chunk)
# 
# app_role_summ = app_df %>%
#   mutate(PS_role_1 = ifelse(PS_role == "Principal imaging", 
#                             "Principal imaging",
#                             "Validation")) %>%
#   group_by(App_broad, PS_role_1) %>%
#   summarize(count = n())



#---- other RS systems plus complementary versus comparison ----

RS_df = df %>%
  filter(!is.na(RS_systems)) %>%
  filter(RS_systems != '-') 
# %>%
# #replace ALOS-1 with PRISM (to be more precise, like saying MODIS instead of Terra)
# rename(PRISM = ALOS_1) %>%
# mutate(RS_systems = str_replace_all(RS_systems, 'ALOS-1', 'PRISM')) %>%
# #replace ALOS-2 with PALSAR-2
# rename(PALSAR_2 = ALOS_2) %>%
# mutate(RS_systems = str_replace_all(RS_systems, 'ALOS-2', 'PALSAR-2'))
nrow(RS_df) #number of publications that include other types of remote sensing data

# updated_rs_col_names = rs_col_names %>% 
#   str_replace_all('ALOS_1', 'PRISM') %>%
#   str_replace_all('ALOS_2', 'PALSAR_2')

RS_summ = RS_df %>%
  cSplit(splitCols = c('RS_systems'), direction = 'long', sep = ', ') %>%
  cSplit(splitCols = rs_col_names, direction = 'long', sep = ', ') %>%
  pivot_longer(cols = rs_col_names, names_to = 'sys', values_to = 'sys_value') %>%
  mutate(sys = str_replace_all(sys, '\\.', ' ')) %>%
  mutate(sys = str_replace_all(sys,'_', '-')) %>%
  filter(sys == RS_systems) %>%
  filter(!is.na(sys_value)) %>%
  mutate(sys_value = ifelse(sys_value == 1, 'Complementary', 'Comparison')) %>%
  group_by(RS_systems, sys_value) %>%
  summarize(count = n())

RS_summ_tot = RS_summ %>%
  group_by(RS_systems) %>%
  summarise(tot_count = sum(count))

RS_summ = merge(RS_summ, RS_summ_tot)

#
ggplot(RS_summ, aes(x = reorder(RS_systems, -tot_count), y = count, fill = sys_value)) +
  geom_col() +
  labs(
    x = "Remote sensing system",
    y = "Number of references",
    fill = "Type"
  ) +
  theme_classic() +
  scale_fill_manual(values = bicolor_plot_col) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+   # Rotate x-axis labels
  coord_flip()  # Flip the coordinates to make y-axis horizontal


#---- information on remote sensing systems in the review list ----

rs_type_df = tribble(
  ~RS_systems,                 ~platform_type, ~sensor_type,    #~spatial_resolution_m, ~revisit_time_days,
  "PRISM",                    "Satellite",    "Panchromatic",   #2.5,                 46,
  "PALSAR-2",                    "Satellite",    "RADAR",         
  "Aerial RGB",                "Aerial",       "RGB",
  "Aerial hyperspectral",      "Aerial",       "Hyperspectral",
  "Aerial laser scanning",     "Aerial",       "LiDAR",
  "Aerial multispectral",      "Aerial",       "Multispectral",
  "FORMOSAT-2",                 "Satellite",    "Multispectral",
  "GEDI",                      "Satellite",    "LiDAR",
  "Ground-based RGB",          "Ground-based", "RGB",
  "Ground-based multispectral","Ground-based", "Multispectral",
  "KOMPSAT-3",                 "Satellite",    "Multispectral",
  "Landsat-4",                 "Satellite",    "Multispectral",
  "Landsat-5",                 "Satellite",    "Multispectral",
  "Landsat-7",                 "Satellite",    "Multispectral",
  "Landsat-8",                 "Satellite",    "Multispectral",
  "MODIS",                     "Satellite",    "Multispectral",
  "PROBA-V",                   "Satellite",    "Multispectral",
  "Point dendrometer",         "Ground-based", "Other",
  "Rapideye",                  "Satellite",    "Multispectral",
  "SPOT",                      "Satellite",    "Multispectral",
  "SRTM",                      "Satellite",    "RADAR",
  "Sentinel-1",                "Satellite",    "RADAR",
  "Sentinel-2",                "Satellite",    "Multispectral",
  "Terrestrial laser scanning","Ground-based", "LiDAR",
  "UAV multispectral",         "UAV",          "Multispectral",
  "UAV RGB",                   "UAV",          "RGB",
  "WorldView-2",               "Satellite",    "Multispectral",
  "Worldview-1",               "Satellite",    "Multispectral",
  "ASTER",                     "Satellite",    "Multispectral",
  "CBERS-4A",                  "Satellite",    "Multispectral",
  "CORONA",                    "Satellite",    "RGB",
  "ICESat",                    "Satellite",    "LiDAR",
  "Sentinel-5p",               "Satellite",    "Multispectral",
  "SMAP",                      "Satellite",    "RADAR",
  "Harmonized Landsat Sentinel-2", "Satellite","Multispectral"
)

#check if the summary table has every system mentioned in the review list
setdiff(RS_summ$RS_systems, rs_type_df$RS_systems)

#add system information to RS_summ dataframe
RS_summ = merge(RS_summ, rs_type_df)

#---- Analysis performance versus other RS systems REDO ----

{
  #clean RS comparison performance data
  new_app_delimiter = '$ '  
  
  rscompare2_df = df %>%
    filter(!is.na(RS_performance_to_aggregate)) %>% #remove NA values
    mutate(RS_performance_to_aggregate = str_replace_all(RS_performance_to_aggregate, ';; ', new_app_delimiter)) %>% #cSplit doesn't work well with more than two characters for sep, so change ;; to a different value
    cSplit(splitCols = 'RS_performance_to_aggregate', direction = 'long', sep = new_app_delimiter) #split by application (top level delimiter)
  for(i in 1:nrow(rscompare2_df)){ #if the application is specified in RS_performance_to_aggregate then use it, otherwise get application from Application_cleaned column
    perf = rscompare2_df$RS_performance_to_aggregate[i]
    rscompare2_df$RS_compare_application[i] = ifelse(str_detect(perf, ':: '),
                                                     str_split_1(perf, ':: ')[1],
                                                     rscompare2_df$Application_cleaned[i])
  }
  #remove applications from RS_performance_to_compare column
  rscompare2_df = rscompare2_df %>% mutate(RS_performance_to_aggregate = ifelse(str_detect(RS_performance_to_aggregate, ':: '),
                                                                                str_extract(RS_performance_to_aggregate,"(?<=:: ).*"),
                                                                                RS_performance_to_aggregate))
  
  #initialize new columns columns to contain the performance statistic and performance gain for the different remote sensing systems
  rs_compare_systems = rscompare2_df$RS_performance_to_aggregate %>% str_split('; ') %>% unlist() %>% str_extract("^[^:]+") %>% unique()
  rs_compare_systems_cols = paste0(rs_compare_systems,'_gain')
  for(i in 1:length(rs_compare_systems_cols)){rscompare2_df[[rs_compare_systems_cols[i]]]=NA}
  rscompare2_df$compare_stat = NA
  
  # view(rscompare2_df %>% select(RS_compare_application, RS_performance_to_aggregate))
  
  # rs_compare_cols = c()
  checker = c()
}
for(i in 1:nrow(rscompare2_df)){
  
  col = rscompare2_df$RS_performance_to_aggregate #column where performance stats are contained
  
  sys_list = unlist(str_split(col[i], '; ')) #split the string into different RS datasets based on the delimiter '; '
  ps_index = which(str_detect(sys_list, 'PS')) #get the index of which item in sys_list contains the Planetscope information
  ps_performance = as.numeric(str_split_1(str_extract(sys_list[ps_index], "(?<= = ).*"),', ')) #get Planetscope performance
  other_rs_list = sys_list[-ps_index] #get list of remote sensing data without the planetscope data
  
  compare_stat = str_extract(sys_list[ps_index], "(?<=: ).*(?= =)") #get the performance statistic (occurs between ": " and " =")
  rscompare2_df$compare_stat[i] = compare_stat
  
  for(j in 1:length(other_rs_list)){
    
    rs_name = str_split_1(other_rs_list[j],": ")[1] #get the name of the other remote sensing dataset
    #get the analysis performance associated with the other remote sensing dataset
    rs_performance = as.numeric(str_split_1(str_extract(other_rs_list[j], ifelse(str_detect(other_rs_list[j],'='), "(?<= = ).*", '(?<=: ).*'))
                                            ,
                                            ', '))
    performance_gain = mean(ps_performance - rs_performance)
    
    
    performance_gain_col = paste0(rs_name,'_gain')
    
    rscompare2_df[[performance_gain_col]][i] = performance_gain
    
    # if(!(performance_gain_col %in% rs_compare_cols)){
    #   rs_compare_cols[length(rs_compare_cols)+1] = performance_gain_col
    # }
  }
  checker[i] = i #data cleaning/debugging tool which returns the row before teh one which fails, comment out if not using
}


#make dataframe for remote sensing datasets to recode labels
{
  rs_replacement_map = tribble(
    ~original_label, ~replacement, ~spatial_resolution, ~sensor_type,
    'S2', 'Sentinel-2', 10, 'Optical',
    'L8', 'Landsat-8', 30, 'Optical',
    'WV2', 'Worldview-2', 1.8, 'Optical',
    'W2', 'Worldview-2', 1.8, 'Optical',
    'RE', 'RapidEye', 5, 'Optical',
    'S1', 'Sentinel-1', 10, 'RADAR',
    'HLS', 'Harmonized Landsat Sentinel-2', 30, 'Optical',
    'CBERS-4', 'CBERS-4', 8, 'Optical',
    'PALSAR2', 'PALSAR-2', 25, 'RADAR',
    'Aerial HS', 'Aerial hyperspectral', 0.1, 'Optical',
    'Aerial', 'Aerial multispectral', 0.1, 'Optical',
    'K3', 'Kompsat-3', 2.8, 'Optical',
    'MODIS', 'MODIS', 250, 'Optical',
    'FORMOSAT-2', 'FORMOSAT-2', 2, 'Optical',
    'L7', 'Landsat-7', 30, 'Optical'
  )
}

#get number of publications per application
compare_pubs_apps_summ = rscompare2_df %>%
  group_by(RS_compare_application) %>%
  summarise(n_application_pubs = n())

#get dataframe in long format for plotting, add additional information about RS datasets and publications
rscompare2_df_long <- rscompare2_df %>%
  pivot_longer(cols = all_of(rs_compare_systems_cols),
               names_to = 'RS_dataset_compared',
               values_to = 'ps_performance_gain') %>%
  filter(!is.na(ps_performance_gain)) %>%
  mutate(RS_dataset_compared = sub("_.*", "", RS_dataset_compared)) %>%
  merge(rs_replacement_map, by.x = 'RS_dataset_compared', by.y = 'original_label') %>%
  rename(RS_dataset_name = replacement) %>%
  # merge(compare_pubs_apps_summ) %>%
  mutate(RS_compare_application = case_match( #relabel or aggregate applications
    RS_compare_application,
    c('Canopy cover', 'Canopy height', 'Canopy openness', 'Max gap', 'Mean gap') ~ 'Canopy structural metrics',
    c('Stem diameter', 'Stem density', 'Basal area') ~ 'Stem structural metrics',
    .default = RS_compare_application
  )) %>%
  mutate(RS_dataset_name = case_match( #relabel rs dataset names for plotting
    RS_dataset_name,
    'Aerial multispectral' ~ 'Aerial MS',
    'Aerial hyperspectral' ~ 'Aerial HS',
    'Harmonized Landsat Sentinel-2' ~ 'HLS',
    .default = RS_dataset_name
  ))
#give publicaton titles identifying numbers for plotting
# title_id_df = tibble(Citation = rscompare2_df_long$Citation[rscompare2_df_long$RS_dataset_name != 'Fused'],
#                      Title = unique(rscompare2_df_long$Title[rscompare2_df_long$RS_dataset_name != 'Fused']),
#                      title_id_number = 1:length(unique(rscompare2_df_long$Title[rscompare2_df_long$RS_dataset_name != 'Fused'])))
title_id_df = rscompare2_df_long %>% 
  filter(RS_dataset_name != 'Fused') %>% 
  distinct(Citation, Title) %>%
  arrange(toupper(Citation)) %>%
  mutate(title_id_number = row_number())
rscompare2_df_long = merge(rscompare2_df_long, title_id_df)

#### stats and information about comparison publications

# get number of publications per application
compare_pubs_apps_summ = rscompare2_df %>%
  group_by(RS_compare_application) %>%
  summarise(n_application_pubs = n())

# number of publications which use different stats
compare_stats_summ = rscompare2_df %>% 
  group_by(compare_stat) %>%
  summarise(count = n())

#number of comparisons per dataset
rs_compare_pubs_summ = rscompare2_df_long %>%
  group_by(RS_dataset_name) %>%
  summarise(count = n())


#### plot all performance comparisons colored by application
#color palettes
{
  
  colors18_1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", 
                 "#A65628", "#984EA3", "#999999", "#FF99FF", "#CCFF33", 
                 "#66C2A5", "#FC8D62", "#8DA0CB", "#FFD92F", "#E78AC3", 
                 "#A6D854", "#FFD92F", "#F9C9D9", "#F7B7A3")
  
  colors18_2 = c(
    "#D73027", "blue", 'darkgreen', "#1A9850", "#F46D43", "#762A83"
    ,"#FFD92F", 'grey', "#66C2A5", "#E78AC3", "#A6761D", 'magenta'
    , "#FB9A99", "#1F78B4", "#B2DF8A", "grey30", "black", 'darkred')
  
  colors14_2 = c(
    "#D73027", 
    "blue", 
    'darkgreen', 
    # "#1A9850", 
    "#F46D43", 
    "#762A83",
    # "#FFD92F",
    'yellow3',
    # 'grey', 
    "#66C2A5", 
    "#E78AC3", 
    "#A6761D", 
    'magenta',
    "#FB9A99", 
    "#1F78B4", 
    # "#B2DF8A", 
    "grey30", 
    # "black", 
    'darkred'
  )
  
  }

#scatter, categories on y axis (Old)
{
  #   
  # ggplot(rscompare2_df_long 
  #        , aes(
  #          x = fct_reorder(RS_dataset_name, spatial_resolution, .desc = TRUE),
  #          color = RS_compare_application,
  #          y = ps_performance_gain
  #          # , shape = stat
  #          # , shape = has_textures
  #          # , size = spatial_resolution
  #        )) +
  #   geom_jitter(
  #     size = 4
  #     ,alpha = 0.7
  #     ,width = 0.1
  #   ) +
  #   geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
  #   scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.05)
  #                      # , limits = c(-0.53, 0.53)
  #                      , limits = c(-0.53, 0.4)
  #   )+
  #   scale_x_discrete(labels = label_wrap(12)) +
  #   labs(
  #     y = 'Performance gain',
  #     color = 'Application (# references)',
  #     shape = 'Statistic'
  #   )+
  #   scale_color_manual(values = colors14_2)+
  #   theme_classic()+
  #   theme(
  #     axis.title.y = element_blank()
  #   )+
  #   guides(size = 'none') +
  #   scale_y_break(c(-0.495,-0.15))+
  #   scale_y_break(c(0.10,0.35))+
  #   coord_flip()
  
}

#scatter, categories on x axis
{
  plot_compare_scat_xcats = ggplot(rscompare2_df_long 
                                   , aes(
                                     x = fct_reorder(RS_dataset_name, spatial_resolution, .desc = F),
                                     color = RS_compare_application,
                                     label = title_id_number,
                                     y = ps_performance_gain
                                   )) +
    #plot lines between categories
    geom_vline(xintercept = 1.5 + 1:length(unique(rscompare2_df_long$RS_dataset_name))-1, color = 'lightgrey', linetype = "solid", size = 0.5) +
    #plot horizontal line at performance gain = 0
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
    #plot points
    geom_jitter(
      size = 1.5
      ,alpha = 0.5
      ,width = 0.15
    ) +
    #set y axis parameters
    scale_y_continuous(breaks = seq(-1, 1, by = 0.1)
                       # , limits = c(-0.53, 0.53)
                       # , limits = c(-0.53, 0.4)
    )+
    # #plot text labels for each point identifying the publication that the point came from
    # geom_text_repel(
    #   size = 2,
    #   max.overlaps = Inf,
    #   seed = 123,
    #   min.segment.length = 0.4,
    #   segment.size = 0.3,
    #   force = 2.5,
    #   force_pull = 0.5,
    #   show.legend = F
    # )+
    scale_x_discrete(labels = label_wrap(12)) +
    labs(
      y = 'Performance gain',
      color = 'Application (# publications, # comparisons)',
      shape = 'Statistic'
    )+
    scale_color_manual(values = colors14_2)+
    theme_classic()+
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 30,hjust = 1))+
    ##break y axis for readability (NOTE: commented out since ggbreak does not work with ggrepel)
    # scale_y_break(c(-0.495,-0.2))+
    # scale_y_break(c(0.10,0.35))+
    guides(size = 'none') 
  
  plot_compare_scat_xcats
}

#boxplot, categories on x axis
{
  plot_compare_box_xcats = ggplot(rscompare2_df_long 
                                  , aes(
                                    x = fct_reorder(RS_dataset_name, spatial_resolution, .desc = F),
                                    y = ps_performance_gain
                                  )) +
    #plot lines between categories
    geom_vline(xintercept = 1.5 + 1:length(unique(rscompare2_df_long$RS_dataset_name))-1, color = 'lightgrey', linetype = "solid", size = 0.5) +
    #plot horizontal line at performance gain = 0
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
    geom_boxplot()+
    scale_y_continuous(breaks = seq(-1, 1, by = 0.1)
                       # , limits = c(-0.53, 0.53)
                       # , limits = c(-0.53, 0.4)
    )+
    scale_x_discrete(labels = label_wrap(12)) +
    labs(
      y = 'Performance gain',
      color = 'Application',
      shape = 'Statistic'
    )+
    scale_color_manual(values = colors14_2)+
    theme_classic()+
    theme(
      axis.title.x = element_blank()
      # ,axis.text.x = element_text(angle = 30,hjust = 1)
      ,axis.text.x = element_blank()
      # ,axis.line.x = element_blank()
      ,axis.ticks.x = element_blank()
    )+
    guides(size = 'none')
  
  
  plot_compare_box_xcats
}

#facet_wrap with both box and scatter arranged together
{
  
  plot_compare_box_xcats / plot_compare_scat_xcats
}

#put scatterplot overlaying boxplot ****
{
  
  
  ggplot(data = rscompare2_df_long,
         aes(
           x = fct_reorder(RS_dataset_name, spatial_resolution, .desc = F),
           # color = RS_compare_application,
           label = title_id_number,
           y = ps_performance_gain
         )) +
    #add boxplot
    geom_boxplot( 
      # data = rscompare2_df_long %>% 
      #               filter(RS_dataset_name %in% rs_compare_pubs_summ$RS_dataset_name[rs_compare_pubs_summ$count > 1])
      # ,
      color = 'grey50'
      ,outliers = F
    )+
    #plot lines between categories
    geom_vline(xintercept = 1.5 + 1:length(unique(rscompare2_df_long$RS_dataset_name))-1, color = 'lightgrey', linetype = "solid", size = 0.5) +
    #plot vline to separate lower spatial resolution datasets from higher spatial resolution
    geom_vline(xintercept = 1.5+4)+
    #plot horizontal line at performance gain = 0
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
    ##plot points
    # geom_jitter(
    #   size = 1.5
    #   ,alpha = 0.5
    #   ,width = 0.15
    #   ,aes(color = RS_compare_application)
    # ) +
    geom_point(
      aes(color = RS_compare_application),  # Aesthetics inside aes()
      size = 1.5,                           # Point size
      alpha = 0.7,                          # Transparency
      position = position_dodge(width = 0.7) # Correct placement of position_dodge()
    )+
    scale_y_continuous(breaks = seq(-1, 1, by = 0.1)
                       # , limits = c(-0.53, 0.53)
                       # , limits = c(-0.53, 0.4)
    )+
    scale_x_discrete(labels = label_wrap(12)) +
    labs(
      y = 'Performance gain',
      color = 'Application',
      shape = 'Statistic'
    )+
    scale_color_manual(values = application_colors)+
    theme_classic()+
    theme(
      axis.title.x = element_blank()
      ,axis.text.x = element_text(angle = 30,hjust = 1)
      # ,axis.text.x = element_blank()
      # ,axis.line.x = element_blank()
      # ,axis.ticks.x = element_blank()
    )+
    guides(size = 'none')
  
}

### factors affecting planetscope performance gain
{
  #other remote sensing dataset
  {
    #Wilcoxon signed rank tests
    
    a = wilcox.test(rscompare2_df_long$ps_performance_gain, mu = 0)
    wilcox_p = sapply(X = unique(rscompare2_df_long$RS_dataset_name), function(rs){
      # dat = rscompare2_df_long %>% filter(RS_dataset_name == rs)
      dat = rscompare2_df_long$ps_performance_gain[rscompare2_df_long$RS_dataset_name == rs]
      wilc = wilcox.test(dat, mu = 0)
      return(wilc[['p.value']])
    })
    rs_compare_sig = tibble(rs = unique(rscompare2_df_long$RS_dataset_name), wilcox_p = wilcox_p)
    
    wilcox_spat = wilcox.test(x = rscompare2_df_long$ps_performance_gain[rscompare2_df_long$spatial_resolution > 3],
                              y = rscompare2_df_long$ps_performance_gain[rscompare2_df_long$spatial_resolution < 3])
    summary(wilcox_spat)
    
    
    #number pubs by sensor type
    a = rs_type_df %>% mutate(RS_systems = toupper(RS_systems))
    rs_compare_pubs_summ %>% 
      mutate(RS_dataset_name = toupper(RS_dataset_name)) %>%
      mutate(RS_dataset_name = case_match(RS_dataset_name,
                                          ' MS' ~ ' multispectral',
                                          ' HS' ~ ' hyperspectral',
                                          'HLS' ~ 'Harmonized Landsat Sentinel-2',
                                          .default = RS_dataset_name)) %>%
      left_join(a, by = join_by(RS_dataset_name == RS_systems))
    
    #pearson correlation between performance gain and spatial resolution of dataset
    
    correlation = cor(rscompare2_df_long %>% select(ps_performance_gain, spatial_resolution))
    
    degree <- 1:5
    lm_l <- list()
    r2_l <- list()
    aic_l <- list()
    
    lapply(degree, function(i) {
      model <- lm(data = rscompare2_df_long, formula = ps_performance_gain ~ poly(spatial_resolution, i))
      lm_l[[i]] <<- model  # Use <<- to assign in the outer environment
      
      summ <- summary(model)
      r2_l[[i]] <<- summ$r.squared
      
      aic_l[[i]] <<- AIC(model)
    })
    
    lm_tbl <- tibble(degree, lm = lm_l, r2 = unlist(r2_l), aic = unlist(aic_l))
    lm_tbl
  }
  
  #year of data acquisition
  {
    #correlation performance gain versus data acquisition year
    
    cor_year_df = rscompare2_df_long %>%
      filter(!is.na(Acquisition_years)) %>%
      filter(Acquisition_years != 'NA')
    
    mean_acq_year = sapply(X = cor_year_df$Acquisition_years, function(a){
      b = mean(as.numeric(str_split_1(a, ', ')))
      b
    })
    
    mean_year_cor = cor(x = cor_year_df$ps_performance_gain, 
                        y = mean_acq_year)
    mean_year_comp_lm = lm(cor_year_df$ps_performance_gain ~ mean_acq_year)
    summary(mean_year_comp_lm)
    
    min_acq_year = sapply(X = cor_year_df$Acquisition_years, function(a){
      b = min(as.numeric(str_split_1(a, ', ')))
      b
    })
    min_year_cor = cor(x = cor_year_df$ps_performance_gain, 
                       y = min_acq_year)
    min_year_comp_lm = lm(cor_year_df$ps_performance_gain ~ min_acq_year)
    summary(min_year_comp_lm)
    
    max_acq_year = sapply(X = cor_year_df$Acquisition_years, function(a){
      b = max(as.numeric(str_split_1(a, ', ')))
      b
    })
    max_year_cor = cor(x = cor_year_df$ps_performance_gain, 
                       y = max_acq_year)
    max_year_comp_lm = lm(cor_year_df$ps_performance_gain ~ max_acq_year)
    summary(max_year_comp_lm)
    
    
    plotting_tbl = tibble(gain = cor_year_df$ps_performance_gain, mean_acq_year, min_acq_year, max_acq_year) %>%
      pivot_longer(cols = c('mean_acq_year', 'min_acq_year', 'max_acq_year')
                   , names_to = 'acq_year'
                   , values_to = 'year')
    ggplot(plotting_tbl, aes(x = year, y = gain, color = acq_year)) +
      geom_point(alpha = 0.5) +  
      # geom_smooth(method = 'lm',
      #             formula = plotting_tbl$gain[plotting_tbl$acq_year == 'mean_acq_year'] ~ plotting_tbl$year[plotting_tbl$acq_year == 'mean_acq_year'])
      geom_smooth(
        data = subset(plotting_tbl, acq_year == "min_acq_year"),
        method = "lm"
        , se = F
        , aes(color = "min_acq_year")
      ) +
      geom_smooth(
        data = subset(plotting_tbl, acq_year == "mean_acq_year"),
        method = "lm"
        , se = F
        , aes(color = "mean_acq_year")
      ) +
      geom_smooth(
        data = subset(plotting_tbl, acq_year == "max_acq_year"),
        method = "lm"
        , se = F
        , aes(color = "max_acq_year")
      )
    
    #performance gain versus publication year
    pub_year_psgain_lm = lm(cor_year_df$ps_performance_gain ~ cor_year_df$Publication.Year)
    summary(pub_year_psgain_lm)
    
    #sentinel_2 versus data acquisition year
    cor_year_df$mean_acq_year = mean_acq_year
    s2_mean_acq_year_lm = lm(ps_performance_gain ~ mean_acq_year, data = cor_year_df %>% filter(RS_dataset_name == 'Sentinel-2'))
    a = summary(s2_mean_acq_year_lm)
    
    cor_year_df$min_acq_year = min_acq_year
    s2_min_acq_year_lm = lm(ps_performance_gain ~ min_acq_year, data = cor_year_df %>% filter(RS_dataset_name == 'Sentinel-2'))
    b = summary(s2_min_acq_year_lm)
    
    cor_year_df$max_acq_year = max_acq_year
    s2_max_acq_year_lm = lm(ps_performance_gain ~ max_acq_year, data = cor_year_df %>% filter(RS_dataset_name == 'Sentinel-2'))
    c = summary(s2_max_acq_year_lm)
    
    acq_year_var = c('mean_acq_year', 'min_acq_year', 'max_acq_year')
    
    s2_acq_year_tbl = tribble(
      ~acq_year_type, ~p_value, ~r2,
      as.character(a$terms[[3]]), a$coefficients[2,4], a$r.squared,
      as.character(b$terms[[3]]), b$coefficients[2,4], b$r.squared,
      as.character(c$terms[[3]]), c$coefficients[2,4], c$r.squared
    )
    
    #Landsat-8 versus acquisition year (no significant findings)
    L8_mean_acq_year_lm = lm(ps_performance_gain ~ mean_acq_year, data = cor_year_df %>% filter(RS_dataset_name == 'Landsat-8'))
    summary(L8_mean_acq_year_lm)
    
    L8_min_acq_year_lm = lm(ps_performance_gain ~ min_acq_year, data = cor_year_df %>% filter(RS_dataset_name == 'Landsat-8'))
    summary(L8_min_acq_year_lm)
    
    L8_max_acq_year_lm = lm(ps_performance_gain ~ max_acq_year, data = cor_year_df %>% filter(RS_dataset_name == 'Landsat-8'))
    summary(L8_max_acq_year_lm)
    
    #graph Sentinel-2 performance gain versus data acquisition year ***
    
    
    intercept = s2_max_acq_year_lm$coefficients[1]
    coef = s2_max_acq_year_lm$coefficients[2]
    
    ggplot(cor_year_df %>% filter(RS_dataset_name == 'Sentinel-2'), aes(x = max_acq_year, y = ps_performance_gain))+
      geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
      geom_smooth(method = 'lm'
                  ,se = F)+
      geom_point(aes(color = RS_compare_application)
                 , alpha = 0.5
                 ,size = 2.5
      )+
      scale_y_continuous(breaks = seq(-1, 1, by = 0.1)
                         # , limits = c(-0.53, 0.53)
                         # , limits = c(-0.53, 0.4)
      )+
      # scale_x_discrete(labels = label_wrap(12)) +
      labs(
        y = 'Performance gain',
        color = 'Application',
        shape = 'Statistic',
        x = 'Planetscope data acquisition year (max)'
      )+
      # stat_regline_equation()+
      # geom_text(x = 2019, y = -0.3,
      #           label = paste0('y = ',round(intercept, digits = 3),' + ',round(coef, digits = 3),'x'
      #                          ,'\nr2 = ',round(s2_acq_year_tbl$r2[3],digits=3),', p = ',round(s2_acq_year_tbl$p_value[3],digits=3))
      #           ,fontface = "plain"  # Use plain font
      #           ,family = "sans"  # Set font family
      #           )+
      annotate(
        "text",
        x = 2018, 
        y = -0.3,
        label = paste0('y = ', round(intercept, digits = 3), ' + ', round(coef, digits = 3), 'x',
                       '\nr² = ', round(s2_acq_year_tbl$r2[3], digits = 3), ', p = ', round(s2_acq_year_tbl$p_value[3], digits = 3)),
        hjust = 0,  # Horizontal justification
        vjust = 0,  # Vertical justification
        size = 4,   # Adjust text size as needed
        fontface = "plain",  # Set font to plain
        family = "sans"  # Set font family
      ) +
      scale_color_manual(values = application_colors)+
      theme_classic()
    
  }  
  
  #application
  {
    n_application_comparisons = rscompare2_df_long %>% 
      group_by(RS_compare_application) %>% 
      summarise(n_application_comparisons = n())
    
    
    n_pubs_comparisons = rscompare2_df %>% 
      group_by(RS_compare_application) %>% 
      summarise(n_publication_comparisons = n())
    
    rscompare2_df_long = rscompare2_df_long %>% 
      merge(n_application_comparisons) %>%
      merge(n_pubs_comparisons) %>%
      mutate(RS_compare_application_label = paste0(RS_compare_application, '(',n_application_comparisons,n_publication_comparisons,')'))
    
    # application
    {
      
      
      ggplot(data = rscompare2_df_long %>% filter(n_application_comparisons > 4),
             aes(
               x = fct_reorder(RS_dataset_name, spatial_resolution, .desc = F),
               color = RS_compare_application_label,
               label = title_id_number,
               y = ps_performance_gain
             )) +
        #add boxplot
        # geom_boxplot( 
        #   # data = rscompare2_df_long %>% 
        #   #               filter(RS_dataset_name %in% rs_compare_pubs_summ$RS_dataset_name[rs_compare_pubs_summ$count > 1])
        #   # ,
        #   color = 'grey50'
        #   ,outliers = F
        # )+
        #plot lines between categories
        geom_vline(xintercept = 1.5 + 1:length(unique(rscompare2_df_long$RS_dataset_name))-1, color = 'lightgrey', linetype = "solid", size = 0.5) +
        #plot vline to separate lower spatial resolution datasets from higher spatial resolution
        geom_vline(xintercept = 1.5+4)+
        #plot horizontal line at performance gain = 0
        geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
        #plot points
        geom_jitter(
          size = 5
          ,alpha = 0.5
          ,width = 0.5
          ,aes(color = RS_compare_application_label)
        ) +
        geom_text_repel(
          size = 3,
          max.overlaps = Inf,
          seed = 123,
          min.segment.length = 0.4,
          segment.size = 0.3,
          force = 3.5,
          force_pull = 0.5,
          show.legend = F
        )+
        scale_y_continuous(breaks = seq(-1, 1, by = 0.1)
                           # , limits = c(-0.53, 0.53)
                           # , limits = c(-0.53, 0.4)
        )+
        scale_x_discrete(labels = label_wrap(12)) +
        labs(
          y = 'Performance gain',
          color = 'Application (# comparisons, # publications)',
          shape = 'Statistic'
        )+
        scale_color_manual(values = colors14_2)+
        theme_classic()+
        theme(
          axis.title.x = element_blank()
          ,axis.text.x = element_text(angle = 30,hjust = 1)
          # ,axis.text.x = element_blank()
          # ,axis.line.x = element_blank()
          # ,axis.ticks.x = element_blank()
        )+
        guides(size = 'none')
      
      }
    
    # time series
    {
      rscompare2_df_long$PS_timeseries = toupper(rscompare2_df_long$PS_timeseries)
      #ggplot
      {
        
        
        ggplot(data = rscompare2_df_long 
               # %>% filter(n_application_comparisons > 3)
               ,aes(
                 x = fct_reorder(RS_dataset_name, spatial_resolution, .desc = F),
                 shape = toupper(PS_timeseries),
                 color = RS_compare_application_label,
                 label = title_id_number,
                 y = ps_performance_gain
               )) +
          #add boxplot
          # geom_boxplot( 
          #   # data = rscompare2_df_long %>% 
          #   #               filter(RS_dataset_name %in% rs_compare_pubs_summ$RS_dataset_name[rs_compare_pubs_summ$count > 1])
          #   # ,
          #   color = 'grey50'
          #   ,outliers = F
          # )+
          #plot lines between categories
          geom_vline(xintercept = 1.5 + 1:length(unique(rscompare2_df_long$RS_dataset_name))-1, color = 'lightgrey', linetype = "solid", size = 0.5) +
          #plot vline to separate lower spatial resolution datasets from higher spatial resolution
          geom_vline(xintercept = 1.5+4)+
          #plot horizontal line at performance gain = 0
          geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')+
          #plot points
          geom_jitter(
            size = 5
            ,alpha = 0.5
            ,width = 0.3
            # ,aes(color = RS_compare_application_label)
          ) +
          geom_text_repel(
            size = 3,
            max.overlaps = Inf,
            seed = 123,
            min.segment.length = 0.4,
            segment.size = 0.3,
            force = 3.5,
            force_pull = 0.5,
            show.legend = F
          )+
          scale_y_continuous(breaks = seq(-1, 1, by = 0.1)
                             # , limits = c(-0.53, 0.53)
                             # , limits = c(-0.53, 0.4)
          )+
          scale_x_discrete(labels = label_wrap(12)) +
          labs(
            y = 'Performance gain',
            color = 'Application',
            shape = 'Timeseries used'
          )+
          scale_color_manual(values = colors14_2)+
          theme_classic()+
          theme(
            axis.title.x = element_blank()
            ,axis.text.x = element_text(angle = 30,hjust = 1)
            # ,axis.text.x = element_blank()
            # ,axis.line.x = element_blank()
            # ,axis.ticks.x = element_blank()
          )+
          guides(size = 'none')
      }
      
      #significance testing
      {
        #equal variances?
        leveneTest(ps_performance_gain ~ PS_timeseries, data = rscompare2_df_long)
        shapiro.test(rscompare2_df_long$ps_performance_gain)
        
        timeseries_wilcox = wilcox.test(x = rscompare2_df_long$ps_performance_gain[rscompare2_df_long$PS_timeseries=='N'],
                                        y = rscompare2_df_long$ps_performance_gain[rscompare2_df_long$PS_timeseries=='Y'])
      }
    }
  }
}
#---- RS comparison v3 ----

#cleaning and main results
{
  relative_rs_df_long = df %>%
    #remove NAs and invalid entries
    filter(!is.na(Relative_RS_performance_2)) %>% #remove NA values
    filter(Relative_RS_performance_2 != '-') %>% #remove invalid entries
    #separate different applications
    mutate(Relative_RS_performance_2 = str_replace_all(Relative_RS_performance_2, ';;', '@')) %>% #replace since cSplit doesn't handle repeat symbols well
    cSplit(splitCols = 'Relative_RS_performance_2', sep = '@ ', direction = 'long') %>% #separate different applications into rows
    mutate(Application_to_agg = ifelse(!str_detect(Application_cleaned, ':: '), #get right application for the row
                                       Application_cleaned,
                                       str_extract(Relative_RS_performance_2, "^[^:]+"))) %>%
    mutate(Relative_RS_performance_2 = str_replace(Relative_RS_performance_2, ".*:: ", "")) %>%
    # cSplit(splitCols = 'Application_cleaned', sep = ', ', direction = 'long') %>%
    # filter(Application_to_agg != Application_cleaned) %>%
    ##separate comparisons onto different rows
    cSplit(splitCols = 'Relative_RS_performance_2', sep = ', ', direction = 'long') %>%
    mutate(PS_comparison = ifelse(str_detect(Relative_RS_performance_2, 'PS >'), 'Better', #get whether PS performed better, same, or worse than other data
                                  ifelse(str_detect(Relative_RS_performance_2, '='), 'Same',
                                         'Worse'))) %>%
    mutate(RS_compared = str_replace_all(Relative_RS_performance_2,'PS| |<|>|=', '')) %>% #get RS data compared to PS
    #relabel
    mutate(RS_compared = str_replace(RS_compared, 'Aerial', 'Aer ')) %>%
    mutate(RS_compared = str_replace(RS_compared, 'CBERS-4A', 'CB4A')) %>%
    mutate(RS_compared = str_replace(RS_compared, 'FORMOSAT-2', 'FS2')) %>%
    mutate(RS_compared = str_replace(RS_compared, 'PALSAR-2', 'PS2')) %>%
    mutate(RS_compared = str_replace(RS_compared, 'WV', 'WV2'))
  
  
  
  
  #summarise  
  relative_rs_summ = relative_rs_df_long %>%
    group_by(RS_compared, PS_comparison) %>%
    summarise(count = n())
  
  ggplot(relative_rs_summ, 
         aes(y = count
             , x = PS_comparison
             # , fill = PS_comparison
         ),
  ) +
    geom_col(fill = simple_plot_col) +  # Use geom_col for pre-summarized data
    facet_grid(vars(RS_compared)) +
    labs(x = "Planetscope analysis comparison"
         , y = "Number of references"
         # , title = "Comparison of Remote Sensing Datasets"
    ) +
    # scale_fill_manual(values = c('gold', 'lightgreen', 'skyblue'))+
    theme_minimal() +
    theme(
      strip.text = element_text(angle = 90)  # Keep the facet labels horizontal
      , legend.position = 'none'
      , panel.background = element_rect(fill = NA, color = "black") # add lines around facet panels
    )
}

#reasons behind comparison results
{
  # spectral resolution
  {
    relative_rs_df_long = relative_rs_df_long %>%
      mutate(same_feats = ifelse(Non_PS_bands_in_other_RS_models == 'No',
                                 'Yes',
                                 'No'))
    
    other_RS_feats_summ = relative_rs_df_long %>%
      group_by(RS_compared, PS_comparison, same_feats) %>%
      summarise(count = n())
    
    ggplot(other_RS_feats_summ, aes(y = count, x = PS_comparison, fill = same_feats)) +
      geom_col() +  # Use geom_col for pre-summarized data
      facet_grid(vars(RS_compared)) +
      labs(x = "Planetscope analysis comparison"
           , y = "Number of references"
           # , title = "Comparison of Remote Sensing Datasets"
      ) +
      # scale_fill_manual(values = c('gold', 'lightgreen', 'skyblue'))+
      theme_minimal() +
      theme(
        strip.text = element_text(angle = 90)  # Keep the facet labels horizontal
        , panel.background = element_rect(fill = NA, color = "black") # add lines around facet panels
        # , legend.position = 'none'
      )
  }
  
  #application
  {
    relative_rs_app_summ = relative_rs_df_long %>%
      group_by(RS_compared, PS_comparison, Application_to_agg) %>%
      summarise(count = n())
    
    ggplot(relative_rs_app_summ, aes(y = count, x = PS_comparison, fill = Application_to_agg)) +
      geom_col() +  # Use geom_col for pre-summarized data
      facet_grid(vars(RS_compared)) +
      labs(x = "Planetscope analysis comparison"
           , y = "Number of references"
           # , title = "Comparison of Remote Sensing Datasets"
      ) +
      scale_fill_manual(values = application_colors)+
      theme_minimal() +
      theme(
        strip.text = element_text(angle = 90)  # Keep the facet labels horizontal
        , panel.background = element_rect(fill = NA, color = "black") # add lines around facet panels
        # , legend.position = 'none'
      )
  }
}

#####--------------------------------- How is PS being analyzed -------------------------------------
#---- Radiometric correction ----

rad_summ = df_sub %>%
  filter(Additional_radiometric_normalization != "-") %>%
  filter(!is.na(Additional_radiometric_normalization))

# view(rad_summ %>% select(Citation, Title, Application, Additional_radiometric_normalization))

#---- Model features----

# df_indic = filter(df, PS_role == 'Principal imaging') #only look at indices/bands in Principal Imaging studies
# df_indic = splitter_counter(df_indic$Indices_or_bands_2)

#prep overall features
{df_indic = df %>%
  mutate(Indices_or_bands_2 = str_replace_all(Indices_or_bands_2, "raw bands \\(Dove\\)", "R, G, B, NIR")) %>% #replace raw bands with the actual bands
  mutate(optimal_feature_subset = ifelse(optimal_feature_subset == '-', Indices_or_bands_2, optimal_feature_subset)) %>% #if '-' is given for optimal_feature_subset, replace it with whatever is in Indices_or_bands_2
  mutate(optimal_feature_subset = bracket_cleaner(optimal_feature_subset)) %>%
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, ' d', ' ')) %>% #remove 'd' from the front of features (indicating change over time)
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'PCA', 'PC')) %>% #replace 'PCAx' with 'PCx' (meaning 'principle component x')
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'FOTO textures', 'FOTO')) %>%
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'Hue', 'Hue index')) %>%
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'Cigreen|Clgreen', 'CIgreen'))%>%
  #these next two lines get rid of features from other RS systems by separaing batches of features into rows based on a ; delimiter
  cSplit(splitCols = 'optimal_feature_subset', sep = '; ', direction = 'long') %>%
  filter(!str_detect(optimal_feature_subset, 'S2|RE')) %>%
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'PS:', '')) %>%
  #these next lines filter out additional features that do not come from Planetscope by separating all comma-delimited features into their own rows
  cSplit(splitCols = 'optimal_feature_subset', sep = ', ', direction = 'long') %>%
  filter(!str_detect(optimal_feature_subset, '_s2|_fused|_aerial|_l8|_re')) %>% #remove features that come from sentinel-2, fused bands, Landsat-8, or aerial imagery
  filter(!str_detect(optimal_feature_subset, 'VV|VH')) %>% #remove SAR data
  filter(!str_detect(optimal_feature_subset, 'PC\\d')) %>% #remove Principal components
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, '.*: ', '')) %>% #remove additional text with commas
  mutate(optimal_feature_subset = bracket_cleaner(optimal_feature_subset)) %>% #remove bracket text, since this didn't work before for some reason
  mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, '_.*','')) %>% #remove underscore text
  distinct(Title, optimal_feature_subset, .keep_all = T) %>% #
  filter(!optimal_feature_subset %in% c('DEM', 'Elevation', 'Road proximity', 'Slope', 'Temperature', 'Lidar intensity', 'Canopy height', 'Precipitation')) %>%
  # mutate(optimal_feature_subset = ifelse('\\bS\\b', 'Saturation',
  #                                        ifelse('\\bI\\b', 'Brightness',
  #                                               ifelse('\\bR\\b', 'Red band',
  #                                                      ifelse('\\bG\\b', 'Green band',
  #                                                             ifelse('\\bB\\b', 'Blue band',
  #                                                                    ifelse('NIR', 'NIR band',
  #                                                                           optimal_feature_subset))))))) %>%
  mutate(optimal_feature_subset = case_when(
    str_detect(optimal_feature_subset, '\\bS\\b') ~ 'Saturation',
    str_detect(optimal_feature_subset, '\\bI\\b') ~ 'Intensity',
    str_detect(optimal_feature_subset, '\\bR\\b') ~ 'Red band',
    str_detect(optimal_feature_subset, '\\bG\\b') ~ 'Green band',
    str_detect(optimal_feature_subset, '\\bB\\b') ~ 'Blue band',
    str_detect(optimal_feature_subset, '\\bNIR\\b') ~ 'NIR band',
    str_detect(optimal_feature_subset, 'NDWI') ~ 'NDWI2',
    str_detect(optimal_feature_subset, 'SR-NIRR') ~ 'SR',
    TRUE ~ optimal_feature_subset
  )) %>%
  filter(!optimal_feature_subset %in% c('GLCM', 'FOTO')) %>% #exclude textures
  filter(str_detect(PS_role, "Principal imaging")) #exclude features from papers where PS was only used for validation

feats_summ = splitter_counter(df_indic$optimal_feature_subset)  
# view(feats_summ)
}

feats_summ = feats_summ %>% #make "Other indices" those which appear in only 1 study
  # mutate(relabel = ifelse(n > 1, b, 'Other indices or bands')) %>%
  mutate(relabel = ifelse(b == "raw bands", "raw bands (Dove)", 
                          ifelse(n <= 2, "Features in 2≥ references",
                                 b)))

feats_summ = feats_summ %>% 
  group_by(relabel) %>%
  summarise(n = sum(n))

##
ggplot(feats_summ, aes(x = reorder(relabel, -n), y = n)) +
  geom_col(fill = simple_plot_col) +
  labs(
    x = "Spectral features",
    y = "Number of references",
    fill = "Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels
  # coord_flip()  # Flip the coordinates to make y-axis horizontal

#examine texture metrics
df_textures = df_indic %>%
  filter(str_detect(optimal_feature_subset, 'GLCM')|str_detect(optimal_feature_subset, 'FOTO'))

#features and applications
{df_indic2 = df %>%
    mutate(Indices_or_bands_2 = str_replace_all(Indices_or_bands_2, "raw bands \\(Dove\\)", "R, G, B, NIR")) %>% #replace raw bands with the actual bands
    mutate(optimal_feature_subset = ifelse(optimal_feature_subset == '-', Indices_or_bands_2, optimal_feature_subset)) %>% #if '-' is given for optimal_feature_subset, replace it with whatever is in Indices_or_bands_2
    mutate(optimal_feature_subset = bracket_cleaner(optimal_feature_subset)) %>%
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, ' d', ' ')) %>% #remove 'd' from the front of features (indicating change over time)
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'PCA', 'PC')) %>% #replace 'PCAx' with 'PCx' (meaning 'principle component x')
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'FOTO textures', 'FOTO')) %>%
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'Hue', 'Hue index')) %>%
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'Cigreen|Clgreen', 'CIgreen'))%>%
    #these next two lines get rid of features from other RS systems by separaing batches of features into rows based on a ; delimiter
    cSplit(splitCols = 'optimal_feature_subset', sep = '; ', direction = 'long') %>%
    filter(!str_detect(optimal_feature_subset, 'S2|RE')) %>%
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, 'PS:', '')) %>%
    #these next lines filter out additional features that do not come from Planetscope by separating all comma-delimited features into their own rows
    cSplit(splitCols = 'optimal_feature_subset', sep = ', ', direction = 'long') %>%
    filter(!str_detect(optimal_feature_subset, '_s2|_fused|_aerial|_l8|_re')) %>% #remove features that come from sentinel-2, fused bands, Landsat-8, or aerial imagery
    filter(!str_detect(optimal_feature_subset, 'VV|VH')) %>% #remove SAR data
    filter(!str_detect(optimal_feature_subset, 'PC\\d')) %>% #remove Principal components
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, '.*: ', '')) %>% #remove additional text with commas
    mutate(optimal_feature_subset = bracket_cleaner(optimal_feature_subset)) %>% #remove bracket text, since this didn't work before for some reason
    mutate(optimal_feature_subset = str_replace_all(optimal_feature_subset, '_.*','')) %>% #remove underscore text
    distinct(Title, optimal_feature_subset, .keep_all = T) %>% #
    filter(!optimal_feature_subset %in% c('DEM', 'Elevation', 'Road proximity', 'Slope', 'Temperature', 'Lidar intensity', 'Canopy height', 'Precipitation')) %>%
    # mutate(optimal_feature_subset = ifelse('\\bS\\b', 'Saturation',
    #                                        ifelse('\\bI\\b', 'Brightness',
    #                                               ifelse('\\bR\\b', 'Red band',
    #                                                      ifelse('\\bG\\b', 'Green band',
    #                                                             ifelse('\\bB\\b', 'Blue band',
    #                                                                    ifelse('NIR', 'NIR band',
    #                                                                           optimal_feature_subset))))))) %>%
    mutate(optimal_feature_subset = case_when(
      str_detect(optimal_feature_subset, '\\bS\\b') ~ 'Saturation',
      str_detect(optimal_feature_subset, '\\bI\\b') ~ 'Intensity',
      str_detect(optimal_feature_subset, '\\bR\\b') ~ 'Red band',
      str_detect(optimal_feature_subset, '\\bG\\b') ~ 'Green band',
      str_detect(optimal_feature_subset, '\\bB\\b') ~ 'Blue band',
      str_detect(optimal_feature_subset, '\\bNIR\\b') ~ 'NIR band',
      str_detect(optimal_feature_subset, 'NDWI') ~ 'NDWI2',
      str_detect(optimal_feature_subset, 'SR-NIRR') ~ 'SR',
      str_detect(optimal_feature_subset, 'FOTO') ~ 'FOTO textures',
      str_detect(optimal_feature_subset, 'GLCM') ~ 'GLCM textures',
      TRUE ~ optimal_feature_subset
    )) %>%
    # filter(!optimal_feature_subset %in% c('GLCM', 'FOTO')) %>% #exclude textures
    cSplit(splitCols = 'Application_cleaned', direction = 'long', sep = ', ') %>% #split applications
    distinct(Title, Application_cleaned, optimal_feature_subset, .keep_all = T) %>% #remove duplicate title/application combinations
    filter(str_detect(PS_role, "Principal imaging")) #exclude features from papers where PS was only used for validation
  
  feats_summ2 = splitter_counter(df_indic2$optimal_feature_subset)  
  # view(feats_summ)
  
  feats_apps_summ = df_indic2 %>%
    group_by(optimal_feature_subset, Application_cleaned) %>%
    summarise(count = n()) %>%
    left_join(tibble(Application_cleaned = apps_summ$b, app_freq = apps_summ$n)) %>%
    left_join(tibble(optimal_feature_subset = feats_summ2$b, feat_freq = feats_summ2$n)) %>%
    mutate(Application_label = paste0(Application_cleaned, ' (',app_freq,')'),
           feat_label = paste0(optimal_feature_subset, ' (',feat_freq,')'))
  
  #balloon plot
  x11()
  ggplot(feats_apps_summ %>%
           filter(feat_freq>= 2) %>% #filter out features that only appear in x or more publications
           arrange(desc(feat_freq)) #rearrange dataframe in descending order of feature popularity
         , aes(x = reorder(Application_label, -app_freq)
               , y = 
                 reorder(
                   feat_label
                   , feat_freq)
               ,
               , size = count
               , label = count))+
    # geom_label(
    #   shape = 21
    #   , fill = 'lightblue'
    # )+
    geom_point(
      color = simple_plot_col
    )+
    labs(x = 'Application', y = 'Terrestrial ecosystem'
         , size = 'Publications'
    )+
    theme_minimal() +
    scale_y_discrete(labels = \(x) stringr::str_wrap(x, 40))+
    scale_size_continuous(breaks = scales::breaks_width(2))+
    theme(axis.text.x = element_text(
      angle = 45
      #,vjust = 
      ,hjust = 1)
      # ,legend.position = 'none'
    )
  }


#---- Analysis/modelling techniques----

#separate publications by modelling approach and specific method/algorithm

#data cleaning
{
  methods_df <- df %>%
    filter(!str_detect(PS_role, 'Validation')) %>%  # exclude validation publications
    mutate(Approach_method2 = ifelse(!is.na(Approach_method1), Approach_method1, Approach_method)) %>% #incorporate both approach_method columns
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'caiton', 'cation')) %>%  # fix spelling of classification
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'KNN', 'k-nearest neighboors')) %>%  # relabel
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'K-means', 'k-means')) %>%
    # mutate(Approach_method2 = str_replace_all(Approach_method2, 'NDVI filtering', 'Thresholding')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'manual classification|visual classification|Visual interpretation|Visual classification'
                                              , 'Visual interpretation')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'Xgboost|XGboost', 'XGBoost')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'GEM', 'Generalized exponential model')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'models', 'model')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'BRT', 'Boosted regression trees')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'GLM', 'Generalized linear model')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'RF', 'Random forest')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'NN', 'Neural network')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'SVM', 'Support vector machine')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'MLC', 'Maximum likelihood classifier')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'OBIA', 'Object-based image analysis')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'MARS', 'Multivariate adaptive regression splines')) %>%
    mutate(Approach_method2 = str_replace_all(Approach_method2, 'Otsu', 'Otsu method')) %>%
    mutate(Approach_method2 = gsubfn("\\(([^\\)]+)\\)", 
                                     function(x) paste0("(", gsub(",", ";", x), ")"), 
                                     Approach_method2)) %>% #replace all commas inside of round brackets with semicolons
    cSplit(splitCols = 'Approach_method2', sep = ', ', direction = 'long') %>% #make single row for each approach (for pubs that have multiple)
    mutate(Approach = bracket_cleaner(Approach_method2)) %>% #make new column with just approaches
    mutate(Approach_reclassed = Approach) %>%
    mutate(Approach_reclassed = str_replace_all(Approach, 'Unsupervised classification', 'Classification')) %>% #relabel in new column. Uncomment this line to keep the "unsupervised classification" column
    mutate(Supervision = ifelse(is.na(Supervision), 'Supervised', Supervision)) %>%
    mutate(method_algorithm = extract_between_symbols(Approach_method2)) %>% #put methods/algorithms in separate column
    mutate(method_algorithm_cleaned = bracket_cleaner2(method_algorithm, open_sym = '\\[', close_sym = '\\]')) #remove square bracketed text from methods/algs (square brackets denote different neural net architectures)
  
  methods_df_long = methods_df %>% cSplit(splitCols = 'method_algorithm_cleaned', sep = '; ', direction = 'long')
  
  # view(methods_df %>% select(Citation, Title, Approach_method, Approach_method2, Approach, Approach_reclassed, method_algorithm, method_algorithm_cleaned))
  #view(
  #   methods_df_long %>% 
  #     filter(method_algorithm_cleaned == 'NN') %>%
  #     select(Citation, Title, Approach_method2, Approach_reclassed, method_algorithm_cleaned)
  # )
}

#approaches
{
  approaches_summ = methods_df_long %>%
    distinct(Title, Approach_reclassed) %>%
    group_by(Approach_reclassed) %>%
    summarise(count = n())
  
  approaches_methods_summ = methods_df_long %>%
    group_by(Approach_reclassed, method_algorithm_cleaned) %>%
    summarise(count = n())
}

#unsupervised
{
  unsup_methods_summ = methods_df_long %>%
    filter(Approach_reclassed == 'Unsupervised classification') %>%
    group_by(method_algorithm_cleaned) %>%
    summarise(count = n())
  
  unsup_plot = ggplot(unsup_methods_summ, 
                      aes(x = reorder(method_algorithm_cleaned, count)
                          , y = count))+
    geom_col()+
    theme_classic()+
    coord_flip()
}

#classification
{
  classification_methods_summ = methods_df_long %>%
    filter(Approach_reclassed == 'Classification') %>%
    group_by(method_algorithm_cleaned) %>%
    summarise(count = n())
  #view(classification_methods_summ)
  
  ggplot(classification_methods_summ, 
         aes(x = reorder(method_algorithm_cleaned, count)
             , y = count))+
    geom_col()+
    theme_classic()+
    coord_flip()
}

#regression
{
  regression_methods_summ = methods_df_long %>%
    filter(Approach_reclassed == 'Regression') %>%
    group_by(method_algorithm_cleaned) %>%
    summarise(count = n())
  #view(regression_methods_summ)}
  
  ggplot(regression_methods_summ, 
         aes(x = reorder(method_algorithm_cleaned, count)
             , y = count))+
    geom_col()+
    theme_classic()+
    coord_flip()
}

#classification and regression
{
  {
    methods_summ = methods_df_long %>%
      filter(Approach_reclassed %in% c('Classification', 'Regression')) %>%
      group_by(Approach_reclassed, method_algorithm_cleaned) %>%
      summarise(methods_count = n())
    
    
    methods_plot = ggplot(methods_summ, 
                          aes(x = reorder(method_algorithm_cleaned, count)
                              , y = count))+
      geom_col()+
      facet_wrap(vars(Approach_reclassed)
                 , nrow = 1
                 , scales = 'free')+
      theme_classic()+
      lims(y = c(0, max(methods_summ$count)))+
      coord_flip()
    # 
    # methods_plot
  }
  
  classification_methods_summ = methods_df_long %>%
    filter(Approach_reclassed == 'Classification') %>%
    group_by(method_algorithm_cleaned) %>%
    summarise(count = n())
  #view(classification_methods_summ)
  
  class_plot = ggplot(classification_methods_summ, 
                      aes(x = reorder(method_algorithm_cleaned, count)
                          , y = count))+
    geom_col()+
    theme_classic()+
    theme(
      axis.title.x = element_blank()
    )+
    labs(
      x = 'Classification'
    )+
    lims(y = c(0, max(methods_summ$count)))+
    coord_flip()
  
  regression_methods_summ = methods_df_long %>%
    filter(Approach_reclassed == 'Regression') %>%
    group_by(method_algorithm_cleaned) %>%
    summarise(count = n())
  #view(regression_methods_summ)}
  
  regress_plot = ggplot(regression_methods_summ, 
                        aes(x = reorder(method_algorithm_cleaned, count)
                            , y = count))+
    geom_col()+
    theme_classic()+
    theme()+
    labs(
      x = 'Regression'
      ,y = 'Number of publications'
    )+
    lims(y = c(0, max(methods_summ$count)))+
    coord_flip()
  
  unsup_summ = methods_df_long %>% 
    filter(Approach_reclassed == 'Classification', Supervision != 'Supervised') %>%
    group_by(method_algorithm_cleaned) %>%
    summarise(count = n())
  
  # sup_summ = methods_df_long %>% filter(App)
  
  
  
  class_plot_patch = class_plot + theme(axis.text.x = element_blank()) 
  
  class_plot_patch / regress_plot
  
}

#count number of publications that report specific model performance metrics
{
  stat_count_df = df %>%
    filter(
      !str_detect(PS_role, 'Validation')
      , !is.na(Analysis_performance_3)
      , Analysis_performance_3 != 'NA'
      , str_detect(Approach_method, 'Regression|lassification')
    ) %>%
    mutate(Analysis_performance_32 = gsub('\r\n','',Analysis_performance_3, fixed = T )) %>%
    mutate(Analysis_performance_32 = bracket_cleaner(Analysis_performance_32)) %>%
    cSplit(splitCols = 'Analysis_performance_32', sep = '; ', direction = 'long') %>%
    mutate(Analysis_performance_32 = str_remove_all(Analysis_performance_32, "[^,]+:\\s*")) %>%
    cSplit(splitCols = 'Analysis_performance_32', sep = ', ', direction = 'long') %>%
    mutate(Analysis_performance_32 = str_replace_all(Analysis_performance_32, "=.*", "")) %>%
    mutate(Analysis_performance_32 = str_replace_all(Analysis_performance_32, 'Mean |mean ', '')) %>%
    distinct(Title, Analysis_performance_32, keep_all = T)
  
  stat_count_df$Analysis_performance_32
  #view(splitter_counter(stat_count_df$Analysis_performance_32))
}

#supervision
{
  supervision_summ = df %>%
    mutate(Supervision = ifelse(is.na(Supervision), 'Supervised', Supervision)) %>%
    group_by(Supervision) %>%
    summarise(count = n())
}

#object vs pixel
{
  obpi_summ = methods_df %>%
    group_by(Pixel_object) %>%
    summarise(count = n())
  obpi_summ
}

#segmentation algorithm vs application
{
  seg_apps_df_long = df %>%
    filter(!is.na(Object_segmentation_alg)) %>%
    mutate(Object_segmentation_alg_cleaned = bracket_cleaner(Object_segmentation_alg)) %>%
    # cSplit(splitCols = 'Application_cleaned', sep = ', ', direction = 'long') %>%
    cSplit(splitCols = 'Object_segmentation_alg_cleaned', sep = ', ', direction = 'long')
  
  seg_apps_summ = seg_apps_df_long %>%
    group_by(Object_segmentation_alg_cleaned, Application_cleaned) %>%
    summarise(count = n())
  
  seg_type_summ = df %>%
    group_by(Object_segmentation_alg_type) %>%
    summarise(count = n())
  
  
}

#methods and applications performance
{
  #model performance methods and applications
  {
    model_perf_df = methods_df_long %>%
      #remove image quality papers
      filter(Application_cleaned != 'Data quality') %>%
      #remove entries that have no classification or regression model performance
      filter(!is.na(PS_model_performance_to_aggregate)) %>%
      filter(PS_model_performance_to_aggregate != 'NA') %>%
      #replace entries that have alg_comp with the content of the column Alg_performance_to_aggregate
      mutate(PS_model_performance_to_aggregate = ifelse(str_detect(PS_model_performance_to_aggregate, 'alg_comp'),
                                                        Alg_performance_to_aggregate,
                                                        PS_model_performance_to_aggregate
      )) %>%
      #split into separate rows so that each row contains model performance for a single application
      cSplit(splitCols = 'Application_cleaned', sep = ', ', direction = 'long') %>%
      mutate(PS_model_performance_to_aggregate = str_replace_all(PS_model_performance_to_aggregate, ';; ', '@ ')) %>%
      cSplit(splitCols = 'PS_model_performance_to_aggregate', sep = '@ ', direction = 'long') %>%
      mutate(Application_to_agg = ifelse(str_detect(PS_model_performance_to_aggregate, ':: '), 
                                         str_extract(PS_model_performance_to_aggregate, "^[^:]+"),
                                         Application_cleaned)) %>%
      filter(Application_cleaned == Application_to_agg) %>% #remove rows where the performance doesn't match the application
      #split methods so that there is a single method/algorithm on each row
      cSplit(splitCols = 'PS_model_performance_to_aggregate', sep = '; ', direction = 'long') %>%
      mutate(method_algorithm_to_agg = ifelse(str_detect(PS_model_performance_to_aggregate, ': '), 
                                              str_extract(PS_model_performance_to_aggregate, "^[^:]+"),
                                              method_algorithm_cleaned)) %>%
      #do same relabelling process for the method_alg column as is done earlier
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'caiton', 'cation')) %>%  # fix spelling of classification
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'KNN', 'k-nearest neighboors')) %>%  # relabel
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'K-means', 'k-means')) %>%
      # mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'NDVI filtering', 'Thresholding')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'manual classification|visual classification|Visual interpretation|Visual classification'
                                                       , 'Visual interpretation')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'Xgboost|XGboost', 'XGBoost')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'GEM', 'Generalized exponential model')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'models', 'model')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'BRT', 'Boosted regression trees')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'GLM', 'Generalized linear model')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'RF', 'Random forest')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'NN', 'Neural network')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'SVM', 'Support vector machine')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'MLC', 'Maximum likelihood classifier')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'OBIA', 'Object-based image analysis')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'MARS', 'Multivariate adaptive regression splines')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'Otsu', 'Otsu method')) %>%
      mutate(method_algorithm_to_agg = str_replace_all(method_algorithm_to_agg, 'GAM', 'Generalized additive model')) %>%
      filter(method_algorithm_cleaned == method_algorithm_to_agg) %>%  #remove rows where the performance doesn't match the application
      mutate(PS_model_performance_to_aggregate = ifelse(str_detect(PS_model_performance_to_aggregate, ":"),
                                                        str_extract(PS_model_performance_to_aggregate, "(?<=: ).*"),
                                                        PS_model_performance_to_aggregate)) %>% #remove algorithm, leave statistic and performance
      #extract statistic, remove from performance
      mutate(performance_stat = str_extract(PS_model_performance_to_aggregate, "[^\\s]+(?=\\s=)")) %>%
      mutate(PS_model_performance_to_aggregate = str_extract(PS_model_performance_to_aggregate, "(?<=\\=\\s).*"))
    
    mean_perf = c() #using a for loop since the mutate expression wasn't working very well for this
    for(i in 1:nrow(model_perf_df)){
      a = model_perf_df$PS_model_performance_to_aggregate[i]
      b = mean(
        as.numeric(
          str_split_1(a, ', ')
        )
      )
      mean_perf[i] = b
    }
    model_perf_df$PS_model_performance_to_aggregate = mean_perf
    
    #calculate difference accuracy metrics for titles that provide users and producers accuracy
    
    ua_pa_df = model_perf_df %>%
      filter(performance_stat %in% c('UA', 'PA')) %>%
      pivot_wider(names_from = performance_stat, values_from = PS_model_performance_to_aggregate) %>%
      mutate(F1 = 2 * UA * PA / (UA + PA)) %>%
      pivot_longer(cols = c('PA', 'UA', 'F1'), names_to = 'performance_stat', values_to = 'PS_model_performance_to_aggregate')
    
    model_perf_df = rbind(model_perf_df, ua_pa_df) %>% 
      filter(!performance_stat %in% c('UA', 'PA'))
    
    # view(
    #   model_perf_df %>% select(Citation, Title, Application_cleaned, Application_to_agg, Approach_reclassed, method_algorithm_cleaned, method_algorithm_to_agg,performance_stat, PS_model_performance_to_aggregate)
    # )
    
    #summarize model performance per algorithm and application
    
    model_perf_summ = model_perf_df %>%
      group_by(Application_to_agg, method_algorithm_to_agg, Approach_reclassed) %>%
      summarise(mean_performance = mean(PS_model_performance_to_aggregate))
    
  }
  
  
  #all
  {
    ##clean data to aggregate applications and performance metrics
    methods_apps_df_long = methods_df_long %>%
      #split so that each row only has one application
      cSplit(splitCols = 'Application_cleaned', sep = ', ', direction = 'long')
    
    analysis_apps_summ = df %>%
      filter(PS_role != 'Validation') %>%
      cSplit(splitCols = 'Application_cleaned', sep = ', ', direction = 'long') %>%
      group_by(Application_cleaned) %>%
      summarise(count = n())
    
    methods_apps_summ = methods_apps_df_long %>%
      #remove image quality papers
      filter(Application_cleaned != 'Image quality') %>%
      #remove analysis methods that are not used for classification or regression (e.g. significance testing)
      filter(Approach_reclassed %in% c('Classification', 'Regression')) %>%
      #only keep distinct combinations of approach, method, application, and Title
      distinct(Title, Application_cleaned, method_algorithm_cleaned, Approach_reclassed, .keep_all = T) %>%
      #summarise number of publications for each combination of Application and Method
      group_by(Application_cleaned, Approach_reclassed, method_algorithm_cleaned) %>%
      summarise(app_approach_method_count = n()) %>%
      #add number of publications for each algorithm and application, use these figures to make text labels for the axes
      left_join(tibble(Application_cleaned = analysis_apps_summ$Application_cleaned, app_freq = analysis_apps_summ$count)) %>%
      left_join(methods_summ) %>%
      mutate(Application_label = paste0(Application_cleaned, ' (',app_freq,')'),
             method_label = paste0(method_algorithm_cleaned, ' (',methods_count,')')) %>%
      #add model performance metrics
      left_join(model_perf_summ %>% rename(Application_cleaned = Application_to_agg, method_algorithm_cleaned = method_algorithm_to_agg)) %>%
      mutate(performance_class = case_when(
        mean_performance <= 0.5 ~ "≤0.5",
        mean_performance <= 0.6 ~ "≤0.6",
        mean_performance <= 0.7 ~ "≤0.7",
        mean_performance <= 0.8 ~ "≤0.8",
        mean_performance <= 0.9 ~ "≤0.9",
        mean_performance <= 1 ~ "≤1.0",
        is.na(mean_performance) ~ "NA"
      ))
    
    #balloon plot
    ggplot(methods_apps_summ %>%
             filter(Application_cleaned != 'Data quality') %>%
             filter(methods_count > 1) %>%
             arrange(desc(methods_count))
           , aes(x = reorder(Application_label, -app_freq)
                 , y = 
                   reorder(
                     method_label
                     , methods_count)
                 ,
                 , size = app_approach_method_count
                 , label = app_approach_method_count
                 # ,order = -app_approach_method_count
           ))+
      # geom_label(
      #   # shape = 21
      #   , fill = 'lightblue'
      #   , alpha = 0.5
      #   # , aes(order = -app_approach_method_count)
      # )+
      geom_point(
        # color = simple_plot_col
        aes(color = performance_class)
      )+
      scale_color_manual(values = 
                           # c('', 'gold', 'orange', 'tomato', 'red2','darkred', 'grey50')
                           # c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026", 'grey50')
                           c(brewer.pal(n = 6, name = 'Spectral'),'grey50'))+
      labs(x = 'Application', 
           y = 'Algorithm or method'
           , color = 'Mean model performance'
           , size = 'Number of publications'
      )+
      theme_minimal() +
      # scale_y_discrete(labels = \(x) stringr::str_wrap(x, 40))+
      facet_wrap(vars(Approach_reclassed), ncol = 1, drop = T
                 ,strip.position = 'right'
                 ,axis.labels = 'all_x'
                 , scales = 'free_y'
      ) +
      scale_size(name = 'Number of publications', range = c(2,7.9), breaks = c(3,6,9,12,15))+
      labs(x = 'Application', 
           y = 'Algorithm or method'
           , color = 'Mean model performance'
           , size = 'Number of publications'
      )+
      theme(axis.text.x = element_text(
        angle = 45
        #,vjust = 
        ,hjust = 1)
        # ,legend.position = 'none'
        # ,axis.line = element_line()
        ,panel.background = element_rect(fill = NA, color = "black") # add lines around facet panels
      )
    
  }
  
  


#compare methods
{
  #with facet_grid
  {
    
    model_compare_df = model_perf_df %>%
      #filter out publications that don't compare algorithms
      filter(!is.na(Alg_performance_to_aggregate))
    
    compare_algs = unique(model_compare_df$method_algorithm_cleaned)
    
    #pivot wider
    model_compare_df = model_compare_df %>%
      pivot_wider(names_from = 'method_algorithm_cleaned', values_from = 'PS_model_performance_to_aggregate')
    
    long_df <- model_compare_df %>%
      pivot_longer(
        cols = all_of(compare_algs),
        names_to = "algorithm",
        values_to = "performance"
      ) %>%
      filter(!is.na(performance))  # Remove rows with missing performance values
    
    # Create pairwise combinations
    pairwise_df <- long_df %>%
      inner_join(long_df, by = c("Title" = "Title", 'Application_cleaned' = 'Application_cleaned')) %>% # Keep row pairing
      # filter(algorithm.x < algorithm.y) %>% # Retain unique pairs
      rename(
        algorithm_1 = algorithm.x,
        alg_performance_1 = performance.x,
        algorithm_2 = algorithm.y,
        alg_performance_2 = performance.y
      ) %>% 
      filter(algorithm_1 != 'Thresholding')
    # %>%
    #   filter(algorithm_1 != algorithm_2)
    
    pairwise_df$algorithm_1 = pairwise_df$algorithm_1 %>%
      str_replace_all('Generalized exponential model' , 'GEM') %>%
      str_replace_all('Boosted regression trees' , 'BRT') %>%
      str_replace_all('Generalized linear model' , 'GLM') %>%
      str_replace_all('Random forest' , 'RF') %>%
      str_replace_all('Neural network' , 'NN') %>%
      str_replace_all('Support vector machine' , 'SVM') %>%
      str_replace_all('Maximum likelihood classifier' , 'MLC') %>%
      str_replace_all('Object-based image analysis' , 'OBIA') %>%
      str_replace_all('Multivariate adaptive regression splines' , 'MARS') %>%
      str_replace_all('Otsu method' , 'Otsu') %>%
      str_replace_all('model' , 'models') %>%  
      str_replace_all('XGboost' , 'XGBoost') %>%
      str_replace_all('Simple linear regression', 'SLR') %>%
      str_replace_all('Multiple linear regression', 'MLR') %>%
      str_replace_all('Generalized additive model|Generalized additive models', 'GAM') %>%
      str_replace_all('GAMs', 'GAM') %>%
      str_replace_all('k-nearest neighboors', 'KNN')
    
    pairwise_df$algorithm_2 = pairwise_df$algorithm_2 %>%
      str_replace_all('Generalized exponential model' , 'GEM') %>%
      str_replace_all('Boosted regression trees' , 'BRT') %>%
      str_replace_all('Generalized linear model' , 'GLM') %>%
      str_replace_all('Random forest' , 'RF') %>%
      str_replace_all('Neural network' , 'NN') %>%
      str_replace_all('Support vector machine' , 'SVM') %>%
      str_replace_all('Maximum likelihood classifier' , 'MLC') %>%
      str_replace_all('Object-based image analysis' , 'OBIA') %>%
      str_replace_all('Multivariate adaptive regression splines' , 'MARS') %>%
      str_replace_all('Otsu method' , 'Otsu') %>%
      str_replace_all('model' , 'models') %>%  
      str_replace_all('XGboost' , 'XGBoost') %>%
      str_replace_all('Simple linear regression', 'SLR') %>%
      str_replace_all('Multiple linear regression', 'MLR') %>%
      str_replace_all('Generalized additive model|Generalized additive models', 'GAM') %>%
      str_replace_all('GAMs', 'GAM') %>%
      str_replace_all('k-nearest neighboors', 'KNN')
    
    
    #plot classification performance comparison
    ggplot(pairwise_df %>% 
             mutate(diagonal_facet = ifelse(algorithm_1 == algorithm_2, 1, 0)) %>% #column for formatting facet_grid panels 
             filter(Approach_reclassed.x == 'Classification'),
           aes(x = alg_performance_1, 
               y = alg_performance_2)) +
      geom_point(aes(color = Application_cleaned)
                 ,size = 2.5
                 ,alpha = 0.8) +
      scale_color_manual(values = application_colors)+
      geom_abline()+
      geom_rect(data = . %>% filter(diagonal_facet == 1), # add rectangles to fill in the diagonal panels
                xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                fill = 'grey20', alpha = 1, color = NA) +
      facet_grid(vars(algorithm_1), vars(algorithm_2)) +
      labs(x = 'Algorithm 1 accuracy', y = 'Algorithm 2 accuracy', color = 'Application') +
      # scale_y_continuous(limits = c(0.5,1))+
      # scale_x_continuous(limits = c(0.5,1))+
      theme_classic()+
      theme(panel.background = element_rect(fill = NA, color = "black"))
    
    #plot regression performance comparison
    ggplot(pairwise_df %>% 
             mutate(diagonal_facet = ifelse(algorithm_1 == algorithm_2, 1, 0)) %>% #column for formatting facet_grid panels
             filter(Approach_reclassed.x == 'Regression'),
           aes(x = alg_performance_1, 
               y = alg_performance_2)) +
      geom_point(aes(color = Application_cleaned)
                 ,size = 2.5
                 ,alpha = 0.8) +
      scale_color_manual(values = application_colors)+
      geom_abline()+
      geom_rect(data = . %>% filter(diagonal_facet == 1), # add rectangles to fill in the diagonal panels
                xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                fill = 'grey20', alpha = 1, color = NA) +
      facet_grid(vars(algorithm_1), vars(algorithm_2)) +
      labs(x = 'Algorithm 1 goodness-of-fit', y = 'Algorithm 2 goodness-of-fit', color = 'Application') +
      # scale_y_continuous(limits = c(0,1))+
      # scale_x_continuous(limits = c(0,1))+
      theme_classic()+
      theme(panel.background = element_rect(fill = NA, color = "black"))
  }
  
  # #with ggpairs
  # {
  #   class_model_compare_df = model_perf_df %>%
  #     #filter out publications that don't compare algorithms
  #     filter(!is.na(Alg_performance_to_aggregate)
  #                   ,Approach_reclassed == 'Classification')
  #   
  #   class_algs_to_compare = unique(class_model_compare_df$method_algorithm_to_agg)
  #     #replace NA values with 0 (necessary since ggpairs doesn't work with missing data)
  #     class_model_compare_wide = class_model_compare_df %>%
  #       pivot_wider(names_from = 'method_algorithm_cleaned'
  #                 , values_from = 'PS_model_performance_to_aggregate'
  #                 , values_fill = 0) 
  #   
  #   #classification
  #   ggpairs(class_model_compare_wide
  #           , columns = all_of(class_algs_to_compare))
  #     
  # }
}

}
