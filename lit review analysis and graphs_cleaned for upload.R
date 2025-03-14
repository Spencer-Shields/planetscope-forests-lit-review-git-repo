

#----Load packages and data and do initial cleaning
{
  
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
  library(ggh4x)
  
  
  review_list <<- 'review_list_7.3.xlsx'
  
  review_list_temp = tempfile(fileext = '.xlsx')
  file.copy(review_list, review_list_temp, overwrite = T)
  
  df_big <<- read_excel(review_list_temp, sheet = "main table")
  
  figures_dir = 'figures'
  if(!dir.exists(figures_dir)){dir.create(figures_dir)}
  
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
  
  # columns_to_select = c(
  #   'Item.Type',
  #   'Publication.Year',
  #   'Author',
  #   'Title',
  #   'Publication.Title',
  #   'Volume',
  #   'Issue',
  #   'ISBN',
  #   'ISSN',
  #   'Url',
  #   'PS_role',
  #   'PS_data.product',
  #   'Application_recleaned',
  #   'Country',
  #   'ecosystem_type_biome',
  #   'Analysis_performance_3',
  #   'Approach_method1'
  # )
  
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
  
  
  #save figure to figures directory
  save.fig = function(f){
    ggsave(filename = paste0(figures_dir,'/',f,'.png')
           , dpi = 600
           ,width = 18
           ,height = 18
           ,units = 'cm')
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
  
  df = df_sub} 

#####---------------------------------- Publication info -------------------------------------
#---- Eligibility ----

#access
df_big %>% group_by(No_access) %>% summarise(count = n())

#exclusion
df_big %>% mutate(excluder = ifelse(is.na(Exclude), Maybe_exclude, Exclude)) %>% group_by(excluder) %>% summarise(count = n())
df_big %>% filter(str_detect(Exclude, 'glish')|str_detect(Maybe_exclude, 'glish')) %>% select(Author, Title, Exclude) #how many not in english

#---- Publication type----

pub_type_summ = df %>%
  # filter((is.na(Exclude) & is.na(Maybe_exclude))) %>%
  group_by(Item.Type) %>%
  summarise(n = n())
pub_type_summ


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
    y = "Number of articles",
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


n_min_pubs = 3 #minimum Number of articles that an application must appear in to receive its own entry in the graphs etc.

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
    y = "Number of articles"
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
    y = "Number of articles"
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
    scale_fill_viridis(
      #option = 'H'
    )+
    labs(
      fill = 'Number of articles') +
    theme_minimal_grid()+
    coord_sf(crs= "+proj=robin")
  ggsave(filename = paste0(figures_dir,'/Figure3.png'), #crop crop final image
         dpi = 600
         ,height = 24,
         width = 24,
         units = 'cm')
  # save.fig('Figure3')
  
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
    y = "Number of articles",
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
    y = "Number of articles",
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
{
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
    scale_size(name = 'Number of articles', range = c(2,8), breaks = c(2,4,6,8,10))+
    theme_minimal() +
    scale_y_discrete(labels = \(x) stringr::str_wrap(x, 40))+
    # scale_size_continuous(breaks = c(2,4,6,8,10))+
    theme(axis.text.x = element_text(
      angle = 45
      #,vjust = 
      ,hjust = 1)
      # ,legend.position = 'none'
    )
  
  save.fig('Figure4')
}

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
  
  
  area_summ = area_df %>% #get Number of articles in each size class and scene size class
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
      y = "Number of articles",
      fill = 'Size class'
    ) +
    # scale_fill_viridis(discrete = T, option = 'magma')+
    # scale_fill_manual(values = c("#FC8D59", "#FEE08B", "#E6F598", "#99D594"))+
    scale_fill_manual(values = rev(c('powderblue', 'turquoise', 'steelblue2', 'dodgerblue4')))+
    # scale_fill_brewer(palette = 'Purples', direction = -1)+
    # scale_x_continuous(labels = area_hist$class_lab, breaks = area_hist$class_no) +  # Customize x-axis labels
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
  save.fig('Figure6')
}
#median size of area imaged
median(area_df$Total_area_imaged_km2_clean)

#---- Temporal resolution ----

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
    
    temp_p = ggplot(temp_res_df,
                    aes(x = Revisit_cleaned
                        ,y = Timeseries_length
                        ,size = n_timeseries_dates)) +
      annotate('rect', xmin = 0, xmax = 120, ymin = -Inf, ymax = 1200
               ,alpha = 0.1
               ,color = 'black'
      )+
      geom_point(alpha = 0.5, color = simple_plot_col) +
      labs(
        x = 'Average revisit time (days)'
        ,y = 'Total length of time series (days)'
        ,size = 'Image days'
      )+
      scale_size(range = c(1.5,8), breaks = c(2,50,100,150,200))+
      facet_zoom(xy = Revisit_cleaned <=120
                 ,horizontal = F
                 ,show.area = F
      )+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(), # Remove major gridlines
        panel.grid.minor = element_blank()  # Remove minor gridlines
      )
    
    temp_pb = ggplot_build(temp_p)
    temp_pb$data[[1]][4, 'alpha'] = 0
    temp_pb$data[[1]][4, 'colour'] = 'white'
    temp_pg = ggplot_gtable(temp_pb)
    plot(temp_pg)
    # save.fig('Figure5')
    ggsave(filename = paste0(figures_dir,'/Figure5.png')
           ,plot = temp_pg
           ,width = 18
           ,height = 18
           ,units = 'cm'
           ,dpi = 600)
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
    y = "Number of articles"
    # fill = "Type"  # Since you're using color, this can be removed
  ) +
  ylim(0, ceiling(max(PS_use_year$count) / 5) * 5) +
  scale_color_manual(values = tricolor_plot_col) +  # Set custom colors
  theme_classic() +
  theme(legend.title = element_blank())
save.fig('Figure2')


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
    y = "Number of articles"
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

#data prep
{
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
  nrow(RS_df) #Number of articles that include other types of remote sensing data
  
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
  
  RS_summ$RS_systems = RS_summ$RS_systems %>%
    str_replace_all('FORMSAT-2', 'FORMOSAT-2') %>%
    str_replace_all('Sentinel 5-p', 'Sentinel-5p')
  
  #information on remote sensing systems in the review list 
  rs_type_df = read.csv('remote_sensing_systems_information.csv')
  
  
  #check if the summary table has every system mentioned in the review list
  setdiff(RS_summ$RS_systems, rs_type_df$RS_systems)
  
  #add system information to RS_summ dataframe
  RS_summ = merge(RS_summ, rs_type_df)
  
  #recode sensor/platform information
  
  RS_summ = RS_summ %>%
    mutate(sensor_type = ifelse(sensor_type %in% c('Multispectral', 'Hyperspectral', 'Panchromatic', 'RGB')
                                , 'Optical'
                                , sensor_type)) %>%
    mutate(platform_type = str_replace_all(platform_type, 'Ground-based', 'Terrestrial'),
           RS_systems = str_replace_all(RS_systems, 'Ground-based', 'Terrestrial')) %>%
    mutate(RS_systems = str_replace_all(RS_systems, 'Aerial laser scanning', 'Airborne laser scanning'))
}

#plotting
{
  #stacked bar chart facet grid
  {
    RS_summ$platform_type = factor(RS_summ$platform_type, levels = c('Satellite', 'Aerial', 'Terrestrial', 'UAV'))
    RS_summ$sensor_type = factor(RS_summ$sensor_type, levels = c('Optical', 'LiDAR', 'RADAR'))
    
    ggplot(RS_summ, aes(x = reorder(RS_systems, tot_count), y = count, fill = sys_value)) +
      geom_col() +
      labs(
        x = "Remote sensing system",
        y = "Number of articles",
        fill = "Type of use"
      ) +
      theme_classic() +
      scale_fill_manual(values = bicolor_plot_col) +
      theme(axis.text.x = element_text(
        # angle = 45, 
                                       hjust = 1)
            , panel.background = element_rect(fill = NA, color = "black")
            ,strip.background = element_blank())+   # Rotate x-axis labels
      # facet_wrap(facets = vars(sensor_type, platform_type)
      #            , scales = 'free_y')+
      # facet_grid(rows = vars(platform_type), cols = vars(sensor_type), scales = 'free_y', space = 'free_y')+
      facet_nested('Platform type'*platform_type ~ 'Sensor type'*sensor_type
                   , scales = 'free_y'
                   , space = 'free_y'
                   , nest_line = element_line())+
      coord_flip()  # Flip the coordinates to make y-axis horizontal
    save.fig('Figure9')
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
         , y = "Number of articles"
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
  # filter(!optimal_feature_subset %in% c('GLCM', 'FOTO')) %>% #exclude textures
  filter(str_detect(PS_role, "Principal imaging")) #exclude features from papers where PS was only used for validation

}

# bar graph (features in 2>= articles)
{
  feats_summ = splitter_counter(df_indic$optimal_feature_subset) %>%  
    mutate(relabel = ifelse(b == "raw bands", "raw bands (Dove)", #make "Other indices" those which appear in only 1 study
                            ifelse(n <= 2, "Features in 2≥ articles",
                                   b))) %>%
    group_by(relabel) %>%
    summarise(n = sum(n))
  
  
ggplot(feats_summ, aes(x = reorder(relabel, -n), y = n)) +
  geom_col(fill = simple_plot_col) +
  labs(
    x = "Features",
    y = "Number of articles",
    fill = "Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)# Rotate x-axis labels
        ,plot.margin = margin(,,,0.7,'cm')) #adjust left plot margin to fit axis text
# coord_flip()  # Flip the coordinates to make y-axis horizontal
save.fig('Figure7')
}

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
  ggplot(feats_apps_summ %>%
           filter(feat_freq>= 2) %>% #only include features that appear in x or more publications
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
      ,y = 'Number of articles'
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

#count Number of articles that report specific model performance metrics
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
      filter(performance_stat %in% c('OA', 'r2')) %>% #only calculate for r2 and Overall accuracy
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
      #summarise Number of articles for each combination of Application and Method
      group_by(Application_cleaned, Approach_reclassed, method_algorithm_cleaned) %>%
      summarise(app_approach_method_count = n()) %>%
      #add Number of articles for each algorithm and application, use these figures to make text labels for the axes
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
           , size = 'Number of articles'
      )+
      theme_minimal() +
      # scale_y_discrete(labels = \(x) stringr::str_wrap(x, 40))+
      facet_wrap(vars(Approach_reclassed), ncol = 1, drop = T
                 ,strip.position = 'right'
                 ,axis.labels = 'all_x'
                 , scales = 'free_y'
      ) +
      scale_size(name = 'Number of articles', range = c(2,7.9), breaks = c(3,6,9,12,15))+
      labs(x = 'Application', 
           y = 'Algorithm or method'
           , color = str_wrap('Mean model performance', width = 19)
           , size = 'Number of articles'
      )+
      theme(axis.text.x = element_text(
        angle = 45
        #,vjust = 
        ,hjust = 1)
        # ,legend.position = 'none'
        # ,axis.line = element_line()
        ,panel.background = element_rect(fill = NA, color = "black") # add lines around facet panels
      )
    save.fig('Figure8')
    
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
