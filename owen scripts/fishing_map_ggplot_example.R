## Libraries
library(sf) # for spatial polygon stuff
library(tidyverse) # for ggplot and data wrangling stuff
library(viridis) # for the nice color ramps
library(here) # makes it easier to find files that are somewhere nested under your root directory

# this is a standard ggplot theme that I use. It makes the default fonts/sizes/overall look of the graphs more pleasing than the defaults, in my opinion
plot_theme <-   theme_minimal()+
  theme(text=element_text(family="serif",size=12,color="black"),
        legend.text = element_text(size=14),
        axis.title=element_text(family="sans",size=14,color="black"),
        axis.text=element_text(family="sans",size=8,color="black"),
        panel.grid.major = element_line(color="gray50",linetype=3))
# theme_set sets the ggplot theme for the session, so it'll be the new default for all ggplots
theme_set(plot_theme)

## The spatial grid data that I got from Blake
grd <- read_sf(here::here("Input_Data","5x5 Grid","regions_master_final_lamb.shp")) # insert whatever filepath to your shapefile here
names(grd)

## grd is a spatial simple features (sf) object but looks like a data frame. And for the most part we can treat it as such.
## it has a TON of complexity, and includes multiple scales of polygons (e.g., all state waters vs. 5km squares or CA fishing blocks)

# Say we wanted a map of central California only, but by CA fishing block
# Treat the spatial data like a data frame to filter out data of interest
ca_fishing_blocks <- grd %>% 
  filter(SUBREGION=="Monterey Bay") %>%
  # you can use sf to do spatial summaries
  group_by(CA_FISHBLO) %>%
  # this will combine any lower level polygons into the polygons we want
  summarise() %>% 
  ungroup()
glimpse(ca_fishing_blocks)
# set polygons with -999 to NA for convenience
ca_fishing_blocks <- ca_fishing_blocks %>% mutate(CA_FISHBLO=ifelse(CA_FISHBLO==-999,NA,CA_FISHBLO))
# now we have just a spatial list of Monterey CA fishing blocks

# get a bounding box for plotting purposes (set x and y limits)
bbox=st_bbox(ca_fishing_blocks)

# do the plot, which looks almost just like any other ggplot
ca_fishing_blocks_plot <-ggplot(ca_fishing_blocks)+
  # this adds the layer, and we'll scale color just on the fishing block ID number (normally this would be some variable of interest, e.g. whale density)
  # here I make it a little transparent too (with alpha)
  geom_sf(aes(fill=CA_FISHBLO),alpha=0.8)+
  # sf can also label the blocks
  geom_sf_text(aes(label=CA_FISHBLO),size=2,color='white')+
  # if you had another other shapefiles (e.g. a basemap of state polygons) you could add them here too
  # geom_sf(data=coaststates,fill='gray50')+
  # then here's the color scale, from the viridis package
  scale_fill_viridis(na.value='gray80')+
  # use our bounding box from above to set limits for plotting
  coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
  # titles and axis labels and such
  labs(fill="CA Fishing\nBlock Number",title="",x="",y="")+
  # some adjustments (legend placement and background color)
  theme(legend.position = 'bottom',
        legend.key.size = unit(1,'cm'))

ca_fishing_blocks_plot
