library(officer)
library(coviData)
library(tidyverse)
library(magrittr)

# Read template
ppt <- read_pptx("C:/Users/Jesse.Smith/Documents/template.pptx")

# View layouts
officer::layout_summary(ppt)
officer::layout_properties(ppt) %>%
  select(master_name, name, type, id, ph_label) %>%
  arrange(master_name, name, type, id, ph_label)

# Create master variable
master <- "HD Blue and White"

# Create title slide
ppt <- add_slide(ppt, layout = "Title Slide", master = master)

# Add title
ppt <- ph_with(
  ppt,
  value = paste0(
    "COVID-19 Daily Status Report\n\n",
    format(Sys.Date(), "%m/%d/%Y")
  ),
  location = ph_location_type(type = "ctrTitle", newlabel = "title")
)

# Add cumulative curve

# Add specimen collection date curve (with new reported cases highlighted?)

#

if (!coviData:::is_open("test.pptx")) {
  print(ppt, "test.pptx")
} else {
  rlang::abort("File is open")
}
