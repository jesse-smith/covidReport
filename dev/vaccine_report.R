import::from("magrittr", "%T>%")
# Load data
data <- coviData::vac_parse(coviData::vac_load())

# Define today's date
today <- format(lubridate::today(), "%m/%d/%y")

# Title of presentation
title <- paste0("COVID-19 Vaccination Status Report (", today, ")")

# Vaccinations total/1 day/7 day
table_recent <- vac_table_recent(data) %T>% show()

# Vaccination doses
table_doses <- vac_table_doses(data) %T>% show()

# Goal figure
fig_goal <- plot_vaccinations() %T>% show()

# Vaccinations over time
