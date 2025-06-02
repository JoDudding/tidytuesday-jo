#-------------------------------------------------------------------------------
#' possible-weeks.r
#-------------------------------------------------------------------------------
#' jo dudding
#' May 2025
#' explore the different weeks
#-------------------------------------------------------------------------------

source("scripts/_setup.r")  
  
#--- download the tt week information ---

tt_week <- tt_available()

#--- ideas ---

#' leaflet
#' animated chart
#' interactive chart
#' api call
#' upset chart
#' spider chart
#' model types
#' dashboard
#' survey likert
#' clustering - https://tidyclust.tidymodels.org/
#' survival - https://www.tidyverse.org/blog/2024/04/tidymodels-survival-analysis/
#' interesting packages
#' shiny
#' quarto code cell chunks
#' time series forecasting
#' bayesian
#' arrow
#' duckdb
#' llm with elmer
#' hex map nz
#' text analysis

#--- Base R Penguins ---

#peng <- tt_load("2025-04-15")
#walk(peng, glimpse)
#use_tt_template("2025-04-15")

#--- Seismic Events at Mount Vesuvius ---

vesuvius <- tt_load("2025-05-13")
walk(vesuvius, glimpse)

#--- Great British Bakeoff ---

bakeoff <- tt_load("2022-10-25")
walk(bakeoff, glimpse)

#--- Bechdel Test ---

bechdel <- tt_load("2021-03-09")
walk(bechdel, glimpse)

#--- Australian Fires ---

fire <- tt_load("2020-01-07")
walk(fire, glimpse)

#--- Marble Races ---

marbles <- tt_load("2020-06-02")
walk(marbles, glimpse)

#--- US Wind Farm locations ---

wind <- tt_load("2018-11-06")
walk(wind, glimpse)

#--- roy kent ---

roy <- tt_load("2023-09-26")
walk(roy, glimpse)

#--- ask a manager salary survey ---

ask_a_manager <- tt_load("2021-05-18")
walk(ask_a_manager, glimpse)

#--- David Robinson's TidyTuesday Functions ---
drob <- tt_load("2024-07-09")
walk(drob, glimpse)

#--- Art Collections ---

tate <- tt_load("2021-01-12")
walk(tate, glimpse)

#--- lego ---

lego <- tt_load("2022-09-06") 
walk(lego, glimpse)

#--- Solar/Wind utilities ---

solar_wind <- tt_load("2022-05-03")
walk(solar_wind, glimpse)

#--- Stack Overflow Annual Developer Survey 2024 ---
stack <- tt_load("2024-09-03")
walk(stack, glimpse)

#--- D&D Monsters ---
dnd <- tt_load("2025-05-27")
walk(dnd, glimpse)

#-------------------------------------------------------------------------------
