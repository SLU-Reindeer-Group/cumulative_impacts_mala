# Render Appendices
library(quarto)

setwd("code/07_synthesis_results/")

# app A
quarto::quarto_render("AppendixA_area_data.qmd")

# app B
quarto::quarto_render("AppendixB_HSA_synthesis.qmd")

# app C
quarto::quarto_render("AppendixC_iSSA_synthesis_v2.qmd")

# app D
quarto::quarto_render("AppendixD_calving_site_selection_synthesis_v2.qmd")
