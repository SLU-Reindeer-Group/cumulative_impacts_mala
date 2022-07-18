#---------------------------------------
# Title: Build main GRASS GIS location for Swedish background vectors
# Author: Bernardo Niebuhr
# 2020-10-09
#---------------------------------------

python

# import modules
import os
import grass.script as grass
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import raster as r

#---------------------------------------
# Setup location

# create locations
# within PERMANENT

prefix = "sam_"
mapset_names = ["env", "reindeer_ancillary", "reindeer_main"]
mapset_names = [prefix + name for name in mapset_names]

for i in mapset_names:
    g.mapset(mapset = i, flags = "c") #-c to create

#---------------------------------------
# Load data

# root folder
root_dir = r"D:\bernardo\00_academico\07_projetos\05_reindeer\05_env_data\02_vector"
os.chdir(root_dir)

#---------------------------------------
# Load data - env

# mapset
g.mapset(mapset = "sam_env")

# folder
env_dir = r"sam_env/"

# load wind turbines in operation 2020 - OK
v.in_ogr(input = env_dir+"wind_turbines_operation_Vindkraftverk_2020.shp", 
    output = "wind_turbines_operation_2020")
    #where = "VerkArende = 'Uppfort'")

# load power lines 2020 - OK
v.in_ogr(input = env_dir+"power_lines_Kraftledningar_vagkartan_2020.shp", output = "power_lines_lm_2020")

# mining 2020
v.in_ogr(input = env_dir+"mining_Beviljade_bearbetningskoncessioner_2020.shp", 
    output = "mining_sgu_2020")

# mining signature 2020
v.in_ogr(input = env_dir+"mining_signature_Beviljade_markkoncessioner_2020.shp", 
    output = "mining_signature_sgu_2020")

# mining - active areas 2020 - OK
v.in_ogr(input = env_dir+"mining_active_2020.gpkg", 
    output = "mining_active_sgu_2020_mala")

# load large public roads 2020 - OK
v.in_ogr(input = env_dir+"public_roads_Allmanna_vagar_2020.shp", output = "public_roads_lm_2020")

# load private roads 2020 - OK
v.in_ogr(input = env_dir+"private_roads_Ovriga_vagar_2020.shp", output = "private_roads_lm_2020")

# load railways 2020 - OK
v.in_ogr(input = env_dir+"railways_Jarnvagar_vagkartan_2020.shp", output = "railways_lm_2020")

# load buildings 2020 - OK
v.in_ogr(input = env_dir+"buildings_Byggnader_2020.shp", output = "buildings_lm_2020",
    overwrite = True)

# load houses 2020 - OK
v.in_ogr(input = env_dir+"houses_Hus_2020.shp", output = "houses_lm_2020")

# load urban 2020 - OK
v.in_ogr(input = env_dir+"urban_Bebyggelseomrade_vagkartan_2020.shp", output = "urban_lm_2020")

# trails 2020 - OK
v.in_ogr(input = env_dir+"trails_Stigar_och_leder_oversiktskartan_2020.shp", 
    output = "trails_lm_2020")

# snowmobile tracks 2020 - OK
v.in_ogr(input = env_dir+"snow_mobile_trails_Skoterleder_i_fjallen_fjallkartan_2020.shp", 
    output = "snowmobile_tracks_lm_2020")

# agriculture 2015 - JBV - OK
v.in_ogr(input = env_dir+"agriculture_Jordbruksmark_JBV.gpkg", 
    output = "agriculture_JBV_2015")

# agriculture 2004 - SMD - OK
v.in_ogr(input = env_dir+"agriculture_Jordbruksmark_SMD.gpkg", 
    output = "agriculture_SMD_2004")

# clearcuts 2020 - SKS - OK
v.in_ogr(input = env_dir+"clear_cuts_Utford_avverkning_SKS.gpkg", 
    output = "clear_cuts_SKS_2020", overwrite = True)


#---------------------------------------
# Load data - ancillary

# mapset
g.mapset(mapset = "sam_reindeer_ancillary")

# folder
ancillary_dir = r"sam_reindeer_ancillary/"

# load general availability data for Mittadalen herding district - OK
v.in_ogr(input = ancillary_dir+"availability_general_mittadalen.shp", output = "availability_general_mittadalen")

# load more strict availability ara for Mittadalen herding district - OK
v.in_ogr(input = ancillary_dir+"availability_mittadalen_herding_line_6.shp", output = "availability_mittadalen_herding_line_6")

# load general availability data for Tassasen herding district - OK
v.in_ogr(input = ancillary_dir+"availability_general_Tassasen.shp", output = "availability_general_tassasen")

# load general availability data for Mala herding district until July (fence) - OK
v.in_ogr(input = ancillary_dir+"availability_mala_calving_summer.gpkg", output = "availability_mala_calving_summer")
# late summer - OK
v.in_ogr(input = ancillary_dir+"availability_mala_late_summer.gpkg", output = "availability_mala_late_summer")
# autumn - OK
v.in_ogr(input = ancillary_dir+"availability_mala_autumn.gpkg", output = "availability_mala_autumn")





