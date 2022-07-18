#---------------------------------------
# Title: Cut maps for a given availability area: Mala sameby
# Author: Bernardo Niebuhr
# 2020-11-20
#---------------------------------------

python

# import modules
import os
import numpy as np
import grass.script as grass
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import raster as r

#---------------------------------------
# Setup

# install extension r.viewshed.cva
# g.extension(extension = "r.viewshed.cva")

# mapset
this_mapset = "availability_mala"
g.mapset(mapset = this_mapset, flags = "c")

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

# vector defining the availability area
availability_vector = "availability_mala_autumn@sam_reindeer_ancillary"

# region
g.region(vector = availability_vector, 
	align = map_to_align, flags = "ap")

# mask
r.mask(vector = availability_vector, overwrite = True)

# external files

# folder with files for map reclassification
landscape_dir = r"D:/bernardo/00_academico/07_projetos/05_reindeer/05_env_data/03_raster/p_sam_landscape/landcover_nmd_ungeneralized/"

#-----------------
# lists of maps, mapset names, new names, and types of maps, for each kind of background data

#---------
# industry
g.mapsets(mapset = "p_sam_industry", operation = "add")
maps_sound = grass.list_grouped(type = "raster", pattern = "sound_model_mala*")["p_sam_industry"]

maps_industry = ["wind_turbines_operation_2020_rast", 
"wind_turbines_Mala_jokk_stor_ytte", "wind_turbines_Mala_amliden",
"wind_turbines_Mala_jokk", "wind_turbines_Mala_stor", "wind_turbines_Mala_ytte",
"power_lines_lm_2020_rast", 
"mining_active_sgu_2020_mala_rast", "mining_Kristinberget_rast"] + maps_sound
mapsets_industry = ["p_sam_industry"]*len(maps_industry)
dist_industry = [1]*9 + [0]*len(maps_sound)
reclass_industry = [0]*len(maps_industry)
names_industry = ["wind_dist", "wind_dist_jsy", "wind_dist_aml", "wind_dist_jokk",
"wind_dist_stor", "wind_dist_ytte", "pl_dist", "mining_dist", 
"mining_dist_Krist"] + maps_sound
specific_study_area_industry = [0]*len(maps_industry)

vector_turbines = "wind_turbines_operation_2020@sam_env"

#---------
# landscape
g.mapsets(mapset = "p_sam_landscape", operation = "add")
maps_tpi = grass.list_grouped(type = "raster", pattern = "tpi*")["p_sam_landscape"]
names_tpi = [i.split("_10m")[0] for i in maps_tpi]

maps_landscape = ["landcover_ungeneralized_nmd1_10m_2018", 
"landcover_smd_25m_clearcuts_2007", "clear_cuts_SKS_2020_rast",
"dem_10m_Sweden_2018", "slope_10m_Sweden_2018", "aspect_10m_Sweden_2018"] + maps_tpi
mapsets_landscape = ["p_sam_landscape"]*len(maps_landscape)
dist_landscape = [0]*len(maps_landscape)
reclass_landscape = [1] + [0]*(len(maps_landscape)-1)
names_landscape = ["landcover_nmd_mod", "landcover_smd_clearcuts2007", "clear_cuts_2020", 
"dem_10m", "dem_slope", "dem_aspect"] + names_tpi
specific_study_area_landscape = [0]*len(maps_landscape)

#---------
# species
g.mapsets(mapset = "p_sam_species", operation = "add")

maps_species = grass.list_grouped(type = "raster", pattern = "*")["p_sam_species"]
mapsets_species = ["p_sam_species"]*len(maps_species)
dist_species = [0]*len(maps_species)
reclass_species = [0]*len(maps_species)
# organize names of maps
names_species = [i.replace("_season", "") for i in maps_species]
names_species = [(i[:7] + i[-2:]) if "wolf" in i else i for i in names_species]
specific_study_area_species = [0]*len(maps_species)

#---------
# tourism
maps_tourism = ["trails_lm_2020_rast"]
mapsets_tourism = ["p_sam_tourism"]*len(maps_tourism)
dist_tourism = [1]*len(maps_tourism)
reclass_tourism = [0]*len(maps_tourism)
names_tourism = ["trail_dist"]
specific_study_area_tourism = [0]*len(maps_tourism)

#---------
# transport_urban
g.mapsets(mapset = "p_sam_transport_urban", operation = "add")
maps_transport_urban = grass.list_grouped(type = "raster", pattern ="*")["p_sam_transport_urban"]#[]
mapsets_transport_urban = ["p_sam_transport_urban"]*len(maps_transport_urban)
dist_transport_urban = [1]*len(maps_transport_urban)
reclass_transport_urban = [0]*len(maps_transport_urban)
names_transport_urban = ["building_dist", "house_dist", "priv_road_dist", "public_road_dist", 
"railway_dist", "urban_dist"]
specific_study_area_transport_urban = [0]*len(maps_transport_urban)

#---------
# merge all kinds of data

# list of maps to be used
maps_to_cut = maps_industry + maps_landscape + maps_species + maps_tourism + maps_transport_urban

# list of mapsets
mapsets_to_cut = mapsets_industry + mapsets_landscape + mapsets_species +\
mapsets_tourism + mapsets_transport_urban

# list of whether or not to calculate distance for the maps
calc_dist = dist_industry + dist_landscape + dist_species + dist_tourism + dist_transport_urban

# list of whether of not to reclass the map
land_use_reclass = reclass_industry + reclass_landscape + reclass_species +\
reclass_tourism + reclass_transport_urban

# list of new simplified map names
new_map_names = names_industry + names_landscape + names_species + names_tourism + names_transport_urban

# maps specific of the study area
specific_study_area = specific_study_area_industry + specific_study_area_landscape +\
specific_study_area_species + specific_study_area_tourism + specific_study_area_transport_urban

#---------
# cut and prepare maps
for i in range(len(maps_to_cut)):
    
    # check calc dist or not to define the output map name
    if calc_dist[i] == 1 or land_use_reclass[i] == 1 or specific_study_area[i] == 1:
        new_name = "temp_"+maps_to_cut[i]
    else:
        new_name = new_map_names[i]
    
    # cut the map
    expression = new_name+" = "+maps_to_cut[i]+"@"+mapsets_to_cut[i]
    r.mapcalc(expression, overwrite = True)

#---------
# prepare maps
for i in range(len(maps_to_cut)):
    
    # check calc dist or not to define the output map name
    if calc_dist[i] == 1 or land_use_reclass[i] == 1 or specific_study_area[i] == 1:
        new_name = "temp_"+maps_to_cut[i]
    else:
        new_name = new_map_names[i]
    
    # calculate distance
    if calc_dist[i] == 1:
        # distance map
        r.grow_distance(input = new_name, distance = new_map_names[i], 
            overwrite = True)
    
    # reclassify land use map
    if land_use_reclass[i] == 1:
        # reclassify
        reclassified = new_name.replace("temp", "temp2")
        r.reclass(input = new_name, output = reclassified, 
            rules = landscape_dir+"nmd_classes_eng_rules_reclassify_v2.txt", 
            overwrite = True)
        # replace tundra from NMD (41) by rocks/tundra from SMD (59)
        input_smd = "landcover_smd"
        exp = "map_landcover_aux1 = if("+reclassified+" == 41 && "+input_smd+" == 59, 201, "+reclassified+")"
        r.mapcalc(exp, overwrite = True)
        # replace other open land from NMD (42) by heath from SMD (52)  
        exp2 = "map_landcover_aux2 = if(map_landcover_aux1 == 42 && "+input_smd+" == 52, 202, map_landcover_aux1)"
        r.mapcalc(exp2, overwrite = True)
        # clasify ski slopes (17) and urban areas as anthropogenic
        urban = "urban_lm_2020_rast@p_sam_transport_urban"
        exp3 = "map_landcover_aux3 = if(!isnull("+urban+") || "+input_smd+" == 17, 51, map_landcover_aux2)"
        r.mapcalc(exp3, overwrite = True)
        # merge what remains as 41 and 42 as 41 = other open lands
        exp4 = "map_landcover_aux4 = if(map_landcover_aux3 == 42, 41, map_landcover_aux3)"
        r.mapcalc(exp4, overwrite = True)
        # in other open lands (41), if there is agriculture SMD or JBM, consider as arable lands (2)
        agriculture_jvb = "agriculture_JBV_2015_rast@p_sam_landscape"
        agriculture_smd = "agriculture_SMD_2004_rast@p_sam_landscape"
        exp5 = new_map_names[i]+" = if(map_landcover_aux4 == 41 && \
        (!isnull("+agriculture_jvb+") || !isnull("+agriculture_smd+")), 3, map_landcover_aux4)"
        r.mapcalc(exp5, overwrite = True)
        
        # colors
        r.colors(map = new_map_names[i], 
            raster = maps_to_cut[i]+"@"+mapsets_to_cut[i])
        # classes
        r.category(map = new_map_names[i],
            rules = landscape_dir+"nmd_classes_eng_reclassified.csv", 
            separator = "comma")
        # report
        r.report(map = new_map_names[i], 
            units = ["k", "p"], flags = "n", output = landscape_dir+"report_land_cover_mala_autumn_availability_area.txt",
            overwrite = True) #sort = "desc") 

# remove temp maps
g.remove(type = "raster", pattern = "temp*", flags = "f")
g.remove(type = "raster", pattern = "*aux*", flags = "f")


#---------
# make Viewshed

# select only turbines within the availability area
vector_turbines
output_turb1 = "turbines_within_aux"
v.select(ainput = vector_turbines, binput = availability_vector, 
    output = output_turb1, operator = "intersects", overwrite = True)

# dem map
dem_map = "dem_10m"
# names of wind parks
wind_names = ["jokkmokksliden", "storliden", "ytterberg", "amliden", "hornberget"]
# codes of wind parks
wind_codes = ["2418-V-007", "2418-V-008", "2418-V-004", "2418-V-005", "2418-V-001"]
# turbine height
tubine_height = [150.0, 150.0, 150.0, 145.0, 125.0]
# reindeer height
reindeer_height = 1.1
# output names
wind_out_names = ["viewshed_"+i+"_mala" for i in wind_names]

# viewshed for each of the wind parks
for i in range(len(wind_names)):
    
    # select only one wind farm at a time
    output_turb2 = "turbines_within_"+wind_names[i]
    v.extract(input = output_turb1, where = "Omrades_ID = '"+wind_codes[i]+"'",
        output = output_turb2, overwrite = True)
    
    # cumulative viewshed analysis
    r.viewshed_cva(input = dem_map, vector = output_turb2, output = wind_out_names[i],
        observer_elevation = tubine_height[i], target_elevation = reindeer_height, flags = "b", 
        overwrite = True)
    # it should have worked for creating a binary output, but it did not
    # binary viewshed
    r.mapcalc(wind_out_names[i]+"_binary = if("+wind_out_names[i]+" > 1, 1, 0)", overwrite = True)

# make a general viewshed layer
out_map = "viewshed_mala_binary"
view = [i+"_binary" for i in wind_out_names]
expr = out_map+" = if("+view[0]+" == 1 || "+view[1]+" == 1 || "+view[2]+" == 1 || \
"+view[3]+" == 1 || "+view[4]+" == 1, 1, 0)"
r.mapcalc(expr, overwrite = True)

# remove intermediate files
g.remove(type = "vector", pattern = "turbines_within*", flags = "f")

#---------
# export all maps
export = grass.list_grouped(type = "raster", pattern = "*")[this_mapset]
export.remove("MASK")

# folder to export
out_dir = r"D:\bernardo\00_academico\07_projetos\05_reindeer\06_analysis\06_analysis_Mala\maps"
os.chdir(out_dir)

for i in export:
    
    # region
    g.region(vector = availability_vector, 
        align = map_to_align, flags = "ap")
    
    # export
    r.out_gdal(input = i, output = i+".tif", createopt = "TFW=YES,COMPRESS=DEFLATE",
        overwrite = True)


#---------------------------------------------------------
# exporting "raw" maps

#---------
# industry
g.mapsets(mapset = "p_sam_industry", operation = "add")

maps_industry = ["wind_turbines_operation_2020_rast", 
"power_lines_lm_2020_rast", 
"mining_active_sgu_2020_mala_rast", "mining_Kristinberget_rast"]
mapsets_industry = ["p_sam_industry"]*len(maps_industry)
out_names_industry = ["wind_turbines_raw", "power_lines_raw", "mining_all_raw", "mining_Krist_raw"]

#---------
# tourism
g.mapsets(mapset = "p_sam_tourism", operation = "add")
maps_tourism = ["trails_lm_2020_rast"]
mapsets_tourism = ["p_sam_tourism"]*len(maps_tourism)
out_names_tourism = ["trails_raw"]

#---------
# transport_urban
g.mapsets(mapset = "p_sam_transport_urban", operation = "add")
maps_transport_urban = grass.list_grouped(type = "raster", pattern ="*")["p_sam_transport_urban"]#[]
mapsets_transport_urban = ["p_sam_transport_urban"]*len(maps_transport_urban)
out_names_transport_urban = ["building_raw", "house_raw", "priv_road_raw", "public_road_raw", 
"railway_raw", "urban_raw"]
#---------
# merge all kinds of data

# list of maps to be used
maps_to_cut = maps_industry + maps_tourism + maps_transport_urban

# list of mapsets
mapsets_to_cut = mapsets_industry + mapsets_tourism + mapsets_transport_urban

#---------
# cut and prepare maps
for i in range(len(maps_to_cut)):
    
    # check calc dist or not to define the output map name
    if calc_dist[i] == 1 or land_use_reclass[i] == 1 or specific_study_area[i] == 1:
        new_name = "temp_"+maps_to_cut[i]
    else:
        new_name = new_map_names[i]
    
    # cut the map
    expression = new_name+" = "+maps_to_cut[i]+"@"+mapsets_to_cut[i]
    r.mapcalc(expression, overwrite = True)