#---------------------------------------
# Title: Build main GRASS GIS location for Swedish background data
# Author: Bernardo Niebuhr
# 2020-10-09
#---------------------------------------

python

# import modules
import os
import subprocess
from pathlib import Path
import grass.script as grass
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import raster as r

# make sure extensions used are installed
#g.extension(extension = "r.vector.ruggedness")

#---------------------------------------
# Setup location

# create locations
# within PERMANENT

prefix = "p_sam_"
mapset_names = ["climate_phenology", "industry", "landscape", "species", "tourism", "transport_urban"]
mapset_names = [prefix + name for name in mapset_names]

for i in mapset_names:
    g.mapset(mapset = i, flags = "c") #-c to create


#---------------------------------------
# Load data

# code folder
code_dir = r"D:\bernardo\00_academico\07_projetos\05_reindeer\05_env_data\00_grassdb\code"
os.chdir(code_dir)

# import functions
from calculate_tpi import calculate_tpi

# root folder
root_dir = r"D:\bernardo\00_academico\07_projetos\05_reindeer\05_env_data\03_raster"
os.chdir(root_dir)

#---------------------------------------
# Load data - landscape data

# mapset
g.mapset(mapset = "p_sam_landscape")

# folder
landscape_dir = r"p_sam_landscape/"

# load vegetation data - NMD - OK
r.in_gdal(input = landscape_dir+"landcover_nmd_ungeneralized/nmd2018bas_ogeneraliserad_v1_0.tif", 
    output = "landcover_ungeneralized_nmd1_10m_2018")

# load auxiliary vegetation data - SMD - OK
r.in_gdal(input = landscape_dir+"landcover_smd_generalized/mosaic/smdb99.tif", 
    output = "landcover_smd_25m_2004")

# load elevation data 50m - OK
# r.in_gdal(input = landscape_dir+"dem_50m/dem50m.tif", output = "dem_lm_50m_2013")

# load elevation related data 10m, resampled from 2m - OK
dem_maps = os.listdir(landscape_dir+"dem_10m/")

for i in dem_maps:
    if "compressed" not in i and i[-4:] == ".tif":
        print(i)
        name = i.replace(".tif", "_2018")
        r.in_gdal(input = landscape_dir+"dem_10m/"+i, output = name)

# load general lichen map for Sweden
r.in_gdal(input = landscape_dir+"lichen_sven/lav_model_south_no_roads_masked.tif", 
    output = "lichen_model_Sweden", overwrite = True)

# load lichen map callibrted for Tassasen
r.in_gdal(input = landscape_dir+"lichen_sven/lichen_model_sven_tassasen_callibrated_mask_roads.tif", 
    output = "lichen_model_tassasen", overwrite = True)

# load lichen map callibrated for Mittadalen
r.in_gdal(input = landscape_dir+"lichen_sven/lichen_model_sven_mittadalen_mask_roads.tif",
    output = "lichen_model_mittadalen")

#---------------------------------------
# Load data - species data

# mapset
g.mapset(mapset = "p_sam_species")

# folder
species_dir = r"raster/p_sam_species/"

# list maps
files = [str(path) for path in Path(species_dir).rglob('*.tif')]

# load predators data - OK
for i in files:
    name = i.split("\\")[-1].replace(".tif", "")
    r.in_gdal(input = i, output = name, overwrite = True)
    grass.run_command("r.import", input = i, output = name, overwrite = True)

#---------------------------------------
# Load data - climatic data

# mapset
g.mapset(mapset = "p_sam_climate_phenology")

# folder
species_dir = r"raster/p_sam_climate_phenology/"

# list maps
files = [str(path) for path in Path(species_dir).rglob('*.nc')]

# load predators data - OK
for i in [files[0]]:
    name = i.split("\\")[-1].replace(".tif", "")
    r.in_gdal(input = i, output = name, flags = "o", overwrite = True)
    #grass.run_command("r.import", input = i, output = name, overwrite = True)


#---------------------------------------
# Load data - industry data

# mapset
g.mapset(mapset = "p_sam_industry")

# folder
industry_dir = r"p_sam_industry/"

#----------------
# Sound models - Tassasen

# folder
sound_tass_dir = industry_dir + r"sound_models/Tassasen/Export_C_Mullberg/"

# list maps
files = [str(path) for path in Path(sound_tass_dir).rglob('*.TXT')]

# The original files have comma as a separator for decimals
# First we replace that with dots. This must be done only once.

# for each file
for i in files:
    # open
    with open(i, 'r+') as f:
        # read the text
        text = f.read()
        # clear the file
        f.seek(0)
        f.truncate()
        # write the text, replacing comma by dot
        f.write(text.replace(',', '.'))

# load

# resolution 50m
res = 50.0

for i in files:
    # name
    name = i.split("\\")[-1].replace(".TXT", "").replace("RRLK00", "sound_model_tassasen_")
    # print
    print(name)
    # region info for the xyz text file
    region = grass.read_command("r.in.xyz", input = i, flags = "sg", separator = ";", 
        skip = 1).replace("\r\n", "").split(" ")
    # remove "n=", "s=", etc
    region = [float(i[2:]) for i in region]
    # the output is in the order n, s, e, w, b, t
    # define region
    n = region[0] + res/2
    s = region[1] - res/2
    e = region[2] + res/2
    w = region[3] - res/2
    # define region
    g.region(n = n, s = s, w = w, e = e, res = res, flags = "p")
    # read data
    r.in_xyz(input = i, output = name, separator = ";", skip = 1, overwrite = True)


#----------------
# Sound models - Mittadalen

# folder
sound_mitt_dir = industry_dir + r"sound_models/Mittadalen/sound_modelling_Glotesvalen/"

# list maps
files = [str(path) for path in Path(sound_mitt_dir).rglob('*.TXT')]

# The original files have comma as a separator for decimals
# First we replace that with dots. This must be done only once.

# for each file
for i in files:
    # open
    with open(i, 'r+') as f:
        # read the text
        text = f.read()
        # clear the file
        f.seek(0)
        f.truncate()
        # write the text, replacing comma by dot
        f.write(text.replace(',', '.'))

# load

# resolution 50m
res = 50.0

for i in files:
    # name
    name = i.split("\\")[-1].replace(".TXT", "").replace("RRLK00", "sound_model_mittadalen_")
    # print
    print(name)
    # region info for the xyz text file
    region = grass.read_command("r.in.xyz", input = i, flags = "sg", separator = ";", 
        skip = 1).replace("\r\n", "").split(" ")
    # remove "n=", "s=", etc
    region = [float(i[2:]) for i in region]
    # the output is in the order n, s, e, w, b, t
    # define region
    n = region[0] + res/2
    s = region[1] - res/2
    e = region[2] + res/2
    w = region[3] - res/2
    # define region
    g.region(n = n, s = s, w = w, e = e, res = res, flags = "p")
    # read data
    r.in_xyz(input = i, output = name, separator = ";", skip = 1, overwrite = True)


#----------------
# Sound models - Mala

# folder
sound_mala_dir = industry_dir + r"sound_models/Mala/Export_B_Mala_201012/"

# list maps
files = [str(path) for path in Path(sound_mala_dir).rglob('*.TXT')]

# The original files have comma as a separator for decimals
# First we replace that with dots. This must be done only once.

# for each file
for i in files:
    # open
    with open(i, 'r+') as f:
        # read the text
        text = f.read()
        # clear the file
        f.seek(0)
        f.truncate()
        # write the text, replacing comma by dot
        f.write(text.replace(',', '.'))

# load

# resolution 50m
res = 50.0

for i in files:
    # name
    name = i.split("\\")[-1].replace(".TXT", "").replace("RRLK00", "sound_model_mala_")
    # print
    print(name)
    # region info for the xyz text file
    region = grass.read_command("r.in.xyz", input = i, flags = "sg", separator = ";", 
        skip = 1).replace("\r\n", "").split(" ")
    # remove "n=", "s=", etc
    region = [float(i[2:]) for i in region]
    # the output is in the order n, s, e, w, b, t
    # define region
    n = region[0] + res/2
    s = region[1] - res/2
    e = region[2] + res/2
    w = region[3] - res/2
    # define region
    g.region(n = n, s = s, w = w, e = e, res = res, flags = "p")
    # read data
    r.in_xyz(input = i, output = name, separator = ";", skip = 1, overwrite = True)


#---------------------------------------
# Process landscape data

# mapset
g.mapset(mapset = "p_sam_landscape")

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

#--------------
# reclassify land use maps

# this is kept here but was done when cutting the maps, since the classification 
# may be different in different contexts


#--------------
# rasterize clear cuts

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

# mapset with vectors
mapset_vector = "sam_env"

# input map
input_map = "clear_cuts_SKS_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "attr", 
    attribute_column = "year", overwrite = True) # ideally we could have the year of operation here

r.null(map = input_map+"_rast", null = 0)

#--------------
# update SMD 2000 with clear cuts on 2007, when the GPS started to be collected
# clear cuts in SMD correspond to class number 54
land_cover_input = "landcover_smd_25m_2004"
clear_cuts = "clear_cuts_SKS_2020_rast"
output_smd_cc = "landcover_smd_25m_clearcuts_2007"

expr = output_smd_cc+" = if(("+clear_cuts+" < 2008 && "+clear_cuts+" > 2000), 54, "+land_cover_input+")"

# region
g.region(raster = land_cover_input, align = map_to_align, flags = "ap")

# consider clear cuts as clear cuts
r.mapcalc(expr, overwrite = True)

# redefine colors
r.colors(map = output_smd_cc, raster = land_cover_input)

#--------------
# calculate aspect, slope and VRM based on DEM 50m
dem_map = "dem_lm_50m_2013"

# region
g.region(raster = map_to_align, res = 50, flags = "ap")
# slope and aspect
slope_map = dem_map.replace("dem", "dem_slope")
aspect_map = dem_map.replace("dem", "dem_aspect")
r.slope_aspect(elevation = dem_map, slope = slope_map, 
    aspect = aspect_map, flags = "e", overwrite = True)
# TPI
radius_m = 510
pixel_size = 10
size = int(2*radius_m/10 + 1)
tpi_map = dem_map.replace("dem", "dem_tpi_s"+str(radius_m)+"m")
calculate_tpi(input = dem_map, output = tpi_map, size = size, flags = "c")

# aspect in 4 direction - NESW
aspect_4_directionsNESW = aspect_map.replace("aspect", "aspect_4_directionsNESW")
expression = aspect_4_directionsNESW+" = eval( \
   compass=(450 - "+aspect_map+" ) % 360, \
     if(compass >=0. && compass < 45., 1) \
   + if(compass >=45. && compass < 135., 2) \
   + if(compass >=135. && compass < 225., 3) \
   + if(compass >=225. && compass < 315., 4) \
   + if(compass >=315., 1))"
r.mapcalc(expression)

# check if worked, and then calculate NE, SE, SW, NW


#--------------
# remove roads from lichen map

# this raster map of roads is calculated below.
# here we use a simple mapcalc approach with all roads with 10m.
# but we should explore the width of the different roads to make a buffer and 
# consider that.

road_map1 = "public_roads_lm_2019_rast@p_sam_transport_urban"
road_map2 = "private_roads_lm_2019_rast@p_sam_transport_urban"

# map to align
map_to_align = "landcover_generalized_nmd1_10m_2018@p_sam_landscape"

# region
g.region(raster = "lichen_model_south_no_roads_masked", align = map_to_align, flags = "ap")

# mapcalc
input_lichen_map = "lichen_model_south_no_roads_masked"
lichen_map_name = "lichen_model_Sweden"
expression = lichen_map_name+" = if(isnull("+road_map1+") && isnull("+road_map2+"), "+input_lichen_map+", null())"
r.mapcalc(expression, overwrite = True)

#---------------------------------------
# Process industry data

# mapset
g.mapset(mapset = "p_sam_industry")

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

# mapset with vectors
mapset_vector = "sam_env"

#--------------
# wind turbines - OK

input_map = "wind_turbines_operation_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

# distance - does it make sense to do it for the whole country?

#--------------
# wind turbines - Mullberg - Tassasen - OK

input_map = "wind_turbines_operation_2020"
wf_code = "2326-V-015"
output_map = "wind_turbines_Mullberg"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = output_map, 
    where = "Omrades_ID = '"+wf_code+"'", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

#--------------
# wind turbines - Mala - OK

# Jokkmokksliden, Storliden, Ytteberget
input_map = "wind_turbines_operation_2020"
wf_code = "('2418-V-007', '2418-V-008', '2418-V-004')"
output_map = "wind_turbines_Mala_jokk_stor_ytte"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = output_map, 
    where = "Omrades_ID in "+wf_code, use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

# Jokkmokksliden
input_map = "wind_turbines_operation_2020"
wf_code = "('2418-V-007')"
output_map = "wind_turbines_Mala_jokk"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = output_map, 
    where = "Omrades_ID in "+wf_code, use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

# Storliden
input_map = "wind_turbines_operation_2020"
wf_code = "('2418-V-008')"
output_map = "wind_turbines_Mala_stor"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = output_map, 
    where = "Omrades_ID in "+wf_code, use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

# Ytteberget
input_map = "wind_turbines_operation_2020"
wf_code = "('2418-V-004')"
output_map = "wind_turbines_Mala_ytte"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = output_map, 
    where = "Omrades_ID in "+wf_code, use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here


# Amliden
input_map = "wind_turbines_operation_2020"
wf_code = "2418-V-005"
output_map = "wind_turbines_Mala_amliden"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = output_map, 
    where = "Omrades_ID = '"+wf_code+"'", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

#--------------
# power lines - OK

input_map = "power_lines_lm_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

#--------------
# mining - OK

input_map = "mining_active_sgu_2020_mala"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

# only Kristinberget
input_map = "mining_Kristinberget"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here


#---------------------------------------
# Process transport_urban data

# mapset
g.mapset(mapset = "p_sam_transport_urban")

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

# mapset with vectors
mapset_vector = "sam_env"

#--------------
# public roads - OK

input_map = "public_roads_lm_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

#--------------
# private roads - OK

input_map = "private_roads_lm_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

#--------------
# railways - OK

input_map = "railways_lm_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here

#--------------
# buildings, houses - OK
urb = ["buildings_lm_2020", "houses_lm_2020"]

for input_map in urb:
    
    print(input_map)
        
    # region
    g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")
    
    # rasterize
    v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
        value = 1, overwrite = True)

#--------------
# urban - OK

input_map = "urban_lm_2020"

# region
g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")

# rasterize
v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
    value = 1, overwrite = True) # ideally we could have the year of operation here


#---------------------------------------
# Process tourism data

# mapset
g.mapset(mapset = "p_sam_tourism")

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

# mapset with vectors
mapset_vector = "sam_env"

#--------------
# trails, snowmobile tracks - OK
tour = ["trails_lm_2020", "snowmobile_tracks_lm_2020"]

for input_map in tour:
    
    print(input_map)
    
    # region
    g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")
    
    # rasterize
    v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
        value = 1, overwrite = True)

#---------------------------------------
# Process landscape data

# mapset
g.mapset(mapset = "p_sam_landscape")

# map to align
map_to_align = "landcover_ungeneralized_nmd1_10m_2018@p_sam_landscape"

# mapset with vectors
mapset_vector = "sam_env"

#--------------
# agriculture from JBV and SMD, snowmobile tracks - OK
land = ["agriculture_JBV_2015", "agriculture_SMD_2004"]

for input_map in land:
    
    print(input_map)
    
    # region
    g.region(vector = input_map+"@"+mapset_vector, align = map_to_align, flags = "ap")
    
    # rasterize
    v.to_rast(input = input_map+"@"+mapset_vector, output = input_map+"_rast", use = "val", 
        value = 1, overwrite = True)

