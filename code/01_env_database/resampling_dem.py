#---------------------------------------
# Title: Resample DEM for Sweden from 2m to 10m and calculate derived variables
# Author: Bernardo Niebuhr
# 2020-11-02
#---------------------------------------

python

# import modules
import os, glob
from pathlib import Path
import grass.script as grass
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import vector as v
from grass.pygrass.modules.shortcuts import raster as r

# make sure extensions used are installed
#g.extension(extension = "r.tri")

#---------------------------------------
# Setup

# root
root_dir = r"D:\bernardo\00_academico\07_projetos\05_reindeer\05_env_data\01_reorganize_RenGIS_data"
os.chdir(root_dir)

#---------------------------------------
# Change to a new mapset

mapset_name = "location_dem"
g.mapset(mapset = mapset_name, flags = "c") #-c to create

#---------------------------------------
# Resampling data for Mullberg in Tassasen sameby

dem2m_dir = r"raster\p_sam_landscape\dem_2m"

# Import DEM_2m for Mullberg
r.in_gdal(input = dem2m_dir + "/MullDEM_merge2m.tif", output = "MullDEM_merge2m")

# try resampling to 10m
final_res = 10

g.region(raster = "MullDEM_merge2m", flags = "p")
g.region(res = final_res, flags = "ap")
# from 2m to 10m: weighted resampling -w
r.resamp_stats(input = "MullDEM_merge2m", output = "MullDEM_10m_resamp", flags = "w", 
    overwrite = True)

# export
r.out_gdal(input = "MullDEM_10m_resamp", output = dem2m_dir + "/MullDEM_2m_resamp_10m.tif",
    createopt = "COMPRESS=DEFLATE")

#---------------------------------------
# Resampling DEM for whole Sweden

# Import DEM_2m for Mullberg
r.in_gdal(input = dem2m_dir + "/hojddata2.tif", output = "dem_2m_Sweden_hojddata2")

# Import reindeer husbandry area, with 50km bugffer
mask_dir = r"data/sam_reindeer_ancillary/"

v.in_ogr(input = mask_dir + "sameby_limits_buff_50km.shp", output = "sameby_limits_buff_50km",
    overwrite = True)

# make mask for reindeer area and reindeer area + 50 km buffer
g.region(vector = "sameby_limits_buff_50km", res = 10, flags = "ap")
r.mask(vector = "sameby_limits_buff_50km")

# resample dem from 2m to 10m: weighted resampling -w
r.resamp_stats(input = "dem_2m_Sweden_hojddata2", output = "dem_10m_Sweden_resampled_hojddata2", flags = "w", 
    overwrite = True)

# there are some holes in the data
# load dem 50m to fill these holes
dem50m_dir = r"raster\p_sam_landscape\dem_50m"

r.in_gdal(input = dem50m_dir + "/dem50m.tif", output = "dem50m")

# fill holes
g.region(vector = "sameby_limits_buff_50km", res = 10, flags = "ap")

dem_maps = ("dem_10m_Sweden_resampled_hojddata2", "dem_2m_resamp_10m_Sweden_pt1",
    "dem_2m_resamp_10m_Sweden_pt2", "dem50m")
r.patch(input = dem_maps, output = "dem_10m_Sweden")

# create slope, aspect, and tri from dem

# region
g.region(raster = "dem_10m_Sweden", flags = "ap")

# slope and aspect
r.slope_aspect(elevation = "dem_10m_Sweden", slope = "slope_10m_Sweden", aspect = "aspect_10m_Sweden", 
    flags = "e", overwrite = True)

# tri
# r.tri(input = i, output = i+"tri", size = 3, flags = "c", overwrite = True)

# TPI
dem_map = "dem_10m_Sweden"
radius_m = 150
pixel_size = 10
size = int(2*radius_m/10 + 1)
tpi_map = dem_map.replace("dem", "tpi_s"+str(radius_m)+"m")
calculate_tpi(input = dem_map, output = tpi_map, size = size, flags = "c")

dem_map = "dem_10m_Sweden"
radius_m = 250
pixel_size = 10
size = int(2*radius_m/10 + 1)
tpi_map = dem_map.replace("dem", "tpi_s"+str(radius_m)+"m")
calculate_tpi(input = dem_map, output = tpi_map, size = size, flags = "c")

dem_map = "dem_10m_Sweden"
radius_m = 510
pixel_size = 10
size = int(2*radius_m/10 + 1)
tpi_map = dem_map.replace("dem", "tpi_s"+str(radius_m)+"m")
calculate_tpi(input = dem_map, output = tpi_map, size = size, flags = "c")

# export maps

# folder
outdir = r"D:\bernardo\00_academico\07_projetos\05_reindeer\05_env_data\01_reorganize_RenGIS_data\raster\p_sam_landscape\dem_10m"
os.chdir(outdir)

# list of maps
maps = grass.list_grouped(type = "raster", pattern = "*10m_Sweden")["location_dem"]

# region
g.region(raster = "dem_10m_Sweden", flags = "ap")

# export
for i in maps:
    #export
    r.out_gdal(input = i, output = i+".tif", overwrite = True,
        createopt = "BIGTIFF=YES")
    # export compressed
    r.out_gdal(input = i, output = i+"_compressed.tif", overwrite = True,
        createopt = "COMPRESS=DEFLATE,TFW=YES,BIGTIFF=YES")