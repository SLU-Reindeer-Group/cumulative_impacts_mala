from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import raster as r

def calculate_tpi(input = "", output = "", size = 3, flags = ""):
    
    # name of avg dem map
    avg_dem = "dem_avg_"+str(size)
    # average dem within window of size = size
    r.neighbors(input = input, output = avg_dem, size = size, flags = flags)
    # TPI
    r.mapcalc(expression = output+" = "+input+" - "+avg_dem)
    # remove aux map
    g.remove(type = "raster", name = avg_dem, flags = "f")

