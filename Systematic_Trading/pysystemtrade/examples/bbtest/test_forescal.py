import inspect
import sys; sys.path.append('../..')
import inspect
from copy import copy
from systems.provided.futures_chapter15.estimatedsystem import futures_system

system=futures_system()
#system.config.forecast_scalar_estimate['min_periods']=50
#system.config.forecast_scalar_estimate['pool_instruments']=False
res = system.forecastScaleCap.get_forecast_scalar("US10", "carry") 
print (res.tail())
