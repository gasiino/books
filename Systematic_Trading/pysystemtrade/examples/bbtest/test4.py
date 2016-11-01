import inspect
import sys; sys.path.append('../..')
import matplotlib.pyplot as plt

from systems.provided.futures_chapter15.basesystem import futures_system
system=futures_system()
pandl = system.accounts.pandl_for_subsystem("CORN")
print (pandl.stats())
#pandl.cumsum().plot()

#print (system.accounts.portfolio().stats()) ## see some statistics
#system.accounts.portfolio().to_csv("out2.csv")

#system.rules.get_raw_forecast("CORN", "carry").to_csv("out-carry-corn.csv")
#print (system.accounts.pandl_for_instrument_forecast("CORN", "carry").sharpe())
#system.forecastScaleCap.get_capped_forecast("CORN", "carry").to_csv("out-capped-carry-corn.csv")

#plt.show()
