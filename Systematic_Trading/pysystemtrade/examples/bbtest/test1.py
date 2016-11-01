import inspect
import sys; sys.path.append('../..')

from systems.provided.example.rules import ewmac_forecast_with_defaults as ewmac
from sysdata.configdata import Config
from systems.account import Account
from systems.forecast_combine import ForecastCombine
from systems.forecast_scale_cap import ForecastScaleCap
from systems.basesystem import System
from sysdata.csvdata import csvFuturesData
from systems.forecasting import Rules
from systems.forecasting import TradingRule

data = csvFuturesData()
my_config = Config()

ewmac_8 = TradingRule((ewmac, [], dict(Lfast=8, Lslow=32)))
ewmac_16 = TradingRule(dict(function=ewmac, other_args=dict(Lfast=16, Lslow=64)))
ewmac_32 = TradingRule(dict(function=ewmac, other_args=dict(Lfast=32, Lslow=128)))
my_rules = Rules(dict(ewmac8=ewmac_8, ewmac16=ewmac_16, ewmac32=ewmac_32))
my_config.trading_rules = dict(ewmac8=ewmac_8, ewmac16=ewmac_16, ewmac32=ewmac_32)

my_config.instruments=[ "SP500"]
my_config.forecast_weight_estimate=dict(method="bootstrap")
my_config.forecast_weight_estimate['monte_runs']=50
my_config.use_forecast_weight_estimates=True
my_system = System([Account(), ForecastScaleCap(), my_rules, ForecastCombine()], data, my_config)
print(my_system.combForecast.get_forecast_weights("SP500").tail(5))

#              ewmac16   ewmac32    ewmac8
# 2016-07-04  0.199582  0.545313  0.255106
# 2016-07-05  0.199625  0.545273  0.255103
# 2016-07-06  0.199668  0.545233  0.255100
# 2016-07-07  0.199710  0.545193  0.255097
# 2016-07-08  0.199752  0.545154  0.255094
