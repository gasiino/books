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

ewmac_8 = TradingRule((ewmac, [], dict(Lfast=8, Lslow=32)))
ewmac_32 = TradingRule(dict(function=ewmac, other_args=dict(Lfast=32, Lslow=128)))
my_rules = Rules(dict(ewmac8=ewmac_8, ewmac32=ewmac_32))

my_config = Config()
my_config
my_config.trading_rules = dict(ewmac8=ewmac_8, ewmac32=ewmac_32)

## we can estimate these ourselves

#my_config.instruments=[ "US20", "NASDAQ", "SP500"]
my_config.instruments=[ "SP500"]
#my_config.forecast_weight_estimate=dict(method="one_period")
my_config.forecast_weight_estimate=dict(method="bootstrap")
my_config.forecast_weight_estimate['monte_runs']=50
my_config.use_forecast_weight_estimates=True
my_account = Account()
combiner = ForecastCombine()
fcs=ForecastScaleCap()
my_system = System([my_account, fcs, my_rules, combiner], data, my_config)

print(my_system.combForecast.get_forecast_weights("SP500").tail(5))
print('forecast_diversification_multiplier')
print(my_system.combForecast.get_forecast_diversification_multiplier("SP500").tail(5))

