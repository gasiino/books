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
from systems.portfolio import PortfoliosEstimated
from systems.positionsizing import PositionSizing

data = csvFuturesData()
my_config = Config()
my_config.instruments=["US20", "SP500"]

ewmac_8 = TradingRule((ewmac, [], dict(Lfast=8, Lslow=32)))
ewmac_32 = TradingRule(
    dict(function=ewmac, other_args=dict(Lfast=32, Lslow=128)))
my_rules = Rules(dict(ewmac8=ewmac_8, ewmac32=ewmac_32))

my_system = System([Account(), PortfoliosEstimated(), PositionSizing(), ForecastScaleCap(), my_rules, ForecastCombine()], data, my_config)
my_system.config.forecast_weight_estimate['method']="equal_weights"
my_system.config.instrument_weight_estimate['method']="bootstrap"
my_system.config.instrument_weight_estimate["monte_runs"]=1
my_system.config.instrument_weight_estimate["bootstrap_length"]=250
print (my_system.portfolio.get_instrument_weights())
print(my_system.portfolio.get_instrument_diversification_multiplier())

# 10,250 weights=0.75,0.25 idm=1.26
# 30,250 weights=0.75,0.25 
