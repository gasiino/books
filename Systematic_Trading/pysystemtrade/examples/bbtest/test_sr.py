import inspect
"""
Sharpe ratio calculation for one strategy / one instrument. 
"""
import sys; sys.path.append('..')
from syscore.algos import robust_vol_calc
import pandas as pd, numpy as np
from syscore.dateutils import BUSINESS_DAYS_IN_YEAR, ROOT_BDAYS_INYEAR, WEEKS_IN_YEAR, ROOT_WEEKS_IN_YEAR
from syscore.dateutils import MONTHS_IN_YEAR, ROOT_MONTHS_IN_YEAR

DEFAULT_CAPITAL = 1.0
DEFAULT_ANN_RISK_TARGET = 0.16

def sharpe(price, forecast):
    """
    Herein the proof why this position calculation is correct (see chapters
    5-11 of 'systematic trading' book)
    Position = forecast x instrument weight x instrument_div_mult x vol_scalar / 10.0
             = forecast x instrument weight x instrument_div_mult x daily cash vol target / (10.0 x instr value volatility)
             = forecast x instrument weight x instrument_div_mult x daily cash vol target / (10.0 x instr ccy volatility x fx rate)
             = forecast x instrument weight x instrument_div_mult x daily cash vol target / (10.0 x block value x % price volatility x fx rate)
             = forecast x instrument weight x instrument_div_mult x daily cash vol target / (10.0 x underlying price x 0.01 x value of price move x 100 x price change volatility/(underlying price) x fx rate)
             = forecast x instrument weight x instrument_div_mult x daily cash vol target / (10.0 x value of price move x price change volatility x fx rate)
    Making some arbitrary assumptions (one instrument, 100% of capital, daily target DAILY_CAPITAL):
             = forecast x 1.0 x 1.0 x DAILY_CAPITAL / (10.0 x value of price move x price diff volatility x fx rate)
             = forecast x  multiplier / (value of price move x price change volatility x fx rate)
    """        
    base_capital = DEFAULT_CAPITAL
    daily_risk_capital = DEFAULT_CAPITAL * DEFAULT_ANN_RISK_TARGET / ROOT_BDAYS_INYEAR        
    ts_capital=pd.Series(np.ones(len(price)) * DEFAULT_CAPITAL, index=price.index)
    ann_risk = ts_capital * DEFAULT_ANN_RISK_TARGET
    daily_returns_volatility = robust_vol_calc(price.diff())
    multiplier = daily_risk_capital * 1.0 * 1.0 / 10.0
    numerator = forecast *  multiplier
    positions = numerator.ffill() /  daily_returns_volatility.ffill()
    cum_trades = positions.shift(1).ffill()
    price_returns = price.diff()
    instr_ccy_returns = cum_trades.shift(1)*price_returns 
    instr_ccy_returns=instr_ccy_returns.cumsum().ffill().reindex(price.index).diff()
    mean_return = instr_ccy_returns.mean() * BUSINESS_DAYS_IN_YEAR
    vol = instr_ccy_returns.std() * ROOT_BDAYS_INYEAR
    return mean_return / vol

if __name__ == "__main__": 
 
    f = '../sysdata/legacycsv/EDOLLAR_price.csv'
    df = pd.read_csv(f,index_col=0,parse_dates=True)
    fast_ewma = pd.ewma(df.PRICE, span=32)
    slow_ewma = pd.ewma(df.PRICE, span=128)
    raw_ewmac = fast_ewma - slow_ewma
    vol = robust_vol_calc(df['PRICE'].diff())
    forecast = raw_ewmac /  vol 
    print (sharpe(df.PRICE, forecast))
