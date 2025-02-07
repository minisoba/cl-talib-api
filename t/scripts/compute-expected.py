#
# https://github.com/TA-Lib/ta-lib-python

import talib as ta
from talib import MA_Type
import pandas as pd
import pandas_ta as tap

pd.options.display.max_rows = 999

data_dir = "~/GitHub/cl-talib-api/t/data/"
df = pd.read_csv(base_dir + "stock-prices.csv")

# accbands (API is not available in talib, and use pandas_ta)
tap.accbands(df.high, df.low, df.close, length=10, mamode="sma").dropna().to_csv(data_dir + "accbands.csv", header=False, index=False)

# bbands
pd.DataFrame(ta.BBANDS(df.close, timeperiod=5, nbdevup=2, nbdevdn=2)).transpose().to_csv(data_dir + "bbands.csv", header=False, index=False)

# dema
ta.DEMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "dema.csv", header=False, index=False)

# ema
ta.EMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "ema.csv", header=False, index=False)

# kama
ta.KAMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "kama.csv", header=False, index=False)

# ma
ta.MA(df.close, timeperiod=10).dropna().to_csv(data_dir + "ma.csv", header=False, index=False)

# mama
pd.DataFrame(ta.MAMA(df.close, fastlimit=0.05, slowlimit=0.2)).transpose().dropna().to_csv(data_dir + "mama.csv", header=False, index=False)

# mavp
ta.MAVP(df.close, (df.high - df.low) / df.close, minperiod=2, maxperiod=10).dropna().to_csv(data_dir + "mavp.csv", header=False, index=False)

# midpoint
ta.MIDPOINT(df.close, timeperiod=2).dropna().to_csv(data_dir + "midpoint.csv", header=False, index=False)

# midprice
ta.MIDPRICE(df.high, df.low, timeperiod=2).dropna().to_csv(data_dir + "midprice.csv", header=False, index=False)

# sar
ta.SAR(df.high, df.low, acceleration=0.02, maximum=0.2).dropna().to_csv(data_dir + "sar.csv", header=False, index=False)

# sar-ext
ta.SAREXT(df.high, df.low).dropna().to_csv(data_dir + "sar-ext.csv", header=False, index=False)

# sma
ta.SMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "sma.csv", header=False, index=False)

# t3
ta.T3(df.close, timeperiod=10).dropna().to_csv(data_dir + "t3.csv", header=False, index=False)

# tema
ta.TEMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "tema.csv", header=False, index=False)

# trima
ta.TRIMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "trima.csv", header=False, index=False)

# wma
ta.WMA(df.close, timeperiod=10).dropna().to_csv(data_dir + "wma.csv", header=False, index=False)

# momentum

# adx
ta.ADX(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "adx.csv", header=False, index=False)

# adxr
ta.ADXR(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "adxr.csv", header=False, index=False)

# apo
ta.APO(df.close, fastperiod=12, slowperiod=26, matype=MA_Type.SMA).dropna().to_csv(data_dir + "apo.csv", header=False, index=False)

# aroon
pd.DataFrame(ta.AROON(df.high, df.low, timeperiod=14)).transpose().dropna().to_csv(data_dir + "aroon.csv", header=False, index=False)

# aroonosc
ta.AROONOSC(df.high, df.low, timeperiod=14).dropna().to_csv(data_dir + "aroon-osc.csv", header=False, index=False)

# bop
ta.BOP(df.open, df.high, df.low, df.close).dropna().to_csv(data_dir + "bop.csv", header=False, index=False)

# cci
ta.CCI(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "cci.csv", header=False, index=False)

# cmo
ta.CMO(df.close, timeperiod=14).dropna().to_csv(data_dir + "cmo.csv", header=False, index=False)

# dx
ta.DX(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "dx.csv", header=False, index=False)

# macd
pd.DataFrame(talib.MACD(df.close, fastperiod=12, slowperiod=26, signalperiod=9)).transpose().dropna().to_csv(data_dir + "macd.csv", header=False, index=False)

# macd-ext
pd.DataFrame(talib.MACDEXT(df.close, fastperiod=12, fastmatype=MA_Type.SMA, slowperiod=26, slowmatype=MA_Type.SMA, signalperiod=9, signalmatype=MA_Type.SMA)).transpose().dropna().to_csv(data_dir + "macd-ext.csv", header=False, index=False)

# macd-fix
pd.DataFrame(talib.MACDFIX(df.close, signalperiod=9)).transpose().dropna().to_csv(data_dir + "macd-fix.csv", header=False, index=False)

# mfi
ta.MFI(df.high, df.low, df.close, df.volume, timeperiod=14).dropna().to_csv(data_dir + "mfi.csv", header=False, index=False)

# minus-di
ta.MINUS_DI(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "minus-di.csv", header=False, index=False)

# minus-dm
ta.MINUS_DM(df.high, df.low, timeperiod=14).dropna().to_csv(data_dir + "minus-dm.csv", header=False, index=False)

# mom
ta.MOM(df.close, timeperiod=10).dropna().to_csv(data_dir + "mom.csv", header=False, index=False)

# plus-di
ta.PLUS_DI(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "plus-di.csv", header=False, index=False)

# plus-dm
ta.PLUS_DM(df.high, df.low, timeperiod=14).dropna().to_csv(data_dir + "plus-dm.csv", header=False, index=False)

# ppo
ta.PPO(df.close, fastperiod=12, slowperiod=26, matype=MA_Type.SMA).dropna().to_csv(data_dir + "ppo.csv", header=False, index=False)

# roc
ta.ROC(df.close, timeperiod=10).dropna().to_csv(data_dir + "roc.csv", header=False, index=False)

# rocp
ta.ROCP(df.close, timeperiod=10).dropna().to_csv(data_dir + "rocp.csv", header=False, index=False)

# rocr
ta.ROCR(df.close, timeperiod=10).dropna().to_csv(data_dir + "rocr.csv", header=False, index=False)

# rocr100
ta.ROCR100(df.close, timeperiod=10).dropna().to_csv(data_dir + "rocr100.csv", header=False, index=False)

# rsi
ta.RSI(df.close, timeperiod=14).dropna().to_csv(data_dir + "rsi.csv", header=False, index=False)

# stoch
pd.DataFrame(talib.STOCH(df.high, df.low, df.close, fastk_period=5, slowk_period=3, slowk_matype=MA_Type.SMA, slowd_period=3, slowd_matype=MA_Type.SMA)).transpose().dropna().to_csv(data_dir + "stoch.csv", header=False, index=False)

# stochf
pd.DataFrame(talib.STOCHF(df.high, df.low, df.close, fastk_period=5, fastd_period=3, fastd_matype=MA_Type.SMA)).transpose().dropna().to_csv(data_dir + "stochf.csv", header=False, index=False)

# stochrsi
pd.DataFrame(talib.STOCHRSI(df.close, timeperiod=14, fastk_period=5, fastd_period=3, fastd_matype=MA_Type.SMA)).transpose().dropna().to_csv(data_dir + "stoch-rsi.csv", header=False, index=False)

# trix
ta.TRIX(df.close, timeperiod=14).dropna().to_csv(data_dir + "trix.csv", header=False, index=False)

# ultosc
ta.ULTOSC(df.high, df.low, df.close, timeperiod1=7, timeperiod2=14, timeperiod3=28).dropna().to_csv(data_dir + "ult-osc.csv", header=False, index=False)

# willr
ta.WILLR(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "willr.csv", header=False, index=False)

# volume

# ad
ta.AD(df.high, df.low, df.close, df.volume).dropna().to_csv(data_dir + "ad.csv", header=False, index=False)

# adosc
ta.ADOSC(df.high, df.low, df.close, df.volume, fastperiod=3, slowperiod=10).dropna().to_csv(data_dir + "ad-osc.csv", header=False, index=False)

# obv
ta.OBV(df.close, df.volume).dropna().to_csv(data_dir + "obv.csv", header=False, index=False)

# volatility

# atr
ta.ATR(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "atr.csv", header=False, index=False)

# natr
ta.NATR(df.high, df.low, df.close, timeperiod=14).dropna().to_csv(data_dir + "natr.csv", header=False, index=False)

# trange
ta.TRANGE(df.high, df.low, df.close).dropna().to_csv(data_dir + "trange.csv", header=False, index=False)
