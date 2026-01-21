> [!IMPORTANT]
> Information on using GeoPressureTemplate is now provided in the [GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-intro.html)

# Luscinia luscinia in Sweden GeopressureR

## Description

Migration position estimates from ambient pressure data recorded with multisensor data loggers built in Lund for Thrush nightingale (Luscinia luscinia) breeding in South Sweden, nearby Lund.

LOGGER TYPE: CAnMove multi-sensor loggers (Lund University)

Raw data is provided for the loggers' accelerometer and pressure sensors. Pressure is recoded once every hour. Accelerometry is recorded every 10 minutes, but stored in an hourly format.

Labelled data indicating migratory flights and stopovers is also provided.

With our type of data, GeopressureR uses entire hours of flight.

We considered one hour of flight in GeopressureR if the bird's accelerometer recorded at least 4 out of 6 values with accelerometer values > 4 (scale: 0 to 5) within one hour.

_This repository was generated based on [GeoPressureTemplate (v1.3)](https://github.com/Rafnuss/GeoPressureTemplate)._
