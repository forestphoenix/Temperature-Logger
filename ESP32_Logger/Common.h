#pragma once

#include <ArduinoUniqueID.h>
#include "Constants.h"

struct TemperatureReadout
{
    float temperature;
    float humidity;
    time_t timestamp;
};

inline void copyReadings(const TemperatureReadout* from, TemperatureReadout* to, unsigned int count)
{
    for (int i = 0; i < count; i++)
    {
        to[i] = from[i];
    }
}

enum ErrorCode : uint32_t
{
    NoError = 0,
    
    WifiConnectFailed = 1,
    ServiceLookupFailed = 2,
    TransmitDataFailed = 3,
    
    DhtError = 4,
    TooManyTransmitsFailed = 5,
    StateMismatch = 6,
};
