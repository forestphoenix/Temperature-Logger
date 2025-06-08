#pragma once

#include "StateMachine.h"
#include "Common.h"
#include "Constants.h"

struct TempLoggerAction
{
    enum class Action
    {
        Initialize = 0,
        TakeReading,
        TransmitData,
        DeepSleep,

        BlinkOutSingleError, // Blink out error, but contine onwards
        BlinkOutError,       // Blink out error in endless loop
    };

    Action action;

    union
    {
        // TakeReading, DeepSleep: No Data
        struct // TransmitData
        {
            TemperatureReadout readings[MAX_TRANSMITTED_READINGS];
            uint readingsCount;
        };
        struct // BlinkOutSingleError, BlinkOutError
        {
            ErrorCode error;
        };
    };

    static TempLoggerAction dataless(Action action);

    static TempLoggerAction transmitData(TemperatureReadout* readings, unsigned count);

    static TempLoggerAction blinkOutError(ErrorCode error, bool fatal);
};

struct TempLoggerState
{
    enum class State
    {
        Initializing = 0,
        TakingReading,
        WaitingForSleep,
        Transmitting,
        CriticalError,
    };

    State state;

    TemperatureReadout readings[MAX_TRANSMITTED_READINGS];
    uint readingsCount;
};

struct TempLoggerEvent
{
    enum class Event
    {
        FirstStart, 
        Initialized,
        WakeByTimer,
        ReadingTaken,

        ReadingsTransmitted,
        ErrorBlinkedOut,

        ErrorOccurred,
    };

    Event event;
    union
    {
        // Initialized, WakeFromSleep: No Data
        struct // ReadingTaken
        {
            TemperatureReadout latestReading;
        };
        struct // ErrorOccurred
        {
            ErrorCode error;
        };
    };

    static TempLoggerEvent dataless(Event event);

    static TempLoggerEvent readingTaken(TemperatureReadout const& reading);

    static TempLoggerEvent errorOccurred(ErrorCode error);
};

struct TempLoggerConfig
{
    uint readingsToTransmit     = 30;
    float sendOnHumidityDifference = 5.0;
    float sendOnTemperatureDifference = 1.0;
};

class TempLoggerSM : public StateMachine<TempLoggerAction, Void, TempLoggerEvent, TempLoggerState>
{
public:
    TempLoggerSM(TempLoggerConfig const& config);

    virtual Result advanceState(TempLoggerEvent const& input, TempLoggerState& state) override;

private:
    TempLoggerConfig m_config;
};
