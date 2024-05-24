#pragma once

#include "StateMachine.h"
#include "Common.h"
#include "Constants.h"

struct TransmitDataAction
{
    enum class Action
    {
        ConnectWifi,
        LookupService,
        TransmitData,
    };
    
    Action action;
    
    union
    {
        // ConnectWifi, LookupService: No data (it's in the config)
        struct // TransmitData
        {
            TemperatureReadout readings[MAX_TRANSMITTED_READINGS];
            uint readingsCount;
            
            uint32_t ip4Address;
            uint16_t port;
        };
    };
    
    static TransmitDataAction dataless(Action action);
    
    static TransmitDataAction transmitData(TemperatureReadout* readings, unsigned count, uint32_t ip4Address, uint port);
};

struct TransmitDataResult
{
    enum class Result
    {
        Initialized,
        TransmitOk,
        ErrorOccurred, 
    };
    
    Result result;
    
    union
    {
        // TransmitOk: No Data.
        struct // ErrorOccurred
        {
            ErrorCode error;
        };
    };
    
    static TransmitDataResult dataless(Result action);
    
    static TransmitDataResult errorOccurred(ErrorCode error);
};

struct TransmitDataState
{
    enum class State
    {
        Inactive = 0,
        ConnectingWifi, 
        LookingUpService,
        TransmittingData,
    };
    
    State state = State::Inactive;
    
    // Service Address
    uint32_t serviceIp4Address;
    uint port;
    
    // Readings to transmist
    TemperatureReadout readings[MAX_TRANSMITTED_READINGS];
    uint readingsCount;
};

struct TransmitDataEvent
{
    enum class Event
    {
        FirstStart,
        StartTransmit,
        
        WifiConnected,
        ServiceFound,
        DataTransmitted,
        
        ErrorOccurred,
    };
    
    Event event;
    
    union
    {
        // WifiConnected, DataTransmitted: No Data.
        struct // StartTransmit
        {
            TemperatureReadout readings[MAX_TRANSMITTED_READINGS];
            uint readingsCount;
        };
        struct // ServiceFound
        {
            uint32_t serviceIp4Address;
            uint16_t port;
        };
        struct // ErrorOccurred
        {
            ErrorCode error;
        };
    };
    
    static TransmitDataEvent dataless(Event event);
    
    static TransmitDataEvent startTransmit(TemperatureReadout const* readings, uint readingsCount);
    
    static TransmitDataEvent serviceFound(uint32_t serviceIp4Address, uint16_t port);
    
    static TransmitDataEvent errorOccurred(ErrorCode error);

};

class TransmitDataSM : public StateMachine<TransmitDataAction, TransmitDataResult, TransmitDataEvent, TransmitDataState>
{
public:
    TransmitDataSM();

    virtual Result advanceState(TransmitDataEvent const& input, TransmitDataState& state) override;
};
