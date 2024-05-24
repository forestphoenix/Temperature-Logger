#include "TransmitDataSM.h"

TransmitDataAction TransmitDataAction::dataless(TransmitDataAction::Action action)
{
    TransmitDataAction ret;
    ret.action = action;
    return ret;
}

TransmitDataAction TransmitDataAction::transmitData(TemperatureReadout* readings, unsigned int count, uint32_t ip4Address, uint port)
{
    TransmitDataAction ret;
    ret.action = Action::TransmitData;
    ret.ip4Address = ip4Address;
    ret.port = port;
    ret.readingsCount = count;
    copyReadings(readings, ret.readings, count);
    return ret;
}

TransmitDataResult TransmitDataResult::dataless(TransmitDataResult::Result action)
{
    TransmitDataResult ret;
    ret.result = action;
    return ret;
}

TransmitDataResult TransmitDataResult::errorOccurred(ErrorCode error)
{
    TransmitDataResult ret;
    ret.result = Result::ErrorOccurred;
    ret.error = error;
    return ret;
}

TransmitDataEvent TransmitDataEvent::dataless(TransmitDataEvent::Event event)
{
    TransmitDataEvent ret;
    ret.event = event;
    return ret;
}

TransmitDataEvent TransmitDataEvent::startTransmit(const TemperatureReadout* readings, uint readingsCount)
{
    TransmitDataEvent ret;
    ret.event = TransmitDataEvent::Event::StartTransmit;
    ret.readingsCount = readingsCount;
    copyReadings(readings, ret.readings, readingsCount);
    return ret;
}

TransmitDataEvent TransmitDataEvent::serviceFound(uint32_t serviceIp4Address, uint16_t port)
{
    TransmitDataEvent ret;
    ret.event = TransmitDataEvent::Event::ServiceFound;
    ret.serviceIp4Address = serviceIp4Address;
    ret.port = port;
    return ret;
}

TransmitDataEvent TransmitDataEvent::errorOccurred(ErrorCode error)
{
    TransmitDataEvent ret;
    ret.event = TransmitDataEvent::Event::ErrorOccurred;
    ret.error = error;
    return ret;
}

TransmitDataSM::TransmitDataSM() :
    StateMachine<TransmitDataAction, TransmitDataResult, TransmitDataEvent, TransmitDataState>()
{
}
StateMachine<TransmitDataAction, TransmitDataResult, TransmitDataEvent, TransmitDataState>::Result TransmitDataSM::advanceState(const TransmitDataEvent& input, TransmitDataState& state)
{
    switch (input.event)
    {
    case TransmitDataEvent::Event::FirstStart:
        state.serviceIp4Address = 0;
        state.port = 0;
        state.readingsCount = 0;
        state.state = TransmitDataState::State::Inactive;
        return Result(TransmitDataResult::dataless(TransmitDataResult::Result::Initialized));
        break;
        
    case TransmitDataEvent::Event::StartTransmit:
        if (state.state == TransmitDataState::State::Inactive)
        {
            state.readingsCount = input.readingsCount;
            copyReadings(input.readings, state.readings, input.readingsCount);
            
            state.state = TransmitDataState::State::ConnectingWifi;
            return Result(TransmitDataAction::dataless(TransmitDataAction::Action::ConnectWifi));
        }
        break;
        
    case TransmitDataEvent::Event::WifiConnected:
        if(state.state == TransmitDataState::State::ConnectingWifi)
        {
            if(state.serviceIp4Address == 0)
            {
                state.state = TransmitDataState::State::LookingUpService;
                return Result(TransmitDataAction::dataless(TransmitDataAction::Action::LookupService));
            }
            else
            {
                state.state = TransmitDataState::State::TransmittingData;
                return Result(TransmitDataAction::transmitData(state.readings, state.readingsCount, state.serviceIp4Address, state.port));
            }
        }
        break;
        
    case TransmitDataEvent::Event::ServiceFound:
        if(state.state == TransmitDataState::State::LookingUpService)
        {
            state.serviceIp4Address = input.serviceIp4Address;
            state.port = input.port;
            state.state = TransmitDataState::State::TransmittingData;
                return Result(TransmitDataAction::transmitData(state.readings, state.readingsCount, state.serviceIp4Address, state.port));
        }
        break;
        
    case TransmitDataEvent::Event::DataTransmitted:
        state.state = TransmitDataState::State::Inactive;
        return Result(TransmitDataResult::dataless(TransmitDataResult::Result::TransmitOk));
        
    case TransmitDataEvent::Event::ErrorOccurred:
        state.state = TransmitDataState::State::Inactive;
        return Result(TransmitDataResult::errorOccurred(input.error));
        break;
    }

    state.state = TransmitDataState::State::Inactive;
    return Result(TransmitDataResult::errorOccurred(ErrorCode::StateMismatch));
}
