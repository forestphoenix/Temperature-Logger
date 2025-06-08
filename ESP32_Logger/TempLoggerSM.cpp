#include "TempLoggerSM.h"

TempLoggerAction TempLoggerAction::dataless(TempLoggerAction::Action action)
{
    TempLoggerAction ret;
    ret.action = action;
    return ret;
}

TempLoggerAction TempLoggerAction::transmitData(TemperatureReadout* readings, unsigned count)
{
    TempLoggerAction action;
    action.action = Action::TransmitData;
    action.readingsCount = count;
    copyReadings(readings, action.readings, count);
    return action;
}

TempLoggerAction TempLoggerAction::blinkOutError(ErrorCode error, bool fatal)
{
    TempLoggerAction action;
    action.action = fatal ? Action::BlinkOutError : Action::BlinkOutSingleError;
    action.error = error;
    return action;
}

TempLoggerEvent TempLoggerEvent::dataless(TempLoggerEvent::Event event)
{
    TempLoggerEvent ret;
    ret.event = event;
    return ret;
}

TempLoggerEvent TempLoggerEvent::errorOccurred(ErrorCode error)
{
    TempLoggerEvent ret;
    ret.event = Event::ErrorOccurred;
    ret.error = error;
    return ret;
}

TempLoggerEvent TempLoggerEvent::readingTaken(const TemperatureReadout& reading)
{
    TempLoggerEvent ret;
    ret.event = Event::ReadingTaken;
    ret.latestReading = reading;
    return ret;
}

TempLoggerSM::TempLoggerSM(TempLoggerConfig const& config) :
    StateMachine<TempLoggerAction, Void, TempLoggerEvent, TempLoggerState>(),
    m_config(config)
{}

StateMachine<TempLoggerAction, Void, TempLoggerEvent, TempLoggerState>::Result TempLoggerSM::advanceState(const TempLoggerEvent& input, TempLoggerState& state)
{
    switch (input.event)
    {
    case TempLoggerEvent::Event::FirstStart:
        state.state = TempLoggerState::State::Initializing;
        state.readingsCount = 0;
        return Result(TempLoggerAction::dataless(TempLoggerAction::Action::Initialize));
        break;
        
    case TempLoggerEvent::Event::Initialized:
        if (state.state == TempLoggerState::State::Initializing)
        {
            state.state = TempLoggerState::State::TakingReading;
            return Result(TempLoggerAction::dataless(TempLoggerAction::Action::TakeReading));
        }
        break;

    case TempLoggerEvent::Event::WakeByTimer:
        if (state.state == TempLoggerState::State::WaitingForSleep)
        {
            state.state = TempLoggerState::State::TakingReading;
            return Result(TempLoggerAction::dataless(TempLoggerAction::Action::TakeReading));
        }
        break;

    case TempLoggerEvent::Event::ReadingTaken:
        if (state.state == TempLoggerState::State::TakingReading)
        {
            if (state.readingsCount == MAX_TRANSMITTED_READINGS)
            {
                return Result(TempLoggerAction::blinkOutError(ErrorCode::TooManyTransmitsFailed, true));
            }
            else
            {
                state.readings[state.readingsCount] = input.latestReading;
                state.readingsCount++;
                
                bool largeDeviation = false;
                if(state.readingsCount > 2)
                {
                    auto firstReading = state.readings[0];
                    auto latestReading = state.readings[state.readingsCount - 1];
                    
                    largeDeviation = largeDeviation || (latestReading.humidity - firstReading.humidity) > m_config.sendOnHumidityDifference;
                    largeDeviation = largeDeviation || (latestReading.temperature - firstReading.temperature) > m_config.sendOnTemperatureDifference;
                }

                if (largeDeviation || state.readingsCount >= m_config.readingsToTransmit)
                {
                    state.state = TempLoggerState::State::Transmitting;
                    return Result(TempLoggerAction::transmitData(state.readings, state.readingsCount));
                }
                else
                {
                    state.state = TempLoggerState::State::WaitingForSleep;
                    return Result(TempLoggerAction::dataless(TempLoggerAction::Action::DeepSleep));
                }
            }
        }
        break;

    case TempLoggerEvent::Event::ReadingsTransmitted:
        if (state.state == TempLoggerState::State::Transmitting)
        {
            state.readingsCount = 0;
            state.state = TempLoggerState::State::WaitingForSleep;
            return Result(TempLoggerAction::dataless(TempLoggerAction::Action::DeepSleep));
        }

    case TempLoggerEvent::Event::ErrorBlinkedOut:
        if (state.state == TempLoggerState::State::Transmitting)
        {
            state.state = TempLoggerState::State::WaitingForSleep;
            return Result(TempLoggerAction::dataless(TempLoggerAction::Action::DeepSleep));
        }
        else if (state.state == TempLoggerState::State::TakingReading)
        {
            state.state = TempLoggerState::State::TakingReading;
            return Result(TempLoggerAction::dataless(TempLoggerAction::Action::TakeReading));
        }
        break;

    case TempLoggerEvent::Event::ErrorOccurred:
        return Result(TempLoggerAction::blinkOutError(input.error, false));
    }

    return Result(TempLoggerAction::blinkOutError(ErrorCode::StateMismatch, true));
}
