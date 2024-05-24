#pragma once

class Void
{
public:
    Void() = delete;
};

class Unit
{
public:
    Unit(){}
};

template<typename TAction, typename TOutput, typename TEvent, typename TState>
class StateMachine
{
public:
    class Result
    {
        enum class ResultType
        {
            Action, 
            Output
            
        };
        ResultType type;
        union
        {
            TAction action;
            TOutput output;
        };
        
    public:
        Result(TAction const& newAction) : type(ResultType::Action), action(newAction){}
        Result(TOutput const& newOutput) : type(ResultType::Output), output(newOutput){}
        
        template<typename TRet, typename FOnAction, typename FOnOutput>
        TRet match(FOnAction const& onAction, FOnOutput const& onOutput) const
        {
            switch(type)
            {
                case ResultType::Action:
                    return onAction(action);
                    break;
                case ResultType::Output:
                    return onOutput(output);
                    break;
            }
        }
        
        bool isAction() const
            { return match<bool>([](TAction const&){return true;}, [](TOutput const&){return false;});}
        bool isOutput() const
            { return match<bool>([](TAction const&){return false;}, [](TOutput const&){return true;});}
    };
    
    StateMachine(){}
    
    Result advance(TEvent const& input)
    {
        Result out = advanceState(input, m_currentState);
        return out;
    }
    
    TOutput run(TEvent const& firstInput, TEvent (*runAction)(TAction const&))
    {
        TEvent event = firstInput;
        TOutput output = {};
        
        Result result = advance(event);
        
        result.template match<Unit>(
            [&](TAction const& newAction) -> Unit {
                event = runAction(newAction);
                return Unit();
            },
            [&](TOutput const& newOutput) -> Unit {
                output = newOutput;
                return Unit();
            }
        );
        
        while(result.isAction()){
            result = advance(event);
        
            result.template match<Unit>(
                [&](TAction const& newAction) -> Unit {
                    event = runAction(newAction);
                    return Unit();
                },
                [&](TOutput const& newOutput) -> Unit {
                    output = newOutput;
                    return Unit();
                }
            );
        }
        
        return output;
    }
    
protected:
    virtual Result advanceState(TEvent const& input, TState &state) = 0;
    
private:
    TState m_currentState;
};
