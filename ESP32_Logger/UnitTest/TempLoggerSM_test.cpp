#include "catch_amalgamated.hpp"

#include "../TempLoggerSM.h"

// TODO: Init --> RecordTemperatures --> Transmit (OK) --> RecordTemperatures --> Transmit --> etc...
// TODO: Init --> CriticalError
// TODO: Init --> RecordTemperatures --> CriticalError
// TODO: Init --> RecordTemperatures --> Transmit (Fail) --> RecordTemperatures --> Transmit (OK) --> RecordTemperatures...
// TODO: Init --> RecordTemperatures --> Transmit (Fail) --> RecordTemperatures --> Transmit (Fail) --> ... --> CriticalError
