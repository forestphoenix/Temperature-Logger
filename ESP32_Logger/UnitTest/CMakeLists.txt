cmake_minimum_required(VERSION 3.0)

project(unittest)

add_executable(unittest main.cpp catch_amalgamated.cpp StateMachine_test.cpp TempLoggerSM_test.cpp ../TempLoggerSM.cpp)

install(TARGETS unittest RUNTIME DESTINATION bin)
