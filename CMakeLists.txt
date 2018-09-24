cmake_minimum_required (VERSION 2.6)
project (ProgramAnalysis)

#Bring the headers
include_directories(include)
file(GLOB SOURCES "src/*.cpp")

add_executable(ProgramAnalysis ${SOURCES} src/Statement.cpp src/Statement.h src/Expression.cpp src/Expression.h src/StatementBlock.cpp src/StatementBlock.h)
