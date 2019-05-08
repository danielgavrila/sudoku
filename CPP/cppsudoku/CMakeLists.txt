cmake_minimum_required(VERSION 3.14)

project(cppsudoku)
add_executable(${PROJECT_NAME} "main.cpp")

target_include_directories(${PROJECT_NAME} SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} /usr/local/ssd/projects/ThirdParty/range-v3-master/include)

target_compile_options(${PROJECT_NAME} PUBLIC -Wall -O3)
target_compile_features(${PROJECT_NAME}  PUBLIC cxx_std_17 )
target_link_libraries(${PROJECT_NAME}  PUBLIC stdc++)
