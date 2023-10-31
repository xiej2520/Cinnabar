cmake_minimum_required(VERSION 3.20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
project(Cinnabar VERSION 1.0 DESCRIPTION "Cinnabar" LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED YES)

include_directories(include)
link_directories(include)

include_directories(include/fmt)

file(GLOB_RECURSE SOURCES "src/*.cpp" main.cpp)

if(MSVC)
    message(STATUS "MSVC detected")
    add_compile_options("/W4" "$<$<CONFIG:RELEASE>:/O2>")
else()
    message(STATUS "Non-MSVC compiler detected")
    # -gdwarf-4 for valgrind?
    add_compile_options("-Wall" "-Wextra" "-Werror" "-Wdouble-promotion"
            # "$<$<CONFIG:Debug>:-gdwarf-4>"
            "$<$<CONFIG:Release>:-O3>")
    if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        message(STATUS "Clang detected")
        set(CMAKE_CXX_FLAGS_DEBUG "-g3 -fsanitize=address,undefined,leak -fsanitize-undefined-trap-on-error")
        add_compile_options("$<$<CONFIG:Debug>:-g3>" "-fdiagnostics-format=clang" "-ftemplate-backtrace-limit=1024" "-ftemplate-backtrace-limit=32")
    endif()
endif()

# see commands in build/compile_commands.json

# comes after add_compile_options
#runs clang-tidy with build
#set(CMAKE_CXX_CLANG_TIDY "clang-tidy-16;-header-filter=.*")
add_executable(cinnabar ${SOURCES})

#find_package(fmt)
#target_link_libraries(cinnabar fmt::fmt)

# Debug build
# cmake -S . -B build/ -D CMAKE_BUILD_TYPE=Debug
# cmake --build build/

# cmake -B build/ -S . -DCMAKE_TOOLCHAIN_FILE=~/vcpkg/scripts/buildsystems/vcpkg.cmake -D CMAKE_BUILD_TYPE=Debug

# Release build
# cmake -S . -B build/ -D CMAKE_BUILD_TYPE=Release
# cmake --build build/
