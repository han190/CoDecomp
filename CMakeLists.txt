cmake_minimum_required(VERSION 3.10)
project(CODECOMP VERSION 0.0.1 LANGUAGES Fortran
  DESCRIPTION "Coarray Decomposition Library"
  HOMEPAGE_URL https://github.com/han190/co-decomp)

# Set default CMAKE_BUILD_TYPE.
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Debug)
  message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# Set number of images
if(NOT NUM_IMAGES)
  include(ProcessorCount)
  ProcessorCount(NUM_IMAGES)
  message(STATUS "Number of images: ${NUM_IMAGES}")
endif()

function(print_compiler compiler compiler_version)
  message(STATUS "Compiler: ${compiler} ${compiler_version}")
endfunction()

# Choose OS, compiler and compiler flags
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2024.1.0)
      message(FATAL_ERROR "ifx < 2024.1.0")
    endif()
    print_compiler(${CMAKE_Fortran_COMPILER} 
      ${CMAKE_Fortran_COMPILER_VERSION})

    if(NOT COARRAY_TYPE)
      set(COARRAY_TYPE shared)
      message(STATUS "Coarray type: ${COARRAY_TYPE}")
    endif()

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
      set(CMAKE_Fortran_FLAGS "/Qcoarray:${COARRAY_TYPE} \
        /Qcoarray-num-images:${NUM_IMAGES} /nologo /QxHost \
        /debug:full /traceback /warn:all /check:all /standard-semantics")
    elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
      set(CMAKE_Fortran_FLAGS "/Qcoarray:${COARRAY_TYPE} \
        /Qcoarray-num-images:${NUM_IMAGES} /nologo /QxHost /O2")
    else()
      message(FATAL_ERROR "Invalid CMAKE_BUILD_TYPE.")
    endif()
    message(STATUS "Profile: ${CMAKE_BUILD_TYPE}")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    message(FATAL_ERROR "Deprecated compiler.")
  endif()
endif()

# Build static library.
file(GLOB_RECURSE sources ${PROJECT_SOURCE_DIR}/src/*.f90)
add_library(codecomp STATIC ${sources})

# Build test
file(GLOB_RECURSE tests ${PROJECT_SOURCE_DIR}/test/*.f90)
add_executable(test_codecomp ${tests})
target_link_libraries(test_codecomp codecomp)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  add_custom_target(test COMMAND test_codecomp)
endif()