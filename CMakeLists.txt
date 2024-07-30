cmake_minimum_required(VERSION 3.10)
project(CODECOMP VERSION 0.0.1 LANGUAGES Fortran
  DESCRIPTION "Coarray Decomposition Library"
  HOMEPAGE_URL https://github.com/han190/CoDecomp)

# Set default CMAKE_BUILD_TYPE.
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Debug)
  message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# Set NUM_IMAGES
if(NOT NUM_IMAGES)
  include(ProcessorCount)
  ProcessorCount(NUM_IMAGES)
  message(STATUS "Number of images: ${NUM_IMAGES}")
endif()

# Set COARRAY_TYPE (it is either "shared" or "distributed")
if(NOT COARRAY_TYPE)
  set(COARRAY_TYPE shared)
  message(STATUS "Coarray type: ${COARRAY_TYPE}")
else()
  if(NOT (${COARRAY_TYPE} STREQUAL "shared" OR
    ${COARRAY_TYPE} STREQUAL "distributed"))
    message(FATAL_ERROR "Invalid coarray type: ${COARRAY_TYPE}")
  endif()
endif()

# Print compiler ID and version
function(print_compiler)
  set(fcid ${CMAKE_Fortran_COMPILER_ID})
  set(fcver ${CMAKE_Fortran_COMPILER_VERSION})
  set(fcpath ${CMAKE_Fortran_COMPILER})
  cmake_path(GET fcpath FILENAME fcname)
  message(STATUS "Compiler: ${fcid} ${fcver} (${fcname})")
endfunction()

# Choose OS, compiler and compiler flags
if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2024.0.0)
      message(FATAL_ERROR "ifx < 2024.0.0")
    endif()
    print_compiler()
    set(IFX_FLAGS /nologo)
    set(COARRAY_FLAGS /Qcoarray:${COARRAY_TYPE} 
      /Qcoarray-num-images:${NUM_IMAGES})
    add_compile_options("${COARRAY_FLAGS}" "${IFX_FLAGS}")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    message(FATAL_ERROR "Compiler ifort is deprecated.")
  endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  find_program(compiler NAMES caf)
  if(NOT compiler)
    message(FATAL_ERROR "OpenCoarrays not found.")
  endif()

  set(compiler_id OpenCoarrays)
  execute_process(COMMAND ${compiler} --version 
    OUTPUT_VARIABLE compiler_info)
  string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" 
    compiler_ver ${compiler_info})
  set(CMAKE_Fortran_COMPILER ${compiler})
  set(CMAKE_Fortran_COMPILER_ID ${compiler_id})
  set(CMAKE_Fortran_COMPILER_VERSION ${compiler_ver})
  print_compiler()
endif()

# Build static library.
file(GLOB_RECURSE sources ${PROJECT_SOURCE_DIR}/src/*.f90)
add_library(codecomp STATIC ${sources})

# Build test
file(GLOB_RECURSE tests ${PROJECT_SOURCE_DIR}/test/*.f90)
add_executable(test_codecomp ${tests})
target_link_libraries(test_codecomp codecomp)