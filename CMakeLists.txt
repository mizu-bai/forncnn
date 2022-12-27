cmake_minimum_required(VERSION 3.0)

# try-ncnn
project(forncnn)

# fortran
enable_language(Fortran)
#set(CMAKE_Fortran_FLAGS)

# c++
set(CMAKE_CXX_FLAGS "-lstdc++")

# require openmp
find_package(OpenMP REQUIRED)

if(OPENMP_FOUND)
    message("OPENMP FOUND")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif()


file(GLOB SRC_FILES
    ${PROJECT_SOURCE_DIR}/src/*.f90
    ${PROJECT_SOURCE_DIR}/src/pretty_print.c
)

# include ncnn headers
include_directories(/usr/local/include)

# include ncnn link files
include_directories(/usr/local/lib)

# executable
add_executable(forncnn ${SRC_FILES})

# link
target_link_libraries(forncnn /usr/local/lib/libncnn.dylib)