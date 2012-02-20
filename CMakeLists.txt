project (aircraft-datcom Fortran)

# settings
cmake_minimum_required (VERSION 2.6)
set(APPLICATION_NAME ${PROJECT_NAME})
set(APPLICATION_VERSION_MAJOR "0")
set(APPLICATION_VERSION_MINOR "0")
set(APPLICATION_VERSION_PATCH "0")
set(APPLICATION_VERSION "${APPLICATION_VERSION_MAJOR}.${APPLICATION_VERSION_MINOR}.${APPLICATION_VERSION_PATCH}")
set(LIBRARY_SOVERSION "0.0.0")
set(PROJECT_CONTACT_EMAIL james.goppert@gmail.com)
set(PROJECT_CONTACT_VENDOR USAF)

# modules
set(CMAKE_MODULE_PATH
	${CMAKE_SOURCE_DIR}/cmake
	${CMAKE_SOURCE_DIR}/cmake/arkcmake
)

# use common setup
include(CommonSetup)
set(LIBRARY_VERSION ${APPLICATION_VERSION})
include(CPackPackage)

# settings
option(WITH_BUILD_STATIC "Prefer static linking." ON)

# static lib options
if (WITH_BUILD_STATIC)
    # prefer static libs
    if(WIN32)
        set(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    else()
        set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    endif()
endif()

# install data files
#install(DIRECTORY "${PROJECT_SOURCE_DIR}/aircraft" DESTINATION share/${PROJECT_NAME} PATTERN "*.git*" EXCLUDE)

# install required dependencies from external projects
#install(DIRECTORY ${EP_BASE_DIR}/${CMAKE_INSTALL_PREFIX}/ DESTINATION ${CMAKE_INSTALL_PREFIX})

# config file
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)

# build settings
add_subdirectory(src)
