project (aircraft-datcom Fortran)

# settings
cmake_minimum_required (VERSION 2.8)
set(PROJECT_VERSION_MAJOR "0")
set(PROJECT_VERSION_MINOR "0")
set(PROJECT_VERSION_PATCH "0")
set(PROJECT_VERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")
set(PROJECT_CONTACT_EMAIL james.goppert@gmail.com)
set(PROJECT_CONTACT_VENDOR arkTools)
set(LIBRARY_VERSION ${PROJECT_VERSION})
set(LIBRARY_SOVERSION "0.0.0")

# options
option(WITH_BUILD_DEPS "Build dependencies." OFF)
option(WITH_PREFER_STATIC "Build preferring static linking." ON)

# force init zero compile flag
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -finit-local-zero")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -finit-local-zero")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -finit-local-zero")

# variables
set(ROOT_THREAD TRUE CACHE INTERNAL "Is this the top level of the recursion?")

# modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake ${CMAKE_SOURCE_DIR}/cmake/arkcmake)

# spawn new cmake to build deps
if (WITH_BUILD_DEPS AND ROOT_THREAD)
    execute_process(COMMAND ${CMAKE_COMMAND} "${CMAKE_SOURCE_DIR}"
        "-DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}"
        "-DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}"
        "-DEP_BASE_DIR=${EP_BASE_DIR}"
        "-DWITH_BUILD_DEPS=${WITH_BUILD_DEPS}"
        "-DWITH_PREFER_STATIC=${WITH_PREFER_STATIC}"
        "-DROOT_THREAD=FALSE"
        RESULT_VARIABLE ERROR)
    if (ERROR)
        message(FATAL_ERROR "error, recursing loop returned error code: ${ERROR}")
    endif()
    message("** Making dependencies")
    execute_process(COMMAND ${CMAKE_MAKE_PROGRAM} "-j4" "-f${CMAKE_BINARY_DIR}/Makefile")
    message("** Configuring ${PROJECT_NAME}")
endif()

# external projects find path
if(NOT EP_BASE_DIR)
    set(EP_BASE_DIR "${CMAKE_BINARY_DIR}/CMakeExternals")
endif()
list(APPEND CMAKE_FIND_ROOT_PATH ${EP_BASE_DIR})

# prefer static packages if building static library
message("** Finding libraries")
if (WITH_PREFER_STATIC)
    # prefer static libs
    if(WIN32)
        set(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    else()
        set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    endif()
endif()

# find libraries with cmake modules


# static lib options
if (WITH_BUILD_STATIC)
    # prefer static libs
    if(WIN32)
        set(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    else()
        set(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    endif()
endif()

# build dependencies
if (WITH_BUILD_DEPS AND (NOT ROOT_THREAD) )
    message("** Configuring dependencies")

    # add external projects

    set(CMAKE_EP_DEFAULT_ARGS
        -DEP_BASE_DIR=${EP_BASE_DIR}
        -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
        -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
        )

    # terminate non root cmake thread
    return()
endif()

# configure
#check_include_files(expat_config.h HAVE_EXPAT_CONFIG_H)
#check_function_exists(memcopy HAVE_MEMCOPY)
#check_symbol_exists(LC_MESSAGES "locale.h" HAVE_LC_MESSAGES) 
#check_library_exists(arkosg  attachNode "" HAVE_ARKOSG)

# config files
#configure_file(config.h.in config.h)

# subdirectory
add_subdirectory(src)

# packaging
if (WIN32)
    # set NSIS image, nsis bug requuires atleast one file with 4 slashes to install correctly
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}\\\\/cmake/aircraft-datcom-nsis.bmp")
    set(CPACK_NSIS_MUI_ICON "${CMAKE_SOURCE_DIR}/cmake/aircraft-datcom.ico")
    set(CPACK_NSIS_MUI_UNIICON "${CMAKE_SOURCE_DIR}/cmake/aircraft-datcom.ico")
endif()
configure_file("COPYING" "COPYING.txt" COPYONLY) # adding file extensions
configure_file("README" "README.txt" COPYONLY)
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_BINARY_DIR}/COPYING.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_BINARY_DIR}/README.txt")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CPACK_RESOURCE_FILE_README}")
set(CPACK_RESOURCE_FILE_WELCOME "${CMAKE_SOURCE_DIR}/cmake/WELCOME.txt")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "aircraft datcom")
set(CPACK_PACKAGE_VENDOR ${PROJECT_CONTACT_VENDOR})
set(CPACK_PACKAGE_CONTACT "${PROJECT_CONTACT_EMAIL}")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_SET_DESTDIR TRUE)
set(CPACK_COMPONENTS_GROUPING "ALL_COMPONENTS_IN_ONE")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libgfortran3, libgcc1, libc6")
set(CPACK_NSIS_MODIFY_PATH ON)
set(CPACK_PACKAGE_EXECUTABLES
    #"aircraft-datcom" "Command Line"
    )
include(CPack)
# vim:sw=4:ts=4:expandtab
