# Install prefixes
if (WIN32)
    set(CPACK_INSTALL_PREFIX "")
elseif(APPLE)
    set(CPACK_INSTALL_PREFIX "")
else()
    set(CPACK_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")
endif()
set(CPACK_PACKAGE_EXECUTABLES 
    #"JSBSim" "Command Line"
    "JSBSimGui" "Trimming GUI"
    )

if(WIN32)
    add_definitions(-D_WIN32_WINNT=0x0501) # target xp
	set(CPACK_GENERATOR "NSIS")
	set(CPACK_SOURCE_GENERATOR "ZIP")
    # There is a bug in NSI that does not handle full unix paths properly. Make
    # sure there is at least one set of four (4) backlasshes.
    set(CPACK_NSIS_INSTALLED_ICON_NAME "jsbsim")
    set(CPACK_NSIS_DISPLAY_NAME "${PROJECT_NAME}-${APPLICATION_VERSION}")
    set(CPACK_NSIS_HELP_LINK "http:\\\\\\\\github.com/jgoppert/jsbsim")
    set(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\github.com/jgoppert/jsbsim")
    set(CPACK_NSIS_CONTACT ${PROJECT_CONTACT_EMAIL})
    set(CPACK_NSIS_MODIFY_PATH ON)
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
    set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README")
    set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}\\\\/jsbsim.bmp")

elseif(APPLE)
    include(MacroConfigureMacOSXBundlePlist)
    set(CPACK_BUNDLE_ICON "${CMAKE_SOURCE_DIR}/jsbsim.icns")
    set(CPACK_COPYRIGHT "Copyright GPL v3")
    ConfigureMacOSXBundlePlist(${PROJECT_NAME} "" ${CPACK_BUNDLE_ICON} ${APPLICATION_VERSION} ${CPACK_COPYRIGHT})
    set(CPACK_GENERATOR "Bundle")
    set(CPACK_INSTALL_PREFIX "jsbsim.app/Contents/Resources/MacOS")
    set(CPACK_SOURCE_GENERATOR "TGZ")
    set(CPACK_PACKAGE_FILE_NAME "${PROJECT_NAME}")
    set(CPACK_PACKAGE_ICON "${CPACK_BUNDLE_ICON}")
    set(CPACK_BUNDLE_NAME "${PROJECT_NAME}")
    set(MACOSX_BUNDLE_EXECUTABLE_NAME "Resources/MacOS/bin/JSBSimGui")
    # generate plist
    configure_file("${CMAKE_SOURCE_DIR}/cmake/MacOSXBundleInfo.plist.in"
        "${CMAKE_BINARY_DIR}/MacOSXBundleInfo.plist")
    set(CPACK_BUNDLE_PLIST "${CMAKE_BINARY_DIR}/MacOSXBundleInfo.plist")
    set(CPACK_BUNDLE_STARTUP_COMMAND "")
    # mac requires all files to have a file extension set
    configure_file("${CMAKE_SOURCE_DIR}/COPYING" "${CMAKE_BINARY_DIR}/COPYING.txt" COPYONLY)
    configure_file("${CMAKE_SOURCE_DIR}/README" "${CMAKE_BINARY_DIR}/README.txt" COPYONLY)
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_BINARY_DIR}/COPYING.txt")
    set(CPACK_RESOURCE_FILE_README "${CMAKE_BINARY_DIR}/README.txt")
    set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_BINARY_DIR}/README.txt")

elseif(UNIX)
    set(CPACK_GENERATOR "DEB")
    set(CPACK_SOURCE_GENERATOR "ZIP")
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
    set(CPACK_RESOURCE_FILE_README "${CMAKE_SOURCE_DIR}/README")
    set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}\\\\/jsbsim.bmp")
else()
    message(FATAL_ERROR "unknown operating system")
endif()
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "jsbsim simplex trimming branch")
set(CPACK_PACKAGE_VENDOR ${PROJECT_VENDOR})
set(CPACK_PACKAGE_CONTACT ${PROJECT_CONTACT_EMAIL})
set(CPACK_PACKAGE_VERSION_MAJOR "${APPLICATION_VERSION_MAJOR}") 
set(CPACK_PACKAGE_VERSION_MINOR "${APPLICATION_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${APPLICATION_VERSION_PATCH}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${PROJECT_NAME}-${APPLICATION_VERSION}")
set(CPACK_SET_DESTDIR TRUE)
set(CPACK_SOURCE_IGNORE_FILES ${CPACK_SOURCE_IGNORE_FILES}
	/.git/;/build/;~$;.*\\\\.bin$;.*\\\\.swp$)

include(CPack)

