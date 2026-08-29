set(EMI_EM_ALQUIMIA_SOURCES
  ExternalModelAlquimiaMod.F90
)

include_directories(${CMAKE_BINARY_DIR}/elm_stub/shr)
include_directories(${CMAKE_BINARY_DIR}/elm_stub/utils)
include_directories(${CMAKE_BINARY_DIR}/elm_stub/elm_types_stub)
include_directories(${CMAKE_BINARY_DIR}/constants)
include_directories(${CMAKE_BINARY_DIR}/emi_data_types)
include_directories(${CMAKE_BINARY_DIR}/emi_data_definition)
include_directories(${CMAKE_BINARY_DIR}/em/base)

# Find PETSC stuff
# Not necessary for Crunch?
find_package(PETSc)
if (NOT PETSC_FOUND)
  message(FATAL_ERROR "PETSc was not found.")
endif()
include_directories(${PETSC_INCLUDES})

# Include PETSc in the rpath.
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_RPATH}:${PETSC_DIR}/${PETSC_ARCH}/lib")

include_directories(${ALQUIMIA_DIR})
link_libraries(${ALQUIMIA_DIR}/libalquimia.so ${PETSC_LIBRARIES}) 
#link_libraries(${PETSC_DIR}/${PETSC_ARCH}/lib/libmpifort.dylib)) # This was necessary on Mac
link_libraries($ENV{PFLOTRAN_DIR}/libpflotranchem.a)


include(add_emi_library)
add_emi_library(emi_em_alquimia ${EMI_EM_ALQUIMIA_SOURCES} )




set(EMI_LIBRARIES emi_em_alquimia;${EMI_LIBRARIES} PARENT_SCOPE)
set(EMI_LIBRARIES emi_em_alquimia;${EMI_LIBRARIES})


if (NOT CMAKE_INSTALL_PREFIX STREQUAL "INSTALL_DISABLED")
  install(TARGETS emi_em_alquimia DESTINATION lib)
  file(GLOB HEADERS *.h)
  install(FILES ${HEADERS} DESTINATION include/)
endif()
