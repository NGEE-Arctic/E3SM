set(EMI_DEMO_SOURCES
  demo.F90
  demo_alquimia.F90
)

include_directories(${CMAKE_BINARY_DIR}/elm_stub/shr)
include_directories(${CMAKE_BINARY_DIR}/elm_stub/utils)
include_directories(${CMAKE_BINARY_DIR}/elm_stub/elm_subgrid_types)
include_directories(${CMAKE_BINARY_DIR}/elm_stub/elm_types_stub)
include_directories(${CMAKE_BINARY_DIR}/constants)
include_directories(${CMAKE_BINARY_DIR}/emi_data_types)
include_directories(${CMAKE_BINARY_DIR}/emi_data_definition)
include_directories(${CMAKE_BINARY_DIR}/emi_data_exchange)
include_directories(${CMAKE_BINARY_DIR}/em/base)
include_directories(${CMAKE_BINARY_DIR}/em/betr)
include_directories(${CMAKE_BINARY_DIR}/em/fates)
include_directories(${CMAKE_BINARY_DIR}/em/ptm)
include_directories(${CMAKE_BINARY_DIR}/em/vsfm)
include_directories(${CMAKE_BINARY_DIR}/em/alquimia)
include_directories(${CMAKE_BINARY_DIR}/emi)

link_libraries(${EMI_LIBRARIES})
link_libraries($ENV{PFLOTRAN_DIR}/libpflotranchem.a)

include_directories(${PETSC_INCLUDES})

include(add_emi_executable)
add_emi_executable(demo demo.F90)
add_emi_executable(demo_alquimia demo_alquimia.F90)

if (NOT CMAKE_INSTALL_PREFIX STREQUAL "INSTALL_DISABLED")
  install(TARGETS demo demo_alquimia DESTINATION bin)
  file(GLOB HEADERS *.mod)
  install(FILES ${HEADERS} DESTINATION include/)
endif()
