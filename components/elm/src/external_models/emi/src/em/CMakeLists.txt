add_subdirectory(base)
add_subdirectory(betr)
add_subdirectory(fates)
add_subdirectory(ptm)
add_subdirectory(vsfm)
add_subdirectory(stub)
add_subdirectory(alquimia)

set(EMI_LIBRARIES emi_em_base; emi_em_betr; emi_em_fates; emi_em_ptm; emi_em_vsfm; emi_em_stub; emi_em_alquimia; ${EMI_LIBRARIES} PARENT_SCOPE)


