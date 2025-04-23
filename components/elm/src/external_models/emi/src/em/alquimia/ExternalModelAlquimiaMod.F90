module ExternalModelAlquimiaMod
  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  use abortutils                   , only : endrun
  use shr_kind_mod                 , only : r8 => shr_kind_r8
  use shr_log_mod                  , only : errMsg => shr_log_errMsg
  use decompMod                    , only : bounds_type
  use elm_varctl                   , only : iulog

  use EMI_DataMod                  , only : emi_data_list, emi_data

  use ExternalModelBaseType        , only : em_base_type
  use ExternalModelConstants
  use EMI_Atm2LndType_Constants
  use EMI_CanopyStateType_Constants
  use EMI_ColumnType_Constants
  use EMI_EnergyFluxType_Constants
  use EMI_Filter_Constants
  use EMI_Landunit_Constants
  use EMI_SoilHydrologyType_Constants
  use EMI_SoilStateType_Constants
  use EMI_WaterFluxType_Constants
  use EMI_WaterStateType_Constants
  use EMI_CNCarbonStateType_Constants
  use EMI_CNNitrogenStateType_Constants
  use EMI_CNNitrogenFluxType_Constants
  use EMI_CNCarbonFluxType_Constants
  use EMI_ChemStateType_Constants
  use EMI_ColumnWaterStateType_Constants
  use EMI_ColumnWaterFluxType_Constants
  use EMI_ColumnEnergyStateType_Constants

#ifdef USE_ALQUIMIA_LIB
  use AlquimiaContainers_module,      only : AlquimiaSizes,               &
                                             AlquimiaProblemMetaData,     &
                                             AlquimiaProperties,          &
                                             AlquimiaState,               &
                                             AlquimiaAuxiliaryData,       &
                                             AlquimiaAuxiliaryOutputData, &
                                             AlquimiaEngineStatus,        &
                                             AlquimiaEngineFunctionality, &
                                             AlquimiaGeochemicalCondition
  use AlquimiaContainers_module,      only : kAlquimiaMaxStringLength
  use alquimia_fortran_interface_mod, only : AlquimiaFortranInterface
  use iso_c_binding,                  only : c_ptr
  use c_f_interface_module,           only : c_f_string_ptr, f_c_string_ptr

  use, intrinsic :: iso_c_binding,    only : C_CHAR, c_double, c_int, c_bool, c_f_pointer
#endif

  implicit none

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  type, public, extends(em_base_type) :: em_alquimia_type

    integer :: index_l2e_col_dz
    integer :: index_l2e_col_zi
    
    ! Solve data needed
    integer :: index_l2e_state_watsatc ! Porosity
    integer :: index_l2e_filter_soilc
    integer :: index_l2e_filter_num_soilc
    integer :: index_l2e_state_h2osoi_liqvol
    integer :: index_l2e_state_h2osoi_icevol
    integer :: index_l2e_state_decomp_cpools
    integer :: index_l2e_state_decomp_npools
    integer :: index_l2e_state_temperature_soil
    integer :: index_l2e_soil_pool_decomp_k
    integer :: index_l2e_state_nh4
    integer :: index_l2e_state_no3
    integer :: index_l2e_flux_plantNdemand
    integer :: index_l2e_flux_qflx_adv
    integer :: index_l2e_flux_qflx_lat_aqu_layer
    integer :: index_l2e_state_wtd
    integer :: index_l2e_state_h2osfc
    integer :: index_l2e_state_tide_height
    integer :: index_l2e_state_flood_salinity
    integer :: index_l2e_state_flood_nitrate
    integer :: index_l2e_flux_qflx_drain
    
    ! Solve data returned to land model
    integer :: index_e2l_state_decomp_cpools
    integer :: index_e2l_state_decomp_npools
    integer :: index_e2l_flux_hr
    integer :: index_e2l_flux_hrimm
    integer :: index_e2l_flux_ch4
    integer :: index_e2l_state_nh4
    integer :: index_e2l_state_no3
    integer :: index_e2l_state_n2o
    integer :: index_e2l_state_n2
    integer :: index_e2l_flux_n2o
    integer :: index_e2l_flux_n2
    integer :: index_e2l_state_DOC
    integer :: index_e2l_state_DON
    integer :: index_e2l_state_DIC
    integer :: index_e2l_state_ch4_vr
    integer :: index_e2l_state_acetate_vr

    integer :: index_e2l_state_ph
    integer :: index_e2l_state_salinity
    integer :: index_e2l_state_sulfate
    integer :: index_e2l_state_sulfide
    integer :: index_e2l_state_O2
    integer :: index_e2l_state_Fe2
    integer :: index_e2l_state_FeOxide
    integer :: index_e2l_state_FeS
    integer :: index_e2l_state_carbonate

    integer :: index_e2l_flux_Nimm
    integer :: index_e2l_flux_Nimp
    integer :: index_e2l_flux_Nmin

    integer :: index_e2l_flux_plantNO3uptake
    integer :: index_e2l_flux_plantNH4uptake

    integer :: index_e2l_flux_NO3runoff
    integer :: index_e2l_flux_DONrunoff
    integer :: index_e2l_flux_DICrunoff
    integer :: index_e2l_flux_DOCrunoff

    ! Alquimia state data gets passed back and forth
    integer :: index_e2l_water_density
    integer :: index_l2e_water_density
    integer :: index_e2l_aqueous_pressure
    integer :: index_l2e_aqueous_pressure
    integer :: index_e2l_total_mobile
    integer :: index_l2e_total_mobile
    integer :: index_e2l_free_mobile
    integer :: index_l2e_free_mobile
    integer :: index_e2l_total_immobile
    integer :: index_l2e_total_immobile
    integer :: index_e2l_mineral_volume_fraction
    integer :: index_l2e_mineral_volume_fraction
    integer :: index_e2l_mineral_specific_surface_area
    integer :: index_l2e_mineral_specific_surface_area
    integer :: index_e2l_surface_site_density
    integer :: index_l2e_surface_site_density
    integer :: index_e2l_cation_exchange_capacity
    integer :: index_l2e_cation_exchange_capacity
    integer :: index_e2l_aux_doubles
    integer :: index_l2e_aux_doubles
    integer :: index_e2l_aux_ints
    integer :: index_l2e_aux_ints

    integer :: index_e2l_chem_dt
    
#ifdef USE_ALQUIMIA_LIB
    ! Chemistry engine: Should be one per thread
    type(AlquimiaFortranInterface)    :: chem
    type(AlquimiaEngineStatus)        :: chem_status
    type(c_ptr)                       :: chem_engine
    
    ! Chemistry metadata
    type(AlquimiaSizes)               :: chem_sizes
    type(AlquimiaProblemMetaData)     :: chem_metadata
    
    ! Chemical properties and state
    type(AlquimiaProperties)          :: chem_properties  ! One copy per processor
    type(AlquimiaState)               :: chem_state       ! Contains a list of species in the structure
    type(AlquimiaAuxiliaryData)       :: chem_aux_data
    type(AlquimiaAuxiliaryOutputData) :: chem_aux_output
    
    ! Initial condition. Maybe this can just be created and destroyed in a subroutine?
    type(AlquimiaGeochemicalCondition):: chem_ic
    
#endif

    
    ! Mapping between ELM and alquimia decomp pools
    integer, pointer, dimension(:)    :: carbon_pool_mapping
    integer, pointer, dimension(:)    :: nitrogen_pool_mapping
    integer, pointer, dimension(:)    :: pool_reaction_mapping

    integer                           :: CO2_pool_number,  CH4_pool_number,  acetate_pool_number
    integer                           :: hrimm_pool_number
    integer                           :: NH4_pool_number,  NO3_pool_number,  N2O_pool_number,    N2_pool_number
    integer                           :: Nimm_pool_number, Nmin_pool_number, Nimp_pool_number
    integer                           :: plantNO3uptake_pool_number,     plantNH4uptake_pool_number
    integer                           :: plantNO3demand_pool_number,     plantNH4demand_pool_number
    integer                           :: plantNO3uptake_reaction_number, plantNH4uptake_reaction_number
    integer                           :: Hplus_pool_number,    O2_pool_number,       &
                                         sodium_pool_number,   chloride_pool_number, &
                                         Fe2_pool_number,      FeOH3_pool_number,    &
                                         Goethite_pool_number, FeS_pool_number,      &
                                         pyrite_pool_number,                         &
                                         sulfate_pool_number,  sulfide_pool_number
    logical, pointer, dimension(:)    :: is_dissolved_gas
    real(r8),pointer, dimension(:)    :: Henry_const,          Henry_Tdep,   atmo_mixing_ratio
    real(r8),pointer, dimension(:)    :: DOC_content,          DIC_content,  DON_content,      carbonate_C_content
    ! Also add extra SOM content tracker for pools beyond ELM's litter and SOM? (TODO)
    real(r8),pointer,dimension(:)     :: bc ! Boundary condition (len of chem_sizes%num_primary)
    
   contains
     procedure, public :: Populate_L2E_Init_List  => EMAlquimia_Populate_L2E_Init_List
     procedure, public :: Populate_E2L_Init_List  => EMAlquimia_Populate_E2L_Init_List
     procedure, public :: Populate_L2E_List       => EMAlquimia_Populate_L2E_List
     procedure, public :: Populate_E2L_List       => EMAlquimia_Populate_E2L_List
     procedure, public :: Init                    => EMAlquimia_Init
     procedure, public :: Solve                   => EMAlquimia_Solve
#ifdef USE_ALQUIMIA_LIB
     procedure, private :: Copy_Alquimia_To_ELM
     procedure, private :: Copy_ELM_To_Alquimia
     procedure, private :: map_alquimia_pools
#endif
  end type em_alquimia_type

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !

  real(r8),parameter :: min_dt = 0.1 ! Minimum time step length(s) before crashing model on non-convergence in ReactionStepOperatorSplit
  real(r8),parameter :: minval = 1e-20_r8 ! Minimum value to pass to PFLOTRAN to avoid numerical errors with concentrations of 0
#ifndef USE_ALQUIMIA_LIB
  integer, parameter :: kAlquimiaMaxStringLength = 512
#endif

contains

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Populate_L2E_Init_List(this, l2e_init_list)
    !
    ! !DESCRIPTION:
    ! Initialze an emi_list for exchanging data from land model to external
    ! model during initialization stage
    !
    implicit none
    !
    ! !ARGUMENTS
    class(em_Alquimia_type)             :: this
    class(emi_data_list), intent(inout) :: l2e_init_list
    
    ! !LOCAL VARIABLES:
    class(emi_data), pointer :: data
    integer        , pointer :: em_stages(:)
    integer                  :: number_em_stages
    integer                  :: id
    integer                  :: index

    write(iulog,*) 'L2EInit List:'
    call l2e_init_list%PrintInfo()

  end subroutine EMAlquimia_Populate_L2E_Init_List

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Populate_E2L_Init_List(this, e2l_init_list)
    !
    ! !DESCRIPTION:
    ! Initialze an emi_list for exchanging data from external model to land
    ! model during initialization stage
    !
    implicit none
    !
    ! !ARGUMENTS
    class(em_Alquimia_type)             :: this
    class(emi_data_list), intent(inout) :: e2l_init_list

    write(iulog,*)'E2LInit List:'
    call e2l_init_list%PrintInfo()

  end subroutine EMAlquimia_Populate_E2L_Init_List

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Populate_L2E_List(this, l2e_list)
    !
    ! !DESCRIPTION:
    ! Initialze an emi_list for exchanging data from land model to external
    ! model during time integration stage
    !
    implicit none
    !
    ! !ARGUMENTS
    class(em_Alquimia_type)             :: this
    class(emi_data_list), intent(inout) :: l2e_list
    
    ! !LOCAL VARIABLES:
    class(emi_data), pointer :: data
    integer        , pointer :: em_stages(:)
    integer                  :: number_em_stages
    integer                  :: id
    integer                  :: index

    !-------

    number_em_stages = 1
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_Alquimia_SOLVE_STAGE

    ! Liquid water
    id                                   = L2E_STATE_SOIL_LIQ_VOL_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_h2osoi_liqvol   = index

    id                                   = L2E_STATE_SOIL_ICE_VOL_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_h2osoi_icevol   = index
    
    ! Carbon pools
    id                                   = L2E_STATE_CARBON_POOLS_VERTICALLY_RESOLVED
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_decomp_cpools   = index
    
    ! Nitrogen pools
    id                                   = L2E_STATE_NITROGEN_POOLS_VERTICALLY_RESOLVED
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_decomp_npools   = index

    ! Soil temperature
    id                                             = L2E_STATE_TSOIL_NLEVSOI_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_temperature_soil              = index
    
    ! Decomposition rate constants
    id                                             = L2E_FLUX_SOIL_POOL_DECOMP_K
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_soil_pool_decomp_k              = index
    
    id                                             = L2E_STATE_NH4_VERTICALLY_RESOLVED
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_nh4              = index
    
    id                                             = L2E_STATE_NO3_VERTICALLY_RESOLVED
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_no3              = index

    id                                             = L2E_FLUX_PLANT_NDEMAND_VERTICALLY_RESOLVED
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_flux_plantNdemand               = index


    ! Alquimia data is sent from ELM to Alquimia only at solve stage (not set yet at cold start stage)

    id                                             = L2E_STATE_WATER_DENSITY
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_water_density                   = index

    id                                             = L2E_STATE_AQUEOUS_PRESSURE
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_aqueous_pressure      = index

    id                                             = L2E_STATE_TOTAL_IMMOBILE
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_total_immobile                  = index

    id                                             = L2E_STATE_TOTAL_MOBILE
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_total_mobile                    = index

    id                                             = L2E_STATE_FREE_MOBILE
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_free_mobile      = index

    id                                             = L2E_STATE_MINERAL_VOLUME_FRACTION
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_mineral_volume_fraction         = index

    id                                             = L2E_STATE_MINERAL_SPECIFIC_SURFACE_AREA
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_mineral_specific_surface_area   = index

    id                                             = L2E_STATE_SURFACE_SITE_DENSITY
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_surface_site_density            = index

    id                                             = L2E_STATE_CATION_EXCHANGE_CAPACITY
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_cation_exchange_capacity        = index

    id                                             = L2E_STATE_AUX_DOUBLES
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_aux_doubles                     = index

    id                                             = L2E_STATE_AUX_INTS
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_aux_ints                        = index

    ! Water flow
    id                                   = L2E_FLUX_SOIL_QFLX_ADV_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_flux_qflx_adv      = index

    id                                   = L2E_FLUX_SOIL_QFLX_LAT_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_flux_qflx_lat_aqu_layer      = index

    id                                   = L2E_FLUX_SOIL_QFLX_DRAIN_VR
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_flux_qflx_drain       = index

    id                                   = L2E_STATE_SALINITY_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_flood_salinity  = index

    id                                   = L2E_STATE_TIDE_NITRATE_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_flood_nitrate   = index

    id                                   = L2E_STATE_H2OSFC_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_h2osfc          = index

    id                                   = L2E_STATE_H2OSFC_TIDE_COL
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_tide_height     = index

    id                                   = L2E_STATE_WTD
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_wtd             = index


    ! Needed for both stages
    deallocate(em_stages)
    number_em_stages = 2
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_ALQUIMIA_SOLVE_STAGE
    em_stages(2) = EM_ALQUIMIA_COLDSTART_STAGE

    id                                        = L2E_PARAMETER_WATSATC
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_state_watsatc              = index

    id                                        = L2E_COLUMN_DZ
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_col_dz                     = index

    id                                        = L2E_COLUMN_ZI
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_col_zi                     = index

    id                                        = L2E_FILTER_SOILC
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_filter_soilc               = index

    id                                        = L2E_FILTER_NUM_SOILC
    call l2e_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_l2e_filter_num_soilc           = index


    deallocate(em_stages)
    
    write(iulog,*) 'L2E List:'
    call l2e_list%PrintInfo()

  end subroutine EMAlquimia_Populate_L2E_List

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Populate_E2L_List(this, e2l_list)
    !
    ! !DESCRIPTION:
    ! Initialze an emi_list for exchanging data from external model to land
    ! model during time integration stage
    !
    implicit none
    !
    ! !ARGUMENTS
    class(em_Alquimia_type)             :: this
    class(emi_data_list), intent(inout) :: e2l_list
    
    ! !LOCAL VARIABLES:
    class(emi_data), pointer :: data
    integer        , pointer :: em_stages(:)
    integer                  :: number_em_stages
    integer                  :: id
    integer                  :: index

    ! Updated Carbon pools
    ! May want to change this to rates of change instead?
    number_em_stages = 1
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_ALQUIMIA_SOLVE_STAGE
    
    id                                             = E2L_STATE_CARBON_POOLS_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_decomp_cpools             = index
    
    ! Nitrogen pools
    id                                             = E2L_STATE_NITROGEN_POOLS_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_decomp_npools             = index

    ! Heterotrophic respiration flux
    id                                             = E2L_FLUX_HETEROTROPHIC_RESP
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_hr                         = index

    id                                             = E2L_FLUX_HETEROTROPHIC_RESP_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_hrimm                      = index

    id                                             = E2L_FLUX_METHANE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_ch4                        = index

    id                                             = E2L_STATE_METHANE_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_ch4_vr                    = index

    id                                             = E2L_STATE_SOIL_ACETATE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_acetate_vr                = index
    
    id                                             = E2L_STATE_NH4_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_nh4                       = index
    
    id                                             = E2L_STATE_NO3_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_no3                       = index

    id                                             = E2L_STATE_N2O_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_n2o                       = index

    id                                             = E2L_STATE_N2_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_n2                        = index

    id                                             = E2L_FLUX_N2O
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_n2o                        = index

    id                                             = E2L_FLUX_N2
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_n2                         = index

    id                                             = E2L_STATE_DOC_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_DOC                       = index

    id                                             = E2L_STATE_DON_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_DON                       = index

    id                                             = E2L_STATE_DIC_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_DIC                       = index

    id                                             = E2L_FLUX_NIMM_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_Nimm                       = index
    
    id                                             = E2L_FLUX_NIMP_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_Nimp                       = index

    id                                             = E2L_FLUX_NMIN_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_Nmin                       = index

    id                                             = E2L_FLUX_SMIN_NO3_TO_PLANT_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_plantNO3uptake             = index

    id                                             = E2L_FLUX_SMIN_NH4_TO_PLANT_VERTICALLY_RESOLVED
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_plantNH4uptake             = index

    id                                             = E2L_FLUX_NO3_RUNOFF
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_NO3runoff                  = index

    id                                             = E2L_FLUX_DON_RUNOFF
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_DONrunoff                  = index

    id                                             = E2L_FLUX_DIC_RUNOFF
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_DICrunoff                  = index

    id                                             = E2L_FLUX_DOC_RUNOFF
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_flux_DOCrunoff                  = index

    id                                             = E2L_STATE_SOIL_PH
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_ph                        = index

    id                                             = E2L_STATE_SOIL_SALINITY
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_salinity                  = index

    id                                             = E2L_STATE_SOIL_O2
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_O2                        = index

    id                                             = E2L_STATE_SOIL_SULFATE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_sulfate                   = index

    id                                             = E2L_STATE_SOIL_SULFIDE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_sulfide                   = index

    id                                             = E2L_STATE_SOIL_FE2
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_Fe2              = index

    id                                             = E2L_STATE_SOIL_FE_OXIDE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_FeOxide                   = index

    id                                             = E2L_STATE_SOIL_FE_SULFIDE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_FeS              = index

    id                                             = E2L_STATE_SOIL_CARBONATE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_state_carbonate                 = index

    id                                             = E2L_STATE_CHEM_DT
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_chem_dt                         = index

    ! These need to be exchanged in both stages
    deallocate(em_stages)
    number_em_stages = 2
    allocate(em_stages(number_em_stages))
    em_stages(1) = EM_ALQUIMIA_SOLVE_STAGE
    em_stages(2) = EM_ALQUIMIA_COLDSTART_STAGE

    id                                             = E2L_STATE_WATER_DENSITY
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_water_density      = index

    id                                             = E2L_STATE_AQUEOUS_PRESSURE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_aqueous_pressure      = index

    id                                             = E2L_STATE_TOTAL_IMMOBILE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_total_immobile      = index

    id                                             = E2L_STATE_TOTAL_MOBILE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_total_mobile      = index

    id                                             = E2L_STATE_FREE_MOBILE
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_free_mobile      = index

    id                                             = E2L_STATE_MINERAL_VOLUME_FRACTION
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_mineral_volume_fraction      = index

    id                                             = E2L_STATE_MINERAL_SPECIFIC_SURFACE_AREA
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_mineral_specific_surface_area      = index

    id                                             = E2L_STATE_SURFACE_SITE_DENSITY
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_surface_site_density      = index

    id                                             = E2L_STATE_CATION_EXCHANGE_CAPACITY
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_cation_exchange_capacity     = index

    id                                             = E2L_STATE_AUX_DOUBLES
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_aux_doubles      = index

    id                                             = E2L_STATE_AUX_INTS
    call e2l_list%AddDataByID(id, number_em_stages, em_stages, index)
    this%index_e2l_aux_ints      = index

    deallocate(em_stages)

    write(iulog,*) 'E2L List:'
    call e2l_list%PrintInfo()

  end subroutine EMAlquimia_Populate_E2L_List

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Init(this, l2e_init_list, e2l_init_list, iam, bounds_clump)
    !
    ! !DESCRIPTION:
    ! Initialize an emi_list for exchanging data from land model to external
    ! model during time integration stage
    !
#ifdef USE_ALQUIMIA_LIB
    use alquimia_fortran_interface_mod, only : AllocateAlquimiaEngineStatus,        &
                                               AllocateAlquimiaProblemMetaData,     &
                                               AllocateAlquimiaState,               &
                                               AllocateAlquimiaProperties,          &
                                               AllocateAlquimiaAuxiliaryData,       &
                                               AllocateAlquimiaAuxiliaryOutputData, &
                                               AllocateAlquimiaGeochemicalCondition
                                            
    use elm_varctl     , only : use_alquimia, alquimia_pf_coupled
    use elm_varctl     , only : alquimia_inputfile, alquimia_engine_name,                    &
                                alquimia_IC_name,   alquimia_handsoff
    use elm_varpar     , only : alquimia_num_primary,       alquimia_num_minerals,           &
                                alquimia_num_surface_sites, alquimia_num_ion_exchange_sites, &
                                alquimia_num_aux_doubles,   alquimia_num_aux_ints
    use landunit_varcon, only : istcrop,istsoil

    use PFloTranAlquimiaInterface_module, only : PrintSizes,                 &
                                                 PrintProblemMetaData,       &
                                                 ProcessCondition,PrintState

    implicit none
    !
    ! !ARGUMENTS
    class(em_Alquimia_type)              :: this
    class(emi_data_list) , intent(in)    :: l2e_init_list
    class(emi_data_list) , intent(inout) :: e2l_init_list
    integer              , intent(in)    :: iam
    type(bounds_type)    , intent (in)   :: bounds_clump
    
    ! Local variables
    
    ! Should read this from a namelist
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: inputfile
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: engine_name
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: IC_name

    logical(C_BOOL)                                     :: hands_off
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: status_message
    type(AlquimiaEngineFunctionality)                   :: chem_engine_functionality

    !------
    
    write(iulog,*) 'Entering Alquimia setup'
    
    ! a control option
    alquimia_pf_coupled = .true.

    inputfile   = alquimia_inputfile
    engine_name = alquimia_engine_name
    IC_name     = alquimia_IC_name   ! Name of initial condition
    hands_off   = alquimia_handsoff  ! hands_off = .false. allows/requires rate constants, mineral rate const, CEC, complexation site density, and isotherms to be passed through alquimia
    
    ! Make sure these are not defined until explicitly set
    this%carbon_pool_mapping   => NULL()
    this%nitrogen_pool_mapping => NULL()
    this%pool_reaction_mapping => NULL()
    this%is_dissolved_gas      => NULL()
    this%Henry_const           => NULL()
    this%Henry_Tdep            => NULL()
    this%atmo_mixing_ratio     => NULL()
    this%bc                    => NULL()

    ! Allocate memory for status container
    call AllocateAlquimiaEngineStatus(this%chem_status)
    ! Point Alquimia interface to correct subroutines (based on engine that was specified in engine_name)
    call this%chem%CreateInterface(engine_name, this%chem_status)
    
    ! Print out the result of the interface creation call
    call c_f_string_ptr(this%chem_status%message,status_message)
    if(this%chem_status%error /= 0) then
      call endrun(msg='Alquimia error: '//status_message)
    endif
    
    ! Set up the engine and get the storage requirements
    ! Should this only happen on one processor and then broadcast?
    call this%chem%Setup(inputfile,                 &
                         hands_off,                 &
                         this%chem_engine,          &
                         this%chem_sizes,           &
                         chem_engine_functionality, &
                         this%chem_status)

    ! Print out the result of the interface creation call
    call c_f_string_ptr(this%chem_status%message, status_message)
    if(this%chem_status%error /= 0) then
      call endrun(msg='Alquimia error: '//status_message)
    endif
    
    ! Copy array sizes over to clm_varpar
    ! EMI xml system doesn't seem to allow single integers to transferred very easily so we are writing directly to clm_varpar
    alquimia_num_primary            = this%chem_sizes%num_primary
    alquimia_num_minerals           = this%chem_sizes%num_minerals
    alquimia_num_surface_sites      = this%chem_sizes%num_surface_sites
    alquimia_num_ion_exchange_sites = this%chem_sizes%num_ion_exchange_sites
    alquimia_num_aux_doubles        = this%chem_sizes%num_aux_doubles
    alquimia_num_aux_ints           = this%chem_sizes%num_aux_integers
    
    ! Allocate memory for chemistry data
    call AllocateAlquimiaProblemMetaData(this%chem_sizes,    &
                                         this%chem_metadata)
    
    call this%chem%GetProblemMetaData(this%chem_engine,      &
                                      this%chem_metadata,    &
                                      this%chem_status)
    if(this%chem_status%error /= 0) then
      call c_f_string_ptr(this%chem_status%message,status_message)
      call endrun(msg='Alquimia error: '//status_message)
    endif
    
    ! Transfer metadata back to ELM? Does EMI allow character data transfers?
    call printproblemmetadata(this%chem_metadata)
    
    ! Initial condition. The zero length for constraints suggest that it must be read in from input file
    ! In principle the input deck could also include constraints for upper boundary condition and lateral boundary conditions (saltwater, freshwater?)
    ! but that could get tricky if those conditions are not constant over time. Would we have to reprocess the BC every time step?
    ! I think we do need some kind of alquimia condition for boundaries because ELM/MOSART/etc won't necessarily have the same chemicals as the alquimia reaction network
    call AllocateAlquimiaGeochemicalCondition(len_trim(ic_name,C_INT),0,0,this%chem_ic)
    call f_c_string_ptr(ic_name, this%chem_ic%name, len_trim(ic_name)+1)
    
    ! Allocate alquimia's data structures. One copy per processor which will be written into as needed
    call AllocateAlquimiaState              (this%chem_sizes, this%chem_state)
    call AllocateAlquimiaProperties         (this%chem_sizes, this%chem_properties)
    call AllocateAlquimiaAuxiliaryData      (this%chem_sizes, this%chem_aux_data)
    call AllocateAlquimiaAuxiliaryOutputData(this%chem_sizes, this%chem_aux_output)

    allocate(this%bc(this%chem_sizes%num_primary))
    

#else

    use elm_varctl, only : use_alquimia, alquimia_pf_coupled
    use elm_varpar, only : alquimia_num_primary,       alquimia_num_minerals,           &
                           alquimia_num_surface_sites, alquimia_num_ion_exchange_sites, &
                           alquimia_num_aux_doubles,   alquimia_num_aux_ints

    implicit none
    !
    ! !ARGUMENTS
    class(em_Alquimia_type)              :: this
    class(emi_data_list) , intent(in)    :: l2e_init_list
    class(emi_data_list) , intent(inout) :: e2l_init_list
    integer              , intent(in)    :: iam
    type(bounds_type)    , intent (in)   :: bounds_clump

    !-----
    if (use_alquimia) then
      ! test EMI for Alquimia to check data passing but bypassing of alquimia BGC solve
      write(iulog,*)
      write(iulog,*) '--WARNING-- Testing data passing of ELM-EMI-alquimia, WIHTOUT actual alquimia calls '
      !

      alquimia_pf_coupled = .false.

      ! the following is set to 1 so that it won't break any fortran array dimensioned from 1:alquimia_num_*
      alquimia_num_primary            = 1
      alquimia_num_minerals           = 1
      alquimia_num_surface_sites      = 1
      alquimia_num_ion_exchange_sites = 1
      alquimia_num_aux_doubles        = 1
      alquimia_num_aux_ints           = 1

    else
      call endrun(msg='ERROR: Attempting to run with alquimia when model not compiled with USE_ALQUIMIA_LIB')

    endif
#endif

  end subroutine EMAlquimia_Init


  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Solve(this, em_stage, dt, nstep, clump_rank, &
                              l2e_list, e2l_list, bounds_clump)
    !
    ! !DESCRIPTION:
    !
    !
    ! !USES:
    use shr_kind_mod              , only : r8 => shr_kind_r8
    use abortutils                , only : endrun
    use shr_log_mod               , only : errMsg => shr_log_errMsg
    use elm_varctl                , only : iulog
    use ExternalModelConstants    , only : EM_ALQUIMIA_SOLVE_STAGE,EM_ALQUIMIA_COLDSTART_STAGE

    !
    implicit none
    !
    ! !ARGUMENTS:
    class(em_alquimia_type)              :: this
    integer              , intent(in)    :: em_stage
    real(r8)             , intent(in)    :: dt
    integer              , intent(in)    :: nstep
    integer              , intent(in)    :: clump_rank
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    type(bounds_type)    , intent (in)   :: bounds_clump

    !------

    select case(em_stage)

      case (EM_ALQUIMIA_SOLVE_STAGE)
        call EMAlquimia_Solve_BGC(this, dt, nstep, clump_rank, l2e_list, e2l_list, bounds_clump)

      case (EM_ALQUIMIA_COLDSTART_STAGE)
        call EMAlquimia_Coldstart(this, clump_rank, l2e_list, e2l_list, bounds_clump)

      case default
        write(iulog,*) 'EM_Alquimia_Solve: Unknown em_stage.'
        call endrun(msg=errMsg(__FILE__, __LINE__))

    end select

  end subroutine EMAlquimia_Solve


  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Coldstart(this, clump_rank, l2e_list, e2l_list, bounds_clump)
    !
    use elm_varpar, only : nlevdecomp

    class(em_alquimia_type)              :: this
    integer              , intent(in)    :: clump_rank
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    type(bounds_type)    , intent (in)   :: bounds_clump

#ifdef USE_ALQUIMIA_LIB
    ! local variables
    real(r8) , pointer, dimension(:,:)   ::  porosity_l2e, dz, h2o_liqvol
    real(r8) , pointer, dimension(:,:)   ::  water_density_e2l,aqueous_pressure_e2l
    real(r8) , pointer, dimension(:,:,:) ::  total_mobile_e2l, free_mobile_e2l
    real(r8) , pointer, dimension(:,:,:) ::  total_immobile_e2l
    real(r8) , pointer, dimension(:,:,:) ::  mineral_volume_fraction_e2l
    real(r8) , pointer, dimension(:,:,:) ::  mineral_specific_surface_area_e2l
    real(r8) , pointer, dimension(:,:,:) ::  surface_site_density_e2l
    real(r8) , pointer, dimension(:,:,:) ::  cation_exchange_capacity_e2l
    real(r8) , pointer, dimension(:,:,:) ::  aux_doubles_e2l
    integer  , pointer, dimension(:,:,:) ::  aux_ints_e2l
    integer  , pointer                   ::  filter_soilc(:)

    integer                                             :: c, fc, j, num_soilc
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: status_message

    !------------

    call l2e_list%GetPointerToInt1D(this%index_l2e_filter_soilc, filter_soilc)
    call l2e_list%GetIntValue(this%index_l2e_filter_num_soilc,   num_soilc)
    call l2e_list%GetPointerToReal2D(this%index_l2e_col_dz,      dz)

    call l2e_list%GetPointerToReal2D(this%index_l2e_state_watsatc,       porosity_l2e)
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_h2osoi_liqvol, h2o_liqvol) ! m3/m3

    ! Alquimia state data to set on ELM side
    call e2l_list%GetPointerToReal2D(this%index_e2l_water_density,    water_density_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_aqueous_pressure, aqueous_pressure_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_total_mobile,     total_mobile_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_free_mobile,      free_mobile_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_total_immobile,   total_immobile_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_mineral_volume_fraction,       mineral_volume_fraction_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_mineral_specific_surface_area, mineral_specific_surface_area_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_surface_site_density,          surface_site_density_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_cation_exchange_capacity,      cation_exchange_capacity_e2l)
    call e2l_list%GetPointerToReal3D(this%index_e2l_aux_doubles,      aux_doubles_e2l)
    call e2l_list%GetPointerToInt3D(this%index_e2l_aux_ints,          aux_ints_e2l)

    do fc = 1, num_soilc
      c = filter_soilc(fc)

      do j = 1, nlevdecomp

          
          ! Initialize the state for the cell
          this%chem_properties%volume      = dz(c,j)
          this%chem_properties%saturation  = 0.5_r8 ! h2o_liqvol(c,j)/porosity_l2e(c,j)
          this%chem_state%water_density    = 1.0e3_r8
          this%chem_state%porosity         = porosity_l2e(c,j)
          this%chem_state%aqueous_pressure = 101325.0
          this%chem_state%temperature      = 20.0_r8 ! Temperature may not have been initialized yet

          call this%chem%ProcessCondition(this%chem_engine,     &
                                          this%chem_ic,         &
                                          this%chem_properties, &
                                          this%chem_state,      &
                                          this%chem_aux_data,   &
                                          this%chem_status)
          if(this%chem_status%error /= 0) then
            call c_f_string_ptr(this%chem_status%message,status_message)
            call endrun(msg='Alquimia error in ProcessCondition: '//status_message)
          endif

          this%chem_state%porosity = porosity_l2e(c,j)
          ! But this can only happen after ELM allocation step, so this whole thing might need to move somewhere else
          call this%copy_Alquimia_to_ELM( j,                         &
                                          water_density_e2l(c,:),    &
                                          aqueous_pressure_e2l(c,:), &
                                          total_mobile_e2l(c,:,:),   &
                                          free_mobile_e2l(c,:,:),    &
                                          total_immobile_e2l(c,:,:), &
                                          mineral_volume_fraction_e2l(c,:,:),       &
                                          mineral_specific_surface_area_e2l(c,:,:), &
                                          surface_site_density_e2l(c,:,:),          &
                                          cation_exchange_capacity_e2l(c,:,:),      &
                                          aux_doubles_e2l(c,:,:),   &
                                          aux_ints_e2l(c,:,:) )
          
      enddo
    enddo

    ! Save condition to use as surface boundary condition. Units here are converted back to mol/m3 H2O
    ! Note: Boundary condition also needs to be set (or saved/read) when initializing from restart
    this%bc(1:this%chem_sizes%num_primary) = total_mobile_e2l(c,1,1:this%chem_sizes%num_primary)/(porosity_l2e(c,1)*0.5_r8)

#endif

  end subroutine EMAlquimia_Coldstart

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine EMAlquimia_Solve_BGC(this, dt, nstep, clump_rank,  &
                                  l2e_list, e2l_list,           &
                                  bounds_clump)

    
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
    use elm_varctl,             only : use_alquimia, alquimia_pf_coupled
    use elm_varpar,             only : alquimia_num_primary,       alquimia_num_minerals,           &
                                       alquimia_num_surface_sites, alquimia_num_ion_exchange_sites, &
                                       alquimia_num_aux_doubles,   alquimia_num_aux_ints
    use elm_varpar,             only : nlevdecomp, ndecomp_pools
    use landunit_varcon,        only : istcrop, istsoil
    ! use clm_varcon,           only : catomw,natomw ! Replacing these with constants that are the same as PFLOTRAN defs
    use CNDecompCascadeConType, only : decomp_cascade_con

#ifdef USE_ALQUIMIA_LIB
    use AlquimiaContainers_module,        only : AlquimiaEngineStatus
    use alquimia_fortran_interface_mod,   only : ReactionStepOperatorSplit, GetAuxiliaryOutput
    use PFloTranAlquimiaInterface_module, only : printState
#endif
    

    implicit none
    !
    ! !ARGUMENTS
    class(em_alquimia_type)              :: this
    real(r8)             , intent(in)    :: dt ! s
    integer              , intent(in)    :: nstep
    integer              , intent(in)    :: clump_rank
    class(emi_data_list) , intent(in)    :: l2e_list
    class(emi_data_list) , intent(inout) :: e2l_list
    type(bounds_type)    , intent (in)   :: bounds_clump
    
    
    ! Local variables
    integer                              :: c, fc, j, k, poolnum
    integer   , pointer                  :: filter_soilc(:)
    integer                              :: num_soilc
    real(r8) , pointer, dimension(:,:,:) :: decomp_k
    real(r8) , pointer, dimension(:,:)   :: temperature, h2o_liqvol, h2o_icevol
    real(r8) , pointer, dimension(:,:)   :: dz, zi

    !
    integer                              :: max_cuts

    real(r8) , pointer, dimension(:,:)   :: porosity_l2e
    real(r8) , pointer, dimension(:,:)   :: qflx_adv_l2e, qflx_lat_aqu_l2e, qflx_drain_l2e
    real(r8) , pointer, dimension(:)     :: h2osfc_l2e, wtd_l2e, tide_height_l2e
    real(r8) , pointer, dimension(:)     :: flood_salinity_l2e, flood_nitrate_l2e

    real(r8) , pointer, dimension(:,:)   :: water_density_l2e,     water_density_e2l
    real(r8) , pointer, dimension(:,:)   :: aqueous_pressure_l2e,  aqueous_pressure_e2l

    real(r8) , pointer, dimension(:,:,:) :: soilcarbon_l2e,        soilcarbon_e2l
    real(r8) , pointer, dimension(:,:,:) :: soilnitrogen_l2e,      soilnitrogen_e2l
    real(r8) , pointer, dimension(:,:)   :: no3_l2e,               no3_e2l
    real(r8) , pointer, dimension(:,:)   :: nh4_l2e,               nh4_e2l
    real(r8) , pointer, dimension(:,:)   :: plantNdemand_l2e,      plantNO3uptake_e2l, plantNH4uptake_e2l

    real(r8) , pointer, dimension(:)     :: hr_e2l, methaneflux_e2l                           ! 1D total surface emission
    real(r8) , pointer, dimension(:)     :: n2oflux_e2l, n2flux_e2l                           ! 1D total surface emission
    real(r8) , pointer, dimension(:)     :: NO3runoff_e2l, DONrunoff_e2l                      ! 1D total column runoff (gN/m2/s)
    real(r8) , pointer, dimension(:)     :: DICrunoff_e2l, DOCrunoff_e2l                      ! 1D total column runoff (gN/m2/s)

    real(r8) , pointer, dimension(:,:)   :: hrimm_e2l                                         ! 2D emission (col, layer)
    real(r8) , pointer, dimension(:,:)   :: n2o_e2l, n2_e2l
    real(r8) , pointer, dimension(:,:)   :: Nimm_e2l, Nimp_e2l, Nmin_e2l

    real(r8) , pointer, dimension(:,:,:) :: total_mobile_l2e,     total_mobile_e2l
    real(r8) , pointer, dimension(:,:,:) :: free_mobile_l2e,      free_mobile_e2l
    real(r8) , pointer, dimension(:,:,:) :: total_immobile_l2e,   total_immobile_e2l
    real(r8) , pointer, dimension(:,:,:) :: mineral_volume_fraction_l2e,       mineral_volume_fraction_e2l
    real(r8) , pointer, dimension(:,:,:) :: mineral_specific_surface_area_l2e, mineral_specific_surface_area_e2l
    real(r8) , pointer, dimension(:,:,:) :: surface_site_density_l2e,          surface_site_density_e2l
    real(r8) , pointer, dimension(:,:,:) :: cation_exchange_capacity_l2e,      cation_exchange_capacity_e2l
    real(r8) , pointer, dimension(:,:,:) :: aux_doubles_l2e,                   aux_doubles_e2l
    integer  , pointer, dimension(:,:,:) :: aux_ints_l2e,                      aux_ints_e2l

    real(r8) , pointer, dimension(:,:)   :: DOC_e2l, DON_e2l, DIC_e2l, methane_vr_e2l, acetate_vr_e2l
    real(r8) , pointer, dimension(:,:)   :: pH_e2l, O2_e2l, salinity_e2l, sulfate_e2l, sulfide_e2l
    real(r8) , pointer, dimension(:,:)   :: Fe2_e2l, FeOxide_e2l, FeS_e2l, carbonate_e2l

    real(r8) , pointer, dimension(:)     :: actual_dt_e2l

    ! misc.
    real(r8)                             :: CO2_before, molperL_to_molperm3
    real(r8)                             :: DON_before, excess_NO3_uptake, excess_NH4_uptake
    real(r8)                             :: totalC_before, totalN_before, totalC_after, totalN_after
    real(r8)                             :: Nflux, Cflux
    real(r8) , dimension(nlevdecomp)     :: liq_frac

    ! Setting these to the values in PFLOTRAN clm_rspfuncs.F90
    real(r8), parameter                  :: natomw = 14.0067d0 ! Value in clmvarcon is 14.007
    real(r8), parameter                  :: catomw = 12.0110d0 ! Value in clmvarcon is 12.011
    
#ifdef USE_ALQUIMIA_LIB
    real(r8),dimension(this%chem_sizes%num_primary)     :: surf_flux, surf_bc, lat_flux, lat_bc
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: status_message
    procedure(ReactionStepOperatorSplit), pointer       :: engine_ReactionStepOperatorSplit
    procedure(GetAuxiliaryOutput), pointer              :: engine_getAuxiliaryOutput
    ! real (c_double), pointer                           :: alquimia_mobile_data(:), alquimia_immobile_data(:), alquimia_rates_data(:)
#endif

    !--------
    
    ! Pass data from ELM
    
    ! Column filters
    call l2e_list%GetPointerToInt1D(this%index_l2e_filter_soilc, filter_soilc)
    call l2e_list%GetIntValue(this%index_l2e_filter_num_soilc,   num_soilc)

    call l2e_list%GetPointerToReal2D(this%index_l2e_col_dz, dz)
    call l2e_list%GetPointerToReal2D(this%index_l2e_col_dz, zi)
    
    ! C and N pools. Units: gC/m2, gN/m2
    call l2e_list%GetPointerToReal3D(this%index_l2e_state_decomp_cpools , soilcarbon_l2e)
    call l2e_list%GetPointerToReal3D(this%index_l2e_state_decomp_npools , soilnitrogen_l2e)
    
    ! (gN/m3)
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_no3 , no3_l2e)
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_nh4 , nh4_l2e)

    ! Abiotic factors
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_temperature_soil , temperature)       ! K
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_h2osoi_liqvol,     h2o_liqvol)        ! m3/m3
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_h2osoi_icevol,     h2o_icevol)        ! m3/m3
    ! call l2e_list%GetPointerToReal2D(this%index_l2e_state_h2osoi_ice,      h2o_ice)           ! kg/m2

    ! Pool turnover rate constants calculated in ELM, incorporating T and moisture effects (1/s)
    call l2e_list%GetPointerToReal3D(this%index_l2e_soil_pool_decomp_k, decomp_k)

    call l2e_list%GetPointerToReal2D(this%index_l2e_flux_plantNdemand,       plantNdemand_l2e)
    call l2e_list%GetPointerToReal2D(this%index_l2e_flux_qflx_adv,           qflx_adv_l2e)
    call l2e_list%GetPointerToReal2D(this%index_l2e_flux_qflx_lat_aqu_layer, qflx_lat_aqu_l2e) ! ELM units are mm/m2 (integrated over time step)
    call l2e_list%GetPointerToReal2D(this%index_l2e_flux_qflx_drain,         qflx_drain_l2e)

    call l2e_list%GetPointerToReal1D(this%index_l2e_state_wtd,               wtd_l2e)
    call l2e_list%GetPointerToReal1D(this%index_l2e_state_h2osfc,            h2osfc_l2e)

    call l2e_list%GetPointerToReal1D(this%index_l2e_state_flood_salinity ,   flood_salinity_l2e)
    call l2e_list%GetPointerToReal1D(this%index_l2e_state_flood_nitrate ,    flood_nitrate_l2e)
    call l2e_list%GetPointerToReal1D(this%index_l2e_state_tide_height ,      tide_height_l2e)

    ! Alquimia state data btw EMI <--> ELM
    call l2e_list%GetPointerToReal2D(this%index_l2e_state_watsatc,    porosity_l2e)
    call e2l_list%GetPointerToReal2D(this%index_e2l_water_density,    water_density_e2l)
    call l2e_list%GetPointerToReal2D(this%index_l2e_water_density,    water_density_l2e)
    call e2l_list%GetPointerToReal2D(this%index_e2l_aqueous_pressure, aqueous_pressure_e2l)
    call l2e_list%GetPointerToReal2D(this%index_l2e_aqueous_pressure, aqueous_pressure_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_total_mobile,     total_mobile_e2l)          ! Note total mobile is stored as mol/m3 bulk and only converted to mol/L water when passed to/from alquimia
    call l2e_list%GetPointerToReal3D(this%index_l2e_total_mobile,     total_mobile_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_free_mobile,      free_mobile_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_free_mobile,      free_mobile_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_total_immobile,   total_immobile_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_total_immobile,   total_immobile_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_mineral_volume_fraction,       mineral_volume_fraction_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_mineral_volume_fraction,       mineral_volume_fraction_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_mineral_specific_surface_area, mineral_specific_surface_area_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_mineral_specific_surface_area, mineral_specific_surface_area_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_surface_site_density,          surface_site_density_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_surface_site_density,          surface_site_density_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_cation_exchange_capacity,      cation_exchange_capacity_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_cation_exchange_capacity,      cation_exchange_capacity_l2e)
    call e2l_list%GetPointerToReal3D(this%index_e2l_aux_doubles, aux_doubles_e2l)
    call l2e_list%GetPointerToReal3D(this%index_l2e_aux_doubles, aux_doubles_l2e)
    call e2l_list%GetPointerToInt3D(this%index_e2l_aux_ints,     aux_ints_e2l)
    call l2e_list%GetPointerToInt3D(this%index_l2e_aux_ints,     aux_ints_l2e)

    ! C and N pools from EMI --> ELM
    call e2l_list%GetPointerToReal3D(this%index_e2l_state_decomp_cpools , soilcarbon_e2l)   ! gC/m2
    call e2l_list%GetPointerToReal3D(this%index_e2l_state_decomp_npools , soilnitrogen_e2l) ! gN/m2
    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_hr ,        hr_e2l)                ! gC/m2/s (vertically integrated)

    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_ch4 ,       methaneflux_e2l)       ! gC/m2/s
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_ch4_vr ,   methane_vr_e2l)        ! gC/m2/s
    
    call e2l_list%GetPointerToReal2D(this%index_e2l_flux_hrimm,      hrimm_e2l)             ! gC/m3/s (vertically resolved)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_no3 ,      no3_e2l)               ! gN/m3
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_nh4 ,      nh4_e2l)               ! gN/m3
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_n2o ,      n2o_e2l)               ! gN/m3
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_n2 ,       n2_e2l)                ! gN/m3
    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_n2o ,       n2oflux_e2l)           ! gN/m2/s
    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_n2 ,        n2flux_e2l)            ! gN/m2/s

    call e2l_list%GetPointerToReal2D(this%index_e2l_flux_Nimm ,      Nimm_e2l)              ! gN/m3/s
    call e2l_list%GetPointerToReal2D(this%index_e2l_flux_Nimp ,      Nimp_e2l)              ! gN/m3/s
    call e2l_list%GetPointerToReal2D(this%index_e2l_flux_Nmin ,      Nmin_e2l)              ! gN/m3/s

    call e2l_list%GetPointerToReal2D(this%index_e2l_flux_plantNO3uptake,  plantNO3uptake_e2l) ! gN/m3/s
    call e2l_list%GetPointerToReal2D(this%index_e2l_flux_plantNH4uptake,  plantNH4uptake_e2l) ! gN/m3/s

    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_NO3runoff , NO3runoff_e2l)         ! gN/m2/s
    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_DONrunoff , DONrunoff_e2l)         ! gN/m2/s
    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_DOCrunoff , DOCrunoff_e2l)         ! gC/m2/s
    call e2l_list%GetPointerToReal1D(this%index_e2l_flux_DICrunoff , DICrunoff_e2l)         ! gC/m2/s

    call e2l_list%GetPointerToReal2D(this%index_e2l_state_DIC ,      DIC_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_DOC ,      DOC_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_DON ,      DON_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_acetate_vr,acetate_vr_e2l)        ! (mol/L)

    call e2l_list%GetPointerToReal2D(this%index_e2l_state_pH ,       pH_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_salinity , salinity_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_O2 ,       O2_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_sulfate ,  sulfate_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_sulfide ,  sulfide_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_Fe2 ,      Fe2_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_FeOxide ,  FeOxide_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_FeS ,      FeS_e2l)
    call e2l_list%GetPointerToReal2D(this%index_e2l_state_carbonate, carbonate_e2l)

    call e2l_list%GetPointerToReal1D(this%index_e2l_chem_dt,         actual_dt_e2l)

     !----------------------------------------------------------------------------------------------------------------------------------

#ifdef USE_ALQUIMIA_LIB

    ! First check if pools have been mapped between ELM and Alquimia
    if(.not. associated(this%carbon_pool_mapping)) then
      call this%map_alquimia_pools()

      c = filter_soilc(1)

      ! At this point, also make sure boundary condition is set
      ! Initialize the state for the cell
      this%chem_properties%volume     = dz(c,1)
      this%chem_properties%saturation = 0.5_r8            ! or, h2o_liqvol(c,j)/porosity_l2e(c,j) ?
      this%chem_state%water_density   = 1.0e3_r8
      this%chem_state%porosity        = porosity_l2e(c,1)
      this%chem_state%aqueous_pressure= 101325.0
      this%chem_state%temperature     = 20.0_r8           ! Temperature may not have been initialized yet

      call this%chem%ProcessCondition(this%chem_engine,     &
                                      this%chem_ic,         &
                                      this%chem_properties, &
                                      this%chem_state,      &
                                      this%chem_aux_data,   &
                                      this%chem_status)
      if(this%chem_status%error /= 0) then
        call c_f_string_ptr(this%chem_status%message,status_message)
        call endrun(msg='Alquimia error in ProcessCondition: '//status_message)
      endif

      this%chem_state%porosity   = porosity_l2e(c,1)
      if(isnan(porosity_l2e(c,1)) .or. porosity_l2e(c,1)<0.01) then
        write(iulog,*) 'Porosity = ',porosity_l2e(c,1)
        write(iulog,*) 'Boundary condition: ',this%bc(1:this%chem_sizes%num_primary)
        call endrun(msg='Alquimia: Problem with porosity in boundary condition')
      endif

      ! Does this need to loop over all columns?
      do fc = 1, num_soilc
        c = filter_soilc(fc)
        call this%copy_Alquimia_to_ELM(1,  &
                water_density_e2l(c,:),    &
                aqueous_pressure_e2l(c,:), &
                total_mobile_e2l(c,:,:),   &
                free_mobile_e2l(c,:,:),    &
                total_immobile_e2l(c,:,:), &
                mineral_volume_fraction_e2l(c,:,:),       &
                mineral_specific_surface_area_e2l(c,:,:), &
                surface_site_density_e2l(c,:,:),          &
                cation_exchange_capacity_e2l(c,:,:),      &
                aux_doubles_e2l(c,:,:),    &
                aux_ints_e2l(c,:,:)) 
      enddo

      c = filter_soilc(1)
      this%bc(1:this%chem_sizes%num_primary) = total_mobile_e2l(c,1,1:this%chem_sizes%num_primary) &
                                              /(porosity_l2e(c,1)*0.5_r8)

      write(iulog,*) 'Initialized boundary condition: '
      call print_alquimia_state(this)

    endif
    
    ! Run the reactions engine for a step
    do fc = 1, num_soilc
      c = filter_soilc(fc)

      DON_before    = 0.0
      totalN_before = 0.0
      totalC_before = 0.0
      do j = 1, nlevdecomp

             ! Set soil carbon and nitrogen from land model
             ! Convert soil C,N from g/m3 to mol/m3. Assumes pool is defined as immobile, not aqueous
             ! May need to deal with case that pools are all zero (initial condition) which PFLOTRAN will not be able to solve.
             
             do poolnum=1,ndecomp_pools
               if(this%carbon_pool_mapping(poolnum)>0) &  
                 total_immobile_l2e(c,j,this%carbon_pool_mapping(poolnum)) = max(soilcarbon_l2e(c,j,poolnum)/catomw,minval)

               ! Separate N pool only exists if floating CN ratio
               if(decomp_cascade_con%floating_cn_ratio_decomp_pools(poolnum) .and. this%nitrogen_pool_mapping(poolnum)>0) then
                  if(soilnitrogen_l2e(c,j,poolnum)<0) then
                    write(iulog,*) 'N pool less than zero going into alquimia: ',c,j,poolnum,soilnitrogen_l2e(c,j,poolnum)
                  endif
                  total_immobile_l2e(c,j,this%nitrogen_pool_mapping(poolnum)) = &
                    max(soilnitrogen_l2e(c,j,poolnum)/natomw,minval/20._r8)
               endif
               
             enddo
             
             CO2_before = total_immobile_l2e(c,j,this%CO2_pool_number)*catomw + &
                          total_mobile_l2e(c,j,this%CO2_pool_number)*catomw


             ! Copy dissolved nitrogen species. Units need to be converted from gN/m3 to M/L. Currently assuming saturated porosity
             if(this%NO3_pool_number>0 .and. no3_l2e(c,j) < 0.0 ) then
                write(iulog,*) c,j,'NO3 < 0 passed to Alquimia',no3_l2e(c,j)
                call endrun(msg = 'NO3 < 0 passed to Alquimia')
             endif
             if(this%NO3_pool_number>0) total_mobile_l2e(c,j,this%NO3_pool_number) = max(no3_l2e(c,j)/natomw,minval)
             if(this%NH4_pool_number>0) total_mobile_l2e(c,j,this%NH4_pool_number) = max(nh4_l2e(c,j)/natomw,minval)

             ! Set rate constant based on plant N demand. Convert from gN/m3/s to mol/L/s
             ! Also scale rates by relative concentrations of NO3 and NH4 so total uptake doesn't exceed demand
             ! Assumes alquimia is running in hands-off mode: Biomass term of N uptake microbial reaction is set to plant NO3 or NH4 demand
             ! This assumes the rate constant of the reaction is set to 1 in the input deck!
             if(this%plantNH4demand_pool_number>0) then
                ! Limits demand to not be too much higher than N availability to avoid cutting to tiny time step at low available N
                total_immobile_l2e(c,j,this%plantNH4demand_pool_number) = &
                       min(plantNdemand_l2e(c,j),(nh4_l2e(c,j)+no3_l2e(c,j))/dt*2)/natomw/(1000.0*porosity_l2e(c,j)*max(h2o_liqvol(c,j)/porosity_l2e(c,j),0.01))
                if(this%NO3_pool_number>0 .and. this%NH4_pool_number>0 .and. (no3_l2e(c,j)+nh4_l2e(c,j)>0)) then
                   total_immobile_l2e(c,j,this%plantNH4demand_pool_number) = &
                       total_immobile_l2e(c,j,this%plantNH4demand_pool_number)*nh4_l2e(c,j)/(nh4_l2e(c,j)+no3_l2e(c,j))
                endif
                total_immobile_l2e(c,j,this%plantNH4demand_pool_number) = &
                       max(total_immobile_l2e(c,j,this%plantNH4demand_pool_number),minval)
             endif
             if(this%plantNO3demand_pool_number>0) then
                total_immobile_l2e(c,j,this%plantNO3demand_pool_number) = &
                       min(plantNdemand_l2e(c,j),(nh4_l2e(c,j)+no3_l2e(c,j))/dt*2)/natomw/(1000.0*porosity_l2e(c,j)*max(h2o_liqvol(c,j)/porosity_l2e(c,j),0.01))
                if(this%NO3_pool_number>0 .and. this%NH4_pool_number>0 .and. (no3_l2e(c,j)+nh4_l2e(c,j)>0)) then
                   total_immobile_l2e(c,j,this%plantNO3demand_pool_number) = &
                       total_immobile_l2e(c,j,this%plantNO3demand_pool_number)*no3_l2e(c,j)/(nh4_l2e(c,j)+no3_l2e(c,j))
                endif
                total_immobile_l2e(c,j,this%plantNO3demand_pool_number) = max(total_immobile_l2e(c,j,this%plantNO3demand_pool_number),minval)
             endif

             ! Reset diagnostic N immobilization, mineralization
             if(this%Nimm_pool_number>0) total_immobile_l2e(c,j,this%Nimm_pool_number) = minval
             if(this%Nimp_pool_number>0) total_immobile_l2e(c,j,this%Nimp_pool_number) = minval
             if(this%Nmin_pool_number>0) total_immobile_l2e(c,j,this%Nmin_pool_number) = minval

             if(this%plantNO3uptake_pool_number>0) total_immobile_l2e(c,j,this%plantNO3uptake_pool_number) = minval
             if(this%plantNO3uptake_pool_number>0) total_mobile_l2e(c,j,this%plantNO3uptake_pool_number)   = minval
             if(this%plantNH4uptake_pool_number>0) total_immobile_l2e(c,j,this%plantNH4uptake_pool_number) = minval
             if(this%plantNH4uptake_pool_number>0) total_mobile_l2e(c,j,this%plantNH4uptake_pool_number)   = minval

             ! Prevent unsaturated layers occurring in the subsurface when there is surface water or the layer above is saturated
             if(h2osfc_l2e(c) > 0.01) then
                h2o_liqvol(c,j) = porosity_l2e(c,j) - h2o_icevol(c,j)
             endif
             if(j>1 .and. h2o_liqvol(c,j)+h2o_icevol(c,j)<porosity_l2e(c,j)) then
                if(porosity_l2e(c,j-1) - (h2o_liqvol(c,j-1)+h2o_icevol(c,j-1)) <0.01_r8 ) then
                  h2o_liqvol(c,j) = porosity_l2e(c,j) - h2o_icevol(c,j)
                endif
             endif

             ! Prevent negative values of h2o_liqvol
             h2o_liqvol(c,j) = max(h2o_liqvol(c,j),0.0001_r8)
             h2o_liqvol(c,j) = min(h2o_liqvol(c,j),1.0_r8)

             if(h2o_liqvol(c,j)+h2o_icevol(c,j)>0) then
                liq_frac(j) = h2o_liqvol(c,j)/(h2o_liqvol(c,j)+h2o_icevol(c,j))
             else
                liq_frac(j) = 0.0_r8
             endif

             do k=1, this%chem_sizes%num_primary
                DON_before = DON_before + total_mobile_l2e(c,j,k)*natomw*this%DON_content(k)*dz(c,j)
                totalC_before = totalC_before + total_mobile_l2e(c,j,k)*catomw*(this%DOC_content(k)+this%DIC_content(k))*dz(c,j)
             enddo

        enddo ! End of layer loop setting things up

        !
        if(any(isnan(liq_frac))) then
             write(iulog,*) __LINE__,j,'liq_frac',liq_frac
             write(iulog,*) h2o_liqvol(c,:)
             write(iulog,*) h2o_icevol(c,:)
             call endrun(msg='liq_frac is NaN')
        endif

        totalN_before = DON_before + sum(no3_l2e(c,:)*dz(c,:)) + sum(nh4_l2e(c,:)*dz(c,:))
        if(this%n2o_pool_number>0) totalN_before = totalN_before + sum(total_mobile_e2l(c,:,this%n2o_pool_number))*natomw*2.0_r8
        if(this%N2_pool_number>0)  totalN_before = totalN_before + sum(total_mobile_e2l(c,:,this%N2_pool_number))*natomw*2.0_r8
        do poolnum=1,ndecomp_pools
             totalN_before = totalN_before + sum(soilnitrogen_l2e(c,:,poolnum)*dz(c,:))
             totalC_before = totalC_before + sum(soilcarbon_l2e(c,:,poolnum)*dz(c,:))
        enddo

        ! Step the chemistry solver, including advection/diffusion and timestep cutting capability for whole column
        ! Need to set surface and lateral boundary condition concentrations

        ! Surface boundary condition should be atmosphere unless there is surface water?
        ! Lateral boundary condition in MARSH mode would be saltwater if we are in the marsh column
        ! If we're in the tidal column and we want to keep track, it's concentrations in water flowing out of the marsh... Makes it trickier
        ! Need to save lateral flow for C balance
        surf_flux(:) = 0.0_r8 ! Positive means into soil
        lat_flux(:)  = 0.0_r8
        lat_bc(:)    = 0.0_r8 ! this%bc(:) ! Currently setting to initial condition.
        surf_bc(:)   = this%bc(:) ! Currently setting to initial condition. Should update so it tracks atmospheric O2, CO2, CH4 concentrations
        ! Assume surface water has no dissolved N. At some point should track N content of surface water though
        if(this%NO3_pool_number>0) surf_bc(this%NO3_pool_number) = 0.0_r8
        if(this%NH4_pool_number>0) surf_bc(this%NH4_pool_number) = 0.0_r8

#ifdef MARSH
        ! Set lateral (tidal flooding) and surface (infiltration) boundary conditions for salinity
        ! Boundary conditions are in mol/m3 of H2O (NOT mol/L). Salinity in parts per thousand (g/kg = g/L = kg/m3)
              ! 1 ppt = 1000 g salt/m3 water / (35.453 g Cl/mol Cl * 1.8066 g salt/g Cl)
        if(this%chloride_pool_number>0) then
             lat_bc(this%chloride_pool_number) = flood_salinity_l2e(c)/(35.453*0.0018066_r8)
             if (this%sulfate_pool_number>0) then
                  lat_bc(this%sulfate_pool_number) = flood_salinity_l2e(c)/0.0018066_r8*0.14_r8/96.06_r8 ! Ratio from Jiaze's manuscript
             endif
             if(this%sodium_pool_number>0) lat_bc(this%sodium_pool_number) = flood_salinity_l2e(c)/0.0018066_r8*0.5769_r8/22.989_r8
             if(this%sulfide_pool_number>0) lat_bc(this%sulfide_pool_number) = flood_salinity_l2e(c)/0.0018066_r8*1e-9_r8/33.1_r8
             ! Assuming ocean water pH is 8 at salinity of 30 and freshwater pH is 6 at salinity of zero !BAM adjusted to 7 for true neutral
             if(this%Hplus_pool_number>0) lat_bc(this%Hplus_pool_number) = 10**(-(7+flood_salinity_l2e(c)*2.0/30.0))
                
             if(this%NO3_pool_number>0) lat_bc(this%NO3_pool_number) = flood_nitrate_l2e(c)*1000.0_r8

             ! Need to distinguish between tidal flooding and rainfall infiltration. Doing based on h2osfc but not sure if that's correct
             ! Now that we have tide height info, we could use that instead. But this won't be correct while tide is going down
             if (h2osfc_l2e(c)>0) then
                  surf_bc(this%chloride_pool_number) = flood_salinity_l2e(c)/(35.453_r8*0.0018066_r8)
                  if (this%sulfate_pool_number>0) then
                    surf_bc(this%sulfate_pool_number) = flood_salinity_l2e(c)/0.0018066_r8*0.14_r8/96.06_r8 ! Ratio from Jiaze's manuscript
                  endif
                  if(this%sodium_pool_number>0) surf_bc(this%sodium_pool_number) = flood_salinity_l2e(c)/0.0018066_r8*0.5769_r8/22.989_r8
                  if(this%sulfide_pool_number>0) surf_bc(this%sulfide_pool_number) = flood_salinity_l2e(c)/0.0018066_r8*1e-9_r8/33.1_r8
                  if(this%Hplus_pool_number>0) surf_bc(this%Hplus_pool_number) = 10**(-(6+flood_salinity_l2e(c)*2.0/30.0))
                  
                  if(this%NO3_pool_number>0) surf_bc(this%NO3_pool_number) = flood_nitrate_l2e(c)*1000.0_r8
             else
                  surf_bc(this%chloride_pool_number) = 0.0_r8
                  if (this%sulfate_pool_number>0) then
                    surf_bc(this%sulfate_pool_number) = 0.0_r8
                  endif
                  if(this%sodium_pool_number>0) surf_bc(this%sodium_pool_number)   = 0.0_r8
                  if(this%sulfide_pool_number>0) surf_bc(this%sulfide_pool_number) = 0.0_r8
                  if(this%Hplus_pool_number>0) surf_bc(this%Hplus_pool_number)     = 10**(-(7+0.0_r8*2.0_r8/30.0_r8))

             endif

        endif

#endif
       ! Limit velocity of vertical water flux to 1 cm/hour for now (for purposes of advection)
       ! Higher velocities tend to produce negative solute concentrations and crash the model
       ! (TODO: needs further thinking)
       ! do j = 0, nlevdecomp
       !   if(qflx_adv_l2e(c,j) > 10.0/dt) then
       !     qflx_adv_l2e(c,j) = 10.0/dt
       !   elseif(qflx_adv_l2e(c,j) < -10.0/dt) then
       !     qflx_adv_l2e(c,j) = -10.0/dt
       !   endif
       !   ! Enforce a small downward flow
       !   ! if(abs(qflx_adv_l2e(c,j))<1e-4)  qflx_adv_l2e(c,j) = 1e-4_r8
       ! enddo

       ! Add subsurface drainage flux to bottom layer of lateral flow
       ! write(iulog,*) 'QFLX_DRAIN',qflx_drain_l2e(c,1:nlevdecomp)*dt
       ! write(iulog,*) 'QFLX_LAT_AQU',qflx_lat_aqu_l2e(c,1:nlevdecomp)*dt
       ! Lateral flow and drainage both calculated in hydrology from the top down. But maybe in reality
       ! it makes more sense to drain from the bottom up, or from the tide water level up.
       ! Maybe lateral drainage is faster above tide level? Currently still doing from the top down.
       ! qflx_lat_aqu_l2e(c,1:nlevdecomp) = qflx_lat_aqu_l2e(c,1:nlevdecomp) - qflx_drain_l2e(c,1:nlevdecomp)

       ! Probably makes more sense to do hydrology stuff in the hydrology modules... Here for now
       ! Above tide level, 75% of tide-driven outgoing lateral flux goes laterally from layer and 25% goes downward and out the next lower layer (compounding)
       ! This actually seems to make the high-salinity problem worse, if anything though...
       ! do k=1,nlevdecomp-1
       !   if(zi(c,k+1)>-1e-3_r8*tide_height_l2e(c)) exit
       ! enddo
       ! do j=1,k
       !   if(qflx_lat_aqu_l2e(c,j) < 0.0_r8) then
       !     qflx_lat_aqu_l2e(c,j+1) = qflx_lat_aqu_l2e(c,j+1) + qflx_lat_aqu_l2e(c,j)*0.25
       !     qflx_adv_l2e(c,j) = qflx_adv_l2e(c,j) + qflx_lat_aqu_l2e(c,j)*0.25/dt
       !     qflx_lat_aqu_l2e(c,j) = qflx_lat_aqu_l2e(c,j) - qflx_lat_aqu_l2e(c,j)*0.25
       !   endif
       ! enddo

       qflx_adv_l2e(c,1:nlevdecomp-1) = max(sum(qflx_drain_l2e(c,1:nlevdecomp))/dt,-10.0/dt)
       qflx_adv_l2e(c,nlevdecomp)     = 0.0_r8
       qflx_adv_l2e(c,0)              = max(min(qflx_adv_l2e(c,0),sum(qflx_drain_l2e(c,1:nlevdecomp))/dt),-10.0/dt)

       ! Do drainage above frozen layer
       do j=1,nlevdecomp
            if((h2o_liqvol(c,j))/porosity_l2e(c,j)<0.7_r8 .or. (liq_frac(j)<0.5)) then
                 qflx_adv_l2e(c,j) = 0.0_r8
            endif
            qflx_lat_aqu_l2e(c,j) = qflx_lat_aqu_l2e(c,j) - (qflx_adv_l2e(c,j-1)-qflx_adv_l2e(c,j))*dt
       enddo



       ! Problem: in elm_driver, vertical water movement and lateral (tidal) flow are calculated, then BGC, then drainage.
       ! So including drainage here is inconsistent order of operations (actually applying drainage from previous time step)

              ! Enforce some groundwater exchange by adding a small drainage flux that increases with depth
              ! Not sure if this is actually a good idea – would be better to actually have hydrology right
              ! Realistically there should be some inputs so some species (like H+) don't get too depleted
              ! From talking with Saubhagya, probably we should have a freshwater inflow boundary condition
              ! do j = 0, nlevdecomp
              !   qflx_lat_aqu_l2e(c,j) = qflx_lat_aqu_l2e(c,j) - 1.0e-5*dt * dz(c,j)
              ! enddo

       call run_column_onestep(this, &
                  dt,       &
                  0,        &
                  max_cuts, &
                  !
                  water_density_l2e(c,:),    &
                  aqueous_pressure_l2e(c,:), &
                  total_mobile_l2e(c,:,:),   &
                  free_mobile_l2e(c,:,:),    &
                  total_immobile_l2e(c,:,:), &
                  mineral_volume_fraction_l2e(c,:,:),       &
                  mineral_specific_surface_area_l2e(c,:,:), &
                  surface_site_density_l2e(c,:,:),          &
                  cation_exchange_capacity_l2e(c,:,:),      &
                  aux_doubles_l2e(c,:,:),   &
                  aux_ints_l2e(c,:,:),      &
                  !
                  porosity_l2e(c,:),        &
                  temperature(c,:),         &
                  dz(c,:),                  &
                  (h2o_liqvol(c,:)+h2o_icevol(c,:))/porosity_l2e(c,:),    &    ! Water content as fraction of saturation
                  liq_frac(:),                      &      ! Liquid fraction of soil water
                  -qflx_adv_l2e(c,0:nlevdecomp),    &      ! Vertical water flux (mm/s)
                  qflx_lat_aqu_l2e(c,:)/dt,         &      ! Horizontal water flux (depth-resolved) mm/s
                  lat_bc,                   &      ! Lateral flux concentration boundary condition
                  lat_flux,                 &      ! Output: Lateral flux of each solute
                  surf_bc,                  &      ! Surface boundary condition
                  surf_flux)                       ! Output: Surface flux of each solute

        !
        actual_dt_e2l(c)=dt/2**max_cuts

        ! Save back to ELM
        water_density_e2l(c,:)                   = water_density_l2e(c,:)
        aqueous_pressure_e2l(c,:)                = aqueous_pressure_l2e(c,:)
        total_mobile_e2l(c,:,:)                  = total_mobile_l2e(c,:,:)
        free_mobile_e2l(c,:,:)                   = free_mobile_l2e(c,:,:)
        total_immobile_e2l(c,:,:)                = total_immobile_l2e(c,:,:)
        mineral_volume_fraction_e2l(c,:,:)       = mineral_volume_fraction_l2e(c,:,:)
        mineral_specific_surface_area_e2l(c,:,:) = mineral_specific_surface_area_l2e(c,:,:)
        surface_site_density_e2l(c,:,:)          = surface_site_density_l2e(c,:,:)
        cation_exchange_capacity_e2l(c,:,:)      = cation_exchange_capacity_l2e(c,:,:)
        aux_doubles_e2l(c,:,:)                   = aux_doubles_l2e(c,:,:)
        aux_ints_e2l(c,:,:)                      = aux_ints_l2e(c,:,:)

        if(this%CO2_pool_number>0) then
            hr_e2l(c) = -surf_flux(this%CO2_pool_number)*catomw/dt          ! Is this an issue if there is surface water?
        else
            hr_e2l(c) = 0.0_r8
        endif

        if(this%CH4_pool_number>0) then
            methaneflux_e2l(c) = -surf_flux(this%CH4_pool_number)*catomw/dt ! Is this an issue if there is surface water?
        else
            methaneflux_e2l(c) = 0.0_r8
        endif

        if(this%N2O_pool_number>0) then
            n2oflux_e2l(c) = -surf_flux(this%N2O_pool_number)*catomw/dt     ! Is this an issue if there is surface water?
        else
            n2oflux_e2l(c) = 0.0_r8
        endif
        if(this%N2O_pool_number>0) then
            n2flux_e2l(c) = -surf_flux(this%N2_pool_number)*catomw/dt       ! Is this an issue if there is surface water?
        else
            n2flux_e2l(c) = 0.0_r8
        endif

        ! Surface flow of dissolved NO3 and NH4 need to be accounted for either by adding to runoff/leaching or tracking content in h2osfc
        ! Infiltration is a potential issue currently since we should really be tracking dissolved N stock in surface water as part of the column
        ! We will need to add DOC and DON runoff to ELM balance calculations eventually as well
        if(this%NO3_pool_number>0) then
            NO3runoff_e2l(c) = -surf_flux(this%NO3_pool_number)*natomw/dt - lat_flux(this%NO3_pool_number)*natomw/dt
        else
            NO3runoff_e2l(c) = 0.0_r8
        endif
        if(this%NH4_pool_number>0) then
            ! For now, including NO3 and NH4 in NO3 runoff since ELM does not include any NH4 runoff
            ! This also allows runoff to be negative if nitrogen is being carried in laterally or through infiltration
            NO3runoff_e2l(c) = NO3runoff_e2l(c) - surf_flux(this%NH4_pool_number)*natomw/dt - lat_flux(this%NH4_pool_number)*natomw/dt
        endif

        DONrunoff_e2l(c) = 0.0_r8
        DOCrunoff_e2l(c) = 0.0_r8
        DICrunoff_e2l(c) = -hr_e2l(c) - methaneflux_e2l(c) ! Subtract HR to avoid double counting surface flux
        do k=1, this%chem_sizes%num_primary
            DONrunoff_e2l(c) = DONrunoff_e2l(c) - (surf_flux(k)+lat_flux(k))*this%DON_content(k)*natomw/dt
            DOCrunoff_e2l(c) = DOCrunoff_e2l(c) - (surf_flux(k)+lat_flux(k))*this%DOC_content(k)*catomw/dt
            DICrunoff_e2l(c) = DICrunoff_e2l(c) - (surf_flux(k)+lat_flux(k))*this%DIC_content(k)*catomw/dt
        enddo

        ! Loop through layers after solve and update ELM values
        do j=1,nlevdecomp

            ! Set updated land model values. Should this be moved into copy subroutine?
            ! Convert from mol/m3 to gC/m2
            do poolnum=1,ndecomp_pools
                if(this%carbon_pool_mapping(poolnum)>0) &
                    soilcarbon_e2l(c,j,poolnum) = total_immobile_e2l(c,j,this%carbon_pool_mapping(poolnum))*catomw

                ! Separate N pool only exists if floating CN ratio
                if(decomp_cascade_con%floating_cn_ratio_decomp_pools(poolnum) .and. this%nitrogen_pool_mapping(poolnum)>0) then
                    soilnitrogen_e2l(c,j,poolnum) = total_immobile_e2l(c,j,this%nitrogen_pool_mapping(poolnum))*natomw
                 elseif (this%carbon_pool_mapping(poolnum)>0) then
                    ! Calculate from CN ratio and C pool
                     soilnitrogen_e2l(c,j,poolnum) = soilcarbon_e2l(c,j,poolnum)/decomp_cascade_con%initial_cn_ratio(poolnum)
                endif
            enddo
            ! Sum together mobile and immobile pools
              ! hr_e2l goes to hr_vr (gC/m3/s)
              ! With vertical transport, comparing CO2 before/after is no longer accurate and also ignores surface exchange
              ! Best bet may be to update total HR instead of vertically resolved HR
              ! Need to add soil DIC and DOC fields to balance C
              ! if(this%CO2_pool_number>0) then 
              !   hr_e2l(c,j) = - CO2_before
              !   ! Immobile: Convert from mol/m3 to gC/m3/s
              !   hr_e2l(c,j) = hr_e2l(c,j) + total_immobile_e2l(c,j,this%CO2_pool_number)*catomw
              !   ! Mobile: convert from mol/L to gC/m3/s. mol/L*gC/mol*1000L/m3*porosity
              !   hr_e2l(c,j) = hr_e2l(c,j) + total_mobile_e2l(c,j,this%CO2_pool_number)*catomw
              !   hr_e2l(c,j) = hr_e2l(c,j)/dt
              ! endif

            ! for tracking
            if (this%hrimm_pool_number>0) then
               hrimm_e2l(c,j) = total_immobile_e2l(c,j,this%hrimm_pool_number)*catomw/dt
            endif

            DOC_e2l(c,j) = 0.0_r8
            DON_e2l(c,j) = 0.0_r8
            DIC_e2l(c,j) = 0.0_r8
            do k=1, this%chem_sizes%num_primary
                DOC_e2l(c,j) = DOC_e2l(c,j) + total_mobile_e2l(c,j,k)*catomw*this%DOC_content(k)
                DON_e2l(c,j) = DON_e2l(c,j) + total_mobile_e2l(c,j,k)*natomw*this%DON_content(k)
                DIC_e2l(c,j) = DIC_e2l(c,j) + total_mobile_e2l(c,j,k)*catomw*this%DIC_content(k) 
            enddo

            if(this%acetate_pool_number>0) then
                acetate_vr_e2l(c,j) = total_mobile_e2l(c,j,this%acetate_pool_number) ! mol/m3 bulk
            else
                acetate_vr_e2l(c,j) = 0.0_r8
            endif

            if(this%CH4_pool_number>0) then
                methane_vr_e2l(c,j) = total_mobile_e2l(c,j,this%CH4_pool_number)*catomw
            else
                methane_vr_e2l(c,j) = 0.0_r8
            endif

            carbonate_e2l(c,j) = 0.0_r8
            do k=1, this%chem_sizes%num_minerals
                ! volume fraction (m3 mineral/m3 bulk) * C_content (mol C/m3 mineral) * catomw (gC/mol) = gC/m3 bulk
                carbonate_e2l(c,j) = carbonate_e2l(c,j) + mineral_volume_fraction_e2l(c,j,k)*this%carbonate_C_content(k)*catomw
            enddo

            ! This should probably use free ion concentration or pH (in aux_output) instead of total concentration
            molperL_to_molperm3 = 1000.0*(h2o_liqvol(c,j)+h2o_icevol(c,j))
            if(this%Hplus_pool_number>0 .and. molperL_to_molperm3>0) then
                pH_e2l(c,j) = -log10(free_mobile_e2l(c,j,this%Hplus_pool_number)/molperL_to_molperm3)
            else
                pH_e2l(c,j) = 0.0_r8
            endif

            ! Maybe these should also use free ion concentration?
            ! May want to add sulfide concentration also
            if(this%sulfate_pool_number>0) then
                sulfate_e2l(c,j) = total_mobile_e2l(c,j,this%sulfate_pool_number)
            else
                sulfate_e2l(c,j) = 0.0_r8
            endif

            if(this%sulfate_pool_number>0) then
                sulfide_e2l(c,j) = total_mobile_e2l(c,j,this%sulfide_pool_number)
            else
                sulfide_e2l(c,j) = 0.0_r8
            endif

            if(this%O2_pool_number>0) then
                O2_e2l(c,j) = total_mobile_e2l(c,j,this%O2_pool_number)
            else
                O2_e2l(c,j) = 0.0_r8
            endif

            if(this%chloride_pool_number>0) then
                ! Chloride concentration needs to be converted to ppt (by mass) in water = mg/L . mol Cl- /m3 water * 35.453 g/mol * 1.8066 g salt/g Cl / 1000 g Cl/kg Cl
                salinity_e2l(c,j) = total_mobile_e2l(c,j,this%chloride_pool_number)/(porosity_l2e(c,j)*max(h2o_liqvol(c,j)/porosity_l2e(c,j),0.01))*35.453*1.80655/1000.0
            else
                salinity_e2l(c,j) = 0.0_r8
            endif

            if(this%Fe2_pool_number>0) then
                Fe2_e2l(c,j) = total_mobile_e2l(c,j,this%Fe2_pool_number)
            else
                Fe2_e2l(c,j) = 0.0_r8
            endif

            if(this%FeOH3_pool_number>0) then
                ! Minerals need to be divided by molar volume (m3/mol) since alquimia units are m3/m3
                ! Molar volume of Fe(OH)3 is 34.3600 cm3/mol from hanford.dat
                FeOxide_e2l(c,j) = mineral_volume_fraction_e2l(c,j,this%FeOH3_pool_number)/34.36e-6
            else
                FeOxide_e2l(c,j) = 0.0_r8
            endif

            if(this%Goethite_pool_number>0) then
                ! Minerals need to be divided by molar volume (m3/mol) since alquimia units are m3/m3
                ! Molar volume of Geothite is 20.8200 cm3/mol from hanford.dat
                FeOxide_e2l(c,j) = FeOxide_e2l(c,j) + mineral_volume_fraction_e2l(c,j,this%Goethite_pool_number)/20.82e-6
            endif

            if(this%FeS_pool_number>0) then
                ! Minerals need to be divided by molar volume (m3/mol) since alquimia units are m3/m3
                ! Molar volume of Pyrrhotite is 18.2000 cm3/mol from hanford.dat
                FeS_e2l(c,j) = mineral_volume_fraction_e2l(c,j,this%FeS_pool_number)/18.2000e-6
            else
                FeS_e2l(c,j) = 0.0_r8
            endif

            if(this%pyrite_pool_number>0) then
                ! Minerals need to be divided by molar volume (m3/mol) since alquimia units are m3/m3
                ! Molar volume of Pyrrhotite is 23.9400 cm3/mol from hanford.dat
                FeS_e2l(c,j) = FeS_e2l(c,j) + mineral_volume_fraction_e2l(c,j,this%pyrite_pool_number)/23.94000e-6
            endif

            if(this%NO3_pool_number>0) no3_e2l(c,j) = total_mobile_e2l(c,j,this%NO3_pool_number)*natomw
            if(this%NH4_pool_number>0) nh4_e2l(c,j) = total_mobile_e2l(c,j,this%NH4_pool_number)*natomw
            if(this%n2o_pool_number>0) n2o_e2l(c,j) = total_mobile_e2l(c,j,this%n2o_pool_number)*natomw*2
            if(this%N2_pool_number>0)  n2_e2l(c,j)   = total_mobile_e2l(c,j,this%N2_pool_number)*natomw*2

            if(this%Nimm_pool_number>0) Nimm_e2l(c,j) = total_immobile_e2l(c,j,this%Nimm_pool_number)*natomw/dt
            if(this%Nimp_pool_number>0) Nimp_e2l(c,j) = total_immobile_e2l(c,j,this%Nimp_pool_number)*natomw/dt
            ! Nmin will be added to the NH4 pool elsewhere in ELM so skip that for now
            ! if(this%Nmin_pool_number>0) Nmin_e2l(c,j) = alquimia_immobile_data(this%Nmin_pool_number)*natomw/dt

            ! PFLOTRAN may use an aqueous tracer to model plant N uptake if defining using Microbial reaction
            ! Do we need to transfer N back to aqueous pools if uptake exceeds demand? Plant model assumes that will never happen
            if(this%plantNO3uptake_pool_number>0) plantNO3uptake_e2l(c,j) = (total_immobile_e2l(c,j,this%plantNO3uptake_pool_number))*natomw/dt + &
                                                (total_mobile_e2l(c,j,this%plantNO3uptake_pool_number))*natomw/dt
            if(this%plantNH4uptake_pool_number>0) plantNH4uptake_e2l(c,j) = (total_immobile_e2l(c,j,this%plantNH4uptake_pool_number))*natomw/dt + &
                                                (total_mobile_e2l(c,j,this%plantNH4uptake_pool_number))*natomw/dt

            if( plantNdemand_l2e(c,j) == 0.0_r8) then
                no3_e2l(c,j) = no3_e2l(c,j) + plantNO3uptake_e2l(c,j)*dt
                nh4_e2l(c,j) = nh4_e2l(c,j) + plantNH4uptake_e2l(c,j)*dt
                plantNO3uptake_e2l(c,j) = 0.0_r8
                plantNH4uptake_e2l(c,j) = 0.0_r8
            endif

            if(plantNO3uptake_e2l(c,j)+plantNH4uptake_e2l(c,j) > plantNdemand_l2e(c,j)) then
                
                excess_NO3_uptake = plantNO3uptake_e2l(c,j)*(1-plantNdemand_l2e(c,j)/(plantNO3uptake_e2l(c,j)+plantNH4uptake_e2l(c,j)))
                excess_NH4_uptake = plantNH4uptake_e2l(c,j)*(1-plantNdemand_l2e(c,j)/(plantNO3uptake_e2l(c,j)+plantNH4uptake_e2l(c,j)))
                NO3_e2l(c,j) = NO3_e2l(c,j) + excess_NO3_uptake*dt
                NH4_e2l(c,j) = NH4_e2l(c,j) + excess_NH4_uptake*dt
                plantNO3uptake_e2l(c,j) = plantNO3uptake_e2l(c,j) - excess_NO3_uptake
                plantNH4uptake_e2l(c,j) = plantNH4uptake_e2l(c,j) - excess_NH4_uptake
            endif
                                                  

            ! Todo: Add C check
            ! Note: Generates errors if not multiplied by layer volume (imbalance on the order of 1e-8 gN/m3)
            ! Note: Generates error after restart at precision of 1e-9. But doesn't set off N conservation errors in model when precision here is relaxed.
            ! if(abs(sum(soilnitrogen_l2e(c,j,:))+no3_l2e(c,j)+nh4_l2e(c,j)-&
            !           (sum(soilnitrogen_e2l(c,j,:))+no3_e2l(c,j)+nh4_e2l(c,j)+plantNO3uptake_e2l(c,j)*dt+plantNH4uptake_e2l(c,j)*dt))*dz(c,j)>1e-8) then
            !   write(iulog,'(a,1x,i3,a,i5)') 'Nitrogen imbalance after alquimia solve step in layer',j,' Column ',c,__FILE__,__LINE__
            !   ! call endrun(msg='N imbalance after alquimia solve')
            ! endif
        enddo   ! Layer loop

        !
        if(this%NO3_pool_number>0 .and. any(no3_e2l(c,:) < 0.0) ) then
            write(iulog,*) c,'NO3 < 0 after Alquimia',no3_e2l(c,:)
            write(iulog,*) 'NO3 before',no3_l2e(c,:)
            write(iulog,*) 'Porosity',porosity_l2e(c,:)
            write(iulog,*) 'qflx_lat_aqu',qflx_lat_aqu_l2e(c,:)
            write(iulog,*) 'qflx_adv',qflx_adv_l2e(c,:)
            write(iulog,*) 'water content',(h2o_liqvol(c,:)+h2o_icevol(c,:))/porosity_l2e(c,:)
            write(iulog,*) 'h2o_liqvol',h2o_liqvol(c,:)
            write(iulog,*) 'h2o_icevol',h2o_icevol(c,:)
            call endrun(msg = 'NO3 < 0 after Alquimia')
        endif

        ! wonder if these errors are occurring because we are dividing by a very small liquid water amount somewhere in the chemistry?
        totalN_after = sum(DON_e2l(c,:)*dz(c,:)) + sum(nh4_e2l(c,:)*dz(c,:)) + sum(no3_e2l(c,:)*dz(c,:)) + sum(n2o_e2l(c,:)*dz(c,:)) + sum(n2_e2l(c,:)*dz(c,:))
        do poolnum=1,ndecomp_pools
            totalN_after = totalN_after + sum(soilnitrogen_e2l(c,:,poolnum)*dz(c,:))
        enddo
        Nflux = sum((plantNO3uptake_e2l(c,:)+plantNH4uptake_e2l(c,:))*dz(c,:))*dt + (NO3runoff_e2l(c) + DONrunoff_e2l(c) + n2oflux_e2l(c) + n2flux_e2l(c))*dt
        if(abs(totalN_after + Nflux - totalN_before) > 1e-9 ) then
            ! Assuming this is a precision issue in PFLOTRAN solve, change NO3 runoff or top layer NO3 to balance things
            ! This is to fix a tradeoff where making the precision of the PFLOTRAN solve too fine means it crashes on convergence errors, but making it too low violates ELM N balance limit of 1e-8
            if (  abs(totalN_after + Nflux - totalN_before) < 5e-7 ) then ! Only do it for relatively small errors
                NO3runoff_e2l(c) = NO3runoff_e2l(c) - (totalN_after + Nflux - totalN_before)/dt
            endif
        endif

        totalC_after = sum(DOC_e2l(c,:)*dz(c,:)) + sum(DIC_e2l(c,:)*dz(c,:))
        do poolnum=1,ndecomp_pools
            totalC_after = totalC_after + sum(soilcarbon_e2l(c,:,poolnum)*dz(c,:))
        enddo
        Cflux =  (DICrunoff_e2l(c) + DOCrunoff_e2l(c) + hr_e2l(c) + methaneflux_e2l(c))*dt
        if(abs(totalC_after + Cflux - totalC_before) > 1e-9 ) then
            if(  abs(totalC_after + Cflux - totalC_before) < 6e-7 & ! Only do it for relatively small errors
               ! .and. abs(hr_e2l(c)*dt) > abs(totalC_after + Cflux - totalC_before)*100 & ! Only if it's a small percent of Cflux
            ) then
                hr_e2l(c) = hr_e2l(c) - (totalC_after + Cflux - totalC_before)/dt
            else
                write(iulog,*) "Alquimia: Couldn't add C imbalance to HR. Column ",c," HR = ",hr_e2l(c)*dt,' imbalance =',totalC_after + Cflux - totalC_before
            endif
        endif

    enddo ! Column loop
     

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! alquimia libs are NOT available
#else

    if (use_alquimia .and. .not. alquimia_pf_coupled) then
      ! test EMI for Alquimia to check data passing but bypassing of alquimia BGC solve
       write(iulog,*)
       write(iulog,*) '--WARNING-- Testing data passing of ELM-EMI-alquimia, WIHTOUT actual initial alquimia/pflotran condition setting '

       do fc = 1, num_soilc
           c = filter_soilc(fc)

           ! pass 'l2e' data back to 'e2l',
           ! or assign a zero to 'e2l' if not from 'l2e'.
           ! (NOTE: this is necessary, because those 'call e2l_list%' may overwrite global variables if any in ELM datatypes.
           do j=1, nlevdecomp
              do poolnum=1,ndecomp_pools
                 soilcarbon_e2l(c,j,poolnum)   = soilcarbon_l2e(c,j,poolnum)
                 soilnitrogen_e2l(c,j,poolnum) = soilnitrogen_l2e(c,j,poolnum)
              enddo
              no3_e2l(c,j)   = no3_l2e(c,j)
              nh4_e2l(c,j)   = nh4_l2e(c,j)

              n2o_e2l(c,j)        = 0._r8
              n2_e2l(c,j)         = 0._r8
              methane_vr_e2l(c,j) = 0._r8
              DIC_e2l(c,j)        = 0._r8
              DOC_e2l(c,j)        = 0._r8
              DON_e2l(c,j)        = 0._r8
              carbonate_e2l(c,j)  = 0._r8

              !
              hrimm_e2l(c,j)          = 0._r8
              Nimm_e2l(c,j)           = 0._r8
              Nimp_e2l(c,j)           = 0._r8
              Nmin_e2l(c,j)           = 0._r8
              plantNO3uptake_e2l(c,j) = plantNdemand_l2e(c,j)/2.0
              plantNH4uptake_e2l(c,j) = plantNdemand_l2e(c,j)/2.0

           end do

           hr_e2l(c)           = 0._r8
           methaneflux_e2l(c)  = 0._r8
           n2oflux_e2l(c)      = 0._r8
           n2flux_e2l(c)       = 0._r8
           NO3runoff_e2l(c)    = 0._r8
           DONrunoff_e2l(c)    = 0._r8
           DOCrunoff_e2l(c)    = 0._r8
           DICrunoff_e2l(c)    = 0._r8
       enddo ! Column loop

    !
    else

       call endrun(msg='ERROR: Attempting to run with alquimia when model not compiled with USE_ALQUIMIA_LIB')

    endif

#endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  end subroutine EMAlquimia_Solve_BGC
  
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ifdef USE_ALQUIMIA_LIB

! alquimia libs dependent subroutines

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine copy_Alquimia_to_ELM(this, &
    j,                &
    water_density,    &
    aqueous_pressure, &
    total_mobile,     &
    free_mobile,      &
    total_immobile,   &
    mineral_volume_fraction,       &
    mineral_specific_surface_area, &
    surface_site_density,          &
    cation_exchange_capacity,      &
    aux_doubles,      &
    aux_ints)

    implicit None

    ! !ARGUMENTS
    class(em_alquimia_type)              :: this
    integer                              :: j     ! layer
    ! Pointer arrays that were previously mapped using EMI
    real(r8) :: water_density(:),  aqueous_pressure(:)
    real(r8) :: total_mobile(:,:), total_immobile(:,:), free_mobile(:,:)
    real(r8) :: mineral_volume_fraction(:,:), mineral_specific_surface_area(:,:)
    real(r8) :: surface_site_density(:,:), cation_exchange_capacity(:,:), aux_doubles(:,:)
    integer  :: aux_ints(:,:)

    ! locals
    real (c_double), pointer :: alquimia_data(:)
    integer (c_int), pointer :: alquimia_int_data(:)
    real(r8) :: molperL_to_molperm3

    water_density(j)    = this%chem_state%water_density    ! kg/m3
    aqueous_pressure(j) = this%chem_state%aqueous_pressure

    ! We will store mobile concentrations as  mol/m3 bulk on ELM side and mol/L on alquimia side
    ! This is so changes in layer water content across time steps are properly reflected in concentrations
    molperL_to_molperm3 = 1000.0*this%chem_state%porosity*this%chem_properties%saturation

    ! c_f_pointer just points an array to the right data, so it needs to be actually copied
    call c_f_pointer(this%chem_state%total_mobile%data, alquimia_data, (/this%chem_sizes%num_primary/))
    ! total_mobile is converted to mol/m3 units for ELM side
    total_mobile(j,1:this%chem_sizes%num_primary)   = alquimia_data(1:this%chem_sizes%num_primary)*molperL_to_molperm3

    call c_f_pointer(this%chem_state%total_immobile%data, alquimia_data, (/this%chem_sizes%num_primary/))
    total_immobile(j,1:this%chem_sizes%num_primary) = alquimia_data(1:this%chem_sizes%num_primary)

    call c_f_pointer(this%chem_aux_output%primary_free_ion_concentration%data, alquimia_data, (/this%chem_sizes%num_primary/))
    ! free_mobile coming out of alquimia is in molal units (mol/kg H2O). Convert to mol/m3 to match totals. mol/kg H2O * kg H2O/m3 * saturation
    free_mobile(j,1:this%chem_sizes%num_primary)    = alquimia_data(1:this%chem_sizes%num_primary)*water_density(j)*this%chem_properties%saturation

    call c_f_pointer(this%chem_state%mineral_volume_fraction%data, alquimia_data, (/this%chem_sizes%num_minerals/))
    mineral_volume_fraction(j,1:this%chem_sizes%num_minerals)       = alquimia_data(1:this%chem_sizes%num_minerals)

    call c_f_pointer(this%chem_state%mineral_specific_surface_area%data, alquimia_data, (/this%chem_sizes%num_minerals/))
    mineral_specific_surface_area(j,1:this%chem_sizes%num_minerals) = alquimia_data(1:this%chem_sizes%num_minerals)

    call c_f_pointer(this%chem_state%surface_site_density%data, alquimia_data, (/this%chem_sizes%num_surface_sites/))
    surface_site_density(j,1:this%chem_sizes%num_surface_sites)     = alquimia_data(1:this%chem_sizes%num_surface_sites)

    call c_f_pointer(this%chem_state%cation_exchange_capacity%data, alquimia_data, (/this%chem_sizes%num_ion_exchange_sites/))
    cation_exchange_capacity(j,1:this%chem_sizes%num_ion_exchange_sites) = alquimia_data(1:this%chem_sizes%num_ion_exchange_sites)

    call c_f_pointer(this%chem_aux_data%aux_doubles%data, alquimia_data, (/this%chem_sizes%num_aux_doubles/))
    aux_doubles(j,1:this%chem_sizes%num_aux_doubles) = alquimia_data(1:this%chem_sizes%num_aux_doubles)

    call c_f_pointer(this%chem_aux_data%aux_ints%data, alquimia_int_data, (/this%chem_sizes%num_aux_integers/))
    aux_ints(j,1:this%chem_sizes%num_aux_integers)   = alquimia_int_data(1:this%chem_sizes%num_aux_integers)

  end subroutine copy_Alquimia_to_ELM

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine Copy_ELM_To_Alquimia(this, &
    j,                &
    water_density,    &
    aqueous_pressure, &
    total_mobile,     &
    total_immobile,   &
    mineral_volume_fraction,       &
    mineral_specific_surface_area, &
    surface_site_density,          &
    cation_exchange_capacity,      &
    aux_doubles,      &
    aux_ints)


    implicit None

    ! !ARGUMENTS
    class(em_alquimia_type)              :: this
    integer                              :: j, k ! layer, primary species
    ! Pointer arrays that were previously mapped using EMI
    real(r8) :: water_density(:), aqueous_pressure(:)
    real(r8) :: total_mobile(:,:), total_immobile(:,:)
    real(r8) :: mineral_volume_fraction(:,:), mineral_specific_surface_area(:,:)
    real(r8) :: surface_site_density(:,:), cation_exchange_capacity(:,:), aux_doubles(:,:)
    integer  :: aux_ints(:,:)

    real (c_double), pointer :: alquimia_data(:)
    integer (c_int), pointer :: alquimia_int_data(:)

    real(r8) :: molperL_to_molperm3

    !----------
    this%chem_state%water_density = water_density(j)
    this%chem_state%aqueous_pressure = aqueous_pressure(j)

    ! We will store mobile concentrations as  mol/m3 bulk on ELM side and mol/L on alquimia side
    ! This is so changes in layer water content across time steps are properly reflected in concentrations
    molperL_to_molperm3 = 1000.0_r8*this%chem_state%porosity*this%chem_properties%saturation
    molperL_to_molperm3 = max(molperL_to_molperm3, minval) !BAM: capping to avoid tiny denominators that might cause later divisions to blow up

    ! c_f_pointer just points an array to the right data, so it needs to be actually copied
    call c_f_pointer(this%chem_state%total_mobile%data, alquimia_data, (/this%chem_sizes%num_primary/))
    alquimia_data(1:this%chem_sizes%num_primary) = total_mobile(j,1:this%chem_sizes%num_primary)/molperL_to_molperm3
    ! Don't let concentrations get below a minimal value to prevent crashes
    do k=1,this%chem_sizes%num_primary
        if(abs(alquimia_data(k))>0.0 .and. abs(alquimia_data(k))<minval) alquimia_data(k)=minval
    enddo

    call c_f_pointer(this%chem_state%total_immobile%data, alquimia_data, (/this%chem_sizes%num_primary/))
    alquimia_data(1:this%chem_sizes%num_primary) = total_immobile(j,1:this%chem_sizes%num_primary)

    call c_f_pointer(this%chem_state%mineral_volume_fraction%data, alquimia_data, (/this%chem_sizes%num_minerals/))
    alquimia_data(1:this%chem_sizes%num_minerals) = mineral_volume_fraction(j,1:this%chem_sizes%num_minerals)

    call c_f_pointer(this%chem_state%mineral_specific_surface_area%data, alquimia_data, (/this%chem_sizes%num_minerals/))
    alquimia_data(1:this%chem_sizes%num_minerals) = mineral_specific_surface_area(j,1:this%chem_sizes%num_minerals)

    call c_f_pointer(this%chem_state%surface_site_density%data, alquimia_data, (/this%chem_sizes%num_surface_sites/))
    alquimia_data(1:this%chem_sizes%num_surface_sites) = surface_site_density(j,1:this%chem_sizes%num_surface_sites)

    call c_f_pointer(this%chem_state%cation_exchange_capacity%data, alquimia_data, (/this%chem_sizes%num_ion_exchange_sites/))
    alquimia_data(1:this%chem_sizes%num_ion_exchange_sites) = cation_exchange_capacity(j,1:this%chem_sizes%num_ion_exchange_sites) 

    call c_f_pointer(this%chem_aux_data%aux_doubles%data, alquimia_data, (/this%chem_sizes%num_aux_doubles/))
    alquimia_data(1:this%chem_sizes%num_aux_doubles) = aux_doubles(j,1:this%chem_sizes%num_aux_doubles) 
    ! Messing with aux data is probably frowned upon, but very low values of free ion concentrations (<1e-200) in aux_doubles were causing crashes
    do k=1,this%chem_sizes%num_primary
        if(abs(alquimia_data(k))>0.0 .and. abs(alquimia_data(k))<minval) alquimia_data(k)=minval
    enddo

    call c_f_pointer(this%chem_aux_data%aux_ints%data, alquimia_int_data, (/this%chem_sizes%num_aux_integers/))
    alquimia_int_data(1:this%chem_sizes%num_aux_integers) = aux_ints(j,1:this%chem_sizes%num_aux_integers) 

  end subroutine Copy_ELM_To_Alquimia

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  integer function find_alquimia_pool(pool_name,name_list,n_names) result(pool_number)
    use c_f_interface_module, only : c_f_string_ptr

    implicit none
    
    character(*),intent(in)          :: pool_name
    type (c_ptr), pointer,intent(in) :: name_list(:)
    integer, intent(in)              :: n_names
    
    integer                          :: jj
    character(len=kAlquimiaMaxStringLength) :: alq_poolname
    
    !-----------------
    
    pool_number=-1
    
    do jj=1, n_names
      call c_f_string_ptr(name_list(jj),alq_poolname)
      if(trim(alq_poolname) == trim(pool_name)) then
        pool_number=jj
        exit
      endif
    enddo
    
  end function find_alquimia_pool

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine map_alquimia_pools(this)


    use elm_varpar,             only : ndecomp_pools
    use CNDecompCascadeConType, only : decomp_cascade_con
    use elm_varctl,             only : alquimia_IC_name,   alquimia_CO2_name, &
                                       alquimia_NO3_name,  alquimia_NH4_name, &
                                       alquimia_Nimp_name, alquimia_Nmin_name, alquimia_Nimm_name, &
                                       alquimia_plantNO3uptake_name,  &
                                       alquimia_plantNH4uptake_name,  &
                                       alquimia_plantNO3demand_name,  &
                                       alquimia_plantNH4demand_name
    use elm_varpar,             only : nlevdecomp, ndecomp_pools, ndecomp_cascade_transitions
    use shr_infnan_mod,         only : isnan => shr_infnan_isnan,nan => shr_infnan_nan

    class(em_alquimia_type) :: this

    integer :: ii
    character(len=kAlquimiaMaxStringLength) :: alq_poolname, donor_poolname, receiver_poolname
    type (c_ptr), pointer   :: name_list(:)
    logical :: found_pool
    integer :: pool_num

    ! Map out the location of pertinent pools in Alquimia data structure
    ! Assumes that organic matter pools in PFLOTRAN are named the same as decomp_pool_name_history
    ! Currently we are not mapping any non-CTC pools.
    ! This could be a problem if column chemical state is initialized before the decomp cascade pool structure in ELM
    ! CN pool names in ELM are assigned in init_decompcascade_cn which is called after chemstatemod initialization and restart reading that require alquimia sizes to be set
    ! Maybe best to move this to solve step but only do it if it hasn't been done previously?
    write(iulog,*) 'Alquimia carbon pool mapping:'
    allocate(this%carbon_pool_mapping(ndecomp_pools))
    call c_f_pointer(this%chem_metadata%primary_names%data, name_list, (/this%chem_sizes%num_primary/))
    do ii=1, ndecomp_pools
      if(decomp_cascade_con%floating_cn_ratio_decomp_pools(ii)) then
        alq_poolname = trim(decomp_cascade_con%decomp_pool_name_history(ii))//'C'
      else
        alq_poolname = trim(decomp_cascade_con%decomp_pool_name_history(ii))
      endif
      pool_num = find_alquimia_pool(alq_poolname,name_list,this%chem_sizes%num_primary)
      if(pool_num>0) then 
        write(iulog, '(a, i3, 1X,a7, a, i3, 1X, a)') 'ELM pool',ii,trim(decomp_cascade_con%decomp_pool_name_history(ii)),' <-> Alquimia pool',pool_num,trim(alq_poolname)
      else
        write(iulog,*) 'WARNING: No match for pool',ii,trim(decomp_cascade_con%decomp_pool_name_history(ii))
      endif
      this%carbon_pool_mapping(ii)=pool_num
    enddo
    
    pool_num = find_alquimia_pool(alquimia_CO2_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog, '(a,6x,a,i3,1x,a)') 'CO2 production', '<-> Alquimia pool',pool_num,trim(alquimia_CO2_name)
    else
      write(iulog, '(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_CO2_name)
    endif
    this%CO2_pool_number = pool_num
    
    
    write(iulog,*) 'Alquimia nitrogen pool mapping:'
    allocate(this%nitrogen_pool_mapping(ndecomp_pools))
    do ii=1, ndecomp_pools
      alq_poolname = trim(decomp_cascade_con%decomp_pool_name_history(ii))//'N'
      pool_num = find_alquimia_pool(alq_poolname,name_list,this%chem_sizes%num_primary)
      if(pool_num>0) then 
        write(iulog, '(a, i3, 1X,a7, a, i3, 1X, a)') 'ELM pool',ii,trim(decomp_cascade_con%decomp_pool_name_history(ii)),' <-> Alquimia pool',pool_num,trim(alq_poolname)
      elseif  (decomp_cascade_con%floating_cn_ratio_decomp_pools(ii)) then
        write(iulog, '(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(decomp_cascade_con%decomp_pool_name_history(ii))
      endif
      this%nitrogen_pool_mapping(ii)=pool_num
    enddo
    
    pool_num = find_alquimia_pool(alquimia_NH4_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog, '(a,6x,a,i3,1x,a)') 'NH4', '<-> Alquimia pool',pool_num,trim(alquimia_NH4_name)
    else
      write(iulog, '(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_NH4_name)
    endif
    this%NH4_pool_number = pool_num
    
    pool_num = find_alquimia_pool(alquimia_NO3_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'NO3', '<-> Alquimia pool',pool_num,trim(alquimia_NO3_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_NO3_name)
    endif
    this%NO3_pool_number = pool_num
    pool_num = find_alquimia_pool(alquimia_Nimm_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'N immobilization', '<-> Alquimia pool',pool_num,trim(alquimia_Nimm_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_Nimm_name)
    endif
    this%Nimm_pool_number = pool_num
    
    pool_num = find_alquimia_pool(alquimia_Nimp_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'N potential immobilization', '<-> Alquimia pool',pool_num,trim(alquimia_Nimp_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_Nimp_name)
    endif
    this%Nimp_pool_number = pool_num

    pool_num = find_alquimia_pool(alquimia_Nmin_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'N mineralization', '<-> Alquimia pool',pool_num,trim(alquimia_Nmin_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_Nmin_name)
    endif
    this%Nmin_pool_number = pool_num

    pool_num = find_alquimia_pool(alquimia_plantNH4uptake_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'Plant NH4 uptake', '<-> Alquimia pool',pool_num,trim(alquimia_plantNH4uptake_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_plantNH4uptake_name)
    endif
    this%plantNH4uptake_pool_number = pool_num

    pool_num = find_alquimia_pool(alquimia_plantNO3uptake_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'Plant NO3 uptake', '<-> Alquimia pool',pool_num,trim(alquimia_plantNO3uptake_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_plantNO3uptake_name)
    endif
    this%plantNO3uptake_pool_number = pool_num

    pool_num = find_alquimia_pool(alquimia_plantNH4demand_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'Plant NH4 demand', '<-> Alquimia pool',pool_num,trim(alquimia_plantNH4demand_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_plantNH4demand_name)
    endif
    this%plantNH4demand_pool_number = pool_num

    pool_num = find_alquimia_pool(alquimia_plantNO3demand_name,name_list,this%chem_sizes%num_primary)
    if (pool_num>0) then
      write(iulog,'(a,6x,a,i3,1x,a)') 'Plant NO3 demand', '<-> Alquimia pool',pool_num,trim(alquimia_plantNO3demand_name)
    else
      write(iulog,'(a,i3,1X,a)') 'WARNING: No match for pool',ii,trim(alquimia_plantNO3demand_name)
    endif
    this%plantNO3demand_pool_number = pool_num

    ! Need to map out reactions as well
    allocate(this%pool_reaction_mapping(ndecomp_pools))
    call c_f_pointer(this%chem_metadata%aqueous_kinetic_names%data, name_list, (/this%chem_metadata%aqueous_kinetic_names%size/))
    write(iulog,*) 'Alquimia reactions:'
    do ii=1,this%chem_metadata%aqueous_kinetic_names%size
      call c_f_string_ptr(name_list(ii),alq_poolname)
      write(iulog,*) trim(alq_poolname)
    enddo
    ! cascade_receiver_pool goes to ndecomp_cascade_transitions, not ndecomp_pools
    ! But decomp_k_pools is actually by pool not by transition. So we should map based on donor pool
    do ii=1, ndecomp_cascade_transitions
      donor_poolname = decomp_cascade_con%decomp_pool_name_history(decomp_cascade_con%cascade_donor_pool(ii))
      if(decomp_cascade_con%cascade_receiver_pool(ii)>0) then
        receiver_poolname = decomp_cascade_con%decomp_pool_name_history(decomp_cascade_con%cascade_receiver_pool(ii))
      else
        receiver_poolname = 'CO2'
      endif
      ! This depends on a particular PFLOTRAN/alquimia naming convention and so is not very flexible
      ! Would it be better to provide full names as inputs in varctl or something?
      alq_poolname = trim(donor_poolname)//' decay to '// trim(receiver_poolname)//' (SOMDEC sandbox)'
      pool_num = find_alquimia_pool(alq_poolname,name_list,this%chem_metadata%aqueous_kinetic_names%size)
      if(pool_num>0) then 
        write(iulog,'(a, i3, 1X,a7,a, i3, 1X, a)') 'ELM reaction',ii,trim(decomp_cascade_con%cascade_step_name(ii)),' <-> Alquimia reaction',pool_num,trim(alq_poolname)
      else
        write(iulog,'(a,i3,1x,a,1x,a)') 'WARNING: No match for reaction',ii,trim(decomp_cascade_con%cascade_step_name(ii)),':'//trim(alq_poolname)
      endif
      ! Here the index of the mapping needs to be the index of the donor pool, not the index of the transition
      this%pool_reaction_mapping(decomp_cascade_con%cascade_donor_pool(ii))=pool_num
    enddo

    ! Find plant NO3 and NH4 uptake reactions to rate constants can be set
    ! This is trickier for Microbial reactions because they are named by stoichiometry and representation depends on precision in input deck
    ! Best long-term solution is probably running in hands-off mode to avoid this entirely
    alq_poolname = '1.00000000e+00 NH4+  -> 1.00000000e+00 Tracer2' ! Todo: Fix this! !
    this%plantNH4uptake_reaction_number = find_alquimia_pool(alq_poolname,name_list,this%chem_metadata%aqueous_kinetic_names%size)
    if(this%plantNH4uptake_reaction_number>0) then 
      write(iulog,'(a, i3, 1X, a)') 'ELM plant NH4+ uptake <-> Alquimia reaction',this%plantNH4uptake_reaction_number,trim(alq_poolname)
    else
      write(iulog,'(a,1x,a)') 'WARNING: No match for plant NH4+ uptake reaction',trim(alq_poolname)
    endif
    alq_poolname = '1.00000000e+00 NO3-  -> 1.00000000e+00 Tracer' ! Todo: Fix this! !
    this%plantNO3uptake_reaction_number = find_alquimia_pool(alq_poolname,name_list,this%chem_metadata%aqueous_kinetic_names%size)
    if(this%plantNO3uptake_reaction_number>0) then 
      write(iulog,'(a, i3, 1X, a)') 'ELM plant NO3- uptake <-> Alquimia reaction',this%plantNO3uptake_reaction_number,trim(alq_poolname)
    else
      write(iulog,'(a,1x,a)') 'WARNING: No match for plant NO3- uptake reaction',trim(alq_poolname)
    endif

    ! Find aqueous gas pools
    allocate(this%is_dissolved_gas(this%chem_sizes%num_primary))
    allocate(this%Henry_const(this%chem_sizes%num_primary))
    allocate(this%Henry_Tdep(this%chem_sizes%num_primary))
    allocate(this%atmo_mixing_ratio(this%chem_sizes%num_primary))
    this%is_dissolved_gas(:) = .FALSE.
    this%Henry_const(:) = 0.0 !  mol/(m3*Pa)
    this%Henry_Tdep(:)  = 0.0 ! temperature dependence
    this%atmo_mixing_ratio(:) = 0.0 ! Fraction. Would be better to get from alquimia BC or atmosphere model at some point
    call c_f_pointer(this%chem_metadata%primary_names%data, name_list, (/this%chem_sizes%num_primary/))
    do ii=1, this%chem_sizes%num_primary
      call c_f_string_ptr(name_list(ii),alq_poolname)
      if((trim(alq_poolname) == 'CO2(aq)') .or. (trim(alq_poolname) == 'HCO3-')) then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 3.3e-4_r8
          this%Henry_Tdep(ii)  = 2400_r8
          this%atmo_mixing_ratio(ii) = 400e-6_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      else if (trim(alq_poolname) == 'CH4(aq)') then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 1.4e-5_r8
          this%Henry_Tdep(ii)  = 1900_r8
          this%atmo_mixing_ratio(ii) = 1900e-9_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      else if (trim(alq_poolname) == 'O2(aq)')  then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 1.2e-5_r8
          this%Henry_Tdep(ii)  = 1700_r8
          this%atmo_mixing_ratio(ii) = 0.2_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      else if ((trim(alq_poolname) == 'H2S(aq)')  .or. (trim(alq_poolname) == 'HS-(aq)'))  then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 1.0e-3_r8
          this%Henry_Tdep(ii)  = 2100_r8
          this%atmo_mixing_ratio(ii) = 1e-8_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      else if (trim(alq_poolname) == 'N2(aq)') then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 6.4e-6_r8
          this%Henry_Tdep(ii)  = 1600_r8
          this%atmo_mixing_ratio(ii) = 0.78_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      else if (trim(alq_poolname) == 'N2O(aq)')  then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 2.4e-4_r8
          this%Henry_Tdep(ii)  = 2700_r8
          this%atmo_mixing_ratio(ii) = 336e-9_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      else if (trim(alq_poolname) == 'H2(aq)')   then
          this%is_dissolved_gas(ii) = .TRUE.
          this%Henry_const(ii) = 7.7e-6_r8
          this%Henry_Tdep(ii)  = 530_r8
          this%atmo_mixing_ratio(ii) = 530e-9_r8
          write(iulog,'(a,1x,a,1x,i3,a,f5.0,a,g9.2)') 'Found alquimia dissolved gas: ',trim(alq_poolname),ii,' Henry const = ',this%Henry_const(ii),' Henry T dependence = ',this%Henry_Tdep(ii)
      endif
    enddo

    ! Map DOC and DIC pools. Think about better approaches than hard coding names here
    allocate(this%DOC_content(this%chem_sizes%num_primary))
    allocate(this%DON_content(this%chem_sizes%num_primary))
    allocate(this%DIC_content(this%chem_sizes%num_primary))
    this%DOC_content(:) = 0.0_r8
    this%DIC_content(:) = 0.0_r8
    this%DON_content(:) = 0.0_r8
    call c_f_pointer(this%chem_metadata%primary_names%data, name_list, (/this%chem_sizes%num_primary/))
    do ii=1, this%chem_sizes%num_primary
      call c_f_string_ptr(name_list(ii),alq_poolname)
      if((trim(alq_poolname) == 'CO2(aq)') .or. &
         (trim(alq_poolname) == 'HCO3-') .or. &
         (trim(alq_poolname) == 'CH4(aq)') ) then ! Take methane out of this calculation so it's more standard DIC
        this%DIC_content(ii) = 1.0_r8
      endif
      ! Not sure if there's a good way to pass C:N ratios from PFLOTRAN to here, but this is really clunky
      if(trim(alq_poolname) == 'DOM1') then
        this%DOC_content(ii) = 1.0_r8
        this%DON_content(ii) = 1.0_r8/20.0_r8*12.0110_r8/14.0067_r8
      endif
      if(trim(alq_poolname) == 'DOM2') then
        this%DOC_content(ii) = 1.0_r8
        this%DON_content(ii) = 1.0_r8/12.0_r8*12.0110_r8/14.0067_r8
      endif
      if(trim(alq_poolname) == 'DOM3') then
        this%DOC_content(ii) = 1.0_r8
        this%DON_content(ii) = 1.0_r8/16.0_r8*12.0110_r8/14.0067_r8
      endif
      if(trim(alq_poolname) == 'Acetate-') this%DOC_content(ii) = 2.0_r8
      if(this%DIC_content(ii)>0) write(iulog,'(a,1x,a,f5.2)') 'Found alquimia DIC pool: ',trim(alq_poolname),this%DIC_content(ii)
      if(this%DOC_content(ii)>0) write(iulog,'(a,1x,a,f5.2)') 'Found alquimia DOC pool: ',trim(alq_poolname),this%DOC_content(ii)
      if(this%DON_content(ii)>0) write(iulog,'(a,1x,a,1x,f12.8)') 'Found alquimia DON pool: ',trim(alq_poolname),this%DON_content(ii)
    enddo


    ! Find other important aqueous pools to pass back to ELM
    call c_f_pointer(this%chem_metadata%primary_names%data, name_list, (/this%chem_sizes%num_primary/))

    this%Hplus_pool_number = find_alquimia_pool('H+',name_list,this%chem_sizes%num_primary)
    this%sulfate_pool_number = find_alquimia_pool('SO4--',name_list,this%chem_sizes%num_primary)
    this%O2_pool_number = find_alquimia_pool('O2(aq)',name_list,this%chem_sizes%num_primary)
    this%chloride_pool_number = find_alquimia_pool('Cl-',name_list,this%chem_sizes%num_primary)
    this%Fe2_pool_number = find_alquimia_pool('Fe++',name_list,this%chem_sizes%num_primary)
    this%sodium_pool_number = find_alquimia_pool('Na+',name_list,this%chem_sizes%num_primary)
    this%sulfide_pool_number = find_alquimia_pool('H2S(aq)',name_list,this%chem_sizes%num_primary)
    this%CH4_pool_number = find_alquimia_pool('CH4(aq)',name_list,this%chem_sizes%num_primary)
    this%acetate_pool_number = find_alquimia_pool('Acetate-',name_list,this%chem_sizes%num_primary)
    this%N2O_pool_number = find_alquimia_pool('N2O(aq)',name_list,this%chem_sizes%num_primary)
    this%N2_pool_number = find_alquimia_pool('N2(aq)',name_list,this%chem_sizes%num_primary)

    if(this%Hplus_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'H+', '<-> Alquimia pool',this%Hplus_pool_number
    if(this%sulfate_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'SO4--', '<-> Alquimia pool',this%sulfate_pool_number
    if(this%O2_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'O2(aq)', '<-> Alquimia pool',this%O2_pool_number
    if(this%chloride_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Cl-', '<-> Alquimia pool',this%chloride_pool_number
    if(this%Fe2_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Fe++', '<-> Alquimia pool',this%Fe2_pool_number
    if(this%sodium_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Na+', '<-> Alquimia pool',this%sodium_pool_number
    if(this%sulfide_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'HS-', '<-> Alquimia pool',this%sulfide_pool_number
    if(this%CH4_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'CH4(aq)', '<-> Alquimia pool',this%CH4_pool_number
    if(this%acetate_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Acetate-', '<-> Alquimia pool',this%acetate_pool_number
    if(this%N2O_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'N2O(aq)', '<-> Alquimia pool',this%N2O_pool_number
    if(this%N2_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'N2(aq)', '<-> Alquimia pool',this%N2_pool_number

    ! Minerals might be trickier because they could have different stoichiometries and molar volumes
    ! Might be better to do this similar to DIC_content to allow for different Fe oxide minerals
    call c_f_pointer(this%chem_metadata%mineral_names%data, name_list, (/this%chem_sizes%num_minerals/))
    this%FeOH3_pool_number = find_alquimia_pool('Fe(OH)3',name_list,this%chem_sizes%num_minerals)
    if(this%FeOH3_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Fe(OH)3', '<-> Alquimia mineral',this%FeOH3_pool_number
    this%Goethite_pool_number = find_alquimia_pool('Goethite',name_list,this%chem_sizes%num_minerals)
    if(this%Goethite_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Goethite', '<-> Alquimia mineral',this%Goethite_pool_number

    this%FeS_pool_number = find_alquimia_pool('Pyrrhotite',name_list,this%chem_sizes%num_minerals)
    if(this%FeS_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Pyrrhotite (FeS)', '<-> Alquimia mineral',this%FeS_pool_number
    this%pyrite_pool_number = find_alquimia_pool('Pyrite',name_list,this%chem_sizes%num_minerals)
    if(this%pyrite_pool_number>0) write(iulog,'(a,6x,a,i3,1x)') 'Pyrite', '<-> Alquimia mineral',this%pyrite_pool_number
    


    allocate(this%carbonate_C_content(this%chem_sizes%num_minerals))
    this%carbonate_C_content(:) = 0.0_r8
    do ii=1, this%chem_sizes%num_minerals
      call c_f_string_ptr(name_list(ii),alq_poolname)
      ! One C per mol of calcite divided by molar volume (m3/mol) to give units of mol C/m3
      if(trim(alq_poolname) == 'Calcite') this%carbonate_C_content(ii)=1.0_r8/36.9340e-6_r8
      if(this%carbonate_C_content(ii)>0) write(iulog,'(a,1x,a,x,f10.4,x,a)') 'Found alquimia carbonate mineral pool: ',trim(alq_poolname),this%carbonate_C_content(ii),'mol C/m^3'
    enddo


  end subroutine map_alquimia_pools

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine print_alquimia_state(this)

    use elm_varpar,             only : ndecomp_pools, ndecomp_cascade_transitions
    use iso_c_binding,          only : c_f_pointer, c_double
    use c_f_interface_module,   only : c_f_string_ptr
    use CNDecompCascadeConType, only : decomp_cascade_con

    implicit none

    class(em_alquimia_type)  :: this

    integer                  :: poolnum
    character(len=256)       :: poolname
    real (c_double), pointer :: alquimia_mobile_data(:), alquimia_free_data(:), alquimia_immobile_data(:), &
                                alquimia_mineral_data(:), alquimia_mineral_SSA(:), alquimia_aux_data(:)
    type (c_ptr), pointer    :: name_list(:)

    call c_f_pointer(this%chem_state%total_immobile%data, alquimia_immobile_data, (/this%chem_sizes%num_primary/))
    call c_f_pointer(this%chem_state%total_mobile%data, alquimia_mobile_data, (/this%chem_sizes%num_primary/))
    call c_f_pointer(this%chem_aux_output%primary_free_ion_concentration%data, alquimia_free_data, (/this%chem_sizes%num_primary/))
    call c_f_pointer(this%chem_state%mineral_volume_fraction%data, alquimia_mineral_data, (/this%chem_sizes%num_minerals/))
    call c_f_pointer(this%chem_state%mineral_specific_surface_area%data, alquimia_mineral_SSA, (/this%chem_sizes%num_minerals/))
    ! call c_f_pointer(this%chem_properties%aqueous_kinetic_rate_cnst%data, alquimia_rates_data, (/this%chem_properties%aqueous_kinetic_rate_cnst%size/))
    
    call c_f_pointer(this%chem_aux_data%aux_doubles%data, alquimia_aux_data, (/this%chem_sizes%num_aux_doubles/))
    write(iulog,'(a)') 'Alquimia aux doubles:'
    do poolnum=1,this%chem_sizes%num_aux_doubles
      write(iulog,'(a,i3,e18.6)') 'Aux double ',poolnum,alquimia_aux_data(poolnum)
    enddo

    call c_f_pointer(this%chem_metadata%primary_names%data, name_list, (/this%chem_sizes%num_primary/))
    write(iulog,'(a)') 'Alquimia primary species (mol/m3 bulk):'
    write(iulog,'(23x,a22,5x,a22,5x,a22)') 'Immobile (mol/m3 bulk)','Tot Mobile (mol/L H2O)','Free (mol/L H2O)'
    do poolnum=1,this%chem_sizes%num_primary
      call c_f_string_ptr(name_list(poolnum),poolname)
      write(iulog,'(i3,a20,e22.6,5x,e22.6,5x,e22.6)'),poolnum,trim(poolname),alquimia_immobile_data(poolnum),alquimia_mobile_data(poolnum),alquimia_free_data(poolnum)
    enddo

    call c_f_pointer(this%chem_metadata%mineral_names%data, name_list, (/this%chem_sizes%num_minerals/))
    write(iulog,'(a)') 'Alquimia minerals:'
    write(iulog,'(23x,a22,5x,a22)') 'Vol. frac. (m3/m3)', 'SSA (m2/m3)'
    do poolnum=1,this%chem_sizes%num_minerals
      call c_f_string_ptr(name_list(poolnum),poolname)
      write(iulog,'(i3,a20,e22.6,5x,e22.6)'),poolnum,trim(poolname),alquimia_mineral_data(poolnum),alquimia_mineral_SSA(poolnum)
    enddo

    write(iulog,'(a,f12.4)') 'Porosity      = ',this%chem_state%porosity
    write(iulog,'(a,f12.4)') 'Saturation    = ',this%chem_properties%saturation
    write(iulog,'(a,f12.4)') 'Temperature   = ',this%chem_state%temperature
    write(iulog,'(a,f12.4)') 'Pressure      = ',this%chem_state%aqueous_pressure
    write(iulog,'(a,f12.4)') 'Water density = ',this%chem_state%water_density
    write(iulog,'(a,f12.4)') 'Volume        = ',this%chem_properties%volume


  end subroutine print_alquimia_state

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  recursive subroutine run_onestep(this, &
                                   dt, num_cuts, max_cuts)
    
    !
    use c_f_interface_module, only : c_f_string_ptr
    
    implicit none
    
    class(em_alquimia_type)              :: this
    integer,intent(out)                  :: max_cuts
    integer,intent(in)                   :: num_cuts
    real(r8),intent(in)                  :: dt
    
    ! local variables
    real(r8) :: actual_dt,porosity
    character(512) :: msg
    character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: status_message
    integer :: ncuts2, ncuts, ii
    
    !------------------------------------
    max_cuts = num_cuts
    actual_dt = dt/(2**num_cuts)
    
    ncuts=0
    ncuts2=0
    
    porosity=this%chem_state%porosity

    call this%chem%ReactionStepOperatorSplit(this%chem_engine,     &
                                             actual_dt,            &
                                             this%chem_properties, &
                                             this%chem_state,      &
                                             this%chem_aux_data,   &
                                             this%chem_status)

    ! Reset porosity because Pflotran tends to mess it up
    this%chem_state%porosity = porosity

    if (this%chem_status%converged) then
      ! Success. Can get aux output and finish execution of the subroutine
      ! Get auxiliary output
      call this%chem%getAuxiliaryOutput(this%chem_engine,     &
                                        this%chem_properties, &
                                        this%chem_state,      &
                                        this%chem_aux_data,   &
                                        this%chem_aux_output, &
                                        this%chem_status)
      if(this%chem_status%error /= 0) then
        call c_f_string_ptr(this%chem_status%message,status_message)
        call endrun(msg='Alquimia error in ReactionStepOperatorSplit: '//status_message)
      endif

    !
    else
      ! Solve did not converge. Cut timestep, and bail out if too short

      if(actual_dt/2 < min_dt) then
        call c_f_string_ptr(this%chem_status%message,status_message)
        write(msg,'(a,i3,a,f5.2,a,i4)') "Error: Alquimia ReactionStepOperatorSplit failed to converge after ",num_cuts," cuts to dt = ",actual_dt,' s. Newton iterations = ',this%chem_status%num_newton_iterations!,' Layer = ',j!," Col = ",c
        call print_alquimia_state(this)
        call endrun(msg=msg)

      else
        ! If we are not at minimum timestep yet, cut and keep going
        ! Need to run the step two times because we have cut the timestep in half

        call run_onestep(this,                &
                         dt, num_cuts+1, ncuts)

        if(ncuts>max_cuts) max_cuts=ncuts
        
        ! The second one starts from the maximum number of cuts from the first one so it doesn't waste time retrying a bunch of failed timestep lengths
         do ii=1,2**(max_cuts-(num_cuts+1))
           call run_onestep(this, dt,ncuts,ncuts2)
           if(ncuts2>max_cuts) max_cuts=ncuts2
         enddo

      endif

    !
    endif
      
  end subroutine run_onestep

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  recursive subroutine run_column_onestep(this, &
          dt, num_cuts, max_cuts, &
          !
          water_density,      &
          aqueous_pressure,   &
          total_mobile,       &
          free_mobile,        &
          total_immobile,     &
          mineral_volume_fraction,       &
          mineral_specific_surface_area, &
          surface_site_density,          &
          cation_exchange_capacity,      &
          aux_doubles,        &
          aux_ints,           &
          !
          porosity,    &
          temperature, &
          volume,      &
          saturation,  &
          liq_frac,    &
          adv_flux,    &
          lat_flow,    &
          lat_bc,      &
          lat_flux,    &
          surf_bc,     &
          surf_flux    &
                                          )
    
      !
      use c_f_interface_module, only : c_f_string_ptr
      use elm_varpar          , only : nlevdecomp
      use elm_varcon          , only : dzsoi_decomp
      use elm_varcon          , only : denh2o, grav, rgas, c_h_inv, kh_theta, kh_tbase
      use shr_infnan_mod      , only : isnan => shr_infnan_isnan

      implicit none

      class(em_alquimia_type)              :: this
      integer,intent(out)                  :: max_cuts
      integer,intent(in)                   :: num_cuts
      real(r8),intent(in)                  :: dt
      real(r8),intent(inout)               :: water_density(:),    &
                                              aqueous_pressure(:), &
                                              total_mobile(:,:),   &
                                              free_mobile(:,:),    &
                                              total_immobile(:,:), &
                                              mineral_volume_fraction(:,:),       &
                                              mineral_specific_surface_area(:,:), &
                                              surface_site_density(:,:),          &
                                              cation_exchange_capacity(:,:),      &
                                              aux_doubles(:,:)
      integer,intent(inout)                :: aux_ints(:,:)
      real(r8),intent(in),dimension(:)     :: porosity, temperature, volume, saturation, lat_flow
      real(r8),intent(in),dimension(:)     :: adv_flux
      real(r8),intent(in)                  :: lat_bc(:), surf_bc(:), liq_frac(:)
      real(r8),intent(inout)               :: surf_flux(:), lat_flux(:) ! Total (cumulative) surface flux in time step. Units of mol/time step

      ! local variables
      real(r8) :: water_density_tmp   (nlevdecomp),    &
                  aqueous_pressure_tmp(nlevdecomp),    &
                  total_mobile_tmp    (nlevdecomp, this%chem_sizes%num_primary), &
                  free_mobile_tmp     (nlevdecomp, this%chem_sizes%num_primary), &
                  total_immobile_tmp  (nlevdecomp, this%chem_sizes%num_primary), &
                  mineral_volume_fraction_tmp      (nlevdecomp, this%chem_sizes%num_minerals),           &
                  mineral_specific_surface_area_tmp(nlevdecomp, this%chem_sizes%num_minerals),           &
                  surface_site_density_tmp         (nlevdecomp, this%chem_sizes%num_surface_sites),      &
                  cation_exchange_capacity_tmp     (nlevdecomp, this%chem_sizes%num_ion_exchange_sites), &
                  aux_doubles_tmp     (nlevdecomp, this%chem_sizes%num_aux_doubles)
      integer  :: aux_ints_tmp        (nlevdecomp, this%chem_sizes%num_aux_integers)
      real(r8) :: diffus(nlevdecomp)
      real(r8) :: sat   (nlevdecomp)
      real(r8) :: dissolved_frac(0:nlevdecomp)
      real(r8) :: transport_change_rate(nlevdecomp, this%chem_sizes%num_primary)
      real(r8) :: source_term          (nlevdecomp, this%chem_sizes%num_primary)
      real(r8) :: surf_flux_step(this%chem_sizes%num_primary)
      real(r8) :: lat_flux_step (this%chem_sizes%num_primary)
      ! real(r8) :: bot_adv_step(this%chem_sizes%num_primary)
      real(r8) :: gas_pressure, water_pressure
      real(r8) :: ebul_flux, ebul_atmo_frac

      real(r8) :: actual_dt
      real(r8) :: porosity_tmp
      character(512) :: msg
      character(kind=C_CHAR,len=kAlquimiaMaxStringLength) :: status_message
      integer :: ncuts2, ncuts
      integer :: ii, j, k

      !---------------------------------
      max_cuts = num_cuts
      actual_dt = dt/(2**num_cuts)

      ncuts=0
      ncuts2=0

      do j=1,nlevdecomp
        sat(j) = min(max(saturation(j),0.01_r8),1.0_r8)
      enddo

      ! First half of vertical transport
      call run_vert_transport(this,           &
                              actual_dt/2_r8, &
                              total_mobile,   &
                              free_mobile,    &
                              porosity(:),    &
                              temperature(:), &
                              volume(:),      &
                              saturation(:),  &
                              liq_frac(:),    &
                              adv_flux(:),    &
                              lat_flow(:),    &
                              lat_bc,  lat_flux_step,  &
                              surf_bc, surf_flux_step, &
                              transport_change_rate)

      !
      do j=1,nlevdecomp

        ! Update properties from ELM
        this%chem_state%porosity =    porosity(j)
        this%chem_state%temperature = temperature(j) - 273.15_r8
        this%chem_properties%volume = volume(j)
        this%chem_properties%saturation = sat(j)!*max(liq_frac(j),1.0e-6_r8) ! Set minimum saturation to stop concentrations from blowing up at low soil moisture
        this%chem_properties%saturation = min(max(this%chem_properties%saturation, 1.0e-12_r8), 1.0_r8) !BAM: adding safety control to prevent unreasonable values
        if(liq_frac(j)<0.5) this%chem_state%temperature = -100.0_r8

        call this%copy_ELM_to_Alquimia(j, &
              water_density,    &
              aqueous_pressure, &
              total_mobile,     &
              total_immobile,   &
              mineral_volume_fraction,       &
              mineral_specific_surface_area, &
              surface_site_density,          &
              cation_exchange_capacity,      &
              aux_doubles,      &
              aux_ints)

        porosity_tmp=this%chem_state%porosity
        call this%chem%ReactionStepOperatorSplit(this%chem_engine,     &
                                                 actual_dt,            &
                                                 this%chem_properties, &
                                                 this%chem_state,      &
                                                 this%chem_aux_data,   &
                                                 this%chem_status)
        ! Reset porosity because Pflotran tends to mess it up
        this%chem_state%porosity = porosity_tmp

        ! Get auxiliary output
        call this%chem%getAuxiliaryOutput(this%chem_engine,     &
                                          this%chem_properties, &
                                          this%chem_state,      &
                                          this%chem_aux_data,   &
                                          this%chem_aux_output, &
                                          this%chem_status)

        ! Check for error
        if(this%chem_status%error /= 0) then
          call c_f_string_ptr(this%chem_status%message,status_message)
          call endrun(msg='Alquimia error in getAuxiliaryOutput: '//status_message)
        endif

        ! In top layer, cut time step if gas species being absorbed very fast because it should be close to equilibrium
        ! if(j==1) then
        !   do k=1,this%chem_sizes%num_primary
        !     if(this%is_dissolved_gas(k) .and. (surf_bc(k) > 0.0) .and. (sat(1)<=0.9) .and. &
        !         ((surf_bc(k)*porosity(1)*max(sat(1),0.3) - total_mobile(1,k) )/(surf_bc(k)*porosity(1)*max(sat(1),0.3)) > 0.5) &
        !         .and. max_cuts<4) then
        !           this%chem_status%converged = .FALSE.
        !           write(iulog,'(a,f5.2,x,a,x,i4,x,a)') 'Cutting time step to dt = ',actual_dt,' because species',k,'reduced too fast in layer 1'
        !     endif
        !   enddo
        ! endif


        if (this%chem_status%converged) then
          ! Success. Can finish execution of the subroutine

          ! Copy back to column structure
          call this%copy_Alquimia_to_ELM(j,                    &
                                         water_density_tmp,    &
                                         aqueous_pressure_tmp, &
                                         total_mobile_tmp,     &
                                         free_mobile_tmp,      &
                                         total_immobile_tmp,   &
                                         mineral_volume_fraction_tmp,       &
                                         mineral_specific_surface_area_tmp, &
                                         surface_site_density_tmp,          &
                                         cation_exchange_capacity_tmp,      &
                                         aux_doubles_tmp,      &
                                         aux_ints_tmp)

        else ! Solve did not converge. Cut timestep, and bail out if too short
          if(actual_dt/2 < min_dt) then
            call c_f_string_ptr(this%chem_status%message,status_message)
            ! Sometimes solve fails because time step is too short (not sure why)
            ! I wonder if at bailout we should first try pausing transport and solving each layer separately?
            write(msg,'(a,i3,a,f5.3,a,i4,a,i3,a,i5)') "Error: Alquimia ReactionStepOperatorSplit failed to converge after ",num_cuts," cuts to dt = ",actual_dt,' s. Newton iterations = ',this%chem_status%num_newton_iterations,' Layer = ',j!," Col = ",c
            call print_alquimia_state(this)
            write(iulog,*),'Alquimia status message: '//status_message
            call endrun(msg=msg)
          else
            exit ! Drop out of the layer loop to start over at shorter time step
          endif
        endif
      
      enddo ! Layer loop

      ! the following may not be in correct place
      ! if(actual_dt<=-60.0_r8) then
        !   do j=1,nlevdecomp
        !         ! Update properties from ELM
        !     this%chem_state%porosity =    porosity(j)
        !     this%chem_state%temperature = temperature(j) - 273.15
        !     this%chem_properties%volume = volume(j)
        !     this%chem_properties%saturation = sat(j) ! Set minimum saturation to stop concentrations from blowing up at low soil moisture
        !     call this%copy_ELM_to_Alquimia(j,water_density,&
        !                                       aqueous_pressure,&
        !                                       total_mobile,&
        !                                       total_immobile,&
        !                                       mineral_volume_fraction,&
        !                                       mineral_specific_surface_area,&
        !                                       surface_site_density,&
        !                                       cation_exchange_capacity,&
        !                                       aux_doubles,&
        !                                       aux_ints)
        !     call run_onestep(this,dt,num_cuts,ncuts)
        !     call this%copy_Alquimia_to_ELM(j,water_density_tmp,&
        !                                     aqueous_pressure_tmp,&
        !                                     total_mobile_tmp,free_mobile_tmp,&
        !                                     total_immobile_tmp,&
        !                                     mineral_volume_fraction_tmp,&
        !                                     mineral_specific_surface_area_tmp,&
        !                                     surface_site_density_tmp,&
        !                                     cation_exchange_capacity_tmp,&
        !                                     aux_doubles_tmp,&
        !                                     aux_ints_tmp)
        !   enddo
      ! endif
    
      if(.not. this%chem_status%converged) then
            ! If we are not at minimum timestep yet, cut and keep going
        
            ! Here we are basically throwing out all the _tmp array values and starting over with the originals

            ! Also need to undo transport because we are starting this time step over
          ! Unless we change transport to act on temp arrays
          do k=1,this%chem_sizes%num_primary
            if(k == this%plantNH4uptake_pool_number .or. k == this%plantNO3uptake_pool_number) cycle
            total_mobile(1:nlevdecomp,k) = total_mobile(1:nlevdecomp,k) &
                            - transport_change_rate(1:nlevdecomp,k)*actual_dt/2
          enddo

          ! Need to run the step two times because we have cut the timestep in half
          call run_column_onestep(this, &
              dt, num_cuts+1, ncuts,    &
              water_density,     &
              aqueous_pressure,  &
              total_mobile,      &
              free_mobile,       &
              total_immobile,    &
              mineral_volume_fraction,       &
              mineral_specific_surface_area, &
              surface_site_density,          &
              cation_exchange_capacity,      &
              aux_doubles,       &
              aux_ints,          &
              porosity,          &
              temperature,       &
              volume,            &
              saturation,        &
              liq_frac,          &
              adv_flux,          &
              lat_flow,          &
              lat_bc, lat_flux,  &
              surf_bc,surf_flux)

          !
          if(ncuts>max_cuts) max_cuts=ncuts

          ! The second one starts from the maximum number of cuts from the first one so it doesn't waste time retrying a bunch of failed timestep lengths
          do ii=1,2**(max_cuts-(num_cuts+1))
            call run_column_onestep(this,    &
              dt, ncuts, ncuts2, &
              water_density,     &
              aqueous_pressure,  &
              total_mobile,      &
              free_mobile,       &
              total_immobile,    &
              mineral_volume_fraction,       &
              mineral_specific_surface_area, &
              surface_site_density,          &
              cation_exchange_capacity,      &
              aux_doubles,       &
              aux_ints,          &
              porosity,          &
              temperature,       &
              volume,            &
              saturation,        &
              liq_frac,          &
              adv_flux,          &
              lat_flow,          &
              lat_bc, lat_flux,  &
              surf_bc,surf_flux)

              if(ncuts2>max_cuts) max_cuts=ncuts2
          enddo

      else ! It did converge and we made it to the end of the layer loop

          ! At this point we've successfully updated the column chemistry for all layers. Copy back to inout arrays
          water_density(:)    = water_density_tmp(:)
          aqueous_pressure(:) = aqueous_pressure_tmp(:)
          total_mobile(:,:)   = total_mobile_tmp(:,:)
          free_mobile(:,:)    = free_mobile_tmp(:,:)
          total_immobile(:,:) = total_immobile_tmp(:,:)
          mineral_volume_fraction(:,:)       = mineral_volume_fraction_tmp(:,:)
          mineral_specific_surface_area(:,:) = mineral_specific_surface_area_tmp(:,:)
          surface_site_density(:,:)          = surface_site_density_tmp(:,:)
          cation_exchange_capacity(:,:)      = cation_exchange_capacity_tmp(:,:)
          aux_doubles(:,:)    = aux_doubles_tmp(:,:)
          aux_ints(:,:)       = aux_ints_tmp(:,:)

          surf_flux = surf_flux + surf_flux_step
          lat_flux  = lat_flux  + lat_flux_step

          ! Second half of transport (Strang splitting)
          ! This is only done if we converged at this time step for all layers
          call run_vert_transport(this,        &
                                actual_dt/2,   &
                                total_mobile,  &
                                free_mobile,   &
                                porosity,      &
                                temperature,   &
                                volume,        &
                                saturation,    &
                                liq_frac,      &
                                adv_flux,      &
                                lat_flow,      &
                                lat_bc,  lat_flux_step,  &
                                surf_bc, surf_flux_step, &
                                transport_change_rate)

          surf_flux = surf_flux + surf_flux_step
          lat_flux  = lat_flux  + lat_flux_step
      
      endif ! if converged

      !
  end subroutine run_column_onestep

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine run_vert_transport(this,       &
                              actual_dt,    &
                              total_mobile, &
                              free_mobile,  &
                              porosity,     &
                              temperature,  &
                              volume,       &
                              saturation,   &
                              liq_frac,     &
                              adv_flux,     &
                              lat_flow,     &
                              lat_bc,  lat_flux_step,  &
                              surf_bc, surf_flux_step, &
                              transport_change_rate)
      !
      use c_f_interface_module, only : c_f_string_ptr
      use elm_varpar       , only : nlevdecomp
      use elm_varcon       , only : dzsoi_decomp,zsoi
      use elm_varcon       , only : denh2o, grav, rgas, c_h_inv, kh_theta, kh_tbase
      use shr_infnan_mod   , only : isnan => shr_infnan_isnan

      implicit none

      class(em_alquimia_type)           :: this
      real(r8),intent(inout)            :: total_mobile(:,:), free_mobile(:,:)
      ! integer,intent(in)                :: c
      real(r8),intent(in)               :: actual_dt
      real(r8),intent(in),dimension(:)  :: porosity,temperature,volume,saturation,lat_flow
      real(r8),intent(in),dimension(:)  :: adv_flux
      real(r8),intent(in),dimension(:)  :: lat_bc, surf_bc, liq_frac
      real(r8),intent(out)              :: surf_flux_step(this%chem_sizes%num_primary), lat_flux_step(this%chem_sizes%num_primary) ! Total (cumulative) surface flux in time step. Units of mol/time step
      real(r8),intent(out)              :: transport_change_rate(nlevdecomp,this%chem_sizes%num_primary)

      ! local variables
      real(r8) :: diffus(nlevdecomp), sat(nlevdecomp), dissolved_frac(0:nlevdecomp), source_term(nlevdecomp,this%chem_sizes%num_primary)
      real(r8) :: surf_adv_step(this%chem_sizes%num_primary),surf_equil_step(nlevdecomp,this%chem_sizes%num_primary)
      ! real(r8) :: bot_adv_step(this%chem_sizes%num_primary)
      real(r8) :: gas_pressure,water_pressure,ebul_flux,ebul_atmo_frac,atmo_pressure,total_resist,effective_diffus
      real(r8) :: temp_r

      integer :: ii,j,k

      !----------------------

      ebul_atmo_frac=0.3 ! Fraction of ebullition that goes directly to atmosphere instead of next layer up

      do j=1,nlevdecomp
        sat(j) = min(max(saturation(j),0.01_r8),1.0_r8)
      enddo

      do k=1, this%chem_sizes%num_primary
        diffus(:) = 0.0_r8
        surf_equil_step(:,k) = 0.0_r8
        lat_flux_step(k) = 0.0_r8
        surf_adv_step(k) = 0.0_r8
        ! Set diffusion coefficient depending on saturation and whether species is aqueous gas or not
        ! Need to set boundary condition concentrations for adv flux (top layer infiltration) and lateral flux (source)

        ! Skip species that are not actually mobile
        if(k == this%plantNH4uptake_pool_number .or. k == this%plantNO3uptake_pool_number) cycle

        !
        if(this%is_dissolved_gas(k)) then

          ! For gases, diffusion rates are set using gas diffusive transport (Meslin et al., SSSAJ, 2010. doi:10.2136/sssaj2009.0474)
          ! Estimating gas diffusion coefficient of 0.2 cm2/s and dry soil diffusion coefficient of 30% of gas (Moldrup et al 2004, SSSAJ)
          ! This needs to account for frozen water filling pores instead of air
          do j=1,nlevdecomp
            ! Fan et al 2014, a little higher at high air filled porosity
            diffus(j) = 2.0e-5_r8*0.66_r8*porosity(j)*(1.0_r8-sat(j))*(1-sat(j))**3.0_r8
          enddo

          water_pressure = 101.325e3_r8 ! Should take H2OSFC into account also, and ideally use actual atmospheric pressure
          total_resist = 0.0_r8
          do j=1,nlevdecomp
            water_pressure = water_pressure + porosity(j)*sat(j)*dzsoi_decomp(j)*grav*denh2o
            if(free_mobile(j,k) > 0.0_r8) then
              gas_pressure = free_mobile(j,k)/porosity(j)/(this%Henry_const(k)*exp(-this%Henry_Tdep(k)*(1.0_r8/temperature(j)-1.0_r8/298.15_r8)))
            else
              gas_pressure = total_mobile(j,k)/porosity(j)/(this%Henry_const(k)*exp(-this%Henry_Tdep(k)*(1.0_r8/temperature(j)-1.0_r8/298.15_r8)))
            endif
            ! Approximate ebullition by increasing diffusion coefficient when gas pressure is higher than water pressure
            ! BAM 12/2/25: editing so ebullition only approxim. when pores contain mostly liq. water (frozen ground control), adding liq_frac(j) gating to help w/CH4 release in winter
            if(gas_pressure>water_pressure .and. sat(j)>0.6 .and. liq_frac(j)>0.7_r8) then
              diffus(j) = max(diffus(j), &
                              2.0e-5_r8*0.66_r8*porosity(j)*((gas_pressure-water_pressure)/gas_pressure)*((gas_pressure-water_pressure)/gas_pressure)**3.0_r8)
            endif
            atmo_pressure = 101.325e3_r8 ! Pa
            ! Henry constant mol/(m3*Pa)
            ! gas flux over finite time step = (C_atmo - C_layer)*(1-exp(-kD/z*dt)) based on integrating diffusion equation
            ! Maybe better equation is (C_atmo - C_layer)*erf(z/(2*sqrt(D*dt)))
            total_resist = total_resist + diffus(j)*dzsoi_decomp(j)
            ! total_resist = total_resist + (diffus(j)+2.5e-9_r8*0.005_r8*exp(10.0_r8*sat(j)*liq_frac(j)*porosity(j)))*dzsoi_decomp(j)
            ! Effective diffus is harmonic mean of 1/diffus in each layer above the current one (equals 1/ weighted harmonic mean of resistance)
            ! This could be updated to include other pathways like plant-mediated transport
            effective_diffus = max(1.e-10_r8, total_resist/zsoi(j))  ! zero will cause math error below (division)

            ! Make surface equilibration slower up than down (try to fix methane emission issue)
            ! if(atmo_pressure*this%atmo_mixing_ratio(k) < gas_pressure) effective_diffus = effective_diffus*0.7_r8
            temp_r = erf(zsoi(j)/(2.0_r8*sqrt(effective_diffus*3600.0_r8)))
            temp_r = (1.0_r8-temp_r)*actual_dt/3600.0_r8                                                          ! Integrate effective diffusion over time
            temp_r = temp_r * &
                     (this%Henry_const(k)*exp(-this%Henry_Tdep(k)*(1.0_r8/temperature(j)-1.0_r8/298.15_r8)))      ! Convert to concentration using Henry constant

            surf_equil_step(j,k) = (atmo_pressure*this%atmo_mixing_ratio(k) - gas_pressure)                &      ! Difference in gas pressure
                                   * temp_r                                                                &
                                   * dzsoi_decomp(j) * porosity(j) ! mol/m3                                       ! Convert from concentration to mass using pore volume

            ! Make sure equilibration step doesn't take gas concentration below zero
            if((surf_equil_step(j,k)/dzsoi_decomp(j) < 0.0_r8) .and. (abs(surf_equil_step(j,k)/dzsoi_decomp(j)) > abs(total_mobile(j,k)))) surf_equil_step(j,k) = -abs(total_mobile(j,k))*0.95_r8*dzsoi_decomp(j)
          enddo

          ! Eventually replace this with calculation using actual saturation/ebullition concentration
          dissolved_frac(0:nlevdecomp) = 0.1_r8

        elseif (lat_bc(k) .ne. 0.0_r8) then
          dissolved_frac(0:nlevdecomp) = 1.0_r8

        else
          dissolved_frac(0:nlevdecomp) = 0.1_r8

        endif

        !
        do j=1,nlevdecomp
          if(isnan(total_mobile(j,k))) then
            write(iulog,*) __LINE__,'Chem spec',k,'layer',j,total_mobile(:,k)
            call endrun(msg="Mobile species is NaN")
          endif
          ! Assume diffusion through water according to Wright (1990)
          ! In that paper diffus_water = 0.000025 cm2/s
          diffus(j) = diffus(j) + 2.5e-9_r8*0.005_r8*exp(10.0_r8*sat(j)*liq_frac(j)*porosity(j))

          ! Should add some density-driven mixing if salinity is higher in upper layer than lower layer
          ! Not sure what effective diffusion coefficient should be. 100x molecular times difference in salt content for now
          if(this%chloride_pool_number>0 .and. j<nlevdecomp) then
              if(total_mobile(j,this%chloride_pool_number) > total_mobile(j+1,this%chloride_pool_number) .and. &
                 total_mobile(j,this%chloride_pool_number) > 1.0_r8) then

                  diffus(j) = diffus(j)+ &
                              2.5e-9_r8*0.005_r8*exp(10.0_r8*sat(j)*liq_frac(j)*porosity(j))*100.0_r8 &
                               *(total_mobile(j,this%chloride_pool_number) - total_mobile(j+1,this%chloride_pool_number)) &
                               /(total_mobile(j,this%chloride_pool_number) + total_mobile(j+1,this%chloride_pool_number))
              endif
          endif

          ! Source term is lateral flow. For inflow, use lateral boundary condition. For outflow, use local concentration
          ! lat_flux units are mm H2O/s = 1e-3 m3 h2o/m2/s
          ! lat_bc in units of mol/m3 H2O
          ! source_term in mol/m3 bulk/s
          if(lat_flow(j) > 0) then
              source_term(j,k) = lat_flow(j)*1e-3_r8 * lat_bc(k)*porosity(j)*sat(j)          ! mol/m3 bulk/s
          else
              source_term(j,k) = lat_flow(j)*1e-3_r8 * total_mobile(j,k)*dissolved_frac(j)
          endif
          lat_flux_step(k) = lat_flux_step(k) + source_term(j,k)*dzsoi_decomp(j)*actual_dt

        enddo !j; nlevdecomp

        ! adv_flux units are mm H2O/s
        if(adv_flux(1)<0.0_r8) then ! Downward flow uses surface boundary condition
            surf_adv_step(k) = - adv_flux(1)*1e-3_r8*surf_bc(k)*dissolved_frac(0)*actual_dt
        else ! Upward flow uses surface layer concentration. Should this concentration be per bulk volume or per water volume?
            surf_adv_step(k) = - adv_flux(1)*1e-3_r8*total_mobile(1,k)*dissolved_frac(1)*actual_dt
        endif
        ! if(adv_flux(c,nlevdecomp+1)<0.0_r8) then
        !     bot_adv_step(k) = -adv_flux(c,nlevdecomp+1)*1e-3_r8*total_mobile(c,nlevdecomp,k)*actual_dt
        ! endif

        ! At this point, total_mobile is stored as mol/m3 bulk (ELM side). Dividing by porosity*saturation converts to mol/m3 water
        ! Note adv_flux is defined in advection_diffusion as <0 being downward

        call advection_diffusion(total_mobile(1:nlevdecomp,k),                                             &
                                 adv_flux(1:nlevdecomp+1)*1e-3*dissolved_frac(0:nlevdecomp),               &
                                 diffus(1:nlevdecomp),                                                     &
                                 source_term(1:nlevdecomp,k)+ &
                                   surf_equil_step(1:nlevdecomp,k)/(dzsoi_decomp(1:nlevdecomp)*actual_dt), &
                                 surf_bc(k),                                                               &
                                 actual_dt,                                                                &
                                 transport_change_rate(1:nlevdecomp,k))

        ! At this point perhaps we should go through and re-equilibrate dissolved gases in top layer if unsaturated?
        total_mobile(1:nlevdecomp,k) = total_mobile(1:nlevdecomp,k) + transport_change_rate(1:nlevdecomp,k)*actual_dt

        ! Save surface equil in transport_change_rate in case it needs to be undone after failed alquimia solve
        ! This is done after applying transport_change_rate to total_mobile so it's not double counted
        ! transport_change_rate(1:nlevdecomp,k) = transport_change_rate(1:nlevdecomp,k) !+ surf_equil_step(1:nlevdecomp,k)/actual_dt/dzsoi_decomp(1:nlevdecomp)

        ! Ebullition flux, from the bottom up until reaching unsaturated layer (TODO: currently OFF)
        ! if(this%is_dissolved_gas(k)) then
        !     do j=nlevdecomp,2,-1
        !       if(sat(j)<0.9 .or. liq_frac(j)<0.95) exit
        !       ! Calculate total water pressure. Using calculation from Jiaze
        !       water_pressure = 101.325e3_r8 ! Should take H2OSFC into account also, and ideally use actual atmospheric pressure
        !       do ii=1,j
        !         water_pressure = water_pressure + porosity(ii)*sat(ii)*dzsoi_decomp(ii)*grav*denh2o
        !       enddo
        !       ! Gas pressure in Pa from Jiaze's calculation. Should maybe include a bubble gas fraction or take other gas partial pressures into account
        !       if(free_mobile(j,k)>0.0_r8) then
        !         gas_pressure = free_mobile(j,k)/porosity(j)/(this%Henry_const(k)*exp(-this%Henry_Tdep(k)*(1/temperature(j)-1/298.15)))
        !       else
        !         gas_pressure = total_mobile(j,k)/porosity(j)/(this%Henry_const(k)*exp(-this%Henry_Tdep(k)*(1/temperature(j)-1/298.15)))
        !       endif
        !       ! This is problematic in the loop because free_mobile isn't getting updated with transport changes
        !       ! One way to get around this would be to send it to the first unsaturated layer instead of next layer up
        !       ebul_flux = free_mobile(j,k)*max((gas_pressure-water_pressure)/gas_pressure,0.0)/3600_r8 ! mol/m3
        !       ! Move excess gas up one layer
        !       ! What if we spread it over a larger area? Or have some fraction go directly to atmosphere depending on time step?
        !       if(total_mobile(j,k) < 0.0) then
        !         ebul_flux=0.0
        !       endif
        !       ebul_flux=min(ebul_flux,total_mobile(j,k)*0.9_r8/3600_r8)*0.0
        !       if(ebul_flux>0.0_r8) then
        !         if(ebul_flux>total_mobile(j,k)*0.5/3600.0) write(iulog,*) 'Ebullition: ',j,k,water_pressure,gas_pressure,free_mobile(j,k),total_mobile(j,k),ebul_flux*3600
        !         ! ebul_flux is in mol/m3, so transfering to a different layer requires correcting for difference in layer thickness so it's in mols
        !         ! transport_change_rate is in mol/m3/s so ebul_flux needs to be divided by time step length
        !         ! write(iulog,*),'Ebullition: ',j,k,gas_pressure,water_pressure,ebul_flux,total_mobile(c,j,k),temperature(c,j),this%Henry_const(k),this%Henry_Tdep(k)
        !         total_mobile(j,k) = total_mobile(j,k) - ebul_flux*actual_dt
        !         total_mobile(j-1,k) = total_mobile(j-1,k) + ebul_flux*actual_dt*(dzsoi_decomp(j))/(dzsoi_decomp(j-1))*(1-ebul_atmo_frac)
        !         free_mobile(j,k) = free_mobile(j,k) - ebul_flux*actual_dt
        !         free_mobile(j-1,k) = free_mobile(j-1,k) + ebul_flux*actual_dt*(dzsoi_decomp(j))/(dzsoi_decomp(j-1))*(1-ebul_atmo_frac)
        !         surf_equil_step(1,k) = surf_equil_step(1,k) - ebul_flux*actual_dt*ebul_atmo_frac*dzsoi_decomp(j)
        !         transport_change_rate(j,k) = transport_change_rate(j,k) - ebul_flux
        !         ! transport_change_rate(1,k) = transport_change_rate(1,k) + ebul_flux*ebul_atmo_frac/actual_dt*(dzsoi_decomp(j))/(dzsoi_decomp(1))
        !         transport_change_rate(j-1,k) = transport_change_rate(j-1,k) + ebul_flux*(dzsoi_decomp(j))/(dzsoi_decomp(j-1))*(1-ebul_atmo_frac)
        !       endif
        !     enddo
        ! endif

      enddo  ! k; this%chem_sizes%num_primary

      surf_flux_step = sum(surf_equil_step,dim=1) + surf_adv_step

  end subroutine run_vert_transport
  
#endif

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! Should make sure following is available when alquimia is turned off/not compiled
! in case we want to track e.g. salinity without BGC

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  subroutine advection_diffusion(conc_trcr,       &
                                 adv_flux,        &
                                 diffus,          &
                                 source,          &
                                 surf_bc,         &
                                 dtime,           &
                                 conc_change_rate)

    ! Advection and diffusion for a single tracer in one column given diffusion coefficient, flow, and source-sink terms
    ! Based on SoilLittVertTranspMod, which implements S. V. Patankar, Numerical Heat Transfer and Fluid Flow, Series in Computational Methods in Mechanics and Thermal Sciences, Hemisphere Publishing Corp., 1980. Chapter 5
    ! Not sure if this belongs here or somewhere else. Is it bad to do this in the EMI subroutine?

    use elm_varpar       , only : nlevdecomp
    use elm_varcon       , only : zsoi, zisoi, dzsoi_decomp
    !
    real(r8), intent(in) :: conc_trcr(1:nlevdecomp)   ! Bulk concentration (e.g. mol/m3). Or should it be concentration in water??
    real(r8), intent(in) :: adv_flux(1:nlevdecomp+1)  ! (m/s), vertical into layer (down is negative)
    real(r8), intent(in) :: diffus(1:nlevdecomp)        ! diffusivity (m2/s)
    real(r8), intent(in) :: source(1:nlevdecomp)  ! Source term (mol/m3/s)
 
    real(r8), intent(in) :: surf_bc                 ! Surface boundary layer concentration (for infiltration)
    real(r8), intent(in) :: dtime                   ! Time step (s)
    real(r8), intent(out):: conc_change_rate(1:nlevdecomp) ! Bulk concentration (e.g. mol/m3/s). Or should it be concentration in water??

    ! Local variables
    real(r8) :: aaa                                                ! "A" function in Patankar
    real(r8) :: pe                                                 ! Pe for "A" function in Patankar
    real(r8) :: w_m1, w_p1                                         ! Weights for calculating harmonic mean of diffusivity
    real(r8) :: d_m1, d_p1                                         ! Harmonic mean of diffusivity
    real(r8) :: a_tri(0:nlevdecomp+1)      ! "a" vector for tridiagonal matrix
    real(r8) :: b_tri(0:nlevdecomp+1)      ! "b" vector for tridiagonal matrix
    real(r8) :: c_tri(0:nlevdecomp+1)      ! "c" vector for tridiagonal matrix
    real(r8) :: r_tri(0:nlevdecomp+1)      ! "r" vector for tridiagonal solution
    real(r8) :: d_p1_zp1(1:nlevdecomp+1)   ! diffusivity/delta_z for next j  (set to zero for no diffusion)
    real(r8) :: d_m1_zm1(1:nlevdecomp+1)   ! diffusivity/delta_z for previous j (set to zero for no diffusion)
    real(r8) :: f_p1(1:nlevdecomp+1)       ! water flux for next j
    real(r8) :: f_m1(1:nlevdecomp+1)       ! water flux for previous j
    real(r8) :: pe_p1(1:nlevdecomp+1)      ! Peclet # for next j
    real(r8) :: pe_m1(1:nlevdecomp+1)      ! Peclet # for previous j
    real(r8) :: dz_node(1:nlevdecomp+1)                            ! difference between nodes
    real(r8) :: a_p_0
    real(r8) :: conc_after(0:nlevdecomp+1)
    real(r8) :: rho(1:nlevdecomp)     ! Water density (bulk) in layer
  
    integer  :: j, info
  
    !---------------------------------------------------

    ! Statement function
    aaa (pe) = max (0._r8, (1._r8 - 0.1_r8 * abs(pe))**5.0_r8)  ! "A" function from Patankar, Table 5.2, pg 95

    rho(1:nlevdecomp) = 1.0_r8 ! Placeholder in case we want to account for varying water content

    ! Set the distance between the node and the one ABOVE it
    dz_node(1) = zsoi(1)
    do j = 2,nlevdecomp+1
       dz_node(j)= zsoi(j) - zsoi(j-1)
    enddo

    ! Calculate the D and F terms in the Patankar algorithm
    ! d: diffusivity
    ! f: flow
    ! m: layer above
    ! p: layer below
    ! pe: Peclet number (ratio of convection to diffusion)
    do j = 1,nlevdecomp
      if (j == 1) then
        d_m1_zm1(j) = 0._r8
        w_p1 = (zsoi(j+1) - zisoi(j)) / dz_node(j+1)
        if ( diffus(j+1) > 0._r8 .and. diffus(j) > 0._r8) then
          d_p1 = 1._r8 / ((1._r8 - w_p1) / diffus(j) + w_p1 / diffus(j+1)) ! Harmonic mean of diffus
        else
          d_p1 = 0._r8
        endif
        d_p1_zp1(j) = d_p1 / dz_node(j+1)
        f_m1(j) = adv_flux(j)  ! Include infiltration here
        f_p1(j) = adv_flux(j+1)
        pe_m1(j) = 0._r8
        pe_p1(j) = f_p1(j) / d_p1_zp1(j) ! Peclet #
      elseif (j == nlevdecomp) then
        ! At the bottom, assume no gradient in d_z (i.e., they're the same)
        w_m1 = (zisoi(j-1) - zsoi(j-1)) / dz_node(j)
        if ( diffus(j) > 0._r8 .and. diffus(j-1) > 0._r8) then
          d_m1 = 1._r8 / ((1._r8 - w_m1) / diffus(j) + w_m1 / diffus(j-1)) ! Harmonic mean of diffus
        else
          d_m1 = 0._r8
        endif
        d_m1_zm1(j) = d_m1 / dz_node(j)
        d_p1_zp1(j) = d_m1_zm1(j) ! Set to be the same
        f_m1(j) = adv_flux(j)
        !f_p1(j) = adv_flux(j+1)
        f_p1(j) = 0._r8
        pe_m1(j) = f_m1(j) / d_m1_zm1(j) ! Peclet #
        pe_p1(j) = f_p1(j) / d_p1_zp1(j) ! Peclet #
      else
        ! Use distance from j-1 node to interface with j divided by distance between nodes
        w_m1 = (zisoi(j-1) - zsoi(j-1)) / dz_node(j)
        if ( diffus(j-1) > 0._r8 .and. diffus(j) > 0._r8) then
          d_m1 = 1._r8 / ((1._r8 - w_m1) / diffus(j) + w_m1 / diffus(j-1)) ! Harmonic mean of diffus
        else
          d_m1 = 0._r8
        endif
        w_p1 = (zsoi(j+1) - zisoi(j)) / dz_node(j+1)
        if ( diffus(j+1) > 0._r8 .and. diffus(j) > 0._r8) then
          d_p1 = 1._r8 / ((1._r8 - w_p1) / diffus(j) + w_p1 / diffus(j+1)) ! Harmonic mean of diffus
        else
          d_p1 = (1._r8 - w_p1) * diffus(j) + w_p1 * diffus(j+1) ! Arithmetic mean of diffus
        endif
        d_m1_zm1(j) = d_m1 / dz_node(j)
        d_p1_zp1(j) = d_p1 / dz_node(j+1)
        f_m1(j) = adv_flux(j)
        f_p1(j) = adv_flux(j+1)
        pe_m1(j) = f_m1(j) / d_m1_zm1(j) ! Peclet #
        pe_p1(j) = f_p1(j) / d_p1_zp1(j) ! Peclet #
      end if
    enddo ! j; nlevdecomp


    ! Calculate the tridiagonal coefficients
    ! Coefficients of tridiagonal problem: a_i*x_(i-1) + b_i*(x_i) + c_i*x_(i+1) = r_i
    ! Here, this is equivalent to Patankar equation 5.56 and 5.57 (but in one dimension):
    ! a_P*phi_P = a_E*phi_E + a_W*phi_W + b [phi is concentration, = x in tridiagonal]. Converting East/West to above/below
    ! -> -a_E*phi_E + a_P*phi_P - a_W+phi_W = b
    ! -a_tri = a_above = D_above*A(Pe)+max(-F_above,0); D_above=diffus_above/dz
    ! b_tri = a_above+a_below+rho*dz/dt
    ! -c_tri = D_below*A(Pe)+max(F_below,0); D_below = diffus_below/dz
    ! r_tri = b = source_const*dz + conc*rho*dz/dt
    do j = 0,nlevdecomp +1

      if (j > 0 .and. j < nlevdecomp+1) then
        a_p_0 =  dzsoi_decomp(j) / dtime * rho(j) ! Should this be multiplied by layer water content (for rho)?
      endif

      if (j == 0) then ! top layer (atmosphere)
        a_tri(j) = 0._r8
        b_tri(j) = 1._r8
        c_tri(j) = -1._r8
        r_tri(j) = 0._r8

      elseif (j == 1) then
        a_tri(j) = -(d_m1_zm1(j) * aaa(pe_m1(j)) + max( f_m1(j), 0._r8)) ! Eqn 5.47 Patankar
        c_tri(j) = -(d_p1_zp1(j) * aaa(pe_p1(j)) + max(-f_p1(j), 0._r8))
        b_tri(j) = -a_tri(j) - c_tri(j) + a_p_0
        ! r_tri includes infiltration assuming same concentration as top layer. May want to change to either provide upper boundary condition or include in source term
        ! r_tri(j) = source(j) * dzsoi_decomp(j) + (a_p_0 - adv_flux(j)) * conc_trcr(j)
        r_tri(j) = source(j) * dzsoi_decomp(j) + a_p_0 * conc_trcr(j)
        if(adv_flux(j)<0) then ! downward flow (infiltration)
          r_tri(j) = r_tri(j) - adv_flux(j)*surf_bc
        else ! upward flow to the surface
          r_tri(j) = r_tri(j) - adv_flux(j)*conc_trcr(j)
        endif
        
      elseif (j < nlevdecomp+1) then
        a_tri(j) = -(d_m1_zm1(j) * aaa(pe_m1(j)) + max( f_m1(j), 0._r8)) ! Eqn 5.47 Patankar
        c_tri(j) = -(d_p1_zp1(j) * aaa(pe_p1(j)) + max(-f_p1(j), 0._r8))
        b_tri(j) = -a_tri(j) - c_tri(j) + a_p_0
        r_tri(j) = source(j) * dzsoi_decomp(j) + a_p_0 * conc_trcr(j) ! Eq. 5.57

      else ! j==nlevdecomp+1; 0 concentration gradient at bottom
        a_tri(j) = -1._r8
        b_tri(j) = 1._r8
        c_tri(j) = 0._r8 
        r_tri(j) = 0._r8

      endif
    enddo ! j; nlevdecomp

    ! Solve for the concentration profile for this time step
    ! call Tridiagonal(0, nlevdecomp+1, 0, a_tri, b_tri, c_tri, r_tri, conc_after)
    ! This is the LAPACK tridiagonal solver which gave more accurate results in my testing
    call dgtsv( nlevdecomp+2, 1,     &
              c_tri(0:nlevdecomp),   &
              b_tri,                 &
              a_tri(1:nlevdecomp+1), &
              r_tri,                 &
              nlevdecomp+2,          &
              info )

    if(info < 0) call endrun(msg='dgtsv error in adv_diff line __LINE__: illegal argument')
    if(info > 0) call endrun(msg='dgtsv error in adv_diff line __LINE__: singular matrix')
    conc_after = r_tri

    conc_change_rate = (conc_after(1:nlevdecomp)-conc_trcr)/dtime

  end subroutine advection_diffusion

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !
  ! Modified to operate on a single column instead of passing bounds and filters
  subroutine Tridiagonal (lbj, ubj,     &
                          jtop,         &
                          a, b, c, r, u)
    !
    ! !DESCRIPTION:
    ! Tridiagonal matrix solution
    ! A x = r
    ! where x and r are vectors

    !
    ! !ARGUMENTS:
    implicit none

    integer           , intent(in)    :: lbj, ubj                                 ! lbinning and ubing level indices
    integer           , intent(in)    :: jtop         ! top level for each column [col]
    real(r8)          , intent(in)    :: a(lbj:ubj)    ! "a" left off diagonal of tridiagonal matrix [col , j]
    real(r8)          , intent(in)    :: b(lbj:ubj)    ! "b" diagonal column for tridiagonal matrix [col  , j]
    real(r8)          , intent(in)    :: c(lbj:ubj)    ! "c" right off diagonal tridiagonal matrix [col   , j]
    real(r8)          , intent(in)    :: r(lbj:ubj)    ! "r" forcing term of tridiagonal matrix [col      , j]
    real(r8)          , intent(inout) :: u(lbj:ubj)    ! solution [col                                    , j]
                                                                                !
    integer                           :: j                                 ! indices

    real(r8)                          :: gam(lbj:ubj)     ! temporary
    real(r8)                          :: bet            ! temporary

    !--------------------------------------

    bet = b(jtop)

    do j = lbj, ubj
      if (j >= jtop) then
        if (j == jtop) then
          u(j) = r(j) / bet
        else
          gam(j) = c(j-1) / bet
          bet = b(j) - a(j) * gam(j)
          u(j) = (r(j) - a(j)*u(j-1)) / bet
        end if
      end if
    end do

    do j = ubj-1,lbj,-1
      if (j >= jtop) then
        u(j) = u(j) - gam(j+1) * u(j+1)
      end if
    end do

  end subroutine Tridiagonal

  !
  !---------------------------------------------------------------------------------------------------------------------------------------
  !

end module ExternalModelAlquimiaMod
