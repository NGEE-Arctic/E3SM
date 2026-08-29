module EMI_ChemStateType_DataMod
  !
  use EMI_ChemStateType_Constants
  !
  implicit none
  !
  public :: EMI_ChemStateType_DataInfoByID

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_ChemStateType_DataInfoByID(data_id, id_val, name_val, long_name_val,&
        units_val, is_int_type, is_real_type, ndim, &
        dim1_beg_name, dim1_end_name, dim2_beg_name, dim2_end_name, &
        dim3_beg_name, dim3_end_name, dim4_beg_name, dim4_end_name, &
        data_found)
    !
    ! !DESCRIPTION:
    ! Defines information of data exchanged between ELM and EM
    !
    ! !USES: 
    use EMI_DataDimensionMod
    implicit none
    !
    ! !ARGUMENTS:
    integer            , intent(in)  :: data_id
    integer            , intent(out) :: id_val
    character (len=32) , intent(out) :: name_val
    character (len=128), intent(out) :: long_name_val
    character (len=32) , intent(out) :: units_val
    logical            , intent(out) :: is_int_type
    logical            , intent(out) :: is_real_type
    integer            , intent(out) :: ndim
    character (len=32) , intent(out) :: dim1_beg_name
    character (len=32) , intent(out) :: dim1_end_name
    character (len=32) , intent(out) :: dim2_beg_name
    character (len=32) , intent(out) :: dim2_end_name
    character (len=32) , intent(out) :: dim3_beg_name
    character (len=32) , intent(out) :: dim3_end_name
    character (len=32) , intent(out) :: dim4_beg_name
    character (len=32) , intent(out) :: dim4_end_name
    logical            , intent(inout) :: data_found

    is_int_type    = .false.
    is_real_type   = .false.
    dim1_beg_name  = ''
    dim2_beg_name  = ''
    dim3_beg_name  = ''
    dim4_beg_name  = ''
    dim1_end_name  = ''
    dim2_end_name  = ''
    dim3_end_name  = ''
    dim4_end_name  = ''

    select case(data_id)

    case(L2E_STATE_SOIL_PH)
       id_val         =  L2E_STATE_SOIL_PH
       name_val       =  'Soil pH'
       long_name_val  =  'Soil pH: ELM to EM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(L2E_STATE_WATER_DENSITY)
       id_val         =  L2E_STATE_WATER_DENSITY
       name_val       =  'Water density'
       long_name_val  =  'Water density: ELM to EM'
       units_val      =  '[kg/m^3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(L2E_STATE_AQUEOUS_PRESSURE)
       id_val         =  L2E_STATE_AQUEOUS_PRESSURE
       name_val       =  'aqueous pressure'
       long_name_val  =  'aqueous pressure: ELM to EM'
       units_val      =  '[Pa]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(L2E_STATE_TOTAL_MOBILE)
       id_val         =  L2E_STATE_TOTAL_MOBILE
       name_val       =  'total mobile'
       long_name_val  =  'total mobile: ELM to EM'
       units_val      =  '[M]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_primary
       data_found   =  .true.

   case(L2E_STATE_FREE_MOBILE)
      id_val         =  L2E_STATE_FREE_MOBILE
      name_val       =  'free mobile'
      long_name_val  =  'free mobile: ELM to EM'
      units_val      =  '[M]'
      is_real_type   =  .true.
      ndim           =  3
      dim1_beg_name  =  dimname_begc
      dim1_end_name  =  dimname_endc
      dim2_beg_name  =  dimname_one
      dim2_end_name  =  dimname_nlevsoi
      dim3_beg_name  =  dimname_one
      dim3_end_name  =  dimname_alquimia_num_primary
      data_found   =  .true.

    case(L2E_STATE_TOTAL_IMMOBILE)
       id_val         =  L2E_STATE_TOTAL_IMMOBILE
       name_val       =  'total immobile'
       long_name_val  =  'total immobile: ELM to EM'
       units_val      =  '[mol/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_primary
       data_found   =  .true.

    case(L2E_STATE_MINERAL_VOLUME_FRACTION)
       id_val         =  L2E_STATE_MINERAL_VOLUME_FRACTION
       name_val       =  'mineral volume fraction'
       long_name_val  =  'mineral volume fraction: ELM to EM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_minerals
       data_found   =  .true.

    case(L2E_STATE_MINERAL_SPECIFIC_SURFACE_AREA)
       id_val         =  L2E_STATE_MINERAL_SPECIFIC_SURFACE_AREA
       name_val       =  'mineral specific surface area'
       long_name_val  =  'mineral specific surface area: ELM to EM'
       units_val      =  '[m^2/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_minerals
       data_found   =  .true.

    case(L2E_STATE_SURFACE_SITE_DENSITY)
       id_val         =  L2E_STATE_SURFACE_SITE_DENSITY
       name_val       =  'surface site density'
       long_name_val  =  'surface site density: ELM to EM'
       units_val      =  '[moles/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_surface_sites
       data_found   =  .true.

    case(L2E_STATE_CATION_EXCHANGE_CAPACITY)
       id_val         =  L2E_STATE_CATION_EXCHANGE_CAPACITY
       name_val       =  'cation exchange capacity'
       long_name_val  =  'cation exchange capacity: ELM to EM'
       units_val      =  '[moles/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_ion_exchange_sites
       data_found   =  .true.

    case(L2E_STATE_AUX_DOUBLES)
       id_val         =  L2E_STATE_AUX_DOUBLES
       name_val       =  'aux doubles'
       long_name_val  =  'aux doubles: ELM to EM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_aux_doubles
       data_found   =  .true.

    case(L2E_STATE_AUX_INTS)
       id_val         =  L2E_STATE_AUX_INTS
       name_val       =  'aux ints'
       long_name_val  =  'aux ints: ELM to EM'
       units_val      =  '[-]'
       is_int_type    =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_aux_ints
       data_found   =  .true.

    case(E2L_STATE_SOIL_PH)
       id_val         =  E2L_STATE_SOIL_PH
       name_val       =  'Soil pH'
       long_name_val  =  'Soil pH: EM to ELM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_SALINITY)
       id_val         =  E2L_STATE_SOIL_SALINITY
       name_val       =  'Soil salinity'
       long_name_val  =  'Soil salinity: EM to ELM'
       units_val      =  '[ppt]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_O2)
       id_val         =  E2L_STATE_SOIL_O2
       name_val       =  'Soil oxygen'
       long_name_val  =  'Soil oxygen: EM to ELM'
       units_val      =  '[mol m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_SULFATE)
       id_val         =  E2L_STATE_SOIL_SULFATE
       name_val       =  'Soil sulfate'
       long_name_val  =  'Soil sulfate: EM to ELM'
       units_val      =  '[mol m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_SULFIDE)
       id_val         =  E2L_STATE_SOIL_SULFIDE
       name_val       =  'Soil sulfide'
       long_name_val  =  'Soil sulfide: EM to ELM'
       units_val      =  '[mol m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_FE2)
       id_val         =  E2L_STATE_SOIL_FE2
       name_val       =  'Soil Fe(II)'
       long_name_val  =  'Soil Fe(II): EM to ELM'
       units_val      =  '[mol m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_FE_OXIDE)
       id_val         =  E2L_STATE_SOIL_FE_OXIDE
       name_val       =  'Soil iron oxide'
       long_name_val  =  'Soil iron oxide: EM to ELM'
       units_val      =  '[mol Fe m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_FE_SULFIDE)
       id_val         =  E2L_STATE_SOIL_FE_SULFIDE
       name_val       =  'Soil iron sulfide'
       long_name_val  =  'Soil iron sulfide: EM to ELM'
       units_val      =  '[mol Fe m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_SOIL_ACETATE)
       id_val         =  E2L_STATE_SOIL_ACETATE
       name_val       =  'Soil acetate'
       long_name_val  =  'Soil acetate: EM to ELM'
       units_val      =  '[mol m^-3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_WATER_DENSITY)
       id_val         =  E2L_STATE_WATER_DENSITY
       name_val       =  'Water density'
       long_name_val  =  'Water density: EM to ELM'
       units_val      =  '[kg/m^3]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_AQUEOUS_PRESSURE)
       id_val         =  E2L_STATE_AQUEOUS_PRESSURE
       name_val       =  'aqueous pressure'
       long_name_val  =  'aqueous pressure: EM to ELM'
       units_val      =  '[Pa]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_TOTAL_MOBILE)
       id_val         =  E2L_STATE_TOTAL_MOBILE
       name_val       =  'total mobile'
       long_name_val  =  'total mobile: EM to ELM'
       units_val      =  '[M]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_primary
       data_found   =  .true.

    case(E2L_STATE_FREE_MOBILE)
       id_val         =  E2L_STATE_FREE_MOBILE
       name_val       =  'free mobile'
       long_name_val  =  'free mobile: EM to ELM'
       units_val      =  '[M]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_primary
       data_found   =  .true.

    case(E2L_STATE_TOTAL_IMMOBILE)
       id_val         =  E2L_STATE_TOTAL_IMMOBILE
       name_val       =  'total immobile'
       long_name_val  =  'total immobile: EM to ELM'
       units_val      =  '[mol/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_primary
       data_found   =  .true.

    case(E2L_STATE_MINERAL_VOLUME_FRACTION)
       id_val         =  E2L_STATE_MINERAL_VOLUME_FRACTION
       name_val       =  'mineral volume fraction'
       long_name_val  =  'mineral volume fraction: EM to ELM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_minerals
       data_found   =  .true.

    case(E2L_STATE_MINERAL_SPECIFIC_SURFACE_AREA)
       id_val         =  E2L_STATE_MINERAL_SPECIFIC_SURFACE_AREA
       name_val       =  'mineral specific surface area'
       long_name_val  =  'mineral specific surface area: EM to ELM'
       units_val      =  '[m^2/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_minerals
       data_found   =  .true.

    case(E2L_STATE_SURFACE_SITE_DENSITY)
       id_val         =  E2L_STATE_SURFACE_SITE_DENSITY
       name_val       =  'surface site density'
       long_name_val  =  'surface site density: EM to ELM'
       units_val      =  '[moles/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_surface_sites
       data_found   =  .true.

    case(E2L_STATE_CATION_EXCHANGE_CAPACITY)
       id_val         =  E2L_STATE_CATION_EXCHANGE_CAPACITY
       name_val       =  'cation exchange capacity'
       long_name_val  =  'cation exchange capacity: EM to ELM'
       units_val      =  '[moles/m^3]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_ion_exchange_sites
       data_found   =  .true.

    case(E2L_STATE_AUX_DOUBLES)
       id_val         =  E2L_STATE_AUX_DOUBLES
       name_val       =  'aux doubles'
       long_name_val  =  'aux doubles: EM to ELM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_aux_doubles
       data_found   =  .true.

    case(E2L_STATE_AUX_INTS)
       id_val         =  E2L_STATE_AUX_INTS
       name_val       =  'aux ints'
       long_name_val  =  'aux ints: EM to ELM'
       units_val      =  '[-]'
       is_int_type    =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_alquimia_num_aux_ints
       data_found   =  .true.

    case(E2L_STATE_CHEM_DT)
       id_val         =  E2L_STATE_CHEM_DT
       name_val       =  'chem dt'
       long_name_val  =  'chem dt: EM to ELM'
       units_val      =  '[-]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.
    end select
    
  end subroutine EMI_ChemStateType_DataInfoByID
    
end module EMI_ChemStateType_DataMod
