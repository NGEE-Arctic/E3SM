module EMI_ChemStateType_ExchangeMod
  !
  use shr_kind_mod                          , only : r8 => shr_kind_r8
  use shr_log_mod                           , only : errMsg => shr_log_errMsg
  use abortutils                            , only : endrun
  use elm_varctl                            , only : iulog
  use EMI_DataMod                           , only : emi_data_list, emi_data
  use EMI_DataDimensionMod                  , only : emi_data_dimension_list_type
  use ColumnDataType       , only : column_chem_state
  use EMI_Atm2LndType_Constants
  use EMI_CanopyStateType_Constants
  use EMI_ChemStateType_Constants
  use EMI_CNCarbonStateType_Constants
  use EMI_CNNitrogenStateType_Constants
  use EMI_CNNitrogenFluxType_Constants
  use EMI_CNCarbonFluxType_Constants
  use EMI_ColumnEnergyStateType_Constants
  use EMI_ColumnWaterStateType_Constants
  use EMI_ColumnWaterFluxType_Constants
  use EMI_EnergyFluxType_Constants
  use EMI_SoilHydrologyType_Constants
  use EMI_SoilStateType_Constants
  use EMI_TemperatureType_Constants
  use EMI_WaterFluxType_Constants
  use EMI_WaterStateType_Constants
  use EMI_Filter_Constants
  use EMI_ColumnType_Constants
  use EMI_Landunit_Constants
  !
  implicit none
  !
  !
  public :: EMI_Pack_ChemStateType_at_Column_Level_for_EM
  public :: EMI_Unpack_ChemStateType_at_Column_Level_from_EM

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_Pack_ChemStateType_at_Column_Level_for_EM(data_list, em_stage, &
        num_filter, filter, col_chem)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM col_chem for EM
    !
    ! !USES:
    use elm_varpar             , only : nlevsoi
    use elm_varpar             , only : alquimia_num_primary
    use elm_varpar             , only : alquimia_num_minerals
    use elm_varpar             , only : alquimia_num_surface_sites
    use elm_varpar             , only : alquimia_num_ion_exchange_sites
    use elm_varpar             , only : alquimia_num_aux_doubles
    use elm_varpar             , only : alquimia_num_aux_ints
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)    , intent(in) :: data_list
    integer                 , intent(in) :: em_stage
    integer                 , intent(in) :: num_filter
    integer                 , intent(in) :: filter(:)
    type(column_chem_state) , intent(in) :: col_chem
    !
    ! !LOCAL_VARIABLES:
    integer                             :: fc,c,j,k
    class(emi_data), pointer            :: cur_data
    logical                             :: need_to_pack
    integer                             :: istage
    integer                             :: count

    associate(& 
         soil_ph                       => col_chem%soil_ph                       , &
         water_density                 => col_chem%water_density                 , &
         aqueous_pressure              => col_chem%aqueous_pressure              , &
         total_mobile                  => col_chem%total_mobile                  , &
         free_mobile                   => col_chem%free_mobile                   , &
         total_immobile                => col_chem%total_immobile                , &
         mineral_volume_fraction       => col_chem%mineral_volume_fraction       , &
         mineral_specific_surface_area => col_chem%mineral_specific_surface_area , &
         surface_site_density          => col_chem%surface_site_density          , &
         cation_exchange_capacity      => col_chem%cation_exchange_capacity      , &
         aux_doubles                   => col_chem%aux_doubles                   , &
         aux_ints                      => col_chem%aux_ints                        &
         )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (L2E_STATE_SOIL_PH)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   cur_data%data_real_2d(c,j) = soil_ph(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_WATER_DENSITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   cur_data%data_real_2d(c,j) = water_density(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_AQUEOUS_PRESSURE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   cur_data%data_real_2d(c,j) = aqueous_pressure(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_TOTAL_MOBILE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_primary
                      cur_data%data_real_3d(c,j,k) = total_mobile(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

            case (L2E_STATE_FREE_MOBILE)
               do fc = 1, num_filter
                  c = filter(fc)
                  do j = 1, nlevsoi
                     do k = 1, alquimia_num_primary
                        cur_data%data_real_3d(c,j,k) = free_mobile(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_TOTAL_IMMOBILE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_primary
                      cur_data%data_real_3d(c,j,k) = total_immobile(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_MINERAL_VOLUME_FRACTION)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_minerals
                      cur_data%data_real_3d(c,j,k) = mineral_volume_fraction(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_MINERAL_SPECIFIC_SURFACE_AREA)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_minerals
                      cur_data%data_real_3d(c,j,k) = mineral_specific_surface_area(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_SURFACE_SITE_DENSITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_surface_sites
                      cur_data%data_real_3d(c,j,k) = surface_site_density(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_CATION_EXCHANGE_CAPACITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_ion_exchange_sites
                      cur_data%data_real_3d(c,j,k) = cation_exchange_capacity(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_AUX_DOUBLES)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_aux_doubles
                      cur_data%data_real_3d(c,j,k) = aux_doubles(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_STATE_AUX_INTS)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_aux_ints
                      cur_data%data_int_3d(c,j,k) = aux_ints(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMI_Pack_ChemStateType_at_Column_Level_for_EM

!-----------------------------------------------------------------------
  subroutine EMI_Unpack_ChemStateType_at_Column_Level_from_EM(data_list, em_stage, &
        num_filter, filter, col_chem)
    !
    ! !DESCRIPTION:
    ! Unpack data for ALM col_chem from EM
    !
    ! !USES:
    use elm_varpar             , only : nlevsoi
    use elm_varpar             , only : alquimia_num_primary
    use elm_varpar             , only : alquimia_num_minerals
    use elm_varpar             , only : alquimia_num_surface_sites
    use elm_varpar             , only : alquimia_num_ion_exchange_sites
    use elm_varpar             , only : alquimia_num_aux_doubles
    use elm_varpar             , only : alquimia_num_aux_ints
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)    , intent(in) :: data_list
    integer                 , intent(in) :: em_stage
    integer                 , intent(in) :: num_filter
    integer                 , intent(in) :: filter(:)
    type(column_chem_state) , intent(in) :: col_chem
    !
    ! !LOCAL_VARIABLES:
    integer                             :: fc,c,j,k
    class(emi_data), pointer            :: cur_data
    logical                             :: need_to_pack
    integer                             :: istage
    integer                             :: count

    associate(& 
         soil_ph                       => col_chem%soil_ph                       , &
         soil_salinity                 => col_chem%soil_salinity                 , &
         soil_O2                       => col_chem%soil_O2                       , &
         soil_sulfate                  => col_chem%soil_sulfate                  , &
         soil_sulfide                  => col_chem%soil_sulfide                  , &
         soil_Fe2                      => col_chem%soil_Fe2                      , &
         soil_FeOxide                  => col_chem%soil_FeOxide                  , &
         soil_FeS                      => col_chem%soil_FeS                      , &
         soil_acetate                  => col_chem%soil_acetate                  , &
         water_density                 => col_chem%water_density                 , &
         aqueous_pressure              => col_chem%aqueous_pressure              , &
         total_mobile                  => col_chem%total_mobile                  , &
         free_mobile                   => col_chem%free_mobile                   , &
         total_immobile                => col_chem%total_immobile                , &
         mineral_volume_fraction       => col_chem%mineral_volume_fraction       , &
         mineral_specific_surface_area => col_chem%mineral_specific_surface_area , &
         surface_site_density          => col_chem%surface_site_density          , &
         cation_exchange_capacity      => col_chem%cation_exchange_capacity      , &
         aux_doubles                   => col_chem%aux_doubles                   , &
         aux_ints                      => col_chem%aux_ints                      , &
         chem_dt                       => col_chem%chem_dt                         &
         )

    count = 0
    cur_data => data_list%first
    do
       if (.not.associated(cur_data)) exit
       count = count + 1

       need_to_pack = .false.
       do istage = 1, cur_data%num_em_stages
          if (cur_data%em_stage_ids(istage) == em_stage) then
             need_to_pack = .true.
             exit
          endif
       enddo

       if (need_to_pack) then

          select case (cur_data%id)

          case (E2L_STATE_SOIL_PH)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_ph(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_SALINITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_salinity(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_O2)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_O2(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_SULFATE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_sulfate(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_SULFIDE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_sulfide(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_FE2)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_Fe2(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_FE_OXIDE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_FeOxide(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_FE_SULFIDE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_FeS(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SOIL_ACETATE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   soil_acetate(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_WATER_DENSITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   water_density(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_AQUEOUS_PRESSURE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   aqueous_pressure(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_TOTAL_MOBILE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_primary
                      total_mobile(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

            case (E2L_STATE_FREE_MOBILE)
               do fc = 1, num_filter
                  c = filter(fc)
                  do j = 1, nlevsoi
                     do k = 1, alquimia_num_primary
                        free_mobile(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_TOTAL_IMMOBILE)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_primary
                      total_immobile(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_MINERAL_VOLUME_FRACTION)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_minerals
                      mineral_volume_fraction(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_MINERAL_SPECIFIC_SURFACE_AREA)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_minerals
                      mineral_specific_surface_area(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_SURFACE_SITE_DENSITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_surface_sites
                      surface_site_density(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_CATION_EXCHANGE_CAPACITY)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_ion_exchange_sites
                      cation_exchange_capacity(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_AUX_DOUBLES)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_aux_doubles
                      aux_doubles(c,j,k) = cur_data%data_real_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_AUX_INTS)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevsoi
                   do k = 1, alquimia_num_aux_ints
                      aux_ints(c,j,k) = cur_data%data_int_3d(c,j,k)
                   enddo
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_STATE_CHEM_DT)
             do fc = 1, num_filter
                c = filter(fc)
                chem_dt(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMI_Unpack_ChemStateType_at_Column_Level_from_EM


end module EMI_ChemStateType_ExchangeMod
