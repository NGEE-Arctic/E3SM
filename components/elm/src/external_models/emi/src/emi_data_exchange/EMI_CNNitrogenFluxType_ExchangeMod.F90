module EMI_CNNitrogenFluxType_ExchangeMod
  !
  use shr_kind_mod                          , only : r8 => shr_kind_r8
  use shr_log_mod                           , only : errMsg => shr_log_errMsg
  use abortutils                            , only : endrun
  use elm_varctl                            , only : iulog
  use EMI_DataMod                           , only : emi_data_list, emi_data
  use EMI_DataDimensionMod                  , only : emi_data_dimension_list_type
  use ColumnDataType       , only : column_nitrogen_flux
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
  public :: EMI_Pack_CNNitrogenFluxType_at_Column_Level_for_EM
  public :: EMI_Unpack_CNNitrogenFluxType_at_Column_Level_from_EM

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_Pack_CNNitrogenFluxType_at_Column_Level_for_EM(data_list, em_stage, &
        num_filter, filter, col_nf)
    !
    ! !DESCRIPTION:
    ! Pack data from ALM col_nf for EM
    !
    ! !USES:
    use elm_varpar             , only : nlevdecomp_full
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)       , intent(in) :: data_list
    integer                    , intent(in) :: em_stage
    integer                    , intent(in) :: num_filter
    integer                    , intent(in) :: filter(:)
    type(column_nitrogen_flux) , intent(in) :: col_nf
    !
    ! !LOCAL_VARIABLES:
    integer                             :: fc,c,j,k
    class(emi_data), pointer            :: cur_data
    logical                             :: need_to_pack
    integer                             :: istage
    integer                             :: count

    associate(& 
         actual_immob_vr    => col_nf%actual_immob_vr    , &
         potential_immob_vr => col_nf%potential_immob_vr , &
         gross_nmin_vr      => col_nf%gross_nmin_vr      , &
         plant_ndemand_vr   => col_nf%plant_ndemand_vr     &
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

          case (L2E_FLUX_NIMM_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   cur_data%data_real_2d(c,j) = actual_immob_vr(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_FLUX_NIMP_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   cur_data%data_real_2d(c,j) = potential_immob_vr(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_FLUX_NMIN_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   cur_data%data_real_2d(c,j) = gross_nmin_vr(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (L2E_FLUX_PLANT_NDEMAND_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   cur_data%data_real_2d(c,j) = plant_ndemand_vr(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMI_Pack_CNNitrogenFluxType_at_Column_Level_for_EM

!-----------------------------------------------------------------------
  subroutine EMI_Unpack_CNNitrogenFluxType_at_Column_Level_from_EM(data_list, em_stage, &
        num_filter, filter, col_nf)
    !
    ! !DESCRIPTION:
    ! Unpack data for ALM col_nf from EM
    !
    ! !USES:
    use elm_varpar             , only : nlevdecomp_full
    !
    implicit none
    !
    ! !ARGUMENTS:
    class(emi_data_list)       , intent(in) :: data_list
    integer                    , intent(in) :: em_stage
    integer                    , intent(in) :: num_filter
    integer                    , intent(in) :: filter(:)
    type(column_nitrogen_flux) , intent(in) :: col_nf
    !
    ! !LOCAL_VARIABLES:
    integer                             :: fc,c,j,k
    class(emi_data), pointer            :: cur_data
    logical                             :: need_to_pack
    integer                             :: istage
    integer                             :: count

    associate(& 
         actual_immob_vr      => col_nf%actual_immob_vr      , &
         potential_immob_vr   => col_nf%potential_immob_vr   , &
         gross_nmin_vr        => col_nf%gross_nmin_vr        , &
         sminn_to_plant_vr    => col_nf%sminn_to_plant_vr    , &
         smin_no3_to_plant_vr => col_nf%smin_no3_to_plant_vr , &
         smin_nh4_to_plant_vr => col_nf%smin_nh4_to_plant_vr , &
         smin_no3_runoff      => col_nf%smin_no3_runoff      , &
         DON_runoff           => col_nf%DON_runoff           , &
         f_n2o_soil           => col_nf%f_n2o_soil           , &
         f_n2_soil            => col_nf%f_n2_soil              &
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

          case (E2L_FLUX_NIMM_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   actual_immob_vr(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_NIMP_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   potential_immob_vr(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_NMIN_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   gross_nmin_vr(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_SMINN_TO_PLANT_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   sminn_to_plant_vr(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_SMIN_NO3_TO_PLANT_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   smin_no3_to_plant_vr(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_SMIN_NH4_TO_PLANT_VERTICALLY_RESOLVED)
             do fc = 1, num_filter
                c = filter(fc)
                do j = 1, nlevdecomp_full
                   smin_nh4_to_plant_vr(c,j) = cur_data%data_real_2d(c,j)
                enddo
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_NO3_RUNOFF)
             do fc = 1, num_filter
                c = filter(fc)
                smin_no3_runoff(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_DON_RUNOFF)
             do fc = 1, num_filter
                c = filter(fc)
                DON_runoff(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_N2O)
             do fc = 1, num_filter
                c = filter(fc)
                f_n2o_soil(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          case (E2L_FLUX_N2)
             do fc = 1, num_filter
                c = filter(fc)
                f_n2_soil(c) = cur_data%data_real_1d(c)
             enddo
             cur_data%is_set = .true.

          end select

       endif

       cur_data => cur_data%next
    enddo

    end associate

  end subroutine EMI_Unpack_CNNitrogenFluxType_at_Column_Level_from_EM


end module EMI_CNNitrogenFluxType_ExchangeMod
