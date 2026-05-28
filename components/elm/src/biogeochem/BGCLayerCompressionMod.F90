module BGCLayerCompressionMod

  !-----------------------------------------------------------------------
  ! !MODULE: BGCLayerCompressionMod
  !
  ! !DESCRIPTION:
  ! Adjusts biogeochemistry pool concentrations when soil layers compress
  ! due to excess ice melt (thermokarst). Physical basis: mass =
  ! concentration × volume. When volume decreases by compression factor
  ! volrat = dz_old/dz_new, concentration must increase by volrat to
  ! conserve mass.
  !
  ! Called immediately after SoilLittVertTransp in EcosystemDynMod, so
  ! vertical transport operates with physically correct (compressed)
  ! geometry, then concentrations are corrected before downstream BGC
  ! processes access the pools.
  !
  ! !USES:
  use shr_kind_mod        , only : r8 => shr_kind_r8
  use shr_log_mod         , only : errMsg => shr_log_errMsg
  use elm_varctl          , only : iulog, use_c13, use_c14
  use elm_varpar          , only : nlevgrnd, ndecomp_pools
  use decompMod           , only : bounds_type
  use abortutils          , only : endrun
  use ColumnType          , only : col_pp
  use LandunitType        , only : lun_pp
  use landunit_varcon     , only : istsoil
  use ColumnDataType      , only : col_cs, c13_col_cs, c14_col_cs
  use ColumnDataType      , only : col_ns, col_ps
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: adjust_bgc_for_layer_compression
  !
  ! !REVISION HISTORY:
  ! 2026-05-28: R. Fiorella - created to centralize BGC concentration
  !             adjustment for layer compression
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine adjust_bgc_for_layer_compression(bounds)
    !
    ! !DESCRIPTION:
    ! Adjust all BGC pool concentrations when soil layers compress due to
    ! excess ice melt. Concentration must increase proportional to volume
    ! decrease to conserve mass.
    !
    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer  :: c, j, k, l
    real(r8) :: volrat_layer
    !-----------------------------------------------------------------------

    ! Loop over all columns
    do c = bounds%begc, bounds%endc

       ! Only process active soil columns
       if (.not. col_pp%active(c)) cycle

       l = col_pp%landunit(c)
       if (lun_pp%itype(l) /= istsoil) cycle

       ! Loop over soil layers
       do j = 1, nlevgrnd

          ! Check if layer compressed this timestep
          volrat_layer = col_pp%volrat(c,j)

          if (abs(volrat_layer - 1.0_r8) > 1.e-10_r8) then

             ! ========== CARBON POOLS ==========
             ! Decomposition pools
             do k = 1, ndecomp_pools
                col_cs%decomp_cpools_vr_col(c,j,k) = &
                     col_cs%decomp_cpools_vr_col(c,j,k) * volrat_layer
             end do

             ! Truncation pool
             col_cs%ctrunc_vr_col(c,j) = &
                  col_cs%ctrunc_vr_col(c,j) * volrat_layer

             ! C13 pools (if enabled)
             if (use_c13) then
                do k = 1, ndecomp_pools
                   c13_col_cs%decomp_cpools_vr_col(c,j,k) = &
                        c13_col_cs%decomp_cpools_vr_col(c,j,k) * volrat_layer
                end do
                c13_col_cs%ctrunc_vr_col(c,j) = &
                     c13_col_cs%ctrunc_vr_col(c,j) * volrat_layer
             end if

             ! C14 pools (if enabled)
             if (use_c14) then
                do k = 1, ndecomp_pools
                   c14_col_cs%decomp_cpools_vr_col(c,j,k) = &
                        c14_col_cs%decomp_cpools_vr_col(c,j,k) * volrat_layer
                end do
                c14_col_cs%ctrunc_vr_col(c,j) = &
                     c14_col_cs%ctrunc_vr_col(c,j) * volrat_layer
             end if

             ! ========== NITROGEN POOLS ==========
             ! Decomposition pools
             do k = 1, ndecomp_pools
                col_ns%decomp_npools_vr_col(c,j,k) = &
                     col_ns%decomp_npools_vr_col(c,j,k) * volrat_layer
             end do

             ! Mineral nitrogen pools
             col_ns%sminn_vr_col(c,j) = &
                  col_ns%sminn_vr_col(c,j) * volrat_layer
             col_ns%smin_nh4_vr_col(c,j) = &
                  col_ns%smin_nh4_vr_col(c,j) * volrat_layer
             col_ns%smin_no3_vr_col(c,j) = &
                  col_ns%smin_no3_vr_col(c,j) * volrat_layer

             ! Truncation pool
             col_ns%ntrunc_vr_col(c,j) = &
                  col_ns%ntrunc_vr_col(c,j) * volrat_layer

             ! ========== PHOSPHORUS POOLS ==========
             ! Decomposition pools
             do k = 1, ndecomp_pools
                col_ps%decomp_ppools_vr_col(c,j,k) = &
                     col_ps%decomp_ppools_vr_col(c,j,k) * volrat_layer
             end do

             ! Mineral phosphorus pools
             col_ps%solutionp_vr_col(c,j) = &
                  col_ps%solutionp_vr_col(c,j) * volrat_layer
             col_ps%labilep_vr_col(c,j) = &
                  col_ps%labilep_vr_col(c,j) * volrat_layer
             col_ps%secondp_vr_col(c,j) = &
                  col_ps%secondp_vr_col(c,j) * volrat_layer
             col_ps%occlp_vr_col(c,j) = &
                  col_ps%occlp_vr_col(c,j) * volrat_layer
             col_ps%primp_vr_col(c,j) = &
                  col_ps%primp_vr_col(c,j) * volrat_layer

             ! Truncation pool
             col_ps%ptrunc_vr_col(c,j) = &
                  col_ps%ptrunc_vr_col(c,j) * volrat_layer

             ! Reset volrat after adjustment
             col_pp%volrat(c,j) = 1.0_r8

          end if  ! volrat != 1

       end do  ! layer loop

    end do  ! column loop

  end subroutine adjust_bgc_for_layer_compression

end module BGCLayerCompressionMod
