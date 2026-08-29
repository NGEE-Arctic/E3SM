module CNNitrogenStateType

#include "shr_assert.h"

  use shr_kind_mod           , only : r8 => shr_kind_r8
  use shr_infnan_mod         , only : nan => shr_infnan_nan, assignment(=)
  use shr_log_mod            , only : errMsg => shr_log_errMsg
  use clm_varpar             , only : ndecomp_cascade_transitions, ndecomp_pools, nlevcan
  use clm_varpar             , only : nlevdecomp_full, nlevdecomp, crop_prog
  use clm_varcon             , only : spval!, ispval, dzsoi_decomp, zisoi
  use landunit_varcon        , only : istcrop, istsoil 
  use clm_varctl             , only : use_nitrif_denitrif, use_vertsoilc, use_century_decomp
  use clm_varctl             , only : iulog, override_bgc_restart_mismatch_dump, spinup_state
  use decompMod              , only : bounds_type
  ! use pftvarcon              , only : npcropmin, nstor
  use CNDecompCascadeConType , only : decomp_cascade_con
  ! use VegetationPropertiesType         , only : veg_vp
  use abortutils             , only : endrun
  use spmdMod                , only : masterproc 
  use LandunitType           , only : lun_pp                
  use ColumnType             , only : col_pp                
  ! use VegetationType         , only : veg_pp
  ! use clm_varctl             , only : use_pflotran, pf_cmode
  ! use clm_varctl             , only : nu_com, use_crop
  ! use dynPatchStateUpdaterMod, only : patch_state_updater_type               
  ! use SpeciesMod           , only : CN_SPECIES_N
  ! 
  ! !PUBLIC TYPES:
  implicit none
  save
  private

  real(r8) , parameter :: npool_seed_param     = 0.1_r8

  type, public :: nitrogenstate_type

     real(r8), pointer :: grainn_patch                 (:)     ! patch (gN/m2) grain N (crop)
     real(r8), pointer :: grainn_storage_patch         (:)     ! patch (gN/m2) grain N storage (crop)
     real(r8), pointer :: grainn_xfer_patch            (:)     ! patch (gN/m2) grain N transfer (crop)
     real(r8), pointer :: leafn_patch                  (:)     ! patch (gN/m2) leaf N 
     real(r8), pointer :: leafn_storage_patch          (:)     ! patch (gN/m2) leaf N storage
     real(r8), pointer :: leafn_xfer_patch             (:)     ! patch (gN/m2) leaf N transfer
     real(r8), pointer :: frootn_patch                 (:)     ! patch (gN/m2) fine root N
     real(r8), pointer :: frootn_storage_patch         (:)     ! patch (gN/m2) fine root N storage
     real(r8), pointer :: frootn_xfer_patch            (:)     ! patch (gN/m2) fine root N transfer
     real(r8), pointer :: livestemn_patch              (:)     ! patch (gN/m2) live stem N
     real(r8), pointer :: livestemn_storage_patch      (:)     ! patch (gN/m2) live stem N storage
     real(r8), pointer :: livestemn_xfer_patch         (:)     ! patch (gN/m2) live stem N transfer
     real(r8), pointer :: deadstemn_patch              (:)     ! patch (gN/m2) dead stem N
     real(r8), pointer :: deadstemn_storage_patch      (:)     ! patch (gN/m2) dead stem N storage
     real(r8), pointer :: deadstemn_xfer_patch         (:)     ! patch (gN/m2) dead stem N transfer
     real(r8), pointer :: livecrootn_patch             (:)     ! patch (gN/m2) live coarse root N
     real(r8), pointer :: livecrootn_storage_patch     (:)     ! patch (gN/m2) live coarse root N storage
     real(r8), pointer :: livecrootn_xfer_patch        (:)     ! patch (gN/m2) live coarse root N transfer
     real(r8), pointer :: deadcrootn_patch             (:)     ! patch (gN/m2) dead coarse root N
     real(r8), pointer :: deadcrootn_storage_patch     (:)     ! patch (gN/m2) dead coarse root N storage
     real(r8), pointer :: deadcrootn_xfer_patch        (:)     ! patch (gN/m2) dead coarse root N transfer
     real(r8), pointer :: retransn_patch               (:)     ! patch (gN/m2) plant pool of retranslocated N
     real(r8), pointer :: npool_patch                  (:)     ! patch (gN/m2) temporary plant N pool
     real(r8), pointer :: ntrunc_patch                 (:)     ! patch (gN/m2) pft-level sink for N truncation
     real(r8), pointer :: plant_n_buffer_patch         (:)     ! patch (gN/m2) pft-level abstract N storage
     real(r8), pointer :: plant_n_buffer_col           (:)     ! patch (gN/m2) col-level abstract N storage
     real(r8), pointer :: decomp_npools_vr_col         (:,:,:) ! col (gN/m3) vertically-resolved decomposing (litter, cwd, soil) N pools
     real(r8), pointer :: sminn_vr_col                 (:,:)   ! col (gN/m3) vertically-resolved soil mineral N
     real(r8), pointer :: ntrunc_vr_col                (:,:)   ! col (gN/m3) vertically-resolved column-level sink for N truncation

     ! NITRIF_DENITRIF
     real(r8), pointer :: smin_no3_vr_col              (:,:)   ! col (gN/m3) vertically-resolved soil mineral NO3
     real(r8), pointer :: smin_no3_col                 (:)     ! col (gN/m2) soil mineral NO3 pool
     real(r8), pointer :: smin_nh4_vr_col              (:,:)   ! col (gN/m3) vertically-resolved soil mineral NH4
     real(r8), pointer :: smin_nh4_col                 (:)     ! col (gN/m2) soil mineral NH4 pool

     ! wood product pools, for dynamic landcover
     real(r8), pointer :: cropseedn_deficit_patch      (:)     ! (gN/m2) pool for seeding new crop growth; this is a NEGATIVE term, indicating the amount of seed usage that needs to be repaid     
     real(r8), pointer :: seedn_grc                    (:)     ! (gN/m2) gridcell-level pool for seeding new PFTs via dynamic landcover
     real(r8), pointer :: seedn_col                    (:)     ! col (gN/m2) column-level pool for seeding new Patches
     real(r8), pointer :: prod1n_col                   (:)     ! col (gN/m2) crop product N pool, 1-year lifespan
     real(r8), pointer :: prod10n_col                  (:)     ! col (gN/m2) wood product N pool, 10-year lifespan
     real(r8), pointer :: prod100n_col                 (:)     ! col (gN/m2) wood product N pool, 100-year lifespan
     real(r8), pointer :: totprodn_col                 (:)     ! col (gN/m2) total wood product N
     real(r8), pointer :: dyn_nbal_adjustments_col     (:)     ! (gN/m2) adjustments to each column made in this timestep via dynamic column area adjustments

     ! summary (diagnostic) state variables, not involved in mass balance
     real(r8), pointer :: dispvegn_patch               (:)     ! patch (gN/m2) displayed veg nitrogen, excluding storage
     real(r8), pointer :: storvegn_patch               (:)     ! patch (gN/m2) stored vegetation nitrogen
     real(r8), pointer :: totvegn_patch                (:)     ! patch (gN/m2) total vegetation nitrogen
     real(r8), pointer :: totpftn_patch                (:)     ! patch (gN/m2) total pft-level nitrogen
     real(r8), pointer :: decomp_npools_col            (:,:)   ! col (gN/m2)  decomposing (litter, cwd, soil) N pools
     real(r8), pointer :: decomp_npools_1m_col         (:,:)   ! col (gN/m2)  diagnostic: decomposing (litter, cwd, soil) N pools to 1 meter
     real(r8), pointer :: sminn_col                    (:)     ! col (gN/m2) soil mineral N
     real(r8), pointer :: ntrunc_col                   (:)     ! col (gN/m2) column-level sink for N truncation
     real(r8), pointer :: cwdn_col                     (:)     ! col (gN/m2) Diagnostic: coarse woody debris N
     real(r8), pointer :: totlitn_col                  (:)     ! col (gN/m2) total litter nitrogen
     real(r8), pointer :: totsomn_col                  (:)     ! col (gN/m2) total soil organic matter nitrogen
     real(r8), pointer :: totlitn_1m_col               (:)     ! col (gN/m2) total litter nitrogen to 1 meter
     real(r8), pointer :: totsomn_1m_col               (:)     ! col (gN/m2) total soil organic matter nitrogen to 1 meter
     real(r8), pointer :: totecosysn_col               (:)     ! col (gN/m2) total ecosystem nitrogen, incl veg 
     real(r8), pointer :: totcoln_col                  (:)     ! col (gN/m2) total column nitrogen, incl veg
     real(r8), pointer :: totabgn_col                  (:)     ! col (gN/m2)
     real(r8), pointer :: totblgn_col                  (:)     ! col (gN/m2) total below ground nitrogen
     ! patch averaged to column variables 
     real(r8), pointer :: totvegn_col                  (:)     ! col (gN/m2) total vegetation nitrogen (p2c)
     real(r8), pointer :: totpftn_col                  (:)     ! col (gN/m2) total pft-level nitrogen (p2c)

     ! col balance checks
     real(r8), pointer :: begnb_patch                  (:)     ! patch nitrogen mass, beginning of time step (gN/m**2)
     real(r8), pointer :: endnb_patch                  (:)     ! patch nitrogen mass, end of time step (gN/m**2)
     real(r8), pointer :: errnb_patch                  (:)     ! patch nitrogen balance error for the timestep (gN/m**2)
     real(r8), pointer :: begnb_col                    (:)     ! col nitrogen mass, beginning of time step (gN/m**2)
     real(r8), pointer :: endnb_col                    (:)     ! col nitrogen mass, end of time step (gN/m**2)
     real(r8), pointer :: errnb_col                    (:)     ! colnitrogen balance error for the timestep (gN/m**2)
     real(r8), pointer :: begnb_grc                    (:)     ! grid cell nitrogen mass, beginning of time step (gN/m**2)
     real(r8), pointer :: endnb_grc                    (:)     ! grid cell nitrogen mass, end of time step (gN/m**2)
     real(r8), pointer :: errnb_grc                    (:)     ! grid cell nitrogen balance error for the timestep (gN/m**2)

     ! for newly-added coupled codes with pflotran (it should be included in total 'sminn' defined above when doing summation)
     real(r8), pointer :: smin_nh4sorb_vr_col          (:,:)   ! col (gN/m3) vertically-resolved soil mineral NH4 absorbed
     real(r8), pointer :: smin_nh4sorb_col             (:)     ! col (gN/m2) soil mineral NH4 pool absorbed

     real(r8), pointer :: plant_nbuffer_col            (:)     ! col plant nitrogen buffer, (gN/m2), used to exchange info with betr 

     real(r8), pointer :: totpftn_beg_col              (:)
     real(r8), pointer :: cwdn_beg_col                 (:)
     real(r8), pointer :: totlitn_beg_col              (:)
     real(r8), pointer :: totsomn_beg_col              (:)
     real(r8), pointer :: sminn_beg_col                (:)
     real(r8), pointer :: smin_no3_beg_col             (:)
     real(r8), pointer :: smin_nh4_beg_col             (:)
     real(r8), pointer :: totprodn_beg_col             (:)
     real(r8), pointer :: seedn_beg_col                (:)
     real(r8), pointer :: ntrunc_beg_col               (:)

     
     real(r8), pointer :: totpftn_end_col              (:)
     real(r8), pointer :: cwdn_end_col                 (:)
     real(r8), pointer :: totlitn_end_col              (:)
     real(r8), pointer :: totsomn_end_col              (:)
     real(r8), pointer :: sminn_end_col                (:)
     real(r8), pointer :: smin_no3_end_col             (:)
     real(r8), pointer :: smin_nh4_end_col             (:)
     real(r8), pointer :: totprodn_end_col             (:)
     real(r8), pointer :: seedn_end_col                (:)
     real(r8), pointer :: ntrunc_end_col               (:)

     ! for dynamic C/N/P allocation cost-benefit analysis
     real(r8), pointer :: npimbalance_patch                         (:)
     real(r8), pointer :: pnup_pfrootc_patch                        (:)
     real(r8), pointer :: ppup_pfrootc_patch                        (:)
     real(r8), pointer :: ptlai_pleafc_patch                        (:)
     
     real(r8), pointer :: ppsnsun_ptlai_patch                       (:)
     real(r8), pointer :: ppsnsun_pleafn_patch                      (:)
     real(r8), pointer :: ppsnsun_pleafp_patch                      (:)
     
     real(r8), pointer :: plmrsun_ptlai_patch                       (:)
     real(r8), pointer :: plmrsun_pleafn_patch                      (:)
     real(r8), pointer :: plaisun_ptlai_patch                       (:)
     
     real(r8), pointer :: ppsnsha_ptlai_patch                       (:)
     real(r8), pointer :: ppsnsha_pleafn_patch                      (:)
     real(r8), pointer :: ppsnsha_pleafp_patch                      (:)
     
     real(r8), pointer :: plmrsha_ptlai_patch                       (:)
     real(r8), pointer :: plmrsha_pleafn_patch                      (:)
     real(r8), pointer :: plaisha_ptlai_patch                       (:)
     
     real(r8), pointer :: benefit_pgpp_pleafc_patch                 (:)     ! partial gpp / partial leaf carbon (used by symbiotic n2 fixation and dynamic allocation)
     real(r8), pointer :: benefit_pgpp_pleafn_patch                 (:)     ! partial gpp / partial leaf nitrogen (used by phosphatase activity and dynamic allocation)
     real(r8), pointer :: benefit_pgpp_pleafp_patch                 (:)     ! partial gpp / partial leaf phosphorus (used by phosphatase activity and dynamic allocation)
     real(r8), pointer :: cost_pgpp_pfrootc_patch                   (:)     ! partial gpp /  partial fine root carbon (used by dynamic allocation)
     real(r8), pointer :: cost_plmr_pleafc_patch                    (:)     ! partial maintenance respiration /  partial leaf carbon (used by dynamic allocation)
     real(r8), pointer :: cost_plmr_pleafn_patch                    (:)     ! partial maintenance respiration /  partial leaf nitrogen (used by dynamic allocation)
     
     real(r8), pointer :: ppsn_ptlai_z                              (:,:)
     real(r8), pointer :: ppsn_pleafn_z                             (:,:)
     real(r8), pointer :: ppsn_pleafp_z                             (:,:)
     
     real(r8), pointer :: ppsn_ptlai_z_vcmax                        (:,:)
     real(r8), pointer :: ppsn_pleafn_z_vcmax                       (:,:)
     real(r8), pointer :: ppsn_pleafp_z_vcmax                       (:,:)
     
     real(r8), pointer :: ppsn_ptlai_z_jmax                         (:,:)
     real(r8), pointer :: ppsn_pleafn_z_jmax                        (:,:)
     real(r8), pointer :: ppsn_pleafp_z_jmax                        (:,:)
     
     real(r8), pointer :: ppsn_ptlai_z_tpu                          (:,:)
     real(r8), pointer :: ppsn_pleafn_z_tpu                         (:,:)
     real(r8), pointer :: ppsn_pleafp_z_tpu                         (:,:)
    
     real(r8), pointer :: plmr_ptlai_z                              (:,:)
     real(r8), pointer :: plmr_pleafn_z                             (:,:)

   contains

     procedure , public  :: Init   
     ! procedure , public  :: Restart
     ! procedure , public  :: SetValues
     ! procedure , public  :: ZeroDWT
     ! procedure , public  :: Summary
     procedure , private :: InitAllocate 
     ! procedure , private :: InitHistory  
     ! procedure , private :: InitCold     

  end type nitrogenstate_type
  !------------------------------------------------------------------------

contains

  !------------------------------------------------------------------------
  subroutine Init(this, bounds)

    class(nitrogenstate_type)         :: this
    type(bounds_type) , intent(in)    :: bounds  
    ! real(r8)          , intent(in)    :: leafc_patch          (bounds%begp:)
    ! real(r8)          , intent(in)    :: leafc_storage_patch  (bounds%begp:)
    ! real(r8)          , intent(in)    :: frootc_patch         (bounds%begp:)
    ! real(r8)          , intent(in)    :: frootc_storage_patch (bounds%begp:)
    ! real(r8)          , intent(in)    :: deadstemc_patch      (bounds%begp:)
    ! real(r8)          , intent(in)    :: decomp_cpools_vr_col (bounds%begc:, 1:, 1:)
    ! real(r8)          , intent(in)    :: decomp_cpools_col    (bounds%begc:, 1:)
    ! real(r8)          , intent(in)    :: decomp_cpools_1m_col (bounds%begc:, 1:)

    call this%InitAllocate (bounds )

    ! call this%InitHistory (bounds)
    ! 
    ! call this%InitCold ( bounds, leafc_patch, leafc_storage_patch, &
    !      frootc_patch, frootc_storage_patch, deadstemc_patch, &
    !      decomp_cpools_vr_col, decomp_cpools_col, decomp_cpools_1m_col)

  end subroutine Init

  !------------------------------------------------------------------------
  subroutine InitAllocate(this, bounds)
    !
    ! !ARGUMENTS:
    class (nitrogenstate_type) :: this
    type(bounds_type) , intent(in) :: bounds  
    !
    ! !LOCAL VARIABLES:
    integer           :: begp,endp
    integer           :: begc,endc
    integer           :: begg,endg
    !------------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp
    begc = bounds%begc; endc = bounds%endc
    begg = bounds%begg; endg = bounds%endg

    allocate(this%grainn_patch             (begp:endp))                   ; this%grainn_patch             (:)   = nan     
    allocate(this%grainn_storage_patch     (begp:endp))                   ; this%grainn_storage_patch     (:)   = nan
    allocate(this%grainn_xfer_patch        (begp:endp))                   ; this%grainn_xfer_patch        (:)   = nan     
    allocate(this%leafn_patch              (begp:endp))                   ; this%leafn_patch              (:)   = nan
    allocate(this%leafn_storage_patch      (begp:endp))                   ; this%leafn_storage_patch      (:)   = nan     
    allocate(this%leafn_xfer_patch         (begp:endp))                   ; this%leafn_xfer_patch         (:)   = nan     
    allocate(this%frootn_patch             (begp:endp))                   ; this%frootn_patch             (:)   = nan
    allocate(this%frootn_storage_patch     (begp:endp))                   ; this%frootn_storage_patch     (:)   = nan     
    allocate(this%frootn_xfer_patch        (begp:endp))                   ; this%frootn_xfer_patch        (:)   = nan     
    allocate(this%livestemn_patch          (begp:endp))                   ; this%livestemn_patch          (:)   = nan
    allocate(this%livestemn_storage_patch  (begp:endp))                   ; this%livestemn_storage_patch  (:)   = nan
    allocate(this%livestemn_xfer_patch     (begp:endp))                   ; this%livestemn_xfer_patch     (:)   = nan
    allocate(this%deadstemn_patch          (begp:endp))                   ; this%deadstemn_patch          (:)   = nan
    allocate(this%deadstemn_storage_patch  (begp:endp))                   ; this%deadstemn_storage_patch  (:)   = nan
    allocate(this%deadstemn_xfer_patch     (begp:endp))                   ; this%deadstemn_xfer_patch     (:)   = nan
    allocate(this%livecrootn_patch         (begp:endp))                   ; this%livecrootn_patch         (:)   = nan
    allocate(this%livecrootn_storage_patch (begp:endp))                   ; this%livecrootn_storage_patch (:)   = nan
    allocate(this%livecrootn_xfer_patch    (begp:endp))                   ; this%livecrootn_xfer_patch    (:)   = nan
    allocate(this%deadcrootn_patch         (begp:endp))                   ; this%deadcrootn_patch         (:)   = nan
    allocate(this%deadcrootn_storage_patch (begp:endp))                   ; this%deadcrootn_storage_patch (:)   = nan
    allocate(this%deadcrootn_xfer_patch    (begp:endp))                   ; this%deadcrootn_xfer_patch    (:)   = nan
    allocate(this%retransn_patch           (begp:endp))                   ; this%retransn_patch           (:)   = nan
    allocate(this%npool_patch              (begp:endp))                   ; this%npool_patch              (:)   = nan
    allocate(this%ntrunc_patch             (begp:endp))                   ; this%ntrunc_patch             (:)   = nan
    allocate(this%dispvegn_patch           (begp:endp))                   ; this%dispvegn_patch           (:)   = nan
    allocate(this%storvegn_patch           (begp:endp))                   ; this%storvegn_patch           (:)   = nan
    allocate(this%totvegn_patch            (begp:endp))                   ; this%totvegn_patch            (:)   = nan
    allocate(this%totpftn_patch            (begp:endp))                   ; this%totpftn_patch            (:)   = nan
    allocate(this%plant_n_buffer_patch    (begp:endp))                    ; this%plant_n_buffer_patch     (:)   = nan
    allocate(this%plant_n_buffer_col    (begc:endc))                      ; this%plant_n_buffer_col       (:)   = nan
    allocate(this%sminn_vr_col             (begc:endc,1:nlevdecomp_full)) ; this%sminn_vr_col             (:,:) = nan
    allocate(this%ntrunc_vr_col            (begc:endc,1:nlevdecomp_full)) ; this%ntrunc_vr_col            (:,:) = nan
    allocate(this%smin_no3_vr_col          (begc:endc,1:nlevdecomp_full)) ; this%smin_no3_vr_col          (:,:) = nan
    allocate(this%smin_nh4_vr_col          (begc:endc,1:nlevdecomp_full)) ; this%smin_nh4_vr_col          (:,:) = nan
    allocate(this%smin_no3_col             (begc:endc))                   ; this%smin_no3_col             (:)   = nan
    allocate(this%smin_nh4_col             (begc:endc))                   ; this%smin_nh4_col             (:)   = nan
    allocate(this%cwdn_col                 (begc:endc))                   ; this%cwdn_col                 (:)   = nan
    allocate(this%sminn_col                (begc:endc))                   ; this%sminn_col                (:)   = nan
    allocate(this%ntrunc_col               (begc:endc))                   ; this%ntrunc_col               (:)   = nan

    allocate(this%cropseedn_deficit_patch  (begp:endp))                   ; this%cropseedn_deficit_patch  (:)   = nan
    allocate(this%seedn_grc                (begg:endg))                   ; this%seedn_grc                (:)   = nan
    allocate(this%seedn_col                (begc:endc))                   ; this%seedn_col                (:)   = nan
    allocate(this%prod1n_col               (begc:endc))                   ; this%prod1n_col               (:)   = nan
    allocate(this%prod10n_col              (begc:endc))                   ; this%prod10n_col              (:)   = nan
    allocate(this%prod100n_col             (begc:endc))                   ; this%prod100n_col             (:)   = nan
    allocate(this%totprodn_col             (begc:endc))                   ; this%totprodn_col             (:)   = nan
    allocate(this%dyn_nbal_adjustments_col (begc:endc))                   ; this%dyn_nbal_adjustments_col (:)   = nan
    allocate(this%totlitn_col              (begc:endc))                   ; this%totlitn_col              (:)   = nan
    allocate(this%totsomn_col              (begc:endc))                   ; this%totsomn_col              (:)   = nan
    allocate(this%totlitn_1m_col           (begc:endc))                   ; this%totlitn_1m_col           (:)   = nan
    allocate(this%totsomn_1m_col           (begc:endc))                   ; this%totsomn_1m_col           (:)   = nan
    allocate(this%totecosysn_col           (begc:endc))                   ; this%totecosysn_col           (:)   = nan
    allocate(this%totcoln_col              (begc:endc))                   ; this%totcoln_col              (:)   = nan
    allocate(this%decomp_npools_col        (begc:endc,1:ndecomp_pools))   ; this%decomp_npools_col        (:,:) = nan
    allocate(this%decomp_npools_1m_col     (begc:endc,1:ndecomp_pools))   ; this%decomp_npools_1m_col     (:,:) = nan
    allocate(this%totpftn_col              (begc:endc))                   ; this%totpftn_col              (:)   = nan
    allocate(this%totvegn_col              (begc:endc))                   ; this%totvegn_col              (:)   = nan
    allocate(this%totabgn_col              (begc:endc))                   ; this%totabgn_col              (:)   = nan
    allocate(this%totblgn_col              (begc:endc))                   ; this%totblgn_col              (:)   = nan
    allocate(this%decomp_npools_vr_col(begc:endc,1:nlevdecomp_full,1:ndecomp_pools));
    this%decomp_npools_vr_col(:,:,:)= nan

    allocate(this%begnb_patch (begp:endp));     this%begnb_patch (:) =nan
    allocate(this%begnb_col   (begc:endc));     this%begnb_col   (:) =nan
    allocate(this%endnb_patch (begp:endp));     this%endnb_patch (:) =nan
    allocate(this%endnb_col   (begc:endc));     this%endnb_col   (:) =nan
    allocate(this%errnb_patch (begp:endp));     this%errnb_patch (:) =nan
    allocate(this%errnb_col   (begc:endc));     this%errnb_col   (:) =nan 

    allocate(this%begnb_grc   (begg:endg));     this%begnb_grc   (:) =nan
    allocate(this%endnb_grc   (begg:endg));     this%endnb_grc   (:) =nan
    allocate(this%errnb_grc   (begg:endg));     this%errnb_grc   (:) =nan
    
    allocate(this%totpftn_beg_col     (begc:endc))   ; this%totpftn_beg_col     (:) = nan
    allocate(this%cwdn_beg_col        (begc:endc))   ; this%cwdn_beg_col        (:) = nan
    allocate(this%totlitn_beg_col     (begc:endc))   ; this%totlitn_beg_col     (:) = nan
    allocate(this%totsomn_beg_col     (begc:endc))   ; this%totsomn_beg_col     (:) = nan
    allocate(this%sminn_beg_col       (begc:endc))   ; this%sminn_beg_col       (:) = nan
    allocate(this%smin_no3_beg_col    (begc:endc))   ; this%smin_no3_beg_col    (:) = nan
    allocate(this%smin_nh4_beg_col    (begc:endc))   ; this%smin_nh4_beg_col    (:) = nan
    allocate(this%totprodn_beg_col    (begc:endc))   ; this%totprodn_beg_col    (:) = nan
    allocate(this%seedn_beg_col       (begc:endc))   ; this%seedn_beg_col       (:) = nan
    allocate(this%ntrunc_beg_col      (begc:endc))   ; this%ntrunc_beg_col      (:) = nan
    
    allocate(this%totpftn_end_col     (begc:endc))   ; this%totpftn_end_col     (:) = nan
    allocate(this%cwdn_end_col        (begc:endc))   ; this%cwdn_end_col        (:) = nan
    allocate(this%totlitn_end_col     (begc:endc))   ; this%totlitn_end_col     (:) = nan
    allocate(this%totsomn_end_col     (begc:endc))   ; this%totsomn_end_col     (:) = nan
    allocate(this%sminn_end_col       (begc:endc))   ; this%sminn_end_col       (:) = nan
    allocate(this%smin_no3_end_col    (begc:endc))   ; this%smin_no3_end_col    (:) = nan
    allocate(this%smin_nh4_end_col    (begc:endc))   ; this%smin_nh4_end_col    (:) = nan
    allocate(this%totprodn_end_col    (begc:endc))   ; this%totprodn_end_col    (:) = nan
    allocate(this%seedn_end_col       (begc:endc))   ; this%seedn_end_col       (:) = nan
    allocate(this%ntrunc_end_col      (begc:endc))   ; this%ntrunc_end_col      (:) = nan

    ! for dynamic C/N/P allocation
    allocate(this%npimbalance_patch           (begp:endp)) ;             this%npimbalance_patch           (:) = nan
    allocate(this%pnup_pfrootc_patch          (begp:endp)) ;             this%pnup_pfrootc_patch          (:) = nan
    allocate(this%ppup_pfrootc_patch          (begp:endp)) ;             this%ppup_pfrootc_patch          (:) = nan
    allocate(this%ptlai_pleafc_patch          (begp:endp)) ;             this%ptlai_pleafc_patch          (:) = nan
    allocate(this%ppsnsun_ptlai_patch         (begp:endp)) ;             this%ppsnsun_ptlai_patch         (:) = nan
    allocate(this%ppsnsun_pleafn_patch        (begp:endp)) ;             this%ppsnsun_pleafn_patch        (:) = nan
    allocate(this%ppsnsun_pleafp_patch        (begp:endp)) ;             this%ppsnsun_pleafp_patch        (:) = nan
    allocate(this%plmrsun_ptlai_patch         (begp:endp)) ;             this%plmrsun_ptlai_patch         (:) = nan
    allocate(this%plmrsun_pleafn_patch        (begp:endp)) ;             this%plmrsun_pleafn_patch        (:) = nan
    allocate(this%plaisun_ptlai_patch         (begp:endp)) ;             this%plaisun_ptlai_patch         (:) = nan
    allocate(this%ppsnsha_ptlai_patch         (begp:endp)) ;             this%ppsnsha_ptlai_patch         (:) = nan
    allocate(this%ppsnsha_pleafn_patch        (begp:endp)) ;             this%ppsnsha_pleafn_patch        (:) = nan
    allocate(this%ppsnsha_pleafp_patch        (begp:endp)) ;             this%ppsnsha_pleafp_patch        (:) = nan
    allocate(this%plmrsha_ptlai_patch         (begp:endp)) ;             this%plmrsha_ptlai_patch         (:) = nan
    allocate(this%plmrsha_pleafn_patch        (begp:endp)) ;             this%plmrsha_pleafn_patch        (:) = nan
    allocate(this%plaisha_ptlai_patch         (begp:endp)) ;             this%plaisha_ptlai_patch         (:) = nan
    allocate(this%benefit_pgpp_pleafc_patch   (begp:endp)) ;             this%benefit_pgpp_pleafc_patch   (:) = nan
    allocate(this%benefit_pgpp_pleafn_patch   (begp:endp)) ;             this%benefit_pgpp_pleafn_patch   (:) = nan
    allocate(this%benefit_pgpp_pleafp_patch   (begp:endp)) ;             this%benefit_pgpp_pleafp_patch   (:) = nan
    allocate(this%cost_pgpp_pfrootc_patch     (begp:endp)) ;             this%cost_pgpp_pfrootc_patch     (:) = nan
    allocate(this%cost_plmr_pleafc_patch      (begp:endp)) ;             this%cost_plmr_pleafc_patch      (:) = nan
    allocate(this%cost_plmr_pleafn_patch      (begp:endp)) ;             this%cost_plmr_pleafn_patch      (:) = nan
    allocate(this%ppsn_ptlai_z                (begp:endp,1:nlevcan)) ;   this%ppsn_ptlai_z                (:,:) = nan
    allocate(this%ppsn_pleafn_z               (begp:endp,1:nlevcan)) ;   this%ppsn_pleafn_z               (:,:) = nan
    allocate(this%ppsn_pleafp_z               (begp:endp,1:nlevcan)) ;   this%ppsn_pleafp_z               (:,:) = nan
    allocate(this%ppsn_ptlai_z_vcmax          (begp:endp,1:nlevcan)) ;   this%ppsn_ptlai_z_vcmax          (:,:) = nan
    allocate(this%ppsn_pleafn_z_vcmax         (begp:endp,1:nlevcan)) ;   this%ppsn_pleafn_z_vcmax         (:,:) = nan
    allocate(this%ppsn_pleafp_z_vcmax         (begp:endp,1:nlevcan)) ;   this%ppsn_pleafp_z_vcmax         (:,:) = nan
    allocate(this%ppsn_ptlai_z_jmax           (begp:endp,1:nlevcan)) ;   this%ppsn_ptlai_z_jmax           (:,:) = nan
    allocate(this%ppsn_pleafn_z_jmax          (begp:endp,1:nlevcan)) ;   this%ppsn_pleafn_z_jmax          (:,:) = nan
    allocate(this%ppsn_pleafp_z_jmax          (begp:endp,1:nlevcan)) ;   this%ppsn_pleafp_z_jmax          (:,:) = nan
    allocate(this%ppsn_ptlai_z_tpu            (begp:endp,1:nlevcan)) ;   this%ppsn_ptlai_z_tpu            (:,:) = nan
    allocate(this%ppsn_pleafn_z_tpu           (begp:endp,1:nlevcan)) ;   this%ppsn_pleafn_z_tpu           (:,:) = nan
    allocate(this%ppsn_pleafp_z_tpu           (begp:endp,1:nlevcan)) ;   this%ppsn_pleafp_z_tpu           (:,:) = nan
    allocate(this%plmr_ptlai_z                (begp:endp,1:nlevcan)) ;   this%plmr_ptlai_z                (:,:) = nan
    allocate(this%plmr_pleafn_z               (begp:endp,1:nlevcan)) ;   this%plmr_pleafn_z               (:,:) = nan

    allocate(this%smin_nh4sorb_vr_col      (begc:endc,1:nlevdecomp_full)) ; this%smin_nh4sorb_vr_col      (:,:) = nan
    allocate(this%smin_nh4sorb_col         (begc:endc))                   ; this%smin_nh4sorb_col         (:)   = nan

    allocate(this%plant_nbuffer_col(begc:endc));this%plant_nbuffer_col(:) = nan

  end subroutine InitAllocate

end module CNNitrogenStateType
