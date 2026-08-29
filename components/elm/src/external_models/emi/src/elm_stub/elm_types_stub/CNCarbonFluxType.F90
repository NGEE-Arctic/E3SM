module CNCarbonFluxType

  use shr_kind_mod           , only : r8 => shr_kind_r8
  use shr_infnan_mod         , only : nan => shr_infnan_nan, assignment(=)
  use decompMod              , only : bounds_type
  use clm_varpar             , only : ndecomp_cascade_transitions, ndecomp_pools, nlevcan
  use clm_varpar             , only : crop_prog
  use clm_varpar             , only : nlevdecomp_full, nlevgrnd, nlevdecomp
  use clm_varcon             , only : spval !, ispval, dzsoi_decomp
  use landunit_varcon        , only : istsoil, istcrop, istdlak 
  use clm_varctl             , only : use_c13, use_fates 
  ! use CH4varcon              , only : allowlakeprod
  ! use pftvarcon              , only : npcropmin
  use CNDecompCascadeConType , only : decomp_cascade_con
  ! use VegetationType         , only : veg_pp
  ! use ColumnType             , only : col_pp                
  ! use LandunitType           , only : lun_pp
  ! use clm_varctl             , only : nu_com
  ! use clm_varctl             , only : use_clm_interface, use_pflotran, pf_cmode, use_vertsoilc
  ! use AnnualFluxDribbler     , only : annual_flux_dribbler_type, annual_flux_dribbler_gridcell
  ! 
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  ! NOTE(bandre, 2013-10) according to Charlie Koven, nfix_timeconst
  ! is currently used as a flag and rate constant. Rate constant: time
  ! over which to exponentially relax the npp flux for N fixation term
  ! flag: (if  <=  0. or  >=  365; use old annual method). Default value is
  ! junk that should always be overwritten by the namelist or init function!
  !
  ! (days) time over which to exponentially relax the npp flux for N fixation term
  real(r8), public :: nfix_timeconst = -1.2345_r8 
  !
  type, public :: carbonflux_type

     ! gap mortality fluxes
     real(r8), pointer :: m_leafc_to_litter_patch                   (:)     ! leaf C mortality (gC/m2/s)
     real(r8), pointer :: m_leafc_storage_to_litter_patch           (:)     ! leaf C storage mortality (gC/m2/s)
     real(r8), pointer :: m_leafc_xfer_to_litter_patch              (:)     ! leaf C transfer mortality (gC/m2/s)
     real(r8), pointer :: m_frootc_to_litter_patch                  (:)     ! fine root C mortality (gC/m2/s)
     real(r8), pointer :: m_frootc_storage_to_litter_patch          (:)     ! fine root C storage mortality (gC/m2/s)
     real(r8), pointer :: m_frootc_xfer_to_litter_patch             (:)     ! fine root C transfer mortality (gC/m2/s)
     real(r8), pointer :: m_livestemc_to_litter_patch               (:)     ! live stem C mortality (gC/m2/s)
     real(r8), pointer :: m_livestemc_storage_to_litter_patch       (:)     ! live stem C storage mortality (gC/m2/s)
     real(r8), pointer :: m_livestemc_xfer_to_litter_patch          (:)     ! live stem C transfer mortality (gC/m2/s)
     real(r8), pointer :: m_deadstemc_to_litter_patch               (:)     ! dead stem C mortality (gC/m2/s)
     real(r8), pointer :: m_deadstemc_storage_to_litter_patch       (:)     ! dead stem C storage mortality (gC/m2/s)
     real(r8), pointer :: m_deadstemc_xfer_to_litter_patch          (:)     ! dead stem C transfer mortality (gC/m2/s)
     real(r8), pointer :: m_livecrootc_to_litter_patch              (:)     ! live coarse root C mortality (gC/m2/s)
     real(r8), pointer :: m_livecrootc_storage_to_litter_patch      (:)     ! live coarse root C storage mortality (gC/m2/s)
     real(r8), pointer :: m_livecrootc_xfer_to_litter_patch         (:)     ! live coarse root C transfer mortality (gC/m2/s)
     real(r8), pointer :: m_deadcrootc_to_litter_patch              (:)     ! dead coarse root C mortality (gC/m2/s)
     real(r8), pointer :: m_deadcrootc_storage_to_litter_patch      (:)     ! dead coarse root C storage mortality (gC/m2/s)
     real(r8), pointer :: m_deadcrootc_xfer_to_litter_patch         (:)     ! dead coarse root C transfer mortality (gC/m2/s)
     real(r8), pointer :: m_gresp_storage_to_litter_patch           (:)     ! growth respiration storage mortality (gC/m2/s)
     real(r8), pointer :: m_gresp_xfer_to_litter_patch              (:)     ! growth respiration transfer mortality (gC/m2/s)
     real(r8), pointer :: m_cpool_to_litter_patch                   (:)     ! plant storage C pool to litter (gC/m2/s)

     ! harvest mortality fluxes
     real(r8), pointer :: hrv_leafc_to_litter_patch                 (:)     ! leaf C harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_leafc_storage_to_litter_patch         (:)     ! leaf C storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_leafc_xfer_to_litter_patch            (:)     ! leaf C transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_frootc_to_litter_patch                (:)     ! fine root C harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_frootc_storage_to_litter_patch        (:)     ! fine root C storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_frootc_xfer_to_litter_patch           (:)     ! fine root C transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_livestemc_to_litter_patch             (:)     ! live stem C harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_livestemc_storage_to_litter_patch     (:)     ! live stem C storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_livestemc_xfer_to_litter_patch        (:)     ! live stem C transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_deadstemc_to_prod10c_patch            (:)     ! dead stem C harvest to 10-year product pool (gC/m2/s)
     real(r8), pointer :: hrv_deadstemc_to_prod100c_patch           (:)     ! dead stem C harvest to 100-year product pool (gC/m2/s)
     real(r8), pointer :: hrv_deadstemc_storage_to_litter_patch     (:)     ! dead stem C storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_deadstemc_xfer_to_litter_patch        (:)     ! dead stem C transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_livecrootc_to_litter_patch            (:)     ! live coarse root C harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_livecrootc_storage_to_litter_patch    (:)     ! live coarse root C storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_livecrootc_xfer_to_litter_patch       (:)     ! live coarse root C transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_deadcrootc_to_litter_patch            (:)     ! dead coarse root C harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_deadcrootc_storage_to_litter_patch    (:)     ! dead coarse root C storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_deadcrootc_xfer_to_litter_patch       (:)     ! dead coarse root C transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_gresp_storage_to_litter_patch         (:)     ! growth respiration storage harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_gresp_xfer_to_litter_patch            (:)     ! growth respiration transfer harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_xsmrpool_to_atm_patch                 (:)     ! excess MR pool harvest mortality (gC/m2/s)
     real(r8), pointer :: hrv_cpool_to_litter_patch                 (:)     ! Harvest cpool to litter (gC/m2/s)
     ! crop harvest
     real(r8), pointer :: hrv_leafc_to_prod1c_patch                 (:)     ! crop leafc harvested (gC/m2/s)
     real(r8), pointer :: hrv_livestemc_to_prod1c_patch             (:)     ! crop stemc harvested (gC/m2/s)
     real(r8), pointer :: hrv_grainc_to_prod1c_patch                (:)     ! crop grain harvested (gC/m2/s)
     real(r8), pointer :: hrv_cropc_to_prod1c_patch                 (:)     ! total amount of crop C harvested (gC/m2/s)

     ! fire C fluxes 
     real(r8), pointer :: m_leafc_to_fire_patch                     (:)     ! (gC/m2/s) fire C emissions from leafc 
     real(r8), pointer :: m_leafc_storage_to_fire_patch             (:)     ! (gC/m2/s) fire C emissions from leafc_storage             
     real(r8), pointer :: m_leafc_xfer_to_fire_patch                (:)     ! (gC/m2/s) fire C emissions from leafc_xfer
     real(r8), pointer :: m_livestemc_to_fire_patch                 (:)     ! (gC/m2/s) fire C emissions from livestemc
     real(r8), pointer :: m_livestemc_storage_to_fire_patch         (:)     ! (gC/m2/s) fire C emissions from livestemc_storage       
     real(r8), pointer :: m_livestemc_xfer_to_fire_patch            (:)     ! (gC/m2/s) fire C emissions from livestemc_xfer
     real(r8), pointer :: m_deadstemc_to_fire_patch                 (:)     ! (gC/m2/s) fire C emissions from deadstemc_xfer
     real(r8), pointer :: m_deadstemc_storage_to_fire_patch         (:)     ! (gC/m2/s) fire C emissions from deadstemc_storage         
     real(r8), pointer :: m_deadstemc_xfer_to_fire_patch            (:)     ! (gC/m2/s) fire C emissions from deadstemc_xfer
     real(r8), pointer :: m_frootc_to_fire_patch                    (:)     ! (gC/m2/s) fire C emissions from frootc
     real(r8), pointer :: m_frootc_storage_to_fire_patch            (:)     ! (gC/m2/s) fire C emissions from frootc_storage
     real(r8), pointer :: m_frootc_xfer_to_fire_patch               (:)     ! (gC/m2/s) fire C emissions from frootc_xfer
     real(r8), pointer :: m_livecrootc_to_fire_patch                (:)     ! (gC/m2/s) fire C emissions from livecrootc
     real(r8), pointer :: m_livecrootc_storage_to_fire_patch        (:)     ! (gC/m2/s) fire C emissions from livecrootc_storage     
     real(r8), pointer :: m_livecrootc_xfer_to_fire_patch           (:)     ! (gC/m2/s) fire C emissions from livecrootc_xfer
     real(r8), pointer :: m_deadcrootc_to_fire_patch                (:)     ! (gC/m2/s) fire C emissions from deadcrootc
     real(r8), pointer :: m_deadcrootc_storage_to_fire_patch        (:)     ! (gC/m2/s) fire C emissions from deadcrootc_storage 
     real(r8), pointer :: m_deadcrootc_xfer_to_fire_patch           (:)     ! (gC/m2/s) fire C emissions from deadcrootc_xfer
     real(r8), pointer :: m_gresp_storage_to_fire_patch             (:)     ! (gC/m2/s) fire C emissions from gresp_storage 
     real(r8), pointer :: m_gresp_xfer_to_fire_patch                (:)     ! (gC/m2/s) fire C emissions from gresp_xfer
     real(r8), pointer :: m_cpool_to_fire_patch                     (:)     ! (gC/m2/s) fire C emissions from cpool

     real(r8), pointer :: m_leafc_to_litter_fire_patch              (:)     ! (gC/m2/s) from leafc to litter c due to fire
     real(r8), pointer :: m_leafc_storage_to_litter_fire_patch      (:)     ! (gC/m2/s) from leafc_storage to litter C  due to fire               
     real(r8), pointer :: m_leafc_xfer_to_litter_fire_patch         (:)     ! (gC/m2/s) from leafc_xfer to litter C  due to fire               
     real(r8), pointer :: m_livestemc_to_litter_fire_patch          (:)     ! (gC/m2/s) from livestemc to litter C  due to fire               
     real(r8), pointer :: m_livestemc_storage_to_litter_fire_patch  (:)     ! (gC/m2/s) from livestemc_storage to litter C due to fire      
     real(r8), pointer :: m_livestemc_xfer_to_litter_fire_patch     (:)     ! (gC/m2/s) from livestemc_xfer to litter C due to fire      
     real(r8), pointer :: m_livestemc_to_deadstemc_fire_patch       (:)     ! (gC/m2/s) from livestemc to deadstemc due to fire       
     real(r8), pointer :: m_deadstemc_to_litter_fire_patch          (:)     ! (gC/m2/s) from deadstemc to litter C due to fire      
     real(r8), pointer :: m_deadstemc_storage_to_litter_fire_patch  (:)     ! (gC/m2/s) from deadstemc_storage to litter C due to fire               
     real(r8), pointer :: m_deadstemc_xfer_to_litter_fire_patch     (:)     ! (gC/m2/s) from deadstemc_xfer to litter C due to fire               
     real(r8), pointer :: m_frootc_to_litter_fire_patch             (:)     ! (gC/m2/s) from frootc to litter C due to fire               
     real(r8), pointer :: m_frootc_storage_to_litter_fire_patch     (:)     ! (gC/m2/s) from frootc_storage to litter C due to fire               
     real(r8), pointer :: m_frootc_xfer_to_litter_fire_patch        (:)     ! (gC/m2/s) from frootc_xfer to litter C due to fire               
     real(r8), pointer :: m_livecrootc_to_litter_fire_patch         (:)     ! (gC/m2/s) from livecrootc to litter C due to fire                     
     real(r8), pointer :: m_livecrootc_storage_to_litter_fire_patch (:)     ! (gC/m2/s) from livecrootc_storage to litter C due to fire                     
     real(r8), pointer :: m_livecrootc_xfer_to_litter_fire_patch    (:)     ! (gC/m2/s) from livecrootc_xfer to litter C due to fire                     
     real(r8), pointer :: m_livecrootc_to_deadcrootc_fire_patch     (:)     ! (gC/m2/s) from livecrootc to deadstemc due to fire        
     real(r8), pointer :: m_deadcrootc_to_litter_fire_patch         (:)     ! (gC/m2/s) from deadcrootc to litter C due to fire                       
     real(r8), pointer :: m_deadcrootc_storage_to_litter_fire_patch (:)     ! (gC/m2/s) from deadcrootc_storage to litter C due to fire                       
     real(r8), pointer :: m_deadcrootc_xfer_to_litter_fire_patch    (:)     ! (gC/m2/s) from deadcrootc_xfer to litter C due to fire                       
     real(r8), pointer :: m_gresp_storage_to_litter_fire_patch      (:)     ! (gC/m2/s) from gresp_storage to litter C due to fire                       
     real(r8), pointer :: m_gresp_xfer_to_litter_fire_patch         (:)     ! (gC/m2/s) from gresp_xfer to litter C due to fire                       
     real(r8), pointer :: m_cpool_to_litter_fire_patch              (:)     ! (gC/m2/s) from cpool to litter C due to fire              

     ! phenology fluxes from transfer pools                     
     real(r8), pointer :: grainc_xfer_to_grainc_patch               (:)     ! grain C growth from storage for prognostic crop(gC/m2/s)
     real(r8), pointer :: leafc_xfer_to_leafc_patch                 (:)     ! leaf C growth from storage (gC/m2/s)
     real(r8), pointer :: frootc_xfer_to_frootc_patch               (:)     ! fine root C growth from storage (gC/m2/s)
     real(r8), pointer :: livestemc_xfer_to_livestemc_patch         (:)     ! live stem C growth from storage (gC/m2/s)
     real(r8), pointer :: deadstemc_xfer_to_deadstemc_patch         (:)     ! dead stem C growth from storage (gC/m2/s)
     real(r8), pointer :: livecrootc_xfer_to_livecrootc_patch       (:)     ! live coarse root C growth from storage (gC/m2/s)
     real(r8), pointer :: deadcrootc_xfer_to_deadcrootc_patch       (:)     ! dead coarse root C growth from storage (gC/m2/s)

     ! leaf and fine root litterfall fluxes                          
     real(r8), pointer :: leafc_to_litter_patch                     (:)     ! leaf C litterfall (gC/m2/s)
     real(r8), pointer :: frootc_to_litter_patch                    (:)     ! fine root C litterfall (gC/m2/s)
     real(r8), pointer :: livestemc_to_litter_patch                 (:)     ! live stem C litterfall (gC/m2/s)
     real(r8), pointer :: grainc_to_food_patch                      (:)     ! grain C to food for prognostic crop(gC/m2/s)

     ! maintenance respiration fluxes                          
     real(r8), pointer :: leaf_mr_patch                             (:)     ! leaf maintenance respiration (gC/m2/s)
     real(r8), pointer :: froot_mr_patch                            (:)     ! fine root maintenance respiration (gC/m2/s)
     real(r8), pointer :: livestem_mr_patch                         (:)     ! live stem maintenance respiration (gC/m2/s)
     real(r8), pointer :: livecroot_mr_patch                        (:)     ! live coarse root maintenance respiration (gC/m2/s)
     real(r8), pointer :: grain_mr_patch                            (:)     ! crop grain or organs maint. respiration (gC/m2/s)
     real(r8), pointer :: leaf_curmr_patch                          (:)     ! leaf maintenance respiration from current GPP (gC/m2/s)
     real(r8), pointer :: froot_curmr_patch                         (:)     ! fine root maintenance respiration from current GPP (gC/m2/s)
     real(r8), pointer :: livestem_curmr_patch                      (:)     ! live stem maintenance respiration from current GPP (gC/m2/s)
     real(r8), pointer :: livecroot_curmr_patch                     (:)     ! live coarse root maintenance respiration from current GPP (gC/m2/s)
     real(r8), pointer :: grain_curmr_patch                         (:)     ! crop grain or organs maint. respiration from current GPP (gC/m2/s)
     real(r8), pointer :: leaf_xsmr_patch                           (:)     ! leaf maintenance respiration from storage (gC/m2/s)
     real(r8), pointer :: froot_xsmr_patch                          (:)     ! fine root maintenance respiration from storage (gC/m2/s)
     real(r8), pointer :: livestem_xsmr_patch                       (:)     ! live stem maintenance respiration from storage (gC/m2/s)
     real(r8), pointer :: livecroot_xsmr_patch                      (:)     ! live coarse root maintenance respiration from storage (gC/m2/s)
     real(r8), pointer :: grain_xsmr_patch                          (:)     ! crop grain or organs maint. respiration from storage (gC/m2/s)
     !turnover of excess carbon
     real(r8), pointer :: xr_patch                                  (:)     ! respiration from excess carbon cpool (gC/m2/s)

     ! photosynthesis fluxes                                   
     real(r8), pointer :: psnsun_to_cpool_patch                     (:)     ! C fixation from sunlit canopy (gC/m2/s)
     real(r8), pointer :: psnshade_to_cpool_patch                   (:)     ! C fixation from shaded canopy (gC/m2/s)

     ! allocation fluxes, from current GPP                     
     real(r8), pointer :: cpool_to_xsmrpool_patch                   (:)     ! allocation to maintenance respiration storage pool (gC/m2/s)
     real(r8), pointer :: cpool_to_grainc_patch                     (:)     ! allocation to grain C for prognostic crop(gC/m2/s)
     real(r8), pointer :: cpool_to_grainc_storage_patch             (:)     ! allocation to grain C storage for prognostic crop(gC/m2/s)
     real(r8), pointer :: cpool_to_leafc_patch                      (:)     ! allocation to leaf C (gC/m2/s)
     real(r8), pointer :: cpool_to_leafc_storage_patch              (:)     ! allocation to leaf C storage (gC/m2/s)
     real(r8), pointer :: cpool_to_frootc_patch                     (:)     ! allocation to fine root C (gC/m2/s)
     real(r8), pointer :: cpool_to_frootc_storage_patch             (:)     ! allocation to fine root C storage (gC/m2/s)
     real(r8), pointer :: cpool_to_livestemc_patch                  (:)     ! allocation to live stem C (gC/m2/s)
     real(r8), pointer :: cpool_to_livestemc_storage_patch          (:)     ! allocation to live stem C storage (gC/m2/s)
     real(r8), pointer :: cpool_to_deadstemc_patch                  (:)     ! allocation to dead stem C (gC/m2/s)
     real(r8), pointer :: cpool_to_deadstemc_storage_patch          (:)     ! allocation to dead stem C storage (gC/m2/s)
     real(r8), pointer :: cpool_to_livecrootc_patch                 (:)     ! allocation to live coarse root C (gC/m2/s)
     real(r8), pointer :: cpool_to_livecrootc_storage_patch         (:)     ! allocation to live coarse root C storage (gC/m2/s)
     real(r8), pointer :: cpool_to_deadcrootc_patch                 (:)     ! allocation to dead coarse root C (gC/m2/s)
     real(r8), pointer :: cpool_to_deadcrootc_storage_patch         (:)     ! allocation to dead coarse root C storage (gC/m2/s)
     real(r8), pointer :: cpool_to_gresp_storage_patch              (:)     ! allocation to growth respiration storage (gC/m2/s)

     ! growth respiration fluxes                               
     real(r8), pointer :: xsmrpool_to_atm_patch                     (:)     ! excess MR pool harvest mortality (gC/m2/s)
     real(r8), pointer :: cpool_leaf_gr_patch                       (:)     ! leaf growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_leaf_storage_gr_patch               (:)     ! leaf growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_leaf_gr_patch                    (:)     ! leaf growth respiration from storage (gC/m2/s)
     real(r8), pointer :: cpool_froot_gr_patch                      (:)     ! fine root growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_froot_storage_gr_patch              (:)     ! fine root  growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_froot_gr_patch                   (:)     ! fine root  growth respiration from storage (gC/m2/s)
     real(r8), pointer :: cpool_livestem_gr_patch                   (:)     ! live stem growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_livestem_storage_gr_patch           (:)     ! live stem growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_livestem_gr_patch                (:)     ! live stem growth respiration from storage (gC/m2/s)
     real(r8), pointer :: cpool_deadstem_gr_patch                   (:)     ! dead stem growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_deadstem_storage_gr_patch           (:)     ! dead stem growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_deadstem_gr_patch                (:)     ! dead stem growth respiration from storage (gC/m2/s)
     real(r8), pointer :: cpool_livecroot_gr_patch                  (:)     ! live coarse root growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_livecroot_storage_gr_patch          (:)     ! live coarse root growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_livecroot_gr_patch               (:)     ! live coarse root growth respiration from storage (gC/m2/s)
     real(r8), pointer :: cpool_deadcroot_gr_patch                  (:)     ! dead coarse root growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_deadcroot_storage_gr_patch          (:)     ! dead coarse root growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_deadcroot_gr_patch               (:)     ! dead coarse root growth respiration from storage (gC/m2/s)

     ! growth respiration for prognostic crop model
     real(r8), pointer :: cpool_grain_gr_patch                      (:)     ! grain growth respiration (gC/m2/s)
     real(r8), pointer :: cpool_grain_storage_gr_patch              (:)     ! grain growth respiration to storage (gC/m2/s)
     real(r8), pointer :: transfer_grain_gr_patch                   (:)     ! grain growth respiration from storage (gC/m2/s)

     ! annual turnover of storage to transfer pools            
     real(r8), pointer :: grainc_storage_to_xfer_patch              (:)     ! grain C shift storage to transfer for prognostic crop model (gC/m2/s)
     real(r8), pointer :: leafc_storage_to_xfer_patch               (:)     ! leaf C shift storage to transfer (gC/m2/s)
     real(r8), pointer :: frootc_storage_to_xfer_patch              (:)     ! fine root C shift storage to transfer (gC/m2/s)
     real(r8), pointer :: livestemc_storage_to_xfer_patch           (:)     ! live stem C shift storage to transfer (gC/m2/s)
     real(r8), pointer :: deadstemc_storage_to_xfer_patch           (:)     ! dead stem C shift storage to transfer (gC/m2/s)
     real(r8), pointer :: livecrootc_storage_to_xfer_patch          (:)     ! live coarse root C shift storage to transfer (gC/m2/s)
     real(r8), pointer :: deadcrootc_storage_to_xfer_patch          (:)     ! dead coarse root C shift storage to transfer (gC/m2/s)
     real(r8), pointer :: gresp_storage_to_xfer_patch               (:)     ! growth respiration shift storage to transfer (gC/m2/s)

     ! turnover of livewood to deadwood
     real(r8), pointer :: livestemc_to_deadstemc_patch              (:)     ! live stem C turnover (gC/m2/s)
     real(r8), pointer :: livecrootc_to_deadcrootc_patch            (:)     ! live coarse root C turnover (gC/m2/s)

     ! summary (diagnostic) flux variables, not involved in mass balance
     real(r8), pointer :: gpp_patch                                 (:)     ! (gC/m2/s) gross primary production 
     real(r8), pointer :: gpp_before_downreg_patch                  (:)     ! (gC/m2/s) gross primary production before down regulation
     real(r8), pointer :: mr_patch                                  (:)     ! (gC/m2/s) maintenance respiration
     real(r8), pointer :: current_gr_patch                          (:)     ! (gC/m2/s) growth resp for new growth displayed in this timestep
     real(r8), pointer :: transfer_gr_patch                         (:)     ! (gC/m2/s) growth resp for transfer growth displayed in this timestep
     real(r8), pointer :: storage_gr_patch                          (:)     ! (gC/m2/s) growth resp for growth sent to storage for later display
     real(r8), pointer :: gr_patch                                  (:)     ! (gC/m2/s) total growth respiration
     real(r8), pointer :: ar_patch                                  (:)     ! (gC/m2/s) autotrophic respiration (MR + GR)
     real(r8), pointer :: rr_patch                                  (:)     ! (gC/m2/s) root respiration (fine root MR + total root GR)
     real(r8), pointer :: npp_patch                                 (:)     ! (gC/m2/s) net primary production
     real(r8), pointer :: agnpp_patch                               (:)     ! (gC/m2/s) aboveground NPP
     real(r8), pointer :: bgnpp_patch                               (:)     ! (gC/m2/s) belowground NPP
     real(r8), pointer :: litfall_patch                             (:)     ! (gC/m2/s) litterfall (leaves and fine roots)
     real(r8), pointer :: vegfire_patch                             (:)     ! (gC/m2/s) patch-level fire loss (obsolete, mark for removal)
     real(r8), pointer :: wood_harvestc_patch                       (:)     ! (gC/m2/s) patch-level wood harvest (to product pools)
     real(r8), pointer :: cinputs_patch                             (:)     ! (gC/m2/s) patch-level carbon inputs (for balance checking)
     real(r8), pointer :: coutputs_patch                            (:)     ! (gC/m2/s) patch-level carbon outputs (for balance checking)

     real(r8), pointer :: plant_calloc_patch                        (:)     ! total allocated C flux (gC/m2/s)
     real(r8), pointer :: excess_cflux_patch                        (:)     ! C flux not allocated due to downregulation (gC/m2/s)
     real(r8), pointer :: prev_leafc_to_litter_patch                (:)     ! previous timestep leaf C litterfall flux (gC/m2/s)
     real(r8), pointer :: prev_frootc_to_litter_patch               (:)     ! previous timestep froot C litterfall flux (gC/m2/s)
     real(r8), pointer :: availc_patch                              (:)     ! C flux available for allocation (gC/m2/s)
     real(r8), pointer :: xsmrpool_recover_patch                    (:)     ! C flux assigned to recovery of negative cpool (gC/m2/s)
     real(r8), pointer :: xsmrpool_c13ratio_patch                   (:)     ! C13/C(12+13) ratio for xsmrpool (proportion)
     real(r8), pointer :: xsmrpool_turnover_patch                   (:)     ! xsmrpool flux to atmosphere due to turnover

     ! CN: CLAMP summary (diagnostic) variables, not involved in mass balance
     real(r8), pointer :: frootc_alloc_patch                        (:)     ! (gC/m2/s) patch-level fine root C alloc
     real(r8), pointer :: frootc_loss_patch                         (:)     ! (gC/m2/s) patch-level fine root C loss
     real(r8), pointer :: leafc_alloc_patch                         (:)     ! (gC/m2/s) patch-level leaf C alloc
     real(r8), pointer :: leafc_loss_patch                          (:)     ! (gC/m2/s) patch-level leaf C loss
     real(r8), pointer :: woodc_alloc_patch                         (:)     ! (gC/m2/s) patch-level wood C alloc
     real(r8), pointer :: woodc_loss_patch                          (:)     ! (gC/m2/s) patch-level wood C loss

     ! fire code
     real(r8), pointer :: fire_closs_patch                          (:)     ! (gC/m2/s) total patch-level fire C loss 

     ! For aerenchyma calculations in CH4 code
     real(r8), pointer :: annavg_agnpp_patch                        (:)     ! (gC/m2/s) annual average aboveground NPP
     real(r8), pointer :: annavg_bgnpp_patch                        (:)     ! (gC/m2/s) annual average belowground NPP
     real(r8), pointer :: tempavg_agnpp_patch                       (:)     ! (gC/m2/s) temp. average aboveground NPP
     real(r8), pointer :: tempavg_bgnpp_patch                       (:)     ! (gC/m2/s) temp. average belowground NPP

     ! For comparison with RAINFOR wood productivity data
     real(r8), pointer :: agwdnpp_patch                             (:)     !(gC/m2/s) aboveground NPP


     !----------------------------------------------------
     ! column carbon flux variables  
     !----------------------------------------------------

     ! phenology: litterfall and crop fluxes
     real(r8), pointer :: phenology_c_to_litr_met_c_col             (:,:)   ! C fluxes associated with phenology (litterfall and crop) to litter metabolic pool (gC/m3/s)
     real(r8), pointer :: phenology_c_to_litr_cel_c_col             (:,:)   ! C fluxes associated with phenology (litterfall and crop) to litter cellulose pool (gC/m3/s)
     real(r8), pointer :: phenology_c_to_litr_lig_c_col             (:,:)   ! C fluxes associated with phenology (litterfall and crop) to litter lignin pool (gC/m3/s)

     ! gap mortality
     real(r8), pointer :: gap_mortality_c_to_litr_met_c_col         (:,:)   ! C fluxes associated with gap mortality to litter metabolic pool (gC/m3/s)
     real(r8), pointer :: gap_mortality_c_to_litr_cel_c_col         (:,:)   ! C fluxes associated with gap mortality to litter cellulose pool (gC/m3/s)
     real(r8), pointer :: gap_mortality_c_to_litr_lig_c_col         (:,:)   ! C fluxes associated with gap mortality to litter lignin pool (gC/m3/s)
     real(r8), pointer :: gap_mortality_c_to_cwdc_col               (:,:)   ! C fluxes associated with gap mortality to CWD pool (gC/m3/s)

     ! fire
     real(r8), pointer :: fire_mortality_c_to_cwdc_col              (:,:)   ! C fluxes associated with fire mortality to CWD pool (gC/m3/s)

     ! harvest
     real(r8), pointer :: harvest_c_to_litr_met_c_col               (:,:)   ! C fluxes associated with harvest to litter metabolic pool (gC/m3/s)
     real(r8), pointer :: harvest_c_to_litr_cel_c_col               (:,:)   ! C fluxes associated with harvest to litter cellulose pool (gC/m3/s)
     real(r8), pointer :: harvest_c_to_litr_lig_c_col               (:,:)   ! C fluxes associated with harvest to litter lignin pool (gC/m3/s)
     real(r8), pointer :: harvest_c_to_cwdc_col                     (:,:)   ! C fluxes associated with harvest to CWD pool (gC/m3/s)

     ! new variables for CN code
     real(r8), pointer :: hrv_deadstemc_to_prod10c_col              (:)     ! dead stem C harvest mortality to 10-year product pool (gC/m2/s)        
     real(r8), pointer :: hrv_deadstemc_to_prod100c_col             (:)     ! dead stem C harvest mortality to 100-year product pool (gC/m2/s)        
     real(r8), pointer :: hrv_cropc_to_prod1c_col                   (:)     ! crop C harvest mortality to 1-year product pool (gC/m2/s)

     ! column-level fire fluxes
     real(r8), pointer :: m_decomp_cpools_to_fire_vr_col            (:,:,:) ! vertically-resolved decomposing C fire loss (gC/m3/s)
     real(r8), pointer :: m_decomp_cpools_to_fire_col               (:,:)   ! vertically-integrated (diagnostic) decomposing C fire loss (gC/m2/s)
     real(r8), pointer :: m_c_to_litr_met_fire_col                  (:,:)   ! C from leaf, froot, xfer and storage C to litter labile C by fire (gC/m3/s) 
     real(r8), pointer :: m_c_to_litr_cel_fire_col                  (:,:)   ! C from leaf, froot, xfer and storage C to litter cellulose C by fire (gC/m3/s) 
     real(r8), pointer :: m_c_to_litr_lig_fire_col                  (:,:)   ! C from leaf, froot, xfer and storage C to litter lignin C by fire (gC/m3/s) 
     real(r8), pointer :: somc_fire_col                             (:)     ! (gC/m2/s) carbon emissions due to peat burning

     real(r8), pointer :: decomp_cpools_sourcesink_col              (:,:,:) ! change in decomposing c pools. Used to update concentrations concurrently with vertical transport (gC/m3/timestep)  
     real(r8), pointer :: decomp_cascade_hr_vr_col                  (:,:,:) ! vertically-resolved het. resp. from decomposing C pools (gC/m3/s)
     real(r8), pointer :: decomp_cascade_hr_col                     (:,:)   ! vertically-integrated (diagnostic) het. resp. from decomposing C pools (gC/m2/s)
     real(r8), pointer :: decomp_cascade_ctransfer_vr_col           (:,:,:) ! vertically-resolved C transferred along deomposition cascade (gC/m3/s)
     real(r8), pointer :: decomp_cascade_ctransfer_col              (:,:)   ! vertically-integrated (diagnostic) C transferred along deomposition cascade (gC/m2/s)
     real(r8), pointer :: decomp_k_col                              (:,:,:) ! rate constant for decomposition (1./sec)
     real(r8), pointer :: hr_vr_col                                 (:,:)   ! total vertically-resolved het. resp. from decomposing C pools (gC/m3/s)
     real(r8), pointer :: o_scalar_col                              (:,:)   ! fraction by which decomposition is limited by anoxia
     real(r8), pointer :: w_scalar_col                              (:,:)   ! fraction by which decomposition is limited by moisture availability
     real(r8), pointer :: t_scalar_col                              (:,:)   ! fraction by which decomposition is limited by temperature
     real(r8), pointer :: som_c_leached_col                         (:)     ! total SOM C loss from vertical transport (gC/m^2/s)
     real(r8), pointer :: decomp_cpools_leached_col                 (:,:)   ! C loss from vertical transport from each decomposing C pool (gC/m^2/s)
     real(r8), pointer :: decomp_cpools_transport_tendency_col      (:,:,:) ! C tendency due to vertical transport in decomposing C pools (gC/m^3/s)

     ! nitrif_denitrif
     real(r8), pointer :: phr_vr_col                                (:,:)   ! potential hr (not N-limited) (gC/m3/s)
     real(r8), pointer :: fphr_col                                  (:,:)   ! fraction of potential heterotrophic respiration

     ! crop fluxes
     real(r8), pointer :: crop_seedc_to_leaf_patch                  (:)     ! (gC/m2/s) seed source to leaf, for crops

     ! CN dynamic landcover fluxes
     real(r8), pointer :: dwt_seedc_to_leaf_patch                   (:)     ! (gC/m2/s) seed source to patch-level; although this is a patch-level flux, it is expressed per unit GRIDCELL area
     real(r8), pointer :: dwt_seedc_to_leaf_grc                     (:)     ! (gC/m2/s) dwt_seedc_to_leaf_patch summed to the gridcell-level
     real(r8), pointer :: dwt_seedc_to_deadstem_patch               (:)     ! (gC/m2/s) seed source to patch-level; although this is a patch-level flux, it is expressed per unit GRIDCELL area
     real(r8), pointer :: dwt_seedc_to_deadstem_grc                 (:)     ! (gC/m2/s) dwt_seedc_to_leaf_patch summed to the gridcell-level
     real(r8), pointer :: dwt_conv_cflux_patch                      (:)     ! (gC/m2/s) conversion C flux (immediate loss to atm); although this is a patch-level flux, it is expressed per unit GRIDCELL area
     real(r8), pointer :: dwt_conv_cflux_grc                        (:)     ! (gC/m2/s) dwt_conv_cflux_patch summed to the gridcell-level
     ! real(r8), pointer :: dwt_conv_cflux_dribbled_grc               (:)     ! (gC/m2/s) dwt_conv_cflux_grc dribbled evenly throughout the year
     real(r8), pointer :: dwt_prod10c_gain_patch                    (:)     ! (gC/m2/s) addition to 10-yr wood product pool; although this is a patch-level flux, it is expressed per unit GRIDCELL area
     real(r8), pointer :: dwt_prod100c_gain_patch                   (:)     ! (gC/m2/s) addition to 100-yr wood product pool; although this is a patch-level flux, it is expressed per unit GRIDCELL area
     real(r8), pointer :: dwt_crop_productc_gain_patch              (:)     ! (gC/m2/s) addition to crop product pools from landcover change; although this is a patch-level flux, it is expressed per unit GRIDCELL area
     real(r8), pointer :: dwt_slash_cflux_col                       (:)     ! (gC/m2/s) conversion slash flux due to landcover change
     
     real(r8), pointer :: dwt_conv_cflux_col                        (:)     ! (gC/m2/s) conversion C flux (immediate loss to atm)
     real(r8), pointer :: dwt_prod10c_gain_col                      (:)     ! (gC/m2/s) addition to 10-yr wood product pool
     real(r8), pointer :: dwt_prod100c_gain_col                     (:)     ! (gC/m2/s) addition to 100-yr wood product pool

     real(r8), pointer :: dwt_frootc_to_litr_met_c_col              (:,:)   ! (gC/m3/s) fine root to litter due to landcover change
     real(r8), pointer :: dwt_frootc_to_litr_cel_c_col              (:,:)   ! (gC/m3/s) fine root to litter due to landcover change
     real(r8), pointer :: dwt_frootc_to_litr_lig_c_col              (:,:)   ! (gC/m3/s) fine root to litter due to landcover change
     real(r8), pointer :: dwt_livecrootc_to_cwdc_col                (:,:)   ! (gC/m3/s) live coarse root to CWD due to landcover change
     real(r8), pointer :: dwt_deadcrootc_to_cwdc_col                (:,:)   ! (gC/m3/s) dead coarse root to CWD due to landcover change
     real(r8), pointer :: dwt_closs_col                             (:)     ! (gC/m2/s) total carbon loss from product pools and conversion
     real(r8), pointer :: landuseflux_col                           (:)     ! (gC/m2/s) dwt_closs+product_closs
     real(r8), pointer :: landuptake_col                            (:)     ! (gC/m2/s) nee-landuseflux

     real(r8), pointer :: dwt_prod10c_gain_grc                      (:)     ! (gC/m2/s) dynamic landcover addition to 10-year wood product pool
     real(r8), pointer :: dwt_prod100c_gain_grc                     (:)     ! (gC/m2/s) dynamic landcover addition to 100-year wood product pool
     real(r8), pointer :: hrv_deadstemc_to_prod10c_grc              (:)     ! (gC/m2/s) dead stem harvest to 10-year wood product pool
     real(r8), pointer :: hrv_deadstemc_to_prod100c_grc             (:)     ! (gC/m2/s) dead stem harvest to 100-year wood product pool

     ! CN wood product pool loss fluxes
     real(r8), pointer :: prod1c_loss_col                           (:)     ! (gC/m2/s) decomposition loss from 1-year product pool
     real(r8), pointer :: prod10c_loss_col                          (:)     ! (gC/m2/s) decomposition loss from 10-yr wood product pool
     real(r8), pointer :: prod100c_loss_col                         (:)     ! (gC/m2/s) decomposition loss from 100-yr wood product pool
     real(r8), pointer :: product_closs_col                         (:)     ! (gC/m2/s) total wood product carbon loss

     ! summary (diagnostic) flux variables, not involved in mass balance
     real(r8), pointer :: lithr_col                                 (:)     ! (gC/m2/s) litter heterotrophic respiration 
     real(r8), pointer :: somhr_col                                 (:)     ! (gC/m2/s) soil organic matter heterotrophic respiration
     real(r8), pointer :: hr_col                                    (:)     ! (gC/m2/s) total heterotrophic respiration
     real(r8), pointer :: sr_col                                    (:)     ! (gC/m2/s) total soil respiration (HR + root resp)
     real(r8), pointer :: er_col                                    (:)     ! (gC/m2/s) total ecosystem respiration, autotrophic + heterotrophic
     real(r8), pointer :: litfire_col                               (:)     ! (gC/m2/s) litter fire losses
     real(r8), pointer :: somfire_col                               (:)     ! (gC/m2/s) soil organic matter fire losses
     real(r8), pointer :: totfire_col                               (:)     ! (gC/m2/s) total ecosystem fire losses
     real(r8), pointer :: nep_col                                   (:)     ! (gC/m2/s) net ecosystem production, excludes fire, landuse, and harvest flux, positive for sink
     real(r8), pointer :: nbp_col                                   (:)     ! (gC/m2/s) net biome production, includes fire, landuse, and harvest flux, positive for sink
     real(r8), pointer :: nee_col                                   (:)     ! (gC/m2/s) net ecosystem exchange of carbon, includes fire, landuse, harvest, and hrv_xsmrpool flux, positive for source

     ! CN CLAMP summary (diagnostic) flux variables, not involved in mass balance
     real(r8), pointer :: cwdc_hr_col                               (:)     ! (gC/m2/s) col-level coarse woody debris C heterotrophic respiration
     real(r8), pointer :: cwdc_loss_col                             (:)     ! (gC/m2/s) col-level coarse woody debris C loss
     real(r8), pointer :: litterc_loss_col                          (:)     ! (gC/m2/s) col-level litter C loss

     real(r8), pointer :: bgc_cpool_ext_inputs_vr_col               (:, :, :)  ! col-level extneral organic carbon input gC/m3 /time step
     real(r8), pointer :: bgc_cpool_ext_loss_vr_col                 (:, :, :)  ! col-level extneral organic carbon loss gC/m3 /time step
     ! patch averaged to column variables - to remove need for pcf_a instance
     real(r8), pointer :: rr_col                                    (:)     ! column (gC/m2/s) root respiration (fine root MR + total root GR) (p2c)
     real(r8), pointer :: ar_col                                    (:)     ! column (gC/m2/s) autotrophic respiration (MR + GR) (p2c)      
     real(r8), pointer :: gpp_col                                   (:)     ! column (gC/m2/s) GPP flux before downregulation  (p2c)         
     real(r8), pointer :: npp_col                                   (:)     ! column (gC/m2/s) net primary production (p2c)                  
     real(r8), pointer :: fire_closs_p2c_col                        (:)     ! column (gC/m2/s) patch2col averaged column-level fire C loss (p2c)
     real(r8), pointer :: fire_closs_col                            (:)     ! column (gC/m2/s) total patch-level fire C loss 
     real(r8), pointer :: fire_decomp_closs_col                     (:)     ! column (gC/m2/s) carbon loss to fire for decomposable pools
     real(r8), pointer :: litfall_col                               (:)     ! column (gC/m2/s) total patch-level litterfall C loss (p2c)       
     real(r8), pointer :: vegfire_col                               (:)     ! column (gC/m2/s) patch-level fire loss (obsolete, mark for removal) (p2c)
     real(r8), pointer :: wood_harvestc_col                         (:)     ! column (p2c)                                                  
     real(r8), pointer :: hrv_xsmrpool_to_atm_col                   (:)     ! column excess MR pool harvest mortality (gC/m2/s) (p2c)
  
     ! Temporary and annual sums
     real(r8), pointer :: tempsum_npp_patch           (:) ! patch temporary annual sum of NPP (gC/m2/yr)
     real(r8), pointer :: annsum_npp_patch            (:) ! patch annual sum of NPP (gC/m2/yr)
     real(r8), pointer :: annsum_npp_col              (:) ! col annual sum of NPP, averaged from pft-level (gC/m2/yr)
     real(r8), pointer :: lag_npp_col                 (:) ! col lagged net primary production (gC/m2/s)
     
     ! debug
     real(r8), pointer :: plant_to_litter_cflux		  (:) ! for the purpose of mass balance check
     real(r8), pointer :: plant_to_cwd_cflux		  (:) ! for the purpose of mass balance check
     real(r8), pointer :: allocation_leaf 		  (:) ! check allocation to leaf for dynamic allocation scheme
     real(r8), pointer :: allocation_stem 		  (:) ! check allocation to stem for dynamic allocation scheme
     real(r8), pointer :: allocation_froot 		  (:) ! check allocation to fine root for dynamic allocation scheme

     ! new variables for clm_interface_funcsMod & pflotran
     !------------------------------------------------------------------------
     real(r8), pointer :: externalc_to_decomp_cpools_col            (:,:,:) ! col (gC/m3/s) net C fluxes associated with litter/som-adding/removal to decomp pools
                                                                            ! (sum of all external C additions and removals, excluding decomposition/hr).
     real(r8), pointer :: externalc_to_decomp_delta_col             (:)     ! col (gC/m2) summarized net change of whole column C i/o to decomposing pool bwtn time-step
     real(r8), pointer :: f_co2_soil_vr_col                         (:,:)   ! total vertically-resolved soil-atm. CO2 exchange (gC/m3/s)
     real(r8), pointer :: f_co2_soil_col                            (:)     ! total soil-atm. CO2 exchange (gC/m2/s)
    !------------------------------------------------------------------------

     ! Objects that help convert once-per-year dynamic land cover changes into fluxes
     ! that are dribbled throughout the year
     ! type(annual_flux_dribbler_type) :: dwt_conv_cflux_dribbler
     ! type(annual_flux_dribbler_type) :: hrv_xsmrpool_to_atm_dribbler
   contains

     procedure , public  :: Init   

     procedure , private :: InitAllocate 

  end type carbonflux_type
  !------------------------------------------------------------------------

contains
   
  !------------------------------------------------------------------------
  subroutine Init(this, bounds)

     class(carbonflux_type) :: this
     type(bounds_type), intent(in) :: bounds  
     ! character(len=3) , intent(in) :: carbon_type ! one of ['c12', c13','c14']

     call this%InitAllocate ( bounds)


   end subroutine Init

   !------------------------------------------------------------------------
   subroutine InitAllocate(this, bounds)
     !
     ! !ARGUMENTS:
     class (carbonflux_type) :: this 
     type(bounds_type), intent(in)    :: bounds 
     !
     ! !LOCAL VARIABLES:
     integer           :: begp,endp
     integer           :: begc,endc
     integer           :: begg,endg
     !------------------------------------------------------------------------

     begp = bounds%begp; endp = bounds%endp
     begc = bounds%begc; endc = bounds%endc
     begg = bounds%begg; endg = bounds%endg

     if (.not.use_fates) then
        allocate(this%m_leafc_to_litter_patch                  (begp:endp)) ; this%m_leafc_to_litter_patch                (:) = nan
        allocate(this%m_frootc_to_litter_patch                 (begp:endp)) ; this%m_frootc_to_litter_patch               (:) = nan
        allocate(this%m_leafc_storage_to_litter_patch          (begp:endp)) ; this%m_leafc_storage_to_litter_patch        (:) = nan
        allocate(this%m_frootc_storage_to_litter_patch         (begp:endp)) ; this%m_frootc_storage_to_litter_patch       (:) = nan
        allocate(this%m_livestemc_storage_to_litter_patch      (begp:endp)) ; this%m_livestemc_storage_to_litter_patch    (:) = nan
        allocate(this%m_deadstemc_storage_to_litter_patch      (begp:endp)) ; this%m_deadstemc_storage_to_litter_patch    (:) = nan
        allocate(this%m_livecrootc_storage_to_litter_patch     (begp:endp)) ; this%m_livecrootc_storage_to_litter_patch   (:) = nan
        allocate(this%m_deadcrootc_storage_to_litter_patch     (begp:endp)) ; this%m_deadcrootc_storage_to_litter_patch   (:) = nan
        allocate(this%m_leafc_xfer_to_litter_patch             (begp:endp)) ; this%m_leafc_xfer_to_litter_patch           (:) = nan
        allocate(this%m_frootc_xfer_to_litter_patch            (begp:endp)) ; this%m_frootc_xfer_to_litter_patch          (:) = nan
        allocate(this%m_livestemc_xfer_to_litter_patch         (begp:endp)) ; this%m_livestemc_xfer_to_litter_patch       (:) = nan
        allocate(this%m_deadstemc_xfer_to_litter_patch         (begp:endp)) ; this%m_deadstemc_xfer_to_litter_patch       (:) = nan
        allocate(this%m_livecrootc_xfer_to_litter_patch        (begp:endp)) ; this%m_livecrootc_xfer_to_litter_patch      (:) = nan
        allocate(this%m_deadcrootc_xfer_to_litter_patch        (begp:endp)) ; this%m_deadcrootc_xfer_to_litter_patch      (:) = nan
        allocate(this%m_livestemc_to_litter_patch              (begp:endp)) ; this%m_livestemc_to_litter_patch            (:) = nan
        allocate(this%m_deadstemc_to_litter_patch              (begp:endp)) ; this%m_deadstemc_to_litter_patch            (:) = nan
        allocate(this%m_livecrootc_to_litter_patch             (begp:endp)) ; this%m_livecrootc_to_litter_patch           (:) = nan
        allocate(this%m_deadcrootc_to_litter_patch             (begp:endp)) ; this%m_deadcrootc_to_litter_patch           (:) = nan
        allocate(this%m_gresp_storage_to_litter_patch          (begp:endp)) ; this%m_gresp_storage_to_litter_patch        (:) = nan
        allocate(this%m_gresp_xfer_to_litter_patch             (begp:endp)) ; this%m_gresp_xfer_to_litter_patch           (:) = nan
        allocate(this%m_cpool_to_litter_patch                  (begp:endp)) ; this%m_cpool_to_litter_patch                (:) = nan
        allocate(this%hrv_leafc_to_litter_patch                (begp:endp)) ; this%hrv_leafc_to_litter_patch              (:) = nan
        allocate(this%hrv_leafc_storage_to_litter_patch        (begp:endp)) ; this%hrv_leafc_storage_to_litter_patch      (:) = nan
        allocate(this%hrv_leafc_xfer_to_litter_patch           (begp:endp)) ; this%hrv_leafc_xfer_to_litter_patch         (:) = nan
        allocate(this%hrv_frootc_to_litter_patch               (begp:endp)) ; this%hrv_frootc_to_litter_patch             (:) = nan
        allocate(this%hrv_frootc_storage_to_litter_patch       (begp:endp)) ; this%hrv_frootc_storage_to_litter_patch     (:) = nan
        allocate(this%hrv_frootc_xfer_to_litter_patch          (begp:endp)) ; this%hrv_frootc_xfer_to_litter_patch        (:) = nan
        allocate(this%hrv_livestemc_to_litter_patch            (begp:endp)) ; this%hrv_livestemc_to_litter_patch          (:) = nan
        allocate(this%hrv_livestemc_storage_to_litter_patch    (begp:endp)) ; this%hrv_livestemc_storage_to_litter_patch  (:) = nan
        allocate(this%hrv_livestemc_xfer_to_litter_patch       (begp:endp)) ; this%hrv_livestemc_xfer_to_litter_patch     (:) = nan
        allocate(this%hrv_deadstemc_to_prod10c_patch           (begp:endp)) ; this%hrv_deadstemc_to_prod10c_patch         (:) = nan
        allocate(this%hrv_deadstemc_to_prod100c_patch          (begp:endp)) ; this%hrv_deadstemc_to_prod100c_patch        (:) = nan
        allocate(this%hrv_leafc_to_prod1c_patch                (begp:endp)) ; this%hrv_leafc_to_prod1c_patch              (:) = nan
        allocate(this%hrv_livestemc_to_prod1c_patch            (begp:endp)) ; this%hrv_livestemc_to_prod1c_patch          (:) = nan
        allocate(this%hrv_grainc_to_prod1c_patch               (begp:endp)) ; this%hrv_grainc_to_prod1c_patch             (:) = nan
        allocate(this%hrv_cropc_to_prod1c_patch                (begp:endp)) ; this%hrv_cropc_to_prod1c_patch              (:) = nan
        allocate(this%hrv_deadstemc_storage_to_litter_patch    (begp:endp)) ; this%hrv_deadstemc_storage_to_litter_patch  (:) = nan
        allocate(this%hrv_deadstemc_xfer_to_litter_patch       (begp:endp)) ; this%hrv_deadstemc_xfer_to_litter_patch     (:) = nan
        allocate(this%hrv_livecrootc_to_litter_patch           (begp:endp)) ; this%hrv_livecrootc_to_litter_patch         (:) = nan
        allocate(this%hrv_livecrootc_storage_to_litter_patch   (begp:endp)) ; this%hrv_livecrootc_storage_to_litter_patch (:) = nan
        allocate(this%hrv_livecrootc_xfer_to_litter_patch      (begp:endp)) ; this%hrv_livecrootc_xfer_to_litter_patch    (:) = nan
        allocate(this%hrv_deadcrootc_to_litter_patch           (begp:endp)) ; this%hrv_deadcrootc_to_litter_patch         (:) = nan
        allocate(this%hrv_deadcrootc_storage_to_litter_patch   (begp:endp)) ; this%hrv_deadcrootc_storage_to_litter_patch (:) = nan
        allocate(this%hrv_deadcrootc_xfer_to_litter_patch      (begp:endp)) ; this%hrv_deadcrootc_xfer_to_litter_patch    (:) = nan
        allocate(this%hrv_gresp_storage_to_litter_patch        (begp:endp)) ; this%hrv_gresp_storage_to_litter_patch      (:) = nan
        allocate(this%hrv_gresp_xfer_to_litter_patch           (begp:endp)) ; this%hrv_gresp_xfer_to_litter_patch         (:) = nan
        allocate(this%hrv_xsmrpool_to_atm_patch                (begp:endp)) ; this%hrv_xsmrpool_to_atm_patch              (:) = nan
        allocate(this%hrv_cpool_to_litter_patch                (begp:endp)) ; this%hrv_cpool_to_litter_patch              (:) = nan
        allocate(this%m_leafc_to_fire_patch                    (begp:endp)) ; this%m_leafc_to_fire_patch                  (:) = nan
        allocate(this%m_leafc_storage_to_fire_patch            (begp:endp)) ; this%m_leafc_storage_to_fire_patch          (:) = nan
        allocate(this%m_leafc_xfer_to_fire_patch               (begp:endp)) ; this%m_leafc_xfer_to_fire_patch             (:) = nan
        allocate(this%m_livestemc_to_fire_patch                (begp:endp)) ; this%m_livestemc_to_fire_patch              (:) = nan
        allocate(this%m_livestemc_storage_to_fire_patch        (begp:endp)) ; this%m_livestemc_storage_to_fire_patch      (:) = nan
        allocate(this%m_livestemc_xfer_to_fire_patch           (begp:endp)) ; this%m_livestemc_xfer_to_fire_patch         (:) = nan
        allocate(this%m_deadstemc_to_fire_patch                (begp:endp)) ; this%m_deadstemc_to_fire_patch              (:) = nan
        allocate(this%m_deadstemc_storage_to_fire_patch        (begp:endp)) ; this%m_deadstemc_storage_to_fire_patch      (:) = nan
        allocate(this%m_deadstemc_xfer_to_fire_patch           (begp:endp)) ; this%m_deadstemc_xfer_to_fire_patch         (:) = nan
        allocate(this%m_frootc_to_fire_patch                   (begp:endp)) ; this%m_frootc_to_fire_patch                 (:) = nan
        allocate(this%m_frootc_storage_to_fire_patch           (begp:endp)) ; this%m_frootc_storage_to_fire_patch         (:) = nan
        allocate(this%m_frootc_xfer_to_fire_patch              (begp:endp)) ; this%m_frootc_xfer_to_fire_patch            (:) = nan
        allocate(this%m_livecrootc_to_fire_patch               (begp:endp)) ; this%m_livecrootc_to_fire_patch             (:) = nan
        allocate(this%m_livecrootc_storage_to_fire_patch       (begp:endp)) ; this%m_livecrootc_storage_to_fire_patch     (:) = nan
        allocate(this%m_livecrootc_xfer_to_fire_patch          (begp:endp)) ; this%m_livecrootc_xfer_to_fire_patch        (:) = nan
        allocate(this%m_deadcrootc_to_fire_patch               (begp:endp)) ; this%m_deadcrootc_to_fire_patch             (:) = nan
        allocate(this%m_deadcrootc_storage_to_fire_patch       (begp:endp)) ; this%m_deadcrootc_storage_to_fire_patch     (:) = nan
        allocate(this%m_deadcrootc_xfer_to_fire_patch          (begp:endp)) ; this%m_deadcrootc_xfer_to_fire_patch        (:) = nan
        allocate(this%m_gresp_storage_to_fire_patch            (begp:endp)) ; this%m_gresp_storage_to_fire_patch          (:) = nan
        allocate(this%m_gresp_xfer_to_fire_patch               (begp:endp)) ; this%m_gresp_xfer_to_fire_patch             (:) = nan
        allocate(this%m_cpool_to_fire_patch                    (begp:endp)) ; this%m_cpool_to_fire_patch                  (:) = nan

        allocate(this%m_leafc_to_litter_fire_patch             (begp:endp)) ; this%m_leafc_to_litter_fire_patch           (:) = nan
        allocate(this%m_leafc_storage_to_litter_fire_patch     (begp:endp)) ; this%m_leafc_storage_to_litter_fire_patch   (:) = nan
        allocate(this%m_leafc_xfer_to_litter_fire_patch        (begp:endp)) ; this%m_leafc_xfer_to_litter_fire_patch      (:) = nan
        allocate(this%m_livestemc_to_litter_fire_patch         (begp:endp)) ; this%m_livestemc_to_litter_fire_patch       (:) = nan
        allocate(this%m_livestemc_storage_to_litter_fire_patch (begp:endp))
        this%m_livestemc_storage_to_litter_fire_patch(:) = nan
        allocate(this%m_livestemc_xfer_to_litter_fire_patch    (begp:endp)) ; this%m_livestemc_xfer_to_litter_fire_patch  (:) = nan
        allocate(this%m_livestemc_to_deadstemc_fire_patch      (begp:endp)) ; this%m_livestemc_to_deadstemc_fire_patch    (:) = nan
        allocate(this%m_deadstemc_to_litter_fire_patch         (begp:endp)) ; this%m_deadstemc_to_litter_fire_patch       (:) = nan
        allocate(this%m_deadstemc_storage_to_litter_fire_patch (begp:endp))
        this%m_deadstemc_storage_to_litter_fire_patch(:) = nan
        allocate(this%m_deadstemc_xfer_to_litter_fire_patch    (begp:endp)) ; this%m_deadstemc_xfer_to_litter_fire_patch  (:) = nan
        allocate(this%m_frootc_to_litter_fire_patch            (begp:endp)) ; this%m_frootc_to_litter_fire_patch          (:) = nan
        allocate(this%m_frootc_storage_to_litter_fire_patch    (begp:endp)) ; this%m_frootc_storage_to_litter_fire_patch  (:) = nan
        allocate(this%m_frootc_xfer_to_litter_fire_patch       (begp:endp)) ; this%m_frootc_xfer_to_litter_fire_patch     (:) = nan
        allocate(this%m_livecrootc_to_litter_fire_patch        (begp:endp)) ; this%m_livecrootc_to_litter_fire_patch      (:) = nan
        allocate(this%m_livecrootc_storage_to_litter_fire_patch(begp:endp)) 
        this%m_livecrootc_storage_to_litter_fire_patch(:) = nan
        allocate(this%m_livecrootc_xfer_to_litter_fire_patch   (begp:endp)) ; this%m_livecrootc_xfer_to_litter_fire_patch (:) = nan
        allocate(this%m_livecrootc_to_deadcrootc_fire_patch    (begp:endp)) ; this%m_livecrootc_to_deadcrootc_fire_patch  (:) = nan
        allocate(this%m_deadcrootc_to_litter_fire_patch        (begp:endp)) ; this%m_deadcrootc_to_litter_fire_patch      (:) = nan
        allocate(this%m_deadcrootc_storage_to_litter_fire_patch(begp:endp))
        this%m_deadcrootc_storage_to_litter_fire_patch (:) = nan
        allocate(this%m_deadcrootc_xfer_to_litter_fire_patch   (begp:endp)) ; this%m_deadcrootc_xfer_to_litter_fire_patch (:) = nan
        allocate(this%m_gresp_storage_to_litter_fire_patch     (begp:endp)) ; this%m_gresp_storage_to_litter_fire_patch   (:) = nan
        allocate(this%m_gresp_xfer_to_litter_fire_patch        (begp:endp)) ; this%m_gresp_xfer_to_litter_fire_patch      (:) = nan
        allocate(this%m_cpool_to_litter_fire_patch             (begp:endp)) ; this%m_cpool_to_litter_fire_patch           (:) = nan

        allocate(this%leafc_xfer_to_leafc_patch                (begp:endp)) ; this%leafc_xfer_to_leafc_patch              (:) = nan
        allocate(this%frootc_xfer_to_frootc_patch              (begp:endp)) ; this%frootc_xfer_to_frootc_patch            (:) = nan
        allocate(this%livestemc_xfer_to_livestemc_patch        (begp:endp)) ; this%livestemc_xfer_to_livestemc_patch      (:) = nan
        allocate(this%deadstemc_xfer_to_deadstemc_patch        (begp:endp)) ; this%deadstemc_xfer_to_deadstemc_patch      (:) = nan
        allocate(this%livecrootc_xfer_to_livecrootc_patch      (begp:endp)) ; this%livecrootc_xfer_to_livecrootc_patch    (:) = nan
        allocate(this%deadcrootc_xfer_to_deadcrootc_patch      (begp:endp)) ; this%deadcrootc_xfer_to_deadcrootc_patch    (:) = nan
        allocate(this%leafc_to_litter_patch                    (begp:endp)) ; this%leafc_to_litter_patch                  (:) = nan
        allocate(this%frootc_to_litter_patch                   (begp:endp)) ; this%frootc_to_litter_patch                 (:) = nan
        allocate(this%leaf_mr_patch                            (begp:endp)) ; this%leaf_mr_patch                          (:) = nan
        allocate(this%froot_mr_patch                           (begp:endp)) ; this%froot_mr_patch                         (:) = nan
        allocate(this%livestem_mr_patch                        (begp:endp)) ; this%livestem_mr_patch                      (:) = nan
        allocate(this%livecroot_mr_patch                       (begp:endp)) ; this%livecroot_mr_patch                     (:) = nan
        allocate(this%grain_mr_patch                           (begp:endp)) ; this%grain_mr_patch                         (:) = nan
        allocate(this%leaf_curmr_patch                         (begp:endp)) ; this%leaf_curmr_patch                       (:) = nan
        allocate(this%froot_curmr_patch                        (begp:endp)) ; this%froot_curmr_patch                      (:) = nan
        allocate(this%livestem_curmr_patch                     (begp:endp)) ; this%livestem_curmr_patch                   (:) = nan
        allocate(this%livecroot_curmr_patch                    (begp:endp)) ; this%livecroot_curmr_patch                  (:) = nan
        allocate(this%grain_curmr_patch                        (begp:endp)) ; this%grain_curmr_patch                      (:) = nan
        allocate(this%leaf_xsmr_patch                          (begp:endp)) ; this%leaf_xsmr_patch                        (:) = nan
        allocate(this%froot_xsmr_patch                         (begp:endp)) ; this%froot_xsmr_patch                       (:) = nan
        allocate(this%livestem_xsmr_patch                      (begp:endp)) ; this%livestem_xsmr_patch                    (:) = nan
        allocate(this%livecroot_xsmr_patch                     (begp:endp)) ; this%livecroot_xsmr_patch                   (:) = nan
        allocate(this%grain_xsmr_patch                         (begp:endp)) ; this%grain_xsmr_patch                       (:) = nan
        allocate(this%xr_patch                                 (begp:endp)) ; this%xr_patch                               (:) = nan
        allocate(this%psnsun_to_cpool_patch                    (begp:endp)) ; this%psnsun_to_cpool_patch                  (:) = nan
        allocate(this%psnshade_to_cpool_patch                  (begp:endp)) ; this%psnshade_to_cpool_patch                (:) = nan
        allocate(this%cpool_to_xsmrpool_patch                  (begp:endp)) ; this%cpool_to_xsmrpool_patch                (:) = nan
        allocate(this%cpool_to_leafc_patch                     (begp:endp)) ; this%cpool_to_leafc_patch                   (:) = nan
        allocate(this%cpool_to_leafc_storage_patch             (begp:endp)) ; this%cpool_to_leafc_storage_patch           (:) = nan
        allocate(this%cpool_to_frootc_patch                    (begp:endp)) ; this%cpool_to_frootc_patch                  (:) = nan
        allocate(this%cpool_to_frootc_storage_patch            (begp:endp)) ; this%cpool_to_frootc_storage_patch          (:) = nan
        allocate(this%cpool_to_livestemc_patch                 (begp:endp)) ; this%cpool_to_livestemc_patch               (:) = nan
        allocate(this%cpool_to_livestemc_storage_patch         (begp:endp)) ; this%cpool_to_livestemc_storage_patch       (:) = nan
        allocate(this%cpool_to_deadstemc_patch                 (begp:endp)) ; this%cpool_to_deadstemc_patch               (:) = nan
        allocate(this%cpool_to_deadstemc_storage_patch         (begp:endp)) ; this%cpool_to_deadstemc_storage_patch       (:) = nan
        allocate(this%cpool_to_livecrootc_patch                (begp:endp)) ; this%cpool_to_livecrootc_patch              (:) = nan
        allocate(this%cpool_to_livecrootc_storage_patch        (begp:endp)) ; this%cpool_to_livecrootc_storage_patch      (:) = nan
        allocate(this%cpool_to_deadcrootc_patch                (begp:endp)) ; this%cpool_to_deadcrootc_patch              (:) = nan
        allocate(this%cpool_to_deadcrootc_storage_patch        (begp:endp)) ; this%cpool_to_deadcrootc_storage_patch      (:) = nan
        allocate(this%cpool_to_gresp_storage_patch             (begp:endp)) ; this%cpool_to_gresp_storage_patch           (:) = nan
        allocate(this%cpool_leaf_gr_patch                      (begp:endp)) ; this%cpool_leaf_gr_patch                    (:) = nan
        allocate(this%cpool_leaf_storage_gr_patch              (begp:endp)) ; this%cpool_leaf_storage_gr_patch            (:) = nan
        allocate(this%transfer_leaf_gr_patch                   (begp:endp)) ; this%transfer_leaf_gr_patch                 (:) = nan
        allocate(this%cpool_froot_gr_patch                     (begp:endp)) ; this%cpool_froot_gr_patch                   (:) = nan
        allocate(this%cpool_froot_storage_gr_patch             (begp:endp)) ; this%cpool_froot_storage_gr_patch           (:) = nan
        allocate(this%transfer_froot_gr_patch                  (begp:endp)) ; this%transfer_froot_gr_patch                (:) = nan
        allocate(this%cpool_livestem_gr_patch                  (begp:endp)) ; this%cpool_livestem_gr_patch                (:) = nan
        allocate(this%cpool_livestem_storage_gr_patch          (begp:endp)) ; this%cpool_livestem_storage_gr_patch        (:) = nan
        allocate(this%transfer_livestem_gr_patch               (begp:endp)) ; this%transfer_livestem_gr_patch             (:) = nan
        allocate(this%cpool_deadstem_gr_patch                  (begp:endp)) ; this%cpool_deadstem_gr_patch                (:) = nan
        allocate(this%cpool_deadstem_storage_gr_patch          (begp:endp)) ; this%cpool_deadstem_storage_gr_patch        (:) = nan
        allocate(this%transfer_deadstem_gr_patch               (begp:endp)) ; this%transfer_deadstem_gr_patch             (:) = nan
        allocate(this%cpool_livecroot_gr_patch                 (begp:endp)) ; this%cpool_livecroot_gr_patch               (:) = nan
        allocate(this%cpool_livecroot_storage_gr_patch         (begp:endp)) ; this%cpool_livecroot_storage_gr_patch       (:) = nan
        allocate(this%transfer_livecroot_gr_patch              (begp:endp)) ; this%transfer_livecroot_gr_patch            (:) = nan
        allocate(this%cpool_deadcroot_gr_patch                 (begp:endp)) ; this%cpool_deadcroot_gr_patch               (:) = nan
        allocate(this%cpool_deadcroot_storage_gr_patch         (begp:endp)) ; this%cpool_deadcroot_storage_gr_patch       (:) = nan
        allocate(this%transfer_deadcroot_gr_patch              (begp:endp)) ; this%transfer_deadcroot_gr_patch            (:) = nan
        allocate(this%leafc_storage_to_xfer_patch              (begp:endp)) ; this%leafc_storage_to_xfer_patch            (:) = nan
        allocate(this%frootc_storage_to_xfer_patch             (begp:endp)) ; this%frootc_storage_to_xfer_patch           (:) = nan
        allocate(this%livestemc_storage_to_xfer_patch          (begp:endp)) ; this%livestemc_storage_to_xfer_patch        (:) = nan
        allocate(this%deadstemc_storage_to_xfer_patch          (begp:endp)) ; this%deadstemc_storage_to_xfer_patch        (:) = nan
        allocate(this%livecrootc_storage_to_xfer_patch         (begp:endp)) ; this%livecrootc_storage_to_xfer_patch       (:) = nan
        allocate(this%deadcrootc_storage_to_xfer_patch         (begp:endp)) ; this%deadcrootc_storage_to_xfer_patch       (:) = nan
        allocate(this%gresp_storage_to_xfer_patch              (begp:endp)) ; this%gresp_storage_to_xfer_patch            (:) = nan
        allocate(this%livestemc_to_deadstemc_patch             (begp:endp)) ; this%livestemc_to_deadstemc_patch           (:) = nan
        allocate(this%livecrootc_to_deadcrootc_patch           (begp:endp)) ; this%livecrootc_to_deadcrootc_patch         (:) = nan
        allocate(this%mr_patch                                 (begp:endp)) ; this%mr_patch                               (:) = nan
        allocate(this%current_gr_patch                         (begp:endp)) ; this%current_gr_patch                       (:) = nan
        allocate(this%transfer_gr_patch                        (begp:endp)) ; this%transfer_gr_patch                      (:) = nan
        allocate(this%storage_gr_patch                         (begp:endp)) ; this%storage_gr_patch                       (:) = nan
        allocate(this%gr_patch                                 (begp:endp)) ; this%gr_patch                               (:) = nan
        allocate(this%ar_patch                                 (begp:endp)) ; this%ar_patch                               (:) = nan
        allocate(this%rr_patch                                 (begp:endp)) ; this%rr_patch                               (:) = nan
        allocate(this%npp_patch                                (begp:endp)) ; this%npp_patch                              (:) = nan
        allocate(this%agnpp_patch                              (begp:endp)) ; this%agnpp_patch                            (:) = nan
        allocate(this%bgnpp_patch                              (begp:endp)) ; this%bgnpp_patch                            (:) = nan
        allocate(this%litfall_patch                            (begp:endp)) ; this%litfall_patch                          (:) = nan
        allocate(this%vegfire_patch                            (begp:endp)) ; this%vegfire_patch                          (:) = nan
        allocate(this%wood_harvestc_patch                      (begp:endp)) ; this%wood_harvestc_patch                    (:) = nan
        allocate(this%cinputs_patch                            (begp:endp)) ; this%cinputs_patch                          (:) = nan
        allocate(this%coutputs_patch                           (begp:endp)) ; this%coutputs_patch                         (:) = nan

        allocate(this%plant_calloc_patch                       (begp:endp)) ; this%plant_calloc_patch                     (:) = nan
        allocate(this%excess_cflux_patch                       (begp:endp)) ; this%excess_cflux_patch                     (:) = nan
        allocate(this%prev_leafc_to_litter_patch               (begp:endp)) ; this%prev_leafc_to_litter_patch             (:) = nan
        allocate(this%prev_frootc_to_litter_patch              (begp:endp)) ; this%prev_frootc_to_litter_patch            (:) = nan
        allocate(this%gpp_patch                                (begp:endp)) ; this%gpp_patch                              (:) = nan
        allocate(this%gpp_before_downreg_patch                 (begp:endp)) ; this%gpp_before_downreg_patch               (:) = nan
        allocate(this%availc_patch                             (begp:endp)) ; this%availc_patch                           (:) = nan
        allocate(this%xsmrpool_recover_patch                   (begp:endp)) ; this%xsmrpool_recover_patch                 (:) = nan
        allocate(this%xsmrpool_c13ratio_patch                  (begp:endp)) ; this%xsmrpool_c13ratio_patch                (:) = nan
        allocate(this%xsmrpool_turnover_patch                  (begp:endp)) ; this%xsmrpool_turnover_patch                (:) = nan

        allocate(this%fire_closs_patch                         (begp:endp)) ; this%fire_closs_patch                       (:) = nan
        allocate(this%cpool_to_grainc_patch                    (begp:endp)) ; this%cpool_to_grainc_patch                  (:) = nan
        allocate(this%cpool_to_grainc_storage_patch            (begp:endp)) ; this%cpool_to_grainc_storage_patch          (:) = nan
        allocate(this%livestemc_to_litter_patch                (begp:endp)) ; this%livestemc_to_litter_patch              (:) = nan
        allocate(this%grainc_to_food_patch                     (begp:endp)) ; this%grainc_to_food_patch                   (:) = nan
        allocate(this%grainc_xfer_to_grainc_patch              (begp:endp)) ; this%grainc_xfer_to_grainc_patch            (:) = nan
        allocate(this%cpool_grain_gr_patch                     (begp:endp)) ; this%cpool_grain_gr_patch                   (:) = nan
        allocate(this%cpool_grain_storage_gr_patch             (begp:endp)) ; this%cpool_grain_storage_gr_patch           (:) = nan
        allocate(this%transfer_grain_gr_patch                  (begp:endp)) ; this%transfer_grain_gr_patch                (:) = nan
        allocate(this%xsmrpool_to_atm_patch                    (begp:endp)) ; this%xsmrpool_to_atm_patch                  (:) = nan
        allocate(this%grainc_storage_to_xfer_patch             (begp:endp)) ; this%grainc_storage_to_xfer_patch           (:) = nan
        allocate(this%frootc_alloc_patch                       (begp:endp)) ; this%frootc_alloc_patch                     (:) = nan
        allocate(this%frootc_loss_patch                        (begp:endp)) ; this%frootc_loss_patch                      (:) = nan
        allocate(this%leafc_alloc_patch                        (begp:endp)) ; this%leafc_alloc_patch                      (:) = nan
        allocate(this%leafc_loss_patch                         (begp:endp)) ; this%leafc_loss_patch                       (:) = nan
        allocate(this%woodc_alloc_patch                        (begp:endp)) ; this%woodc_alloc_patch                      (:) = nan
        allocate(this%woodc_loss_patch                         (begp:endp)) ; this%woodc_loss_patch                       (:) = nan          

        allocate(this%tempavg_agnpp_patch               (begp:endp))                  ; this%tempavg_agnpp_patch (:) = spval
        allocate(this%tempavg_bgnpp_patch               (begp:endp))                  ; this%tempavg_bgnpp_patch (:) = spval
        allocate(this%annavg_agnpp_patch                (begp:endp))                  ; this%annavg_agnpp_patch  (:) = spval ! To detect first year
        allocate(this%annavg_bgnpp_patch                (begp:endp))                  ; this%annavg_bgnpp_patch  (:) = spval ! To detect first year

        allocate(this%agwdnpp_patch                             (begp:endp)) ; this%agwdnpp_patch                          (:) = nan


     end if ! if(.not.use_fates)

     allocate(this%t_scalar_col                      (begc:endc,1:nlevdecomp_full)); this%t_scalar_col (:,:)=spval
     allocate(this%w_scalar_col                      (begc:endc,1:nlevdecomp_full)); this%w_scalar_col (:,:)=spval
     allocate(this%o_scalar_col                      (begc:endc,1:nlevdecomp_full)); this%o_scalar_col (:,:)=spval

     allocate(this%phenology_c_to_litr_met_c_col     (begc:endc,1:nlevdecomp_full)); this%phenology_c_to_litr_met_c_col (:,:)=nan
     allocate(this%phenology_c_to_litr_cel_c_col     (begc:endc,1:nlevdecomp_full)); this%phenology_c_to_litr_cel_c_col (:,:)=nan
     allocate(this%phenology_c_to_litr_lig_c_col     (begc:endc,1:nlevdecomp_full)); this%phenology_c_to_litr_lig_c_col (:,:)=nan

     allocate(this%gap_mortality_c_to_litr_met_c_col (begc:endc,1:nlevdecomp_full)); this%gap_mortality_c_to_litr_met_c_col(:,:)=nan
     allocate(this%gap_mortality_c_to_litr_cel_c_col (begc:endc,1:nlevdecomp_full)); this%gap_mortality_c_to_litr_cel_c_col(:,:)=nan
     allocate(this%gap_mortality_c_to_litr_lig_c_col (begc:endc,1:nlevdecomp_full)); this%gap_mortality_c_to_litr_lig_c_col(:,:)=nan

     allocate(this%gap_mortality_c_to_cwdc_col       (begc:endc,1:nlevdecomp_full)); this%gap_mortality_c_to_cwdc_col  (:,:)=nan
     allocate(this%fire_mortality_c_to_cwdc_col      (begc:endc,1:nlevdecomp_full)); this%fire_mortality_c_to_cwdc_col (:,:)=nan
     allocate(this%m_c_to_litr_met_fire_col          (begc:endc,1:nlevdecomp_full)); this%m_c_to_litr_met_fire_col     (:,:)=nan
     allocate(this%m_c_to_litr_cel_fire_col          (begc:endc,1:nlevdecomp_full)); this%m_c_to_litr_cel_fire_col     (:,:)=nan
     allocate(this%m_c_to_litr_lig_fire_col          (begc:endc,1:nlevdecomp_full)); this%m_c_to_litr_lig_fire_col     (:,:)=nan
     allocate(this%harvest_c_to_litr_met_c_col       (begc:endc,1:nlevdecomp_full)); this%harvest_c_to_litr_met_c_col  (:,:)=nan
     allocate(this%harvest_c_to_litr_cel_c_col       (begc:endc,1:nlevdecomp_full)); this%harvest_c_to_litr_cel_c_col  (:,:)=nan
     allocate(this%harvest_c_to_litr_lig_c_col       (begc:endc,1:nlevdecomp_full)); this%harvest_c_to_litr_lig_c_col  (:,:)=nan
     allocate(this%harvest_c_to_cwdc_col             (begc:endc,1:nlevdecomp_full)); this%harvest_c_to_cwdc_col        (:,:)=nan
     allocate(this%phr_vr_col                        (begc:endc,1:nlevdecomp_full)); this%phr_vr_col                   (:,:)=nan 
     allocate(this%fphr_col                          (begc:endc,1:nlevgrnd))       ; this%fphr_col                     (:,:)=nan 

     allocate(this%dwt_frootc_to_litr_met_c_col      (begc:endc,1:nlevdecomp_full)); this%dwt_frootc_to_litr_met_c_col (:,:)=nan
     allocate(this%dwt_frootc_to_litr_cel_c_col      (begc:endc,1:nlevdecomp_full)); this%dwt_frootc_to_litr_cel_c_col (:,:)=nan
     allocate(this%dwt_frootc_to_litr_lig_c_col      (begc:endc,1:nlevdecomp_full)); this%dwt_frootc_to_litr_lig_c_col (:,:)=nan
     allocate(this%dwt_livecrootc_to_cwdc_col        (begc:endc,1:nlevdecomp_full)); this%dwt_livecrootc_to_cwdc_col   (:,:)=nan
     allocate(this%dwt_deadcrootc_to_cwdc_col        (begc:endc,1:nlevdecomp_full)); this%dwt_deadcrootc_to_cwdc_col   (:,:)=nan

     allocate(this%dwt_closs_col                     (begc:endc))                  ; this%dwt_closs_col             (:)  =nan
     allocate(this%crop_seedc_to_leaf_patch          (begp:endp))                  ; this%crop_seedc_to_leaf_patch  (:)  =nan

     allocate(this%dwt_seedc_to_leaf_patch           (begp:endp))                  ; this%dwt_seedc_to_leaf_patch      (:) =nan
     allocate(this%dwt_seedc_to_leaf_grc             (begg:endg))                  ; this%dwt_seedc_to_leaf_grc        (:) =nan
     allocate(this%dwt_seedc_to_deadstem_patch       (begp:endp))                  ; this%dwt_seedc_to_deadstem_patch  (:) =nan
     allocate(this%dwt_seedc_to_deadstem_grc         (begg:endg))                  ; this%dwt_seedc_to_deadstem_grc    (:) =nan
     allocate(this%dwt_conv_cflux_patch              (begp:endp))                  ; this%dwt_conv_cflux_patch         (:) =nan
     allocate(this%dwt_conv_cflux_grc                (begg:endg))                  ; this%dwt_conv_cflux_grc           (:) =nan
     ! allocate(this%dwt_conv_cflux_dribbled_grc       (begg:endg))                  ; this%dwt_conv_cflux_dribbled_grc  (:) =nan
     allocate(this%dwt_prod10c_gain_patch            (begp:endp))                  ; this%dwt_prod10c_gain_patch       (:) =nan
     allocate(this%dwt_prod100c_gain_patch           (begp:endp))                  ; this%dwt_prod100c_gain_patch      (:) =nan
     allocate(this%dwt_crop_productc_gain_patch      (begp:endp))                  ; this%dwt_crop_productc_gain_patch (:) =nan
     allocate(this%dwt_slash_cflux_col               (begc:endc))                  ; this%dwt_slash_cflux_col          (:) =nan

     allocate(this%dwt_conv_cflux_col                (begc:endc))                  ; this%dwt_conv_cflux_col        (:)  =nan
     allocate(this%dwt_prod10c_gain_col              (begc:endc))                  ; this%dwt_prod10c_gain_col      (:)  =nan
     allocate(this%dwt_prod100c_gain_col             (begc:endc))                  ; this%dwt_prod100c_gain_col     (:)  =nan
     allocate(this%som_c_leached_col                 (begc:endc))                  ; this%som_c_leached_col         (:)  =nan
     allocate(this%somc_fire_col                     (begc:endc))                  ; this%somc_fire_col             (:)  =nan
     allocate(this%landuseflux_col                   (begc:endc))                  ; this%landuseflux_col           (:)  =nan
     allocate(this%landuptake_col                    (begc:endc))                  ; this%landuptake_col            (:)  =nan
     allocate(this%prod1c_loss_col                   (begc:endc))                  ; this%prod1c_loss_col           (:)  =nan
     allocate(this%prod10c_loss_col                  (begc:endc))                  ; this%prod10c_loss_col          (:)  =nan
     allocate(this%prod100c_loss_col                 (begc:endc))                  ; this%prod100c_loss_col         (:)  =nan
     allocate(this%product_closs_col                 (begc:endc))                  ; this%product_closs_col         (:)  =nan

     allocate(this%dwt_prod10c_gain_grc              (begg:endg))                  ; this%dwt_prod10c_gain_grc      (:)  =nan
     allocate(this%dwt_prod100c_gain_grc             (begg:endg))                  ; this%dwt_prod100c_gain_grc     (:)  =nan
     allocate(this%hrv_deadstemc_to_prod10c_grc      (begg:endg))                  ; this%hrv_deadstemc_to_prod10c_grc (:) = nan
     allocate(this%hrv_deadstemc_to_prod100c_grc     (begg:endg))                  ; this%hrv_deadstemc_to_prod100c_grc(:) = nan

     allocate(this%bgc_cpool_ext_inputs_vr_col       (begc:endc, 1:nlevdecomp_full,ndecomp_pools))
     this%bgc_cpool_ext_inputs_vr_col(:,:,:) = nan
     allocate(this%bgc_cpool_ext_loss_vr_col         (begc:endc, 1:nlevdecomp_full,ndecomp_pools))
     this%bgc_cpool_ext_loss_vr_col(:,:,:) = nan

     allocate(this%lithr_col                         (begc:endc))                  ; this%lithr_col                 (:)  =nan
     allocate(this%somhr_col                         (begc:endc))                  ; this%somhr_col                 (:)  =nan
     allocate(this%hr_vr_col                         (begc:endc,1:nlevdecomp_full)); this%hr_vr_col                 (:,:)=nan
     allocate(this%hr_col                            (begc:endc))                  ; this%hr_col                    (:)  =nan
     allocate(this%sr_col                            (begc:endc))                  ; this%sr_col                    (:)  =nan
     allocate(this%er_col                            (begc:endc))                  ; this%er_col                    (:)  =nan
     allocate(this%litfire_col                       (begc:endc))                  ; this%litfire_col               (:)  =nan
     allocate(this%somfire_col                       (begc:endc))                  ; this%somfire_col               (:)  =nan
     allocate(this%totfire_col                       (begc:endc))                  ; this%totfire_col               (:)  =nan
     allocate(this%nep_col                           (begc:endc))                  ; this%nep_col                   (:)  =nan
     allocate(this%nbp_col                           (begc:endc))                  ; this%nbp_col                   (:)  =nan
     allocate(this%nee_col                           (begc:endc))                  ; this%nee_col                   (:)  =nan
     allocate(this%cwdc_hr_col                       (begc:endc))                  ; this%cwdc_hr_col               (:)  =nan
     allocate(this%cwdc_loss_col                     (begc:endc))                  ; this%cwdc_loss_col             (:)  =nan
     allocate(this%litterc_loss_col                  (begc:endc))                  ; this%litterc_loss_col          (:)  =nan
     allocate(this%rr_col                            (begc:endc))                  ; this%rr_col                    (:)  =nan
     allocate(this%ar_col                            (begc:endc))                  ; this%ar_col                    (:)  =nan
     allocate(this%gpp_col                           (begc:endc))                  ; this%gpp_col                   (:)  =nan
     allocate(this%npp_col                           (begc:endc))                  ; this%npp_col                   (:)  =nan
     allocate(this%fire_closs_p2c_col                (begc:endc))                  ; this%fire_closs_p2c_col        (:)  =nan
     allocate(this%fire_closs_col                    (begc:endc))                  ; this%fire_closs_col            (:)  =nan
     allocate(this%fire_decomp_closs_col             (begc:endc))                  ; this%fire_decomp_closs_col     (:)  =nan
     allocate(this%litfall_col                       (begc:endc))                  ; this%litfall_col               (:)  =nan
     allocate(this%vegfire_col                       (begc:endc))                  ; this%vegfire_col               (:)  =nan
     allocate(this%wood_harvestc_col                 (begc:endc))                  ; this%wood_harvestc_col         (:)  =nan
     allocate(this%hrv_xsmrpool_to_atm_col           (begc:endc))                  ; this%hrv_xsmrpool_to_atm_col   (:)  =nan 

     allocate(this%hrv_deadstemc_to_prod10c_col(begc:endc))                                                    
     this%hrv_deadstemc_to_prod10c_col(:)= nan

     allocate(this%hrv_deadstemc_to_prod100c_col(begc:endc))                                                   
     this%hrv_deadstemc_to_prod100c_col(:)= nan

     allocate(this%hrv_cropc_to_prod1c_col(begc:endc))
     this%hrv_cropc_to_prod1c_col(:) = nan

     allocate(this%m_decomp_cpools_to_fire_vr_col(begc:endc,1:nlevdecomp_full,1:ndecomp_pools))                
     this%m_decomp_cpools_to_fire_vr_col(:,:,:)= nan

     allocate(this%m_decomp_cpools_to_fire_col(begc:endc,1:ndecomp_pools))                                     
     this%m_decomp_cpools_to_fire_col(:,:)= nan

     allocate(this%decomp_cpools_sourcesink_col(begc:endc,1:nlevdecomp_full,1:ndecomp_pools))                  
     this%decomp_cpools_sourcesink_col(:,:,:)= nan

     allocate(this%decomp_cascade_hr_vr_col(begc:endc,1:nlevdecomp_full,1:ndecomp_cascade_transitions))        
     this%decomp_cascade_hr_vr_col(:,:,:)= spval

     allocate(this%decomp_cascade_hr_col(begc:endc,1:ndecomp_cascade_transitions))                             
     this%decomp_cascade_hr_col(:,:)= nan

     allocate(this%decomp_cascade_ctransfer_vr_col(begc:endc,1:nlevdecomp_full,1:ndecomp_cascade_transitions)) 
     this%decomp_cascade_ctransfer_vr_col(:,:,:)= nan

     allocate(this%decomp_cascade_ctransfer_col(begc:endc,1:ndecomp_cascade_transitions))                      
     this%decomp_cascade_ctransfer_col(:,:)= nan

     allocate(this%decomp_k_col(begc:endc,1:nlevdecomp_full,1:ndecomp_cascade_transitions))                    
     this%decomp_k_col(:,:,:)= spval

     allocate(this%decomp_cpools_leached_col(begc:endc,1:ndecomp_pools))              
     this%decomp_cpools_leached_col(:,:)= nan

     allocate(this%decomp_cpools_transport_tendency_col(begc:endc,1:nlevdecomp_full,1:ndecomp_pools))          
     this%decomp_cpools_transport_tendency_col(:,:,:)= nan


     allocate(this%tempsum_npp_patch     (begp:endp)) ; this%tempsum_npp_patch     (:) = nan
     allocate(this%annsum_npp_patch      (begp:endp)) ; this%annsum_npp_patch      (:) = nan
     allocate(this%annsum_npp_col        (begc:endc)) ; this%annsum_npp_col        (:) = nan
     allocate(this%lag_npp_col           (begc:endc)) ; this%lag_npp_col           (:) = spval

     ! debug
     allocate(this%plant_to_litter_cflux (begc:endc)) ;	this%plant_to_litter_cflux (:) = nan
     allocate(this%plant_to_cwd_cflux    (begc:endc)) ;	this%plant_to_cwd_cflux	   (:) = nan
     allocate(this%allocation_leaf       (begp:endp)) ; this%allocation_leaf       (:) = nan
     allocate(this%allocation_stem       (begp:endp)) ; this%allocation_stem       (:) = nan
     allocate(this%allocation_froot      (begp:endp)) ; this%allocation_froot      (:) = nan

     ! clm_interface & pflotran
     !------------------------------------------------------------------------
     allocate(this%externalc_to_decomp_cpools_col(begc:endc,1:nlevdecomp_full,1:ndecomp_pools))
     this%externalc_to_decomp_cpools_col(:,:,:) = spval
     allocate(this%externalc_to_decomp_delta_col (begc:endc))
     this%externalc_to_decomp_delta_col (:)     = spval
     allocate(this%f_co2_soil_vr_col             (begc:endc,1:nlevdecomp_full))
     this%f_co2_soil_vr_col             (:,:)   = nan
     allocate(this%f_co2_soil_col                (begc:endc))
     this%f_co2_soil_col                (:)     = nan
     !------------------------------------------------------------------------
  end subroutine InitAllocate; 

  
end module CNCarbonFluxType
