program demo_alquimia

  use ExternalModelInterfaceMod
  use clm_varctl                , only : iulog
  use decompMod                 , only : bounds_type, get_proc_bounds, get_proc_clumps, get_clump_bounds
  use clm_instMod               , only : clm_inst_biogeophys
  use clm_varpar                , only : clm_varpar_init
  use clm_varcon                , only : clm_varcon_init
  use clm_varpar                , only : nlevdecomp_full, ndecomp_pools
  use spmdMod                   , only : spmd_init
  use ExternalModelConstants    , only : EM_ID_ALQUIMIA, EM_ALQUIMIA_SOLVE_STAGE
  use clm_instMod               , only : soilstate_vars, waterstate_vars, waterflux_vars
  use clm_instMod               , only : energyflux_vars, temperature_vars, carbonstate_vars, carbonflux_vars, nitrogenstate_vars
  use shr_kind_mod              , only : r8 => shr_kind_r8, SHR_KIND_CL
  use ColumnType                , only : col_pp
  use LandunitType              , only : lun_pp
  use CNDecompCascadeConType, only : init_decomp_cascade_constants
  
#include <petsc/finclude/petsc.h>
  use petscsys
  

  implicit none

  type(bounds_type) :: bounds_proc, bounds_clump
  integer           :: clump_rank, nclumps
  integer           :: c, num_filter_lun, num_hydrologyc
  integer, pointer  :: filter_lun(:), filter_hydrologyc(:)
  
  integer           :: timestep, ntimesteps, k
  
  PetscErrorCode :: ierr

  nclumps = 1
  write(iulog,*)''
  write(iulog,*)'This is a demo for the External Model Interface (EMI)'
  write(iulog,*)''

  call spmd_init()
  call set_namelist_variables()
  call clm_varpar_init()
  call clm_varcon_init()
  call decompInit()

  call get_proc_bounds(bounds_proc)
  
  ! Initialize the landunit data types
  call lun_pp%Init (bounds_proc%begl_all, bounds_proc%endl_all)

  ! Initialize the column data types
  call col_pp%Init (bounds_proc%begc_all, bounds_proc%endc_all)


  call clm_inst_biogeophys(bounds_proc)
  call init_decomp_cascade_constants()
  
  call initialize_clm_data_structures(bounds_proc)
  

  call EMI_Determine_Active_EMs()

  write(iulog,*)'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
  write(iulog,*)'1. Lets initialize the Alquimia EM'
  write(iulog,*)'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

  call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
  call EMI_Init_EM(EM_ID_ALQUIMIA)

  num_hydrologyc = 1
  num_filter_lun = 1
  allocate(filter_lun(num_filter_lun))
  allocate(filter_hydrologyc(num_hydrologyc))
  do c = 1, num_filter_lun
     filter_lun(c) = c
     filter_hydrologyc(c) = c
  end do

  write(iulog,*)'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
  write(iulog,*)'2. Lets now timestep the Alquimia EM'
  write(iulog,*)'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

  write(*,*)'num_filter_lun : ',num_filter_lun
  write(*,*)'nlevdecomp_full: ',nlevdecomp_full
  write(*,*)'ndecomp_pools  : ',ndecomp_pools
  
  ntimesteps = 24

  do timestep = 1, ntimesteps
    write(iulog,'(a,i3,a,i3)')'  TIMESTEP: ',timestep,' of ',ntimesteps

  !$OMP PARALLEL DO PRIVATE (clump_rank, bounds_clump)
  do clump_rank = 1, nclumps

     call get_clump_bounds(clump_rank, bounds_clump)
     
     write(iulog,*) 'Carbon pools: before: '
      write(iulog,'(8e12.5)') carbonstate_vars%decomp_cpools_vr_col(1,1,1:8) ;
     write(iulog,*) 'Nitrogen pools: before: '
     write(iulog,'(8e12.5)') nitrogenstate_vars%decomp_npools_vr_col(1,1,1:8) ;

     write(iulog,*)'  Running Alquimia SOLVE'
     call EMI_Driver(                                                 &
          em_id             = EM_ID_ALQUIMIA                    , &
          em_stage          = EM_ALQUIMIA_SOLVE_STAGE            , &
          dt                = 3600._r8                              , &
          clump_rank        = bounds_clump%clump_index              , &
          num_filter_lun    = num_filter_lun                        , &
          filter_lun        = filter_lun                     , &
          num_hydrologyc    = num_hydrologyc                        , &
          filter_hydrologyc = filter_hydrologyc                     , &
          soilstate_vars    = soilstate_vars                        , &
          carbonstate_vars  = carbonstate_vars                      , &
          carbonflux_vars   = carbonflux_vars                       , &
          nitrogenstate_vars= nitrogenstate_vars                , &
          waterstate_vars   = waterstate_vars                       , &
          temperature_vars  = temperature_vars)

          write(iulog,'(a)') 'Carbon pools: after: '
         write(iulog,'(8e12.5)') carbonstate_vars%decomp_cpools_vr_col(1,1,1:8) ;
          
          write(iulog,'(a)') 'Nitrogen pools: after: '
             write(iulog,'(8e12.5)') nitrogenstate_vars%decomp_npools_vr_col(1,1,1:8) ;
             write(iulog,'(a,e12.4,a,e12.4,a,e12.4)') 'RH: ' ,carbonflux_vars%hr_vr_col(1,1),' NH4: ',&
                    nitrogenstate_vars%smin_nh4_vr_col(1,1),' NO3: ',nitrogenstate_vars%smin_no3_vr_col(1,1)

  enddo
  !$OMP END PARALLEL DO
  
  
  enddo
  
end program demo_alquimia

!-----------------------------------------------------------------------
subroutine set_namelist_variables()

  use clm_varctl, only : use_em_alquimia, use_vertsoilc, alquimia_inputfile, alquimia_CO2_name,alquimia_handsoff

  implicit none

  use_em_alquimia = .true.
  use_vertsoilc      = .true.
  alquimia_inputfile = 'alquimia_io/CTC_generated.in'
  alquimia_CO2_name  = 'HRimm'
  alquimia_handsoff = .false.

end subroutine set_namelist_variables
!-----------------------------------------------------------------------
subroutine decompInit ()
  !
  use decompMod
  use clm_varctl, only : iulog
  use abortutils      , only : endrun
  use shr_log_mod     , only : errMsg => shr_log_errMsg
  use spmdMod , only : iam
  !
  implicit none
  integer :: ier                    ! error code
  integer :: ncells, ntopounits, nlunits, ncols, npfts, nCohorts

  clump_pproc = 1
  nclumps     = 1

  ncells      = 1
  ntopounits  = 1
  nlunits     = 1
  ncols       = 1
  npfts       = 16
  nCohorts    = 1

  allocate(procinfo%cid(clump_pproc), stat=ier)
  if (ier /= 0) then
     write(iulog,*) 'decompInit_lnd(): allocation error for procinfo%cid'
     call endrun(msg=errMsg(__FILE__, __LINE__))
  endif
  procinfo%nclumps    = clump_pproc
  procinfo%cid(:)     = 1
  procinfo%ncells     = ncells
  procinfo%ntopounits = ntopounits
  procinfo%nlunits    = nlunits
  procinfo%ncols      = ncols

  procinfo%npfts      = npfts
  procinfo%nCohorts   = nCohorts
  procinfo%begg       = 1
  procinfo%begt       = 1
  procinfo%begl       = 1
  procinfo%begc       = 1
  procinfo%begp       = 1
  procinfo%begCohort  = 1
  procinfo%endg       = ncells
  procinfo%endt       = ntopounits
  procinfo%endl       = nlunits
  procinfo%endc       = ncols
  procinfo%endp       = npfts
  procinfo%endCohort  = nCohorts

  allocate(clumps(nclumps), stat=ier)
  if (ier /= 0) then
     write(iulog,*) 'decompInit_lnd(): allocation error for clumps'
     call endrun(msg=errMsg(__FILE__, __LINE__))
  end if
  clumps(:)%owner      = iam
  clumps(:)%ncells     = ncells
  clumps(:)%ntopounits = ntopounits
  clumps(:)%nlunits    = nlunits
  clumps(:)%ncols      = ncols
  clumps(:)%npfts      = npfts
  clumps(:)%nCohorts   = nCohorts
  clumps(:)%begg       = 1
  clumps(:)%begt       = 1
  clumps(:)%begl       = 1
  clumps(:)%begc       = 1
  clumps(:)%begp       = 1
  clumps(:)%begCohort  = 1
  clumps(:)%endg       = ncells
  clumps(:)%endt       = ntopounits
  clumps(:)%endl       = nlunits
  clumps(:)%endc       = ncols
  clumps(:)%endp       = npfts
  clumps(:)%endCohort  = nCohorts

end subroutine decompInit

!-----------------------------------------------------------------------
subroutine initialize_clm_data_structures(bounds_proc)

  use decompMod     , only : bounds_type
  use clm_instMod   , only : soilstate_vars, waterstate_vars, waterflux_vars
  use ColumnDataType, only : col_ws, col_es
  use clm_instMod   , only : energyflux_vars, carbonstate_vars, nitrogenstate_vars, carbonflux_vars
  use clm_instMod   , only : temperature_vars
  use shr_kind_mod  , only : r8 => shr_kind_r8, SHR_KIND_CL
  use clm_varpar    , only : nlevgrnd
  use clm_varpar    , only : nlevdecomp_full, ndecomp_pools, nlevdecomp
  use clm_varcon    , only : istsoil
  use shr_const_mod , only : SHR_CONST_PI
  use clm_varctl    , only : iulog
  use ColumnType                , only : col_pp
  use LandunitType              , only : lun_pp
  use CNDecompCascadeConType    , only : decomp_cascade_con
  !
  implicit none
  !
  type(bounds_type) :: bounds_proc
  !
  integer :: begc, endc, ncol
  integer :: c,j,k
  real(r8) :: counter
  real(r8), dimension(8) :: rateconstants

  begc = bounds_proc%begc; endc = bounds_proc%endc;
  ncol = endc-begc+1;
  counter = 0.d0
  
  decomp_cascade_con%decomp_pool_name_history(1)='LITR1'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(1)=.true.
  decomp_cascade_con%initial_cn_ratio(1) = 20_r8
  rateconstants(1) = 1.204
  decomp_cascade_con%cascade_receiver_pool(1)=5
  decomp_cascade_con%decomp_pool_name_history(2)='LITR2'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(2)=.true.
  decomp_cascade_con%initial_cn_ratio(2) = 20_r8;
  rateconstants(2) = 7.26e-02
  decomp_cascade_con%cascade_receiver_pool(2)=6
  decomp_cascade_con%decomp_pool_name_history(3)='LITR3'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(3)=.true.
  decomp_cascade_con%initial_cn_ratio(3) = 20_r8;
  rateconstants(3) = 1.41e-02
  decomp_cascade_con%cascade_receiver_pool(3)=7
  decomp_cascade_con%decomp_pool_name_history(4)='CWD'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(4)=.true.
  decomp_cascade_con%initial_cn_ratio(4) = 20_r8;
  rateconstants(4) = 1.0e-04
  decomp_cascade_con%cascade_receiver_pool(4)=2
  decomp_cascade_con%decomp_pool_name_history(5)='SOIL1'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(5)=.false.
  decomp_cascade_con%initial_cn_ratio(5)=12.0_r8;
  rateconstants(5) = 7.26e-2
  decomp_cascade_con%cascade_receiver_pool(5)=6
  decomp_cascade_con%decomp_pool_name_history(6)='SOIL2'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(6)=.false.
  decomp_cascade_con%initial_cn_ratio(6)=12.0_r8;
  rateconstants(6) = 1.41e-02
  decomp_cascade_con%cascade_receiver_pool(6)=7
  decomp_cascade_con%decomp_pool_name_history(7)='SOIL3'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(7)=.false.
  decomp_cascade_con%initial_cn_ratio(7)=10.0_r8;
  rateconstants(7) = 1.41e-3
  decomp_cascade_con%cascade_receiver_pool(7)=8
  decomp_cascade_con%decomp_pool_name_history(8)='SOIL4'
  decomp_cascade_con%floating_cn_ratio_decomp_pools(8)=.false.
  decomp_cascade_con%initial_cn_ratio(8)=10.0_r8;
  rateconstants(8) = 1.0e-4
  decomp_cascade_con%cascade_receiver_pool(8)=-1

  do c = begc, endc
      col_pp%active(c) = .true.
      col_pp%landunit(c) = c
      lun_pp%itype(col_pp%landunit(c)) = istsoil

     do j = 1, nlevgrnd
        soilstate_vars%cellclay_col(c,j)    = 0.2_r8
        soilstate_vars%watsat_col(c,j)      = 0.25_r8
        waterstate_vars%h2osoi_liq_col(c,j) = (0.3_r8*(c**2._r8) + j)/100._r8
        waterstate_vars%h2osoi_ice_col(c,j) = 1._r8 - (0.3_r8*(c**2._r8) + j)/100._r8
        col_es%t_soisno(c,j)      = 273.15_r8 + j*2_r8
        temperature_vars%t_soisno_col(c,j) = col_es%t_soisno(c,j)
     enddo
     do j = 1, nlevdecomp_full
       counter = 1.0_r8
        do k = 1, ndecomp_pools
           carbonstate_vars%decomp_cpools_vr_col(c,j,k) = 1e-10;
           nitrogenstate_vars%decomp_npools_vr_col(c,j,k) = 1e-10/decomp_cascade_con%initial_cn_ratio(k);
           ! counter = counter + 1.d0
           carbonflux_vars%decomp_k_col(c,j,k) = rateconstants(k)/(3600*24) ! Units of 1/s
           ! Duplicate calculation for rate_decomp in PFLOTRAN SOMDEC, assuming dt=3600
           carbonflux_vars%decomp_k_col(c,j,k) = (1-exp(-carbonflux_vars%decomp_k_col(c,j,k)*3600))/3600
           ! counter=counter+0.01_r8
        end do
        carbonstate_vars%decomp_cpools_vr_col(c,j,1) = 1e3;
        nitrogenstate_vars%decomp_npools_vr_col(c,j,1) = 1e3/decomp_cascade_con%initial_cn_ratio(1);
        nitrogenstate_vars%smin_nh4_vr_col(c,j) = 1e-5
        nitrogenstate_vars%smin_no3_vr_col(c,j) = 1e-5
     end do
     
  enddo

end subroutine initialize_clm_data_structures
