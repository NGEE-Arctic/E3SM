module EMI_CNCarbonFluxType_DataMod
  !
  use EMI_CNCarbonFluxType_Constants
  !
  implicit none
  !
  public :: EMI_CNCarbonFluxType_DataInfoByID

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_CNCarbonFluxType_DataInfoByID(data_id, id_val, name_val, long_name_val,&
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

    case(L2E_FLUX_HETEROTROPHIC_RESP_POOLS_VERTICALLY_RESOLVED)
       id_val         =  L2E_FLUX_HETEROTROPHIC_RESP_POOLS_VERTICALLY_RESOLVED
       name_val       =  'decomp cascade hr vr'
       long_name_val  =  'decomp cascade hr vr: ELM to EM'
       units_val      =  '[gC/m3/s]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevdecomp_full
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_ndecomp_pools
       data_found   =  .true.

    case(L2E_FLUX_HETEROTROPHIC_RESP_VERTICALLY_RESOLVED)
       id_val         =  L2E_FLUX_HETEROTROPHIC_RESP_VERTICALLY_RESOLVED
       name_val       =  'hr vr'
       long_name_val  =  'hr vr: ELM to EM'
       units_val      =  '[gC/m3/s]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevdecomp_full
       data_found   =  .true.

    case(L2E_FLUX_SOIL_POOL_DECOMP_K)
       id_val         =  L2E_FLUX_SOIL_POOL_DECOMP_K
       name_val       =  'decomp k constants'
       long_name_val  =  'decomp k constants: ELM to EM'
       units_val      =  '[1/s]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevdecomp_full
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_ndecomp_pools
       data_found   =  .true.

    case(E2L_FLUX_HETEROTROPHIC_RESP_POOLS_VERTICALLY_RESOLVED)
       id_val         =  E2L_FLUX_HETEROTROPHIC_RESP_POOLS_VERTICALLY_RESOLVED
       name_val       =  'decomp cascade hr vr'
       long_name_val  =  'decomp cascade hr vr: EM to ELM'
       units_val      =  '[gC/m3/s]'
       is_real_type   =  .true.
       ndim           =  3
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevdecomp_full
       dim3_beg_name  =  dimname_one
       dim3_end_name  =  dimname_ndecomp_pools
       data_found   =  .true.

    case(E2L_FLUX_HETEROTROPHIC_RESP_VERTICALLY_RESOLVED)
       id_val         =  E2L_FLUX_HETEROTROPHIC_RESP_VERTICALLY_RESOLVED
       name_val       =  'hr vr'
       long_name_val  =  'hr vr: EM to ELM'
       units_val      =  '[gC/m3/s]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevdecomp_full
       data_found   =  .true.

    case(E2L_FLUX_HETEROTROPHIC_RESP)
       id_val         =  E2L_FLUX_HETEROTROPHIC_RESP
       name_val       =  'hr'
       long_name_val  =  'hr: EM to ELM'
       units_val      =  '[gC/m2/s]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(E2L_FLUX_METHANE)
       id_val         =  E2L_FLUX_METHANE
       name_val       =  'hr'
       long_name_val  =  'hr: EM to ELM'
       units_val      =  '[gC/m2/s]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(E2L_FLUX_DIC_RUNOFF)
       id_val         =  E2L_FLUX_DIC_RUNOFF
       name_val       =  'DIC_runoff'
       long_name_val  =  'DIC_runoff: EM to ELM'
       units_val      =  '[gC/m2/s]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(E2L_FLUX_DOC_RUNOFF)
       id_val         =  E2L_FLUX_DOC_RUNOFF
       name_val       =  'DOC_runoff'
       long_name_val  =  'DOC_runoff: EM to ELM'
       units_val      =  '[gC/m2/s]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.
    end select
    
  end subroutine EMI_CNCarbonFluxType_DataInfoByID
    
end module EMI_CNCarbonFluxType_DataMod
