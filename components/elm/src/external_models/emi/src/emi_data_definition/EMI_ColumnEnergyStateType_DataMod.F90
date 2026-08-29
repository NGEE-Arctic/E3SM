module EMI_ColumnEnergyStateType_DataMod
  !
  use EMI_ColumnEnergyStateType_Constants
  !
  implicit none
  !
  public :: EMI_ColumnEnergyStateType_DataInfoByID

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_ColumnEnergyStateType_DataInfoByID(data_id, id_val, name_val, long_name_val,&
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

    case(L2E_STATE_TSOIL_NLEVGRND_COL)
       id_val         =  L2E_STATE_TSOIL_NLEVGRND_COL
       name_val       =  'Soil temperature'
       long_name_val  =  'Soil temperature: ELM to EM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.

    case(L2E_STATE_TSNOW_COL)
       id_val         =  L2E_STATE_TSNOW_COL
       name_val       =  'Snow temperature'
       long_name_val  =  'Snow temperature: ELM to EM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_nlevsno_plus_one
       dim2_end_name  =  dimname_zero
       data_found   =  .true.

    case(L2E_STATE_TH2OSFC_COL)
       id_val         =  L2E_STATE_TH2OSFC_COL
       name_val       =  'Standing water temperature'
       long_name_val  =  'Standing water temperature: ELM to EM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(L2E_STATE_TSOI10CM_COL)
       id_val         =  L2E_STATE_TSOI10CM_COL
       name_val       =  'Soil temperature in top 10cm'
       long_name_val  =  'Soil temperature in top 10cm: ELM to EM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(L2E_STATE_TSOIL_NLEVSOI_COL)
       id_val         =  L2E_STATE_TSOIL_NLEVSOI_COL
       name_val       =  'Soil temperature in nlevsoi'
       long_name_val  =  'Soil temperature in nlevsoi: ELM to EM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevsoi
       data_found   =  .true.

    case(E2L_STATE_TSOIL_NLEVGRND_COL)
       id_val         =  E2L_STATE_TSOIL_NLEVGRND_COL
       name_val       =  'Soil temperature'
       long_name_val  =  'Soil temperature: EM to ELM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.

    case(E2L_STATE_TSNOW_NLEVSNOW_COL)
       id_val         =  E2L_STATE_TSNOW_NLEVSNOW_COL
       name_val       =  'Snow temperature'
       long_name_val  =  'Snow temperature: EM to ELM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_nlevsno_plus_one
       dim2_end_name  =  dimname_zero
       data_found   =  .true.

    case(E2L_STATE_TH2OSFC_COL)
       id_val         =  E2L_STATE_TH2OSFC_COL
       name_val       =  'Standing water temperature'
       long_name_val  =  'Standing water temperature: EM to ELM'
       units_val      =  '[K]'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.
    end select
    
  end subroutine EMI_ColumnEnergyStateType_DataInfoByID
    
end module EMI_ColumnEnergyStateType_DataMod
