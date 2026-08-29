module EMI_ColumnWaterStateType_DataMod
  !
  use EMI_ColumnWaterStateType_Constants
  !
  implicit none
  !
  public :: EMI_ColumnWaterStateType_DataInfoByID

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_ColumnWaterStateType_DataInfoByID(data_id, id_val, name_val, long_name_val,&
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

    case(L2E_STATE_SOIL_LIQ_VOL_COL)
       id_val         =  L2E_STATE_SOIL_LIQ_VOL_COL
       name_val       =  'Soil liquid water volume'
       long_name_val  =  'Soil liquid water volume: ELM to EM'
       units_val      =  'm3/m3'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.

    case(L2E_STATE_SOIL_ICE_VOL_COL)
       id_val         =  L2E_STATE_SOIL_ICE_VOL_COL
       name_val       =  'Soil ice water volume'
       long_name_val  =  'Soil ice water volume: ELM to EM'
       units_val      =  'm3/m3'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.

    case(L2E_STATE_H2OSFC_COL)
       id_val         =  L2E_STATE_H2OSFC_COL
       name_val       =  'Surface water'
       long_name_val  =  'Surface water: ELM to EM'
       units_val      =  'mm'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(L2E_STATE_SALINITY_COL)
       id_val         =  L2E_STATE_SALINITY_COL
       name_val       =  'Flood salinity'
       long_name_val  =  'Flood salinity: ELM to EM'
       units_val      =  'ppt'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(L2E_STATE_TIDE_NITRATE_COL)
       id_val         =  L2E_STATE_TIDE_NITRATE_COL
       name_val       =  'Flood nitrate'
       long_name_val  =  'Flood nitrate: ELM to EM'
       units_val      =  'ppt'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(L2E_STATE_H2OSFC_TIDE_COL)
       id_val         =  L2E_STATE_H2OSFC_TIDE_COL
       name_val       =  'Tide height'
       long_name_val  =  'Tide height: ELM to EM'
       units_val      =  'mm'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.
    end select
    
  end subroutine EMI_ColumnWaterStateType_DataInfoByID
    
end module EMI_ColumnWaterStateType_DataMod
