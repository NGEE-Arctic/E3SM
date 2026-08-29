module EMI_ColumnWaterFluxType_DataMod
  !
  use EMI_ColumnWaterFluxType_Constants
  !
  implicit none
  !
  public :: EMI_ColumnWaterFluxType_DataInfoByID

contains
  
!-----------------------------------------------------------------------
  subroutine EMI_ColumnWaterFluxType_DataInfoByID(data_id, id_val, name_val, long_name_val,&
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

    case(L2E_FLUX_SOIL_QFLX_ADV_COL)
       id_val         =  L2E_FLUX_SOIL_QFLX_ADV_COL
       name_val       =  'Vertical water flow'
       long_name_val  =  'Vertical water flow: ELM to EM'
       units_val      =  'mm H2O/s'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_zero
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.

    case(L2E_FLUX_SOIL_QFLX_LAT_COL)
       id_val         =  L2E_FLUX_SOIL_QFLX_LAT_COL
       name_val       =  'Lateral water flow'
       long_name_val  =  'Lateral water flow: ELM to EM'
       units_val      =  'mm H2O/s'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.

    case(L2E_FLUX_SOIL_QFLX_DRAIN)
       id_val         =  L2E_FLUX_SOIL_QFLX_DRAIN
       name_val       =  'Subsurface drainage water flow'
       long_name_val  =  'Subsurface drainage water flow: ELM to EM'
       units_val      =  'mm H2O/s'
       is_real_type   =  .true.
       ndim           =  1
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       data_found   =  .true.

    case(L2E_FLUX_SOIL_QFLX_DRAIN_VR)
       id_val         =  L2E_FLUX_SOIL_QFLX_DRAIN_VR
       name_val       =  'Subsurface drainage water flow vr'
       long_name_val  =  'Subsurface drainage water flow vr: ELM to EM'
       units_val      =  'mm H2O/time step'
       is_real_type   =  .true.
       ndim           =  2
       dim1_beg_name  =  dimname_begc
       dim1_end_name  =  dimname_endc
       dim2_beg_name  =  dimname_one
       dim2_end_name  =  dimname_nlevgrnd
       data_found   =  .true.
    end select
    
  end subroutine EMI_ColumnWaterFluxType_DataInfoByID
    
end module EMI_ColumnWaterFluxType_DataMod
