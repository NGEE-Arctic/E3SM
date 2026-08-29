module EMI_ColumnEnergyStateType_Constants
  !
  implicit none
  private
  !
  integer, parameter, public :: L2E_STATE_TSOIL_NLEVGRND_COL  = 0301
  integer, parameter, public :: L2E_STATE_TSNOW_COL           = 0302
  integer, parameter, public :: L2E_STATE_TH2OSFC_COL         = 0303
  integer, parameter, public :: L2E_STATE_TSOI10CM_COL        = 0304
  integer, parameter, public :: L2E_STATE_TSOIL_NLEVSOI_COL   = 0305

  integer, parameter, public :: E2L_STATE_TSOIL_NLEVGRND_COL  = 0306
  integer, parameter, public :: E2L_STATE_TSNOW_NLEVSNOW_COL  = 0307
  integer, parameter, public :: E2L_STATE_TH2OSFC_COL         = 0308

end module EMI_ColumnEnergyStateType_Constants
