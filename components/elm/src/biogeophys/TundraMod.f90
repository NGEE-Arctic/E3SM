module TundraMod

  !------------------------------------------------------------------------
  ! Description:
  !  Subroutines calculating aspects of permafrost/tundra hydrology
  !
  !------------------------------------------------------------------------

  use shr_kind_mod      , only : r8 => shr_kind_r8
  use shr_log_mod       , only : errMsg => shr_log_errMsg
  use decompMod         , only : bounds_type
  use elm_varctl        , only : iulog, use_vichydro
  use EnergyFluxType    , only : energyflux_type
  use SoilHydrologyType , only : soilhydrology_type
  use SoilStateType     , only : soilstate_type
  use WaterfluxType     , only : waterflux_type
  use TopounitType      , only : top_pp
  use TopounitDataType  , only : top_ws
  use LandunitType      , only : lun_pp
  use ColumnType        , only : col_pp
  use ColumnDataType    , only : col_es, col_ws, col_wf
  use VegetationType    , only : veg_pp
  use VegetationDataType, only : veg_wf
  use abortutils      , only : endrun
  use landunit_varcon  , only : istsoil, istcrop, ilowcenpoly, iflatcenpoly, ihighcenpoly


  ! !PUBLIC TYPES:
  implicit none
  save
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: PolygonRunoff         ! Calculate surface runoff
  public :: PolygonInundationFrac ! Calculate inundation fraction
  public :: PolygonWaterDepth     ! Calculate depth of water in polygonal ground

contains

  subroutine PolygonRunoff

  end subroutine PolygonRunoff

  subroutine PolygonInundationFrac(microrel,exclvol,h2osfc,depth)
    ! DESCRIPTION:
    ! Calculate inundation fraction from microtopographic parameters 

    ! ARGUMENTS:
    real(r8),      intent(in) :: microrel ! microtopographic relief [m]
    real(r8),      intent(in) :: exclvol  ! excluded volume normalized by area [m]
    real(r8),      intent(in) :: h2osfc   ! surface water storage [mm]
    real(r8),      intent(out):: depth    ! ponded water depth [m]


    ! Local varibles:
    real(r8) :: swc  ! surface water storage in meters
    real(r8) :: fd, dfdd ! temporary variables for Newton-Raphson solve
    integer  :: k

    swc = h2osfc/1000_r8 ! convert to m

    if (swc > microrel - exclvol) then
        depth = swc + exclvol
    else
        depth = swc + exclvol
        do k=1,10
            fd = (2_r8*exclvol - microrel) * (depth/microrel)**3_r8 &
                + (2_r8*microrel - 3_r8*exclvol) * (depth/microrel)**2_r8 &
                - swc
            dfdd = (3_r8/microrel) * (2_r8*exclvol - microrel) * (depth/microrel)**2_r8 &
                + (2_r8/microrel) * (2_r8*microrel - 3_r8*exclvol) * (depth/microrel)
            if (dfdd < 1.0e-12_r8) then
                write(iulog,*) "careful! getting close to dividing by 0..."
            end if
            depth = depth - fd/dfdd
        enddo
    endif

  end subroutine PolygonInundationFrac

  subroutine PolygonWaterDepth

  end subroutine PolygonWaterDepth



end module TundraMod