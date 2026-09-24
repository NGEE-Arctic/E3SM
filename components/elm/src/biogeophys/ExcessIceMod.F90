module ExcessIceMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Routines for updating soil layer geometry (dz, z, zi) based on excess ice
  ! content in polygon tundra columns. When excess ice is present, layers are
  ! physically larger than the reference (mineral-soil-only) thickness; as ice
  ! melts (thermokarst), layers compress back toward their reference thickness.
  !
  ! Core formula:
  !   dz(c,j) = dz_ref(c,j) + excess_ice(c,j) / denice
  !
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use elm_varpar     , only : nlevgrnd
  use elm_varcon     , only : denice
  use elm_varctl     , only : use_polygonal_tundra
  use decompMod      , only : bounds_type
  use LandunitType   , only : lun_pp
  use ColumnType     , only : col_pp
  use ColumnDataType , only : col_ws
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: inflate_layers_from_excess_ice
  public :: recompute_layer_geometry
  !------------------------------------------------------------------------

contains

  !------------------------------------------------------------------------
  subroutine inflate_layers_from_excess_ice(bounds)
    !
    ! !DESCRIPTION:
    ! Called once after col_ws%Init (or after restart read) to set deformed
    ! dz, z, zi for polygon tundra columns based on their current excess ice.
    ! All other column types are left unchanged.
    !
    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: c, l
    !-----------------------------------------------------------------------

    do c = bounds%begc, bounds%endc
       if (.not. col_pp%active(c)) cycle
       l = col_pp%landunit(c)
       if (.not. (use_polygonal_tundra .and. lun_pp%ispolygon(l))) cycle
       call recompute_layer_geometry(c)
    end do

  end subroutine inflate_layers_from_excess_ice

  !------------------------------------------------------------------------
  subroutine recompute_layer_geometry(c)
    !
    ! !DESCRIPTION:
    ! Update dz, z, zi for column c from dz_ref and current excess_ice.
    ! Snow layers (indices <= 0) are not touched.
    ! Should only be called for active polygon tundra columns.
    !
    ! !ARGUMENTS:
    integer, intent(in) :: c
    !
    ! !LOCAL VARIABLES:
    integer  :: j
    real(r8) :: zi_bot
    real(r8) :: dz_orig ! input dz prior to any compression, used to calculate volrat.
    !-----------------------------------------------------------------------

    zi_bot = 0._r8
    col_pp%volrat(c,:) = 1._r8 ! This should always be 1 unless the layer currently is shrinking in this timestep.
    do j = 1, nlevgrnd
       dz_orig = col_pp%dz(c,j)
       col_pp%dz(c,j) = col_pp%dz_ref(c,j) + col_ws%excess_ice(c,j) / denice
       col_pp%z(c,j)  = zi_bot + 0.5_r8 * col_pp%dz(c,j)
       col_pp%zi(c,j) = zi_bot + col_pp%dz(c,j)
       zi_bot = col_pp%zi(c,j)
       ! calculate volume ratio to scale molar concentrations of BGC species
       ! (i.e., layer compression decreases volume but doesn't remove chemical speices,
       ! so it should increase molar concentrations)
       col_pp%volrat(c,j) = dz_orig/col_pp%dz(c,j)
    end do
    col_pp%zi(c,0) = 0._r8   ! surface interface (always 0)

  end subroutine recompute_layer_geometry

end module ExcessIceMod
