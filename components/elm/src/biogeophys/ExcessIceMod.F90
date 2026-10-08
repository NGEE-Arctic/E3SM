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
    real(r8) :: exice_thk ! thickness (m) of excess ice in layer j
    real(r8) :: exice_abv ! cumulative excess-ice thickness (m) in layers above j; this is
                          ! the downward displacement of layer j's whole reference position
    real(r8) :: frac      ! fractional position (unitless) of the reference node z_ref(c,j)
                          ! within its own layer. zsoi(j) is NOT the midpoint of its
                          ! interfaces on node-based grids (10SL_3.5m, 23SL_3.5m): zsoi is
                          ! built exponentially (initVerticalMod.F90:203) and zisoi derived
                          ! from it as a node midpoint (:216), and that inverse does not
                          ! hold. A hardwired 0.5_r8 would therefore displace z even at
                          ! zero excess ice -- by up to 1.4 m on 10SL_3.5m.
    !-----------------------------------------------------------------------

    ! Geometry is written as a PERTURBATION of the reference grid rather than
    ! rebuilt by cumulative summation of dz. The two are algebraically identical
    ! -- frac*dz_ref(c,j) == z_ref(c,j) - zi_ref(c,j-1) by the definition of frac,
    ! and sum(dz_ref(c,1:j)) == zi_ref(c,j) -- but the perturbation form makes
    ! every term reduce to x + 0._r8 when excess_ice is zero, which is an exact
    ! IEEE754 identity. So excess_ice == 0 reproduces dz_ref/z_ref/zi_ref
    ! BIT-FOR-BIT on all five layer structures.
    !
    ! Re-summation would not: on the node-based grids zisoi is constructed
    ! directly as a node midpoint, never as a running sum of dzsoi, so
    ! accumulating dz rounds differently and leaves z/zi off the reference grid
    ! -- 4 of 15 zi nodes on 10SL_3.5m and 5 of 30 on 23SL_3.5m, worst case
    ! 7.1e-15 m at the bottom interface. That residual is physically negligible
    ! -- one ulp at 42 m depth -- but it would forfeit the bit-for-bit restart
    ! property for polygon columns, which is what makes the Phase 2/3/5
    ! non-polygon BFB criterion provable rather than approximate.
    exice_abv = 0._r8
    do j = 1, nlevgrnd
       exice_thk = col_ws%excess_ice(c,j) / denice
       frac      = (col_pp%z_ref(c,j) - col_pp%zi_ref(c,j-1)) / col_pp%dz_ref(c,j)

       col_pp%dz(c,j) = col_pp%dz_ref(c,j) + exice_thk
       ! Layer j is pushed down by all the excess ice above it, and its node sits
       ! frac of the way through its own (now inflated) thickness.
       col_pp%z(c,j)  = col_pp%z_ref(c,j)  + exice_abv + frac * exice_thk
       col_pp%zi(c,j) = col_pp%zi_ref(c,j) + exice_abv + exice_thk

       exice_abv = exice_abv + exice_thk
    end do
    col_pp%zi(c,0) = col_pp%zi_ref(c,0)

  end subroutine recompute_layer_geometry

end module ExcessIceMod
