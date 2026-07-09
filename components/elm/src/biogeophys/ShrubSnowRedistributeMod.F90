module ShrubSnowRedistributeMod

  !-----------------------------------------------------------------------
  !GAM
  ! Greta Miller 7/6/26
  ! Calculate the snow redistribution factors to be used to modify forc_snow in CanopyHydrologyMod
  ! Modeling blowing snow redistribution from grass/low vegetation to tall shrubs ("snow fence effect")
  ! Originally implemented in CESM by Lawrence and Swenson 2011
 !-----------------------------------------------------------------------

  use shr_kind_mod, only : r8 => shr_kind_r8

  implicit none
  private

  public :: init_shrub_snow_factors
  public :: compute_shrub_snow_factors
  public :: snow_factor_col

  real(r8), allocatable :: snow_factor_col(:)

contains

  subroutine init_shrub_snow_factors(bounds)
    use decompMod, only : bounds_type
    implicit none
    type(bounds_type), intent(in) :: bounds

    if (.not. allocated(snow_factor_col)) then
       allocate(snow_factor_col(bounds%begc:bounds%endc))
    end if

    snow_factor_col(bounds%begc:bounds%endc) = 1._r8
  end subroutine init_shrub_snow_factors



  subroutine compute_shrub_snow_factors(bounds, snow_factor_col_inout)
    use decompMod,      only : bounds_type
    use ColumnType,     only : col_pp
    use VegetationType, only : veg_pp
    use elm_varctl,     only : shrub_snow_redist_alpha, iulog
    implicit none

    type(bounds_type), intent(in) :: bounds
    real(r8), intent(inout) :: snow_factor_col_inout(bounds%begc:bounds%endc)

    integer, parameter :: pft_boreal_shrub = 11
    integer, parameter :: pft_arctic_grass = 12
    real(r8), parameter :: min_wt = 1.0e-12_r8

    integer :: t, c, p
    real(r8) :: f_shrub, f_grass, alpha

    snow_factor_col_inout(bounds%begc:bounds%endc) = 1._r8

    if (shrub_snow_redist_alpha < 0._r8) return

    alpha = shrub_snow_redist_alpha

    do t = bounds%begt, bounds%endt

       f_shrub = 0._r8
       f_grass = 0._r8

       do c = bounds%begc, bounds%endc
          if (.not. col_pp%active(c)) cycle
          if (col_pp%topounit(c) /= t) cycle
          if (col_pp%npfts(c) /= 1) cycle

          p = col_pp%pfti(c)

          if (veg_pp%itype(p) == pft_boreal_shrub) then
             f_shrub = f_shrub + col_pp%wttopounit(c)
          else if (veg_pp%itype(p) == pft_arctic_grass) then
             f_grass = f_grass + col_pp%wttopounit(c)
          end if
       end do

       if (f_shrub <= min_wt .or. f_grass <= min_wt) cycle

       do c = bounds%begc, bounds%endc
          if (.not. col_pp%active(c)) cycle
          if (col_pp%topounit(c) /= t) cycle
          if (col_pp%npfts(c) /= 1) cycle

          p = col_pp%pfti(c)

          if (veg_pp%itype(p) == pft_arctic_grass) then
             snow_factor_col_inout(c) = 1._r8 - alpha
          else if (veg_pp%itype(p) == pft_boreal_shrub) then
             snow_factor_col_inout(c) = 1._r8 + alpha * f_grass / f_shrub
          end if
       end do

       if (t == bounds%begt) then
        do c = bounds%begc,bounds%endc
            if (col_pp%active(c) .and. col_pp%topounit(c) == t) then
                write(iulog,*) 'GAM snow_factor: t,c,wttopounit,factor = ', &
                    t, c, col_pp%wttopounit(c), snow_factor_col_inout(c)
            end if
        end do
       end if

    end do

  end subroutine compute_shrub_snow_factors

end module ShrubSnowRedistributeMod