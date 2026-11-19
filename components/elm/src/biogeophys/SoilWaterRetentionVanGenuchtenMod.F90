module SoilWaterRetentionVanGenuchtenMod

  !---------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Scalar kernels for the van Genuchten (1980) soil water retention curve with
  ! Mualem (1976) relative hydraulic conductivity, in the air-entry-modified form
  ! of Ippisch et al. (2006). Used when soil_water_retention_model = 'van_genuchten_1980'.
  !
  ! Conventions follow the inline Clapp and Hornberger (1978) code in ELM:
  !   - soil suction smp is in mm of water and is negative;
  !   - s = theta/theta_ref is ELM's relative saturation, where theta_ref is whatever
  !     the call site already uses (watsat or eff_porosity);
  !   - sr = watres/watsat is the residual water content as a fraction of porosity,
  !     so the effective saturation is Se = (s - sr)/(1 - sr).
  ! alpha is in 1/mm, n is dimensionless, and m = 1 - 1/n (Mualem).
  !
  ! With suction head h = -smp >= 0 [mm] and Sc = [1 + (alpha*he)^n]^(-m):
  !   Se(h)  = 1                                for h <= he
  !          = [1 + (alpha*h)^n]^(-m) / Sc      for h >  he
  !   Kr(Se) = Se^l * [F(Se)/F(1)]^2,  F(Se) = 1 - [1 - (Se*Sc)^(1/m)]^m
  ! The air-entry suction he keeps dsmp/ds and dKr/dSe finite at saturation, which
  ! the water-content-based Zeng and Decker (2009) solver requires.
  !
  ! This module depends only on shr_kind_mod so that it can be tested standalone.
  ! Callers validate parameters (alpha > 0, n > 1, 0 <= sr < 1).
  !
  ! References:
  !   van Genuchten (1980), Soil Sci. Soc. Am. J., 44, 892-898.
  !   Mualem (1976), Water Resour. Res., 12, 513-522.
  !   Vogel, van Genuchten and Cislerova (2001), Adv. Water Resour., 24, 133-144.
  !   Ippisch, Vogel and Bastian (2006), Adv. Water Resour., 29, 1780-1789.
  !   Lenhard, Parker and Mishra (1989), J. Irrig. Drain. Eng., 115, 744-751.
  !   Zeng and Decker (2009), J. Hydrometeorol., 10, 308-319.
  !
  ! !USES:
  use shr_kind_mod , only : r8 => shr_kind_r8
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: vg_suction             ! soil suction and d(smp)/ds from relative saturation
  public :: vg_se_from_suction     ! effective saturation from soil suction
  public :: vg_s_from_suction      ! relative saturation from soil suction
  public :: vg_theta_from_suction  ! volumetric water content from soil suction
  public :: vg_hk                  ! hydraulic conductivity and d(hk)/dSe
  public :: vg_layer_mean_theta_eq ! layer-mean equilibrium water content above a water table
  public :: vg_se_from_kr          ! effective saturation at a target relative conductivity
  public :: vg_to_ch_lenhard       ! Clapp-Hornberger-equivalent bsw and sucsat
  !
  ! !PUBLIC DATA:
  real(r8), parameter, public :: vg_he = 20._r8  ! air-entry suction [mm] (Vogel et al., 2001)
  real(r8), parameter, public :: vg_l  = 0.5_r8  ! Mualem pore-connectivity exponent [-]
  !$acc declare copyin(vg_he, vg_l)
  !
  ! !PRIVATE DATA:
  ! 6-point Gauss-Legendre nodes and weights on [-1, 1]
  integer , parameter :: ngl = 6
  real(r8), parameter :: gl_x(ngl) = (/ -0.9324695142031520278_r8, -0.6612093864662645137_r8, &
                                        -0.2386191860831969086_r8,  0.2386191860831969086_r8, &
                                         0.6612093864662645137_r8,  0.9324695142031520278_r8 /)
  real(r8), parameter :: gl_w(ngl) = (/  0.1713244923791703450_r8,  0.3607615730481386076_r8, &
                                         0.4679139345726910474_r8,  0.4679139345726910474_r8, &
                                         0.3607615730481386076_r8,  0.1713244923791703450_r8 /)
  !$acc declare copyin(gl_x, gl_w)
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  pure subroutine vg_suction(s, sr, alpha, n, smpmin, smp, dsmpds)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Compute soil suction and its derivative with respect to relative saturation.
    ! Se is clamped to [Se_min, 1] before any power is taken, where Se_min is the
    ! effective saturation at which smp = smpmin. The derivative is evaluated at the
    ! clamped Se, so it is never zero; at Se = 1 it is the (finite) left derivative.
    !
    ! !ARGUMENTS:
    real(r8), intent(in)  :: s       ! relative saturation, theta/theta_ref [-]
    real(r8), intent(in)  :: sr      ! residual saturation, watres/watsat [-]
    real(r8), intent(in)  :: alpha   ! van Genuchten alpha [1/mm]
    real(r8), intent(in)  :: n       ! van Genuchten n [-]
    real(r8), intent(in)  :: smpmin  ! restriction on soil suction, negative [mm]
    real(r8), intent(out) :: smp     ! soil suction, negative [mm]
    real(r8), intent(out) :: dsmpds  ! d(smp)/ds [mm]
    !
    ! !LOCAL VARIABLES:
    real(r8) :: m      ! van Genuchten m = 1 - 1/n [-]
    real(r8) :: lnsc   ! log of Sc [-]
    real(r8) :: lnse   ! log of effective saturation [-]
    real(r8) :: x      ! (Se*Sc)^(-1/m) - 1 = (alpha*h)^n [-]
    real(r8) :: h      ! suction head [mm]
    !-----------------------------------------------------------------------

    m    = 1._r8 - 1._r8/n
    lnsc = -m*log1p_vg((alpha*vg_he)**n)

    ! clamp in log space: ln(Se_min) = -m*ln(1 + (alpha*|smpmin|)^n) - ln(Sc)
    lnse = log(max((s - sr)/(1._r8 - sr), tiny(1._r8)))
    lnse = max(lnse, -m*log1p_vg((-alpha*smpmin)**n) - lnsc)
    lnse = min(lnse, 0._r8)

    x = expm1_vg(-(lnse + lnsc)/m)
    h = x**(1._r8/n)/alpha
    smp = max(smpmin, -h)

    ! dh/dSe = -h*(x+1)/(n*m*x*Se); dSe/ds = 1/(1-sr)
    dsmpds = h*(x + 1._r8)/(n*m*x*exp(lnse)*(1._r8 - sr))

  end subroutine vg_suction

  !-----------------------------------------------------------------------
  pure real(r8) function vg_se_from_suction(smp, alpha, n) result(se)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Effective saturation at a given soil suction (the inverse of vg_suction).
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: smp    ! soil suction, negative [mm]
    real(r8), intent(in) :: alpha  ! van Genuchten alpha [1/mm]
    real(r8), intent(in) :: n      ! van Genuchten n [-]
    !
    ! !LOCAL VARIABLES:
    real(r8) :: m      ! van Genuchten m = 1 - 1/n [-]
    real(r8) :: h      ! suction head [mm]
    !-----------------------------------------------------------------------

    h = -smp
    if (h <= vg_he) then
       se = 1._r8
    else
       m  = 1._r8 - 1._r8/n
       se = exp(-m*(log1p_vg((alpha*h)**n) - log1p_vg((alpha*vg_he)**n)))
    end if

  end function vg_se_from_suction

  !-----------------------------------------------------------------------
  pure real(r8) function vg_s_from_suction(smp, sr, alpha, n) result(s)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Relative saturation (theta/theta_ref) at a given soil suction.
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: smp    ! soil suction, negative [mm]
    real(r8), intent(in) :: sr     ! residual saturation, watres/watsat [-]
    real(r8), intent(in) :: alpha  ! van Genuchten alpha [1/mm]
    real(r8), intent(in) :: n      ! van Genuchten n [-]
    !-----------------------------------------------------------------------

    s = sr + (1._r8 - sr)*vg_se_from_suction(smp, alpha, n)

  end function vg_s_from_suction

  !-----------------------------------------------------------------------
  pure real(r8) function vg_theta_from_suction(smp, watsat, watres, alpha, n) result(theta)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Volumetric water content at a given soil suction.
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: smp     ! soil suction, negative [mm]
    real(r8), intent(in) :: watsat  ! saturated volumetric water content [m3/m3]
    real(r8), intent(in) :: watres  ! residual volumetric water content [m3/m3]
    real(r8), intent(in) :: alpha   ! van Genuchten alpha [1/mm]
    real(r8), intent(in) :: n       ! van Genuchten n [-]
    !-----------------------------------------------------------------------

    theta = watres + (watsat - watres)*vg_se_from_suction(smp, alpha, n)

  end function vg_theta_from_suction

  !-----------------------------------------------------------------------
  pure subroutine vg_hk(hksat, imped, se, alpha, n, hk, dhkdse)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Compute hydraulic conductivity and its derivative with respect to effective
    ! saturation. At Se = 1 the derivative is the (finite) left derivative.
    !
    ! !ARGUMENTS:
    real(r8), intent(in)  :: hksat   ! saturated hydraulic conductivity [mm/s]
    real(r8), intent(in)  :: imped   ! ice impedance factor [-]
    real(r8), intent(in)  :: se      ! effective saturation [-]
    real(r8), intent(in)  :: alpha   ! van Genuchten alpha [1/mm]
    real(r8), intent(in)  :: n       ! van Genuchten n [-]
    real(r8), intent(out) :: hk      ! hydraulic conductivity [mm/s]
    real(r8), intent(out) :: dhkdse  ! d(hk)/dSe [mm/s]
    !
    ! !LOCAL VARIABLES:
    real(r8) :: m      ! van Genuchten m = 1 - 1/n [-]
    real(r8) :: lnsc   ! log of Sc [-]
    real(r8) :: sec    ! effective saturation clamped to [0, 1] [-]
    real(r8) :: y      ! (Se*Sc)^(1/m) [-]
    real(r8) :: f      ! F(Se) = 1 - (1 - y)^m [-]
    real(r8) :: f1     ! F(1) [-]
    real(r8) :: kr     ! relative hydraulic conductivity [-]
    !-----------------------------------------------------------------------

    sec = min(se, 1._r8)
    if (sec <= 0._r8) then
       hk     = 0._r8
       dhkdse = 0._r8
       return
    end if

    m    = 1._r8 - 1._r8/n
    lnsc = -m*log1p_vg((alpha*vg_he)**n)

    ! F = -expm1(m*log1p(-y)) avoids cancellation when y is small
    f1 = -expm1_vg(m*log1p_vg(-exp(lnsc/m)))
    y  = exp((log(sec) + lnsc)/m)
    f  = -expm1_vg(m*log1p_vg(-y))

    ! F underflows only when Kr ~ Se^(l+2/m) is far below double precision
    if (f <= 0._r8) then
       hk     = 0._r8
       dhkdse = 0._r8
       return
    end if

    kr = sec**vg_l*(f/f1)**2
    hk = imped*hksat*kr

    ! dKr/dSe = Kr/Se*(l + 2*y*(1-y)^(m-1)/F); y/F -> 1/m as Se -> 0
    dhkdse = imped*hksat*kr/sec*(vg_l + 2._r8*y*(1._r8 - y)**(m - 1._r8)/f)

  end subroutine vg_hk

  !-----------------------------------------------------------------------
  pure real(r8) function vg_layer_mean_theta_eq(ztop, zbot, zwt, watsat, watres, alpha, n) &
       result(theta_eq)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Layer-mean equilibrium volumetric water content between depths ztop and zbot
    ! for a hydrostatic profile above the water table at depth zwt (Zeng and Decker,
    ! 2009). Above the water table the equilibrium suction is smp_E(z) = -(he + zwt - z),
    ! so smp_E = -he at the water table, analogous to -sucsat in the Clapp-Hornberger
    ! form; below the water table theta = watsat.
    !
    ! There is no closed-form antiderivative of the van Genuchten curve, so the
    ! unsaturated part is integrated by composite 6-point Gauss-Legendre quadrature
    ! in ln(h), on equal panels no wider than dlnh_max. Near saturation the integrand
    ! grows roughly like h^2 = exp(2*ln h); a width of 1 keeps the relative error
    ! below ~1e-10, and h spans at most ln(1e8/he) < 16 panels.
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: ztop    ! depth of the layer top, positive down [mm]
    real(r8), intent(in) :: zbot    ! depth of the layer bottom, positive down [mm]
    real(r8), intent(in) :: zwt     ! depth of the water table, positive down [mm]
    real(r8), intent(in) :: watsat  ! saturated volumetric water content [m3/m3]
    real(r8), intent(in) :: watres  ! residual volumetric water content [m3/m3]
    real(r8), intent(in) :: alpha   ! van Genuchten alpha [1/mm]
    real(r8), intent(in) :: n       ! van Genuchten n [-]
    !
    ! !LOCAL VARIABLES:
    integer  :: i, k       ! indices
    integer  :: npanel     ! number of quadrature panels
    real(r8) :: zu         ! bottom of the unsaturated part of the layer [mm]
    real(r8) :: ulo, uhi   ! ln of suction head at the bottom and top of the unsaturated part
    real(r8) :: uc, ur     ! panel center and half-width in ln(h)
    real(r8) :: h          ! suction head at a quadrature node [mm]
    real(r8) :: ise        ! integral of Se over the unsaturated part [mm]
    real(r8), parameter :: dlnh_max = 1._r8  ! maximum panel width in ln(h)
    !-----------------------------------------------------------------------

    if (zbot <= ztop) then
       theta_eq = vg_theta_from_suction(-(vg_he + max(zwt - ztop, 0._r8)), watsat, watres, alpha, n)
       return
    end if

    ise = 0._r8
    zu  = min(zbot, zwt)
    if (zu > ztop) then
       ulo = log(vg_he + zwt - zu)
       uhi = log(vg_he + zwt - ztop)

       npanel = max(1, ceiling((uhi - ulo)/dlnh_max))
       ur     = 0.5_r8*(uhi - ulo)/npanel

       ! integral of Se(h) dh = integral of Se(h)*h d(ln h)
       do k = 1, npanel
          uc = ulo + (2*k - 1)*ur
          do i = 1, ngl
             h   = exp(uc + ur*gl_x(i))
             ise = ise + gl_w(i)*ur*h*vg_se_from_suction(-h, alpha, n)
          end do
       end do
    end if

    ! unsaturated part plus the saturated part below the water table, Se = 1
    theta_eq = watres + (watsat - watres)* &
         (ise + max(zbot - max(ztop, zwt), 0._r8))/(zbot - ztop)

  end function vg_layer_mean_theta_eq

  !-----------------------------------------------------------------------
  pure real(r8) function vg_se_from_kr(kr_target, alpha, n) result(se)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Effective saturation at which the relative hydraulic conductivity equals
    ! kr_target, found by bisection (Kr is monotonic in Se). Used to compute the
    ! water content at field capacity.
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: kr_target  ! target relative hydraulic conductivity [-]
    real(r8), intent(in) :: alpha      ! van Genuchten alpha [1/mm]
    real(r8), intent(in) :: n          ! van Genuchten n [-]
    !
    ! !LOCAL VARIABLES:
    integer  :: iter       ! iteration index
    real(r8) :: lo, hi     ! bisection bracket [-]
    real(r8) :: kr, dkr    ! relative hydraulic conductivity and its derivative [-]
    integer , parameter :: niter = 60  ! bracket width 2^-60
    !-----------------------------------------------------------------------

    if (kr_target >= 1._r8) then
       se = 1._r8
       return
    else if (kr_target <= 0._r8) then
       se = 0._r8
       return
    end if

    lo = 0._r8
    hi = 1._r8
    do iter = 1, niter
       se = 0.5_r8*(lo + hi)
       call vg_hk(1._r8, 1._r8, se, alpha, n, kr, dkr)
       if (kr < kr_target) then
          lo = se
       else
          hi = se
       end if
    end do
    se = 0.5_r8*(lo + hi)

  end function vg_se_from_kr

  !-----------------------------------------------------------------------
  pure subroutine vg_to_ch_lenhard(alpha, n, bsw, sucsat)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! Clapp-Hornberger (Brooks-Corey) parameters equivalent to a van Genuchten curve,
    ! following Lenhard et al. (1989), as in ATS tools/utils/convert_parameters_vg2bc.py.
    ! Used only where ELM treats bsw and sucsat as pore-structure indices rather than
    ! as a retention curve (e.g. gas tortuosity, decomposition moisture limits).
    !
    ! !ARGUMENTS:
    real(r8), intent(in)  :: alpha   ! van Genuchten alpha [1/mm]
    real(r8), intent(in)  :: n       ! van Genuchten n [-]
    real(r8), intent(out) :: bsw     ! Clapp-Hornberger b = 1/lambda [-]
    real(r8), intent(out) :: sucsat  ! air-entry suction, positive [mm]
    !
    ! !LOCAL VARIABLES:
    real(r8) :: lambda  ! Brooks-Corey pore-size distribution index [-]
    real(r8) :: sestar  ! matching-point effective saturation [-]
    !-----------------------------------------------------------------------

    lambda = (n - 1._r8)*(1._r8 - 0.5_r8**(n/(n - 1._r8)))
    sestar = 0.72_r8 - 0.35_r8*exp(-n**4)
    bsw    = 1._r8/lambda
    sucsat = sestar**(1._r8/lambda)*(sestar**(n/(1._r8 - n)) - 1._r8)**(1._r8/n)/alpha

  end subroutine vg_to_ch_lenhard

  !-----------------------------------------------------------------------
  pure real(r8) function log1p_vg(x)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! log(1 + x), accurate for small |x| (Goldberg, 1991, ACM Comput. Surv., 23, 5-48).
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: x
    !
    ! !LOCAL VARIABLES:
    real(r8) :: u
    !-----------------------------------------------------------------------

    u = 1._r8 + x
    if (u == 1._r8) then
       log1p_vg = x
    else
       log1p_vg = log(u)*x/(u - 1._r8)
    end if

  end function log1p_vg

  !-----------------------------------------------------------------------
  pure real(r8) function expm1_vg(x)
    !$acc routine seq
    !
    ! !DESCRIPTION:
    ! exp(x) - 1, accurate for small |x| (Kahan's method).
    !
    ! !ARGUMENTS:
    real(r8), intent(in) :: x
    !
    ! !LOCAL VARIABLES:
    real(r8) :: u
    !-----------------------------------------------------------------------

    u = exp(x)
    if (u == 1._r8) then
       expm1_vg = x
    else if (u - 1._r8 == -1._r8) then
       expm1_vg = -1._r8
    else
       expm1_vg = (u - 1._r8)*x/log(u)
    end if

  end function expm1_vg

end module SoilWaterRetentionVanGenuchtenMod
