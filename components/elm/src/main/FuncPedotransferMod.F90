module FuncPedotransferMod
!
!DESCRIPTIONS:
!module contains different pedotransfer functions to
!compute the mineral soil hydraulic properties.
!pedotransf gives the Clapp-Hornberger parameters; pedotransf_vg_rosetta_class and
!pedotransf_vg_peat_liu2019 give the van Genuchten parameters used when
!soil_water_retention_model = 'van_genuchten_1980'.
!HISTORY:
!created by Jinyun Tang, Mar.1st, 2014
implicit none
  private
  public :: pedotransf
  public :: get_ipedof
  public :: init_pedof
  public :: usda_texture_class
  public :: pedotransf_vg_rosetta_class
  public :: pedotransf_vg_peat_liu2019

  integer, parameter :: cosby_1984_table5 = 0    !by default uses this form
  integer, parameter :: cosby_1984_table4 = 1
  integer, parameter :: noilhan_lacarrere_1995 = 2
  integer :: ipedof0

  !USDA texture classes, in the order of the Rosetta class-average table
  integer, parameter, public :: ntexture_class      = 12
  integer, parameter, public :: tex_sand            = 1
  integer, parameter, public :: tex_loamy_sand      = 2
  integer, parameter, public :: tex_sandy_loam      = 3
  integer, parameter, public :: tex_loam            = 4
  integer, parameter, public :: tex_silt_loam       = 5
  integer, parameter, public :: tex_silt            = 6
  integer, parameter, public :: tex_sandy_clay_loam = 7
  integer, parameter, public :: tex_clay_loam       = 8
  integer, parameter, public :: tex_silty_clay_loam = 9
  integer, parameter, public :: tex_sandy_clay      = 10
  integer, parameter, public :: tex_silty_clay      = 11
  integer, parameter, public :: tex_clay            = 12
contains

   subroutine init_pedof()
   !
   !DESCRIPTIONS
   !initialize the default pedotransfer function
   implicit none
   
   
   ipedof0 = cosby_1984_table5         !the default pedotransfer function
   end subroutine init_pedof
   
   subroutine pedotransf(ipedof, sand, clay, watsat, bsw, sucsat, xksat)
   !pedotransfer function to compute hydraulic properties of mineral soil
   !based on input soil texture
   
   use shr_kind_mod         , only : r8 => shr_kind_r8
   use abortutils    , only : endrun         
   implicit none
   integer,  intent(in) :: ipedof !type of pedotransfer function, use the default pedotransfer function  
   real(r8), intent(in) :: sand   !% sand
   real(r8), intent(in) :: clay   !% clay
   real(r8), intent(out):: watsat !v/v saturate moisture
   real(r8), intent(out):: bsw    !b shape parameter
   real(r8), intent(out):: sucsat !mm, soil matric potential
   real(r8), intent(out):: xksat  !mm/s, saturated hydraulic conductivity
   
   character(len=32) :: subname = 'pedotransf'  ! subroutine name 
   select case (ipedof)
   case (cosby_1984_table4)
      call pedotransf_cosby1984_table4(sand, clay, watsat, bsw, sucsat, xksat)
   case (noilhan_lacarrere_1995)
      call pedotransf_noilhan_lacarrere1995(sand, clay, watsat, bsw, sucsat, xksat) 
   case (cosby_1984_table5)  
      call pedotransf_cosby1984_table5(sand, clay, watsat, bsw, sucsat, xksat)
   case default
      call endrun(subname // ':: a pedotransfer function must be specified!')  
   end select

   end subroutine pedotransf
   
!------------------------------------------------------------------------------------------
   subroutine pedotransf_cosby1984_table4(sand, clay, watsat, bsw, sucsat, xksat)
   !
   !DESCRIPTIONS
   !compute hydraulic properties based on functions derived from Table 4 in cosby et al, 1984
   use shr_kind_mod         , only : r8 => shr_kind_r8   
   implicit none
   real(r8), intent(in) :: sand   !% sand
   real(r8), intent(in) :: clay   !% clay
   real(r8), intent(out):: watsat !v/v saturate moisture
   real(r8), intent(out):: bsw    !b shape parameter
   real(r8), intent(out):: sucsat !mm, soil matric potential
   real(r8), intent(out):: xksat  !mm/s, saturated hydraulic conductivity
   
   !Cosby et al. Table 4
   watsat = 0.505_r8-0.00142_r8*sand-0.00037*clay
   bsw = 3.10+0.157*clay-0.003*sand            
   sucsat  = 10._r8 * ( 10._r8**(1.54_r8-0.0095_r8*sand+0.0063*(100._r8-sand-clay)))            
   xksat         = 0.0070556 *(10.**(-0.60+0.0126*sand-0.0064*clay) )     !mm/s now use table 4.
      
   end subroutine pedotransf_cosby1984_table4
   
!------------------------------------------------------------------------------------------
   subroutine pedotransf_cosby1984_table5(sand, clay, watsat, bsw, sucsat, xksat)
   !
   !DESCRIPTIONS
   !compute hydraulic properties based on functions derived from Table 5 in cosby et al, 1984
   
   use shr_kind_mod         , only : r8 => shr_kind_r8   
   implicit none
   real(r8), intent(in) :: sand   !% sand
   real(r8), intent(in) :: clay   !% clay
   real(r8), intent(out):: watsat !v/v saturate moisture
   real(r8), intent(out):: bsw    !b shape parameter
   real(r8), intent(out):: sucsat !mm, soil matric potential
   real(r8), intent(out):: xksat  !mm/s, saturated hydraulic conductivity
   
   !Cosby et al. Table 5     
   watsat = 0.489_r8 - 0.00126_r8*sand
   bsw    = 2.91 + 0.159*clay
   sucsat = 10._r8 * ( 10._r8**(1.88_r8-0.0131_r8*sand) )            
   xksat         = 0.0070556 *( 10.**(-0.884+0.0153*sand) ) ! mm/s, from table 5 
      
   end subroutine pedotransf_cosby1984_table5
   
!------------------------------------------------------------------------------------------
   subroutine pedotransf_noilhan_lacarrere1995(sand, clay, watsat, bsw, sucsat, xksat)
   !
   !DESCRIPTIONS
   !compute hydraulic properties based on functions derived from Noilhan and Lacarrere, 1995
   
   use shr_kind_mod         , only : r8 => shr_kind_r8   
   implicit none
   real(r8), intent(in) :: sand   !% sand
   real(r8), intent(in) :: clay   !% clay
   real(r8), intent(out):: watsat !v/v saturate moisture
   real(r8), intent(out):: bsw    !b shape parameter
   real(r8), intent(out):: sucsat !mm, soil matric potential
   real(r8), intent(out):: xksat  !mm/s, saturated hydraulic conductivity
   
   !Noilhan and Lacarrere, 1995
   watsat = -0.00108*sand+0.494305
   bsw = 0.137*clay + 3.501
   sucsat = 10._r8**(-0.0088*sand+2.85)
   xksat = 10._r8**(-0.0582*clay-0.00091*sand+0.000529*clay**2._r8-0.0001203*sand**2._r8-1.38)
   end subroutine pedotransf_noilhan_lacarrere1995
!------------------------------------------------------------------------------------------
   function usda_texture_class(sand_in, clay_in) result(itex)
   !
   !DESCRIPTIONS
   !USDA soil texture class from percent sand and clay, using the class definitions of the
   !Soil Survey Manual (Soil Science Division Staff, 2017, USDA Handbook 18). Sand and clay
   !are rescaled if they sum to more than 100%. Returns 0 if no class matches.
   use shr_kind_mod         , only : r8 => shr_kind_r8
   implicit none
   real(r8), intent(in) :: sand_in   !% sand
   real(r8), intent(in) :: clay_in   !% clay
   integer :: itex

   real(r8) :: sand, clay, silt      !% sand, clay, silt

   sand = sand_in
   clay = clay_in
   if (sand + clay > 100._r8) then
      sand = 100._r8*sand_in/(sand_in + clay_in)
      clay = 100._r8*clay_in/(sand_in + clay_in)
   end if
   silt = max(100._r8 - sand - clay, 0._r8)

   if (silt + 1.5_r8*clay < 15._r8) then
      itex = tex_sand
   else if (silt + 2._r8*clay < 30._r8) then
      itex = tex_loamy_sand
   else if ((clay >= 7._r8 .and. clay < 20._r8 .and. sand > 52._r8) .or. &
            (clay < 7._r8 .and. silt < 50._r8)) then
      itex = tex_sandy_loam
   else if (clay >= 7._r8 .and. clay < 27._r8 .and. silt >= 28._r8 .and. silt < 50._r8 &
            .and. sand <= 52._r8) then
      itex = tex_loam
   else if ((silt >= 50._r8 .and. clay >= 12._r8 .and. clay < 27._r8) .or. &
            (silt >= 50._r8 .and. silt < 80._r8 .and. clay < 12._r8)) then
      itex = tex_silt_loam
   else if (silt >= 80._r8 .and. clay < 12._r8) then
      itex = tex_silt
   else if (clay >= 20._r8 .and. clay < 35._r8 .and. silt < 28._r8 .and. sand > 45._r8) then
      itex = tex_sandy_clay_loam
   else if (clay >= 27._r8 .and. clay < 40._r8 .and. sand > 20._r8 .and. sand <= 45._r8) then
      itex = tex_clay_loam
   else if (clay >= 27._r8 .and. clay < 40._r8 .and. sand <= 20._r8) then
      itex = tex_silty_clay_loam
   else if (clay >= 35._r8 .and. sand > 45._r8) then
      itex = tex_sandy_clay
   else if (clay >= 40._r8 .and. silt >= 40._r8) then
      itex = tex_silty_clay
   else if (clay >= 40._r8 .and. sand <= 45._r8 .and. silt < 40._r8) then
      itex = tex_clay
   else
      itex = 0
   end if

   end function usda_texture_class
!------------------------------------------------------------------------------------------
   subroutine pedotransf_vg_rosetta_class(sand, clay, watsat, watres, alpha, n)
   !
   !DESCRIPTIONS
   !van Genuchten (1980) parameters of mineral soil from the Rosetta3 class averages (model H1w
   !of Zhang and Schaap, 2017, J. Hydrol., 547, 39-53) for the USDA texture class. Zhang and
   !Schaap publish H1w only as ratios to the Rosetta1 class averages (model H1 of Schaap et al.,
   !2001, J. Hydrol., 251, 163-176) in their Table 1, so both tables are kept here and the
   !product is formed below. Rosetta1 gives log10 of alpha in 1/cm and of n; the alpha and n
   !ratios apply to the anti-logged class means.
   !TODO(verify): Rosetta1 values transcribed from memory, not yet checked against a primary source.
   use shr_kind_mod         , only : r8 => shr_kind_r8
   use abortutils           , only : endrun
   implicit none
   real(r8), intent(in) :: sand   !% sand
   real(r8), intent(in) :: clay   !% clay
   real(r8), intent(out):: watsat !v/v, saturated water content
   real(r8), intent(out):: watres !v/v, residual water content
   real(r8), intent(out):: alpha  !1/mm, van Genuchten alpha
   real(r8), intent(out):: n      !van Genuchten n

   integer :: itex
   character(len=32) :: subname = 'pedotransf_vg_rosetta_class'
   !Rosetta1 (H1) class averages
   !          sand    l.sand  s.loam  loam    si.loam silt    s.c.l.  c.loam  si.c.l. s.clay  si.clay clay
   real(r8), parameter :: watres_tab(ntexture_class) = (/ &
              0.053_r8, 0.049_r8, 0.039_r8, 0.061_r8, 0.065_r8, 0.050_r8, &
              0.063_r8, 0.079_r8, 0.090_r8, 0.117_r8, 0.111_r8, 0.098_r8 /)
   real(r8), parameter :: watsat_tab(ntexture_class) = (/ &
              0.375_r8, 0.390_r8, 0.387_r8, 0.399_r8, 0.439_r8, 0.489_r8, &
              0.384_r8, 0.442_r8, 0.482_r8, 0.385_r8, 0.481_r8, 0.459_r8 /)
   real(r8), parameter :: log10alpha_tab(ntexture_class) = (/ &   ! log10(1/cm)
             -1.453_r8, -1.459_r8, -1.574_r8, -1.954_r8, -2.296_r8, -2.182_r8, &
             -1.676_r8, -1.801_r8, -2.076_r8, -1.476_r8, -1.790_r8, -1.825_r8 /)
   real(r8), parameter :: log10n_tab(ntexture_class) = (/ &
              0.502_r8, 0.242_r8, 0.161_r8, 0.168_r8, 0.221_r8, 0.225_r8, &
              0.124_r8, 0.151_r8, 0.182_r8, 0.082_r8, 0.121_r8, 0.098_r8 /)
   !Rosetta3 (H1w) / Rosetta1 (H1) ratios of the class means, Zhang and Schaap (2017), Table 1
   real(r8), parameter :: watres_ratio(ntexture_class) = (/ &
              1.03_r8,  1.20_r8,  1.53_r8,  1.44_r8,  1.30_r8,  1.30_r8, &
              1.52_r8,  1.32_r8,  1.28_r8,  1.26_r8,  1.12_r8,  1.33_r8 /)
   real(r8), parameter :: watsat_ratio(ntexture_class) = (/ &
              0.97_r8,  0.98_r8,  0.98_r8,  1.00_r8,  0.97_r8,  0.97_r8, &
              0.99_r8,  0.98_r8,  0.98_r8,  0.99_r8,  0.98_r8,  1.00_r8 /)
   real(r8), parameter :: alpha_ratio(ntexture_class) = (/ &
              0.93_r8,  0.71_r8,  0.62_r8,  0.59_r8,  0.68_r8,  0.92_r8, &
              0.58_r8,  0.59_r8,  0.64_r8,  0.75_r8,  0.62_r8,  0.57_r8 /)
   real(r8), parameter :: n_ratio(ntexture_class) = (/ &
              0.91_r8,  0.97_r8,  1.00_r8,  0.96_r8,  0.93_r8,  0.94_r8, &
              0.99_r8,  0.99_r8,  0.94_r8,  1.02_r8,  0.96_r8,  1.00_r8 /)

   itex = usda_texture_class(sand, clay)
   if (itex < 1) then
      call endrun(subname // ':: no USDA texture class for the given sand and clay')
   end if

   watres = watres_ratio(itex)*watres_tab(itex)
   watsat = watsat_ratio(itex)*watsat_tab(itex)
   alpha  = alpha_ratio(itex)*0.1_r8*10._r8**log10alpha_tab(itex)   ! 1/cm -> 1/mm
   n      = n_ratio(itex)*10._r8**log10n_tab(itex)

   end subroutine pedotransf_vg_rosetta_class
!------------------------------------------------------------------------------------------
   subroutine pedotransf_vg_peat_liu2019(bd, depth, watsat, watres, alpha, n)
   !
   !DESCRIPTIONS
   !van Genuchten (1980) parameters of peat from bulk density and depth, using the
   !continuous pedotransfer functions fitted to all peat types by Liu and Lennartz (2019),
   !Hydrol. Process., 33, 101-114, Table 4 ("All data", bulk density <= 0.76 g/cm3).
   !The residual water content was fixed at zero in their fits.
   use shr_kind_mod         , only : r8 => shr_kind_r8
   implicit none
   real(r8), intent(in) :: bd     !g/cm3, dry bulk density of peat
   real(r8), intent(in) :: depth  !cm, depth below the soil surface
   real(r8), intent(out):: watsat !v/v, saturated water content
   real(r8), intent(out):: watres !v/v, residual water content
   real(r8), intent(out):: alpha  !1/mm, van Genuchten alpha
   real(r8), intent(out):: n      !van Genuchten n

   watsat = 0.950_r8 - 0.437_r8*bd
   watres = 0._r8
   alpha  = 0.1_r8*10._r8**(0.326_r8 - 9.135_r8*bd + 10.420_r8*bd**2 - 0.014_r8*depth)   ! 1/cm -> 1/mm
   n      = 10._r8**(0.153_r8 - 0.422_r8*bd + 0.450_r8*bd**2)

   end subroutine pedotransf_vg_peat_liu2019
!------------------------------------------------------------------------------------------
   function get_ipedof(soil_order)result(ipedof)
   !
   ! DESCRIPTION
   ! select the pedotransfer function to be used
   implicit none
   integer, intent(in) :: soil_order
   
   integer :: ipedof
   
   if(soil_order==0)then
      ipedof=ipedof0
   endif
   
   end function get_ipedof   
end module FuncpedotransferMod
