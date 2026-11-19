# Soil water retention and hydraulic conductivity

ELM relates soil water content $\theta$ to soil matric potential $\psi$ (the
retention curve) and to unsaturated hydraulic conductivity $K$ with one of two
models, selected by the `elm_inparm` namelist variable
`soil_water_retention_model`:

| Value | Model |
|---|---|
| `'clapp_hornberger_1978'` (default) | Clapp and Hornberger (1978)[@clapp1978empirical] power law |
| `'van_genuchten_1980'` | van Genuchten (1980)[@vangenuchten1980closed] curve with Mualem (1976)[@mualem1976new] conductivity, air-entry modified |

Suction is in mm of water and $\psi$ is negative. $s = \theta/\theta_s$ is the
relative saturation, where $\theta_s$ (`watsat`) is the porosity.

## Clapp and Hornberger (default)

$$
\psi = -\psi_s\, s^{-b}, \qquad K = K_s\, s^{2b+3}
$$

The saturated suction $\psi_s$ (`sucsat`), shape parameter $b$ (`bsw`), and
$\theta_s$ come from the Cosby et al. (1984)[@cosby1984statistical] pedotransfer
function. They are mixed with peat values by organic-matter fraction, following
Lawrence and Slater (2008)[@lawrence2008incorporating]. The curve has no residual
water content.

## van Genuchten - Mualem

With suction head $h = -\psi \ge 0$, residual water content $\theta_r$ (`watres`),
and effective saturation $S_e = (\theta - \theta_r)/(\theta_s - \theta_r)$:

$$
S_e(h) = \begin{cases} 1 & h \le h_e \\
\left[1 + (\alpha h)^n\right]^{-m} / S_c & h > h_e \end{cases},
\qquad S_c = \left[1 + (\alpha h_e)^n\right]^{-m}, \qquad m = 1 - 1/n
$$

$$
K = K_s\, S_e^{\,l} \left[\frac{1 - \left(1 - (S_e S_c)^{1/m}\right)^m}
{1 - \left(1 - S_c^{1/m}\right)^m}\right]^2
$$

This is the air-entry-modified form of Ippisch et al. (2006)[@ippisch2006validity],
with $h_e = 20$ mm (Vogel et al., 2001[@vogel2001effect]) and the Mualem exponent
$l = 0.5$. The air-entry suction keeps $d\psi/d\theta$ and $dK/d\theta$ finite at
saturation. ELM's water-content-based Richards solver (Zeng and Decker,
2009[@zeng2009improving]) needs both derivatives. $\alpha$ (`vg_alpha`) is in
mm$^{-1}$ and $n$ (`vg_n`) is dimensionless. Where $\theta$ falls so low that
$\psi$ would drop below `smpmin` ($-10^8$ mm), $S_e$ is held at the value that
gives `smpmin`.

### Parameters

* **Mineral soil:** Rosetta3 texture-class averages (model H1w of Zhang and Schaap,
  2017[@zhang2017weighted]) for the USDA texture class of each layer's sand and
  clay. Zhang and Schaap give these as ratios to the Rosetta1 class averages of
  Schaap et al. (2001)[@schaap2001rosetta], and both tables are in
  `FuncPedotransferMod.F90`.
* **Peat:** the continuous pedotransfer functions of Liu and Lennartz
  (2019)[@liu2019hydraulic] (all peat types), evaluated at a bulk density of
  `organic_max`. Their depth term is capped at 0.5 m, like ELM's other peat
  properties. They fix $\theta_r = 0$.
* **Mixing:** mineral and peat values are mixed by organic-matter fraction,
  linearly for $\theta_s$ and $\theta_r$ and in log space for $\alpha$ and $n$.
* **Unchanged:** saturated hydraulic conductivity $K_s$ (`hksat`) is the same in
  both models.
* **Bulk density:** by default it still comes from the Cosby et al. (1984)
  porosity, so bulk density, and the biogeochemistry that uses it, is the same in
  both models. Set `use_vg_porosity_for_bulk_density = .true.` to derive it from
  the van Genuchten porosity instead. Thermal properties that depend on porosity
  always use the van Genuchten $\theta_s$.
* **Derived water contents:** `watdry`, `watopt`, and `watmin` are evaluated on
  the van Genuchten curve at the same suctions as before. `watfc` is evaluated
  where $K$ = 0.1 mm/day, as before.
* **`bsw` and `sucsat`:** some code uses these as pore-structure indices rather
  than as a retention curve, for example gas tortuosity in the CH4 and
  nitrification models, and decomposition moisture limits. For that code they are
  replaced by the Brooks-Corey equivalents of each layer's van Genuchten curve
  (Lenhard et al., 1989[@lenhard1989correspondence]). The `bsw` and `sucsat`
  history fields report these equivalents.

### Where the curve is used

The van Genuchten curve replaces Clapp and Hornberger everywhere ELM evaluates a
retention curve:

* the Richards solver, including its equilibrium profile, where $\psi = -h_e$ at
  the water table. The layer-mean equilibrium water content has no closed form
  and is integrated numerically.
* aquifer specific yield and recharge;
* supercooled liquid water in frozen soil;
* `SOILPSI` for vegetation and biogeochemistry;
* root water stress and FATES btran;
* soil-surface humidity;
* the irrigation target.

The van Genuchten parameters are written to the inactive history fields `watres`,
`vg_alpha`, and `vg_n`.

### Restrictions

`'van_genuchten_1980'` cannot be combined with `use_vsfm`, `use_pflotran`,
`use_betr`, `use_fates_planthydro`, or `use_vichydro`. Those models compute soil
suction themselves from the Clapp and Hornberger parameters.

## Usage

In `user_nl_elm`:

```fortran
 soil_water_retention_model = 'van_genuchten_1980'
 use_vg_porosity_for_bulk_density = .false.   ! default
```

Starting a van Genuchten run from a Clapp-Hornberger initial-conditions file
reshapes the initial soil moisture profile, because water above the new porosity
is removed on the first time step.
