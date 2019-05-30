Hector Permafrost
================
Alexey N. Shiklomanov
30 May, 2019

# Introduction

Climate change is a problem (CITE: IPCC, NCA, etc.). Important to reduce
carbon emissions to meet temperature targets. The extent of
anthropogenic emissions reduction depends on what how much help (or
push-back) we get from the physical and biological Earth systems.

Permafrost is an important C reservoir. Total northern soil C pool is
estimated at 1672 PgC, of which approximately 1466 Pg (88%) is in
permafrost (Tarnocai et al., 2009). This frozen C can be released into
the atmosphere by several processes related to warming, including
aerobic respiration in thawed soil (REF) and anaeribic respiration in
thermokarst lakes and wetlands (Turetsky et al., 2002; Wickland et al.,
2006). But emissions from wet soils may be offset by high organic matter
accumulation rates (Camill et al., 2001). Permafrost thaw in boreal
peatlands in north-central Saskatchewan increased CO2 and CH4 fluxes
from soil to atmosphere by 1.6 and 30 times, respectively (Turetsky et
al., 2002). These impacts are exacerbated by the fact that the Arctic is
warming roughly 2.5 (TODO ???) times faster than the global average
(TODO REF). Projections of permafrost C emissions vary. (Schuur et al.,
2009) estimate 0.8 - 1.1 Pg C yr-1. Back-of-the-envelope estimates from
(Zimov, 2006): 10-40 g C m-3 day-1 off the bat, slowing down to
equilibrium (?) rate of 0.5-5 g C m-3 day-1 for several years.

On the other hand, warming and CO2 fertilization may increase vegetation
productivity, which could increase soil C storage through enhanced
litterfall; the balance of these two processes is uncertain (Jones et
al., 2005). Several modeling studies generally predict increases in soil
C in high latitudes (Burke et al., 2017; Ito et al., 2016; Qian et al.,
2010). However, this increase is dampened by including permafrost C
(Burke et al., 2017).

(Previous work on modeling permafrost) (Burke et al., 2017) – JULES and
ORCHIDEE, with new permafrost scheme, combined with intermediate
complexity climate-ocean model (IMOGEN), to look at climate sensitivity
to permafrost C emissions. Models are highly sensitive to representation
of soil processes, which can be more important than differences in
scenario and/or climate drivers (Burke et al., 2017).

Several studies have attempted to evaluate the potential economic
impacts of current and future permafrost C emissions. Lawrence et al.
(2012) project declines in near-surface permafrost by 33% and 72% in RCP
2.6 and 8.5, respectively, and permafrost C feedback will stabilize at
2100 in RCP 2.6. More recently, Hope and Schaefer (2015) projected
permafrost C emissions using the SiBCASA (TODO: write in full) land
surface model and used these projections as inputs to the PAGE09
integrated assessment model (IAM). They found that XXX. Chen et al.
(2019) also used PAGE09. Similarly, González-Eguino and Neumann (2016)
used the DICE (TODO: write in full) model to XXX. Kessler (2017) also
used DICE.

Hope and Schaefer (2015) (and others?) rely on climate damage functions
of the general form \(\delta GDP = f(\delta T)\). Such functions are, at
best, highly uncertain (TODO REF), and possibly conceptually flawed
(TODO REF). (TODO More on this). GCAM (TODO full name) is an alternative
(TODO REF). (TODO More on GCAM).

(Paragraph on simple climate models.) Hector (Hartin et al., 2015).
However, Hector does not have an explicit representation of permafrost C
emissions. In this study, we investigate whether the additional
complexity and parametric and structural uncertainty of an explicit
representation of permafrost may be warranted in Hector. To do this, we
evaluate the sensitivity of climate variables (as predicted by Hector)
to several different exogenous scenarios of permafrost C emissions.
(TODO Modify this to be about economic impact)

Some more relevant references: - (Zimov, 2006) - (Treat and Frolking,
2013) - (Burke et al., 2017; Drake et al., 2015; Hope and Schaefer,
2015; Kessler, 2017; Kuhry et al., 2010; Lee et al., 2011; Schaefer et
al., 2014, 2011; Schuur and Abbott, 2011; Schuur et al., 2015)

# Methods

## Model description: Hector

Hector (Hartin et al., 2015, p.@hartin\_2016\_ocean). Simple climate
model.

## Permafrost emissions scenarios

For all scenarios, we added the emission as a combination of fossil fuel
CO2 emissions (`ffi_emissions` column in Hector) and methane emissions
(`CH4_emissions` column in Hector). Where these were explicitly
separated, we added them accordingly. Where they were presented only in
terms of C, we did ???

Digitized scenarios from (Schaefer et al., 2011). SiBCASA model
predictions of CO2 emissions (permafrost respiration; \(R_{pc}\); note –
no methane\!) through 2300. These results were digitized using
WebPlotDigitizer (<https://apps.automeris.io/wpd/>), and interpolated to
annual resolution (using R `stats::spline` function).

Digitized scenarios from (Hope and Schaefer, 2015). CO2 and CH4
emissions from SiBCASA model.

(Schuur et al., 2009) – Estimate 0.8 - 1.1 Pg C yr-1.

Back-of-the-envelope estimates from (Zimov, 2006): - 500 Gt C in loess
that could be completely emitted by 2100 (plus other C sources). - 10-40
g C m-3 day-1 off the bat, slowing down to equilibrium (?) rate of 0.5-5
g C m-3 day-1 for several years. Combine with data on permafrost spatial
extent, density, etc. to generate estimates (but can back-calculate from
500 Gt C above?)

## Modeling permafrost emissions

Default heterotrophic respiration (\(R\)) scheme for a pool \(p\)
(detritus or soil) in Hector:

\[ R_{p} = C_p \times f_p \times Q_{10} ^ \frac{T}{10} \]

This is case 1.

Case 2 uses the same formulation, but splits permafrost C into its own
“biome”, with its own photosynthesis (\(\beta\)) and respiration
(\(Q_{10}\)) parameters.

Case 3 extends Case 2 by calculating CO<sub>2</sub> and CH<sub>4</sub>
emissions as a fraction of the total respiration, controlled by two new
parameters \(\alpha\) and \(\phi\):

\[ R_{p,tot} = C_p \times f_p \times Q_{10} ^ \frac{T}{10} \]

\[ R_{p, CH_4} = \alpha R_{p, tot} \]

\[ R_{p, CO_2} = (1 - \alpha) ^ \phi R_{p, tot} \]

## Parameter calibration

We used the `BayesianTools` R package (Hartig et al., 2019) for all
parameter calibration. The outputs of these calibrations are joint
posterior distributions of parameters and their covariances, from which
we sample for the sensitivity analysis.

For global parameters, we used the following likelihood:

\[ \log(L) = \sum_s Normal(Hector(\beta, Q_{10}, s) | CMIP5(s), \sigma) \]

where \(s\) is one of the four representative carbon pathways (RCPs),
\(CMIP5(s)\) are the CMIP5 global mean outputs for the corresponding
variables, and \(\sigma\) is the model error (estimated during the fit).
We also used the resulting distributions for \(\beta\) and \(Q_{10}\)
for the non-permafrost biome in cases 2 and 3. We feel this is
appropriate because the CMIP5 models against which these parameters are
calibrated do not include permafrost C feedbacks.

For case 2, we calibrated the permafrost-specific \(\beta\) and
\(Q_{10}\) against various literature sources, including:

  - Land surface model simulations (Burke et al., 2017; Hope and
    Schaefer, 2015; Schaefer et al., 2011).
      - Try to calibrate against NPP and soil respiration if possible
  - Literature surveys (Schaefer et al., 2014)
  - Warming experiments (Wickland et al., 2006)
  - TODO: Others?

Some of these are time series, while others are individual estimates at
particular points in time. To give them equal weight in the likelihood,
we down-weight the time series likelihoods by the number of time steps.

We derived a distribution for the Arctic warming factor from TODO.

TODO: Table and multi-panel figure of input datasets.

For the \(\alpha\) and \(\phi\) parameters in case 3, we looked at the
literature on permafrost methane emissions (e.g., Wickland et al.,
2006).

## Initial conditions

For case 1, we use the Hector defaults (Hartin et al., 2015).

For cases 2 and 3, we assumed the same total pool sizes as case 1, but
divided them into the permafrost and non-permafrost biomes according to
TODO.

## Other notes

Frozen carbon residence time (FCRt) from (Burke et al., 2017):

\[ FCRt = FCRt0 * exp(-\Delta T / \Gamma) (for \Delta T > 0.2 °C) \]

  - \(FCRt_0\) (years) reflects the stability of permafrost C (length of
    time that permafrost C is stable when \(\Delta T = 0\))
  - \(\Gamma\) – decay term (°C); temperature change at which “the
    number of years taken for all of the old permafrost C to be emitted
    reduces to 1/e of its initial value”

Other Hector parameters to consider.

| Variable   | INI name | Description                                 | Value |
| ---------- | -------- | ------------------------------------------- | ----- |
| \(f_{nv}\) | `f_nppv` | Fraction of NPP C transferred to vegetation | 0.35  |
| \(f_{nd}\) | `f_nppd` | Fraction of NPP C transferred to detritus   | 0.60  |
| \(f_{nd}\) |          | Fraction of NPP C transferred to soil       | 0.05  |
| \(f_{lv}\) | `f_lucv` | Fraction of LUC change flux from vegetation | 0.10  |
| \(f_{ld}\) | `f_lucd` | Fraction of LUC change flux from detritus   | 0.01  |
| \(f_{ls}\) |          | Fraction of LUC change flux from soil       | 0.89  |
| \(f_{ds}\) |          | Fraction of detritus C that goes to soil    | 0.60  |
| \(f_{rd}\) |          | Fraction of respiration C to detritus       | 0.25  |
| \(f_{rs}\) |          | Fraction of respriation C to soil           | 0.02  |

According to (Hartin et al., 2015), these were selected to be “generally
consistent with previous simple earth system models (e.g., Meinshausen
et al., 2011)”.

![Input parameter distributions for global
Hector.](paper_files/figure-gfm/fig-parameter-distribution-1.png)

# Results

## Global Hector

![Time series of Hector outputs for different parameter
combinations.](paper_files/figure-gfm/fig-global-timeseries-1.png)

![Hector outputs at 2100 as a function of input parameter
values.](paper_files/figure-gfm/fig-global-scatter-1.png)

![Sensitivity and variance decomposition analysis for global
Hector.](paper_files/figure-gfm/fig-global-sensitivity-1.png)

## Permafrost as a biome

**Figure**: Hector projections of parameter sensitivity (CI ribbon, or
light/transparent lines), colored (faceted?) by case.

**Figure**: PEcAn-like variance decomposition of parameters (for each
parameter: sensitivity, uncertainty, and partial variance)

## Permafrost methane emissions

**Figure**: Hector projections of parameter sensitivity (CI ribbon, or
light/transparent lines), colored (faceted?) by case.

**Figure**: PEcAn-like variance decomposition of parameters (for each
parameter: sensitivity, uncertainty, and partial variance)

# Discussion

Rate of permafrost C release also depends on soil moisture conditions –
drier soils release C much faster (“carbon bomb”) than wetter soils
(“carbon fizz”) (Elberling et al., 2013). Moisture will also affect
the balance of aerobic (CO2 release) vs. anaerobic (CH4) C release
(Turetsky et al., 2002), to the extent that unclear if anaerobic (wet)
areas are C sources or sinks (Wickland et al., 2006). Effects of
permafrost thaw on soil moisture are a complex hydrological problem –
drainage very sensitive to local (micro-)topography (Wickland et al.,
2006). So will vegetation cover (Wickland et al., 2006).

Temperature amplification of permafrost carbon feedback (by 2100) 0.02
to 0.36 °C (Burke et al., 2013; Schneider von Deimling et al., 2015,
2012), or 0.1 to 0.8 °C in (MacDougall et al., 2012, 2013), or 10-40% of
peak temperature change (Crichton et al., 2016), or 0.2 to 12% (Burke et
al., 2017).

Permafrost carbon has greater impact at lower emissions scenarios (Burke
et al., 2017; MacDougall et al., 2012, 2013) .

# Conclusion

# Acknowledgments

Funded by EPA grant XXX. Cyberinfrastructure support from Pacific
Northwest National Laboratory (PNNL).

<!-- The following line inserts a page break when the output is MS Word. For page breaks in PDF, use \newpage on its own line.  -->

##### pagebreak

# References

<!-- The following line ensures the references appear here for the MS Word or HTML output files, rather than right at the end of the document (this will not work for PDF files):  -->

<div id="refs">

<div id="ref-burke_2013_estimating">

Burke, E. J., Jones, C. D. and Koven, C. D.: Estimating the
permafrost-carbon climate response in the CMIP5 climate models using a
simplified approach, Journal of Climate, 26(14), 4897–4909,
doi:[10.1175/jcli-d-12-00550.1](https://doi.org/10.1175/jcli-d-12-00550.1),
2013.

</div>

<div id="ref-burke_2017_quantifying">

Burke, E. J., Ekici, A., Huang, Y., Chadburn, S. E., Huntingford, C.,
Ciais, P., Friedlingstein, P., Peng, S. and Krinner, G.: Quantifying
uncertainties of permafrost carbon-climate feedbacks, Biogeosciences
Discussions, 1–42,
doi:[10.5194/bg-2016-544](https://doi.org/10.5194/bg-2016-544), 2017.

</div>

<div id="ref-camill_2001_changes">

Camill, P., Lynch, J. A., Clark, J. S., Adams, J. B. and Jordan, B.:
Changes in biomass, aboveground net primary production, and peat
accumulation following permafrost thaw in the boreal peatlands of
Manitoba, Canada, Ecosystems, 4(5), 461–478,
doi:[10.1007/s10021-001-0022-3](https://doi.org/10.1007/s10021-001-0022-3),
2001.

</div>

<div id="ref-chen_2019_economic">

Chen, Y., Liu, A., Zhang, Z., Hope, C. and Crabbe, M. J. C.: Economic
losses of carbon emissions from circum-Arctic permafrost regions under
RCP-SSP scenarios, Science of The Total Environment, 658, 1064–1068,
doi:[10.1016/j.scitotenv.2018.12.299](https://doi.org/10.1016/j.scitotenv.2018.12.299),
2019.

</div>

<div id="ref-crichton_2016_permafrost">

Crichton, K. A., Bouttes, N., Roche, D. M., Chappellaz, J. and Krinner,
G.: Permafrost carbon as a missing link to explain CO2 changes during
the last deglaciation, Nature Geoscience, 9(9), 683–686,
doi:[10.1038/ngeo2793](https://doi.org/10.1038/ngeo2793), 2016.

</div>

<div id="ref-drake_2015_ancient">

Drake, T. W., Wickland, K. P., Spencer, R. G. M., McKnight, D. M. and
Striegl, R. G.: Ancient low-molecular-weight organic acids in permafrost
fuel rapid carbon dioxide production upon thaw, Proceedings of the
National Academy of Sciences, 112(45), 13946–13951,
doi:[10.1073/pnas.1511705112](https://doi.org/10.1073/pnas.1511705112),
2015.

</div>

<div id="ref-elberling_2013_long">

Elberling, B., Michelsen, A., Schädel, C., Schuur, E. A. G.,
Christiansen, H. H., Berg, L., Tamstorf, M. P. and Sigsgaard, C.:
Long-term CO2 production following permafrost thaw, Nature Climate
Change, 3(10), 890–894,
doi:[10.1038/nclimate1955](https://doi.org/10.1038/nclimate1955), 2013.

</div>

<div id="ref-gonzalez-eguino_2016_significant">

González-Eguino, M. and Neumann, M. B.: Significant implications of
permafrost thawing for climate change control, Climatic Change, 136(2),
381–388,
doi:[10.1007/s10584-016-1666-5](https://doi.org/10.1007/s10584-016-1666-5),
2016.

</div>

<div id="ref-R-BayesianTools">

Hartig, F., Minunno, F. and Paul, S.: BayesianTools: General-purpose
mcmc and smc samplers and tools for bayesian statistics. \[online\]
Available from: <https://CRAN.R-project.org/package=BayesianTools>,
2019.

</div>

<div id="ref-hartin_2015_simple">

Hartin, C. A., Patel, P., Schwarber, A., Link, R. P. and Bond-Lamberty,
B. P.: A simple object-oriented and open-source model for scientific and
policy analyses of the global climate system - Hector v1.0,
Geoscientific Model Development, 8(4), 939–955,
doi:[10.5194/gmd-8-939-2015](https://doi.org/10.5194/gmd-8-939-2015),
2015.

</div>

<div id="ref-hartin_2016_ocean">

Hartin, C. A., Bond-Lamberty, B., Patel, P. and Mundra, A.: Ocean
acidification over the next three centuries using a simple global
climate carbon-cycle model: Projections and sensitivities,
Biogeosciences, 13(15), 4329–4342,
doi:[10.5194/bg-13-4329-2016](https://doi.org/10.5194/bg-13-4329-2016),
2016.

</div>

<div id="ref-hope_2015_economic">

Hope, C. and Schaefer, K.: Economic impacts of carbon dioxide and
methane released from thawing permafrost, Nature Climate Change, 6(1),
56–59, doi:[10.1038/nclimate2807](https://doi.org/10.1038/nclimate2807),
2015.

</div>

<div id="ref-ito_2016_impacts">

Ito, A., Nishina, K. and Noda, H. M.: Impacts of future climate change
on the carbon budget of northern high-latitude terrestrial ecosystems:
An analysis using ISI-MIP data, Polar Science, 10(3), 346–355,
doi:[10.1016/j.polar.2015.11.002](https://doi.org/10.1016/j.polar.2015.11.002),
2016.

</div>

<div id="ref-jones_2005_global">

Jones, C., McConnell, C., Coleman, K., Cox, P., Falloon, P., Jenkinson,
D. and Powlson, D.: Global climate change and soil carbon stocks;
predictions from two contrasting models for the turnover of organic
carbon in soil, Global Change Biology, 11(1), 154–166,
doi:[10.1111/j.1365-2486.2004.00885.x](https://doi.org/10.1111/j.1365-2486.2004.00885.x),
2005.

</div>

<div id="ref-kessler_2017_estimating">

Kessler, L.: Estimating the economic impact of the permafrost carbon
feedback, Climate Change Economics, 08(02), 1750008,
doi:[10.1142/s2010007817500087](https://doi.org/10.1142/s2010007817500087),
2017.

</div>

<div id="ref-kuhry_2010_potential">

Kuhry, P., Dorrepaal, E., Hugelius, G., Schuur, E. A. G. and Tarnocai,
C.: Potential remobilization of belowground permafrost carbon under
future global warming, Permafrost and Periglacial Processes, 21(2),
208–214, doi:[10.1002/ppp.684](https://doi.org/10.1002/ppp.684), 2010.

</div>

<div id="ref-lawrence_2012_simulation">

Lawrence, D. M., Slater, A. G. and Swenson, S. C.: Simulation of
present-day and future permafrost and seasonally frozen ground
conditions in CCSM4, Journal of Climate, 25(7), 2207–2225,
doi:[10.1175/jcli-d-11-00334.1](https://doi.org/10.1175/jcli-d-11-00334.1),
2012.

</div>

<div id="ref-lee_2011_rate">

Lee, H., Schuur, E. A. G., Inglett, K. S., Lavoie, M. and Chanton, J.
P.: The rate of permafrost carbon release under aerobic and anaerobic
conditions and its potential effects on climate, Global Change Biology,
18(2), 515–527,
doi:[10.1111/j.1365-2486.2011.02519.x](https://doi.org/10.1111/j.1365-2486.2011.02519.x),
2011.

</div>

<div id="ref-macdougall_2012_significant">

MacDougall, A. H., Avis, C. A. and Weaver, A. J.: Significant
contribution to climate warming from the permafrost carbon feedback,
Nature Geoscience, 5(10), 719–721,
doi:[10.1038/ngeo1573](https://doi.org/10.1038/ngeo1573), 2012.

</div>

<div id="ref-macdougall_2013_if">

MacDougall, A. H., Eby, M. and Weaver, A. J.: If anthropogenic CO2
emissions cease, will atmospheric co2concentration continue to
increase?, Journal of Climate, 26(23), 9563–9576,
doi:[10.1175/jcli-d-12-00751.1](https://doi.org/10.1175/jcli-d-12-00751.1),
2013.

</div>

<div id="ref-meinshausen_2011_emulating">

Meinshausen, M., Raper, S. C. B. and Wigley, T. M. L.: Emulating coupled
atmosphere-ocean and carbon cycle models with a simpler model, magicc6 -
part 1: Model description and calibration, Atmospheric Chemistry and
Physics, 11(4), 1417–1456,
doi:[10.5194/acp-11-1417-2011](https://doi.org/10.5194/acp-11-1417-2011),
2011.

</div>

<div id="ref-qian_2010_enhanced">

Qian, H., Joseph, R. and Zeng, N.: Enhanced terrestrial carbon uptake in
the Northern High Latitudes in the 21st century from the Coupled Carbon
Cycle Climate Model Intercomparison Project model projections, Global
Change Biology, 16(2), 641–656,
doi:[10.1111/j.1365-2486.2009.01989.x](https://doi.org/10.1111/j.1365-2486.2009.01989.x),
2010.

</div>

<div id="ref-schaefer_2011_amount">

Schaefer, K., Zhang, T., Bruhwiler, L. and Barrett, A. P.: Amount and
timing of permafrost carbon release in response to climate warming,
Tellus B, 63(2), 165–180,
doi:[10.1111/j.1600-0889.2011.00527.x](https://doi.org/10.1111/j.1600-0889.2011.00527.x),
2011.

</div>

<div id="ref-schaefer_2014_impact">

Schaefer, K., Lantuit, H., Romanovsky, V. E., Schuur, E. A. G. and Witt,
R.: The impact of the permafrost carbon feedback on global climate,
Environmental Research Letters, 9(8), 085003,
doi:[10.1088/1748-9326/9/8/085003](https://doi.org/10.1088/1748-9326/9/8/085003),
2014.

</div>

<div id="ref-schneider_2012_estimating">

Schneider von Deimling, T., Meinshausen, M., Levermann, A., Huber, V.,
Frieler, K., Lawrence, D. M. and Brovkin, V.: Estimating the
near-surface permafrost-carbon feedback on global warming,
Biogeosciences, 9(2), 649–665,
doi:[10.5194/bg-9-649-2012](https://doi.org/10.5194/bg-9-649-2012),
2012.

</div>

<div id="ref-schneider_2015_observation">

Schneider von Deimling, T., Grosse, G., Strauss, J., Schirrmeister, L.,
Morgenstern, A., Schaphoff, S., Meinshausen, M. and Boike, J.:
Observation-based modelling of permafrost carbon fluxes with accounting
for deep carbon deposits and thermokarst activity, Biogeosciences,
12(11), 3469–3488,
doi:[10.5194/bg-12-3469-2015](https://doi.org/10.5194/bg-12-3469-2015),
2015.

</div>

<div id="ref-schuur_2011_climate_change">

Schuur, E. A. G. and Abbott, B.: Climate change: High risk of permafrost
thaw, Nature, 480(7375), 32–33,
doi:[10.1038/480032a](https://doi.org/10.1038/480032a), 2011.

</div>

<div id="ref-schuur_2009_effect">

Schuur, E. A. G., Vogel, J. G., Crummer, K. G., Lee, H., Sickman, J. O.
and Osterkamp, T. E.: The effect of permafrost thaw on old carbon
release and net carbon exchange from tundra, Nature, 459(7246), 556–559,
doi:[10.1038/nature08031](https://doi.org/10.1038/nature08031), 2009.

</div>

<div id="ref-schuur_2015_climate">

Schuur, E. A. G., McGuire, A. D., Schädel, C., Grosse, G., Harden, J.
W., Hayes, D. J., Hugelius, G., Koven, C. D., Kuhry, P., Lawrence, D. M.
and et al.: Climate change and the permafrost carbon feedback, Nature,
520(7546), 171–179,
doi:[10.1038/nature14338](https://doi.org/10.1038/nature14338), 2015.

</div>

<div id="ref-tarnocai_2009_soil">

Tarnocai, C., Canadell, J. G., Schuur, E. A. G., Kuhry, P., Mazhitova,
G. and Zimov, S.: Soil organic carbon pools in the northern circumpolar
permafrost region, Global Biogeochemical Cycles, 23(2), n/a–n/a,
doi:[10.1029/2008gb003327](https://doi.org/10.1029/2008gb003327), 2009.

</div>

<div id="ref-treat_2013_permafrost_carbon_bomb">

Treat, C. C. and Frolking, S.: A permafrost carbon bomb?, Nature Climate
Change, 3(10), 865–867,
doi:[10.1038/nclimate2010](https://doi.org/10.1038/nclimate2010), 2013.

</div>

<div id="ref-turetsky_2002_boreal">

Turetsky, M. R., Wieder, R. and Vitt, D. H.: Boreal peatland C fluxes
under varying permafrost regimes, Soil Biology and Biochemistry, 34(7),
907–912,
doi:[10.1016/s0038-0717(02)00022-6](https://doi.org/10.1016/s0038-0717\(02\)00022-6),
2002.

</div>

<div id="ref-wickland_2006_effects">

Wickland, K. P., Striegl, R. G., Neff, J. C. and Sachs, T.: Effects of
permafrost melting on CO2 and CH4 exchange of a poorly drained black
spruce lowland, Journal of Geophysical Research: Biogeosciences,
111(G2), n/a–n/a,
doi:[10.1029/2005jg000099](https://doi.org/10.1029/2005jg000099), 2006.

</div>

<div id="ref-zimov_2006_climate_change">

Zimov, S. A.: Climate change: Permafrost and the global carbon budget,
Science, 312(5780), 1612–1613,
doi:[10.1126/science.1128908](https://doi.org/10.1126/science.1128908),
2006.

</div>

</div>

##### pagebreak

### Colophon

This report was generated on 2019-05-30 11:45:57 using the following
computational environment and dependencies:

    #> ─ Session info ──────────────────────────────────────────────────────────
    #>  setting  value                       
    #>  version  R version 3.6.0 (2019-04-26)
    #>  os       macOS High Sierra 10.13.6   
    #>  system   x86_64, darwin17.7.0        
    #>  ui       unknown                     
    #>  language (EN)                        
    #>  collate  en_US.UTF-8                 
    #>  ctype    en_US.UTF-8                 
    #>  tz       America/New_York            
    #>  date     2019-05-30                  
    #> 
    #> ─ Packages ──────────────────────────────────────────────────────────────
    #>  ! package                * version    date       lib source        
    #>    assertthat               0.2.1      2019-03-21 [1] CRAN (R 3.6.0)
    #>    backports                1.1.4      2019-04-10 [1] CRAN (R 3.6.0)
    #>    base64url                1.4        2018-05-14 [1] CRAN (R 3.6.0)
    #>    bookdown                 0.10       2019-05-10 [1] CRAN (R 3.6.0)
    #>    callr                    3.2.0      2019-03-15 [1] CRAN (R 3.6.0)
    #>    cli                      1.1.0      2019-03-19 [1] CRAN (R 3.6.0)
    #>    codetools                0.2-16     2018-12-24 [3] CRAN (R 3.6.0)
    #>    colorspace               1.4-1      2019-03-18 [1] CRAN (R 3.6.0)
    #>    cowplot                * 0.9.4      2019-01-08 [1] CRAN (R 3.6.0)
    #>    crayon                   1.3.4      2017-09-16 [1] CRAN (R 3.6.0)
    #>    desc                     1.2.0      2018-05-01 [1] CRAN (R 3.6.0)
    #>    devtools                 2.0.2      2019-04-08 [1] CRAN (R 3.6.0)
    #>    digest                   0.6.19     2019-05-20 [1] CRAN (R 3.6.0)
    #>    dplyr                  * 0.8.1      2019-05-14 [1] CRAN (R 3.6.0)
    #>    drake                  * 7.3.0      2019-05-19 [1] CRAN (R 3.6.0)
    #>    evaluate                 0.14       2019-05-28 [1] CRAN (R 3.6.0)
    #>    fs                       1.3.1      2019-05-06 [1] CRAN (R 3.6.0)
    #>    future                 * 1.13.0     2019-05-08 [1] CRAN (R 3.6.0)
    #>    GGally                   1.4.0      2018-05-17 [1] CRAN (R 3.6.0)
    #>    ggplot2                * 3.1.1      2019-04-07 [1] CRAN (R 3.6.0)
    #>    globals                  0.12.4     2018-10-11 [1] CRAN (R 3.6.0)
    #>    glue                     1.3.1      2019-03-12 [1] CRAN (R 3.6.0)
    #>    gtable                   0.3.0      2019-03-25 [1] CRAN (R 3.6.0)
    #>    hector                   2.2.0      2019-05-29 [1] local         
    #>  P hector.permafrost.emit * 0.0.0.9000 2019-05-30 [?] local         
    #>    here                   * 0.1        2017-05-28 [1] CRAN (R 3.6.0)
    #>    highr                    0.8        2019-03-20 [1] CRAN (R 3.6.0)
    #>    hms                      0.4.2      2018-03-10 [1] CRAN (R 3.6.0)
    #>    htmltools                0.3.6      2017-04-28 [1] CRAN (R 3.6.0)
    #>    igraph                   1.2.4.1    2019-04-22 [1] CRAN (R 3.6.0)
    #>    knitr                    1.23       2019-05-18 [1] CRAN (R 3.6.0)
    #>    labeling                 0.3        2014-08-23 [1] CRAN (R 3.6.0)
    #>    lattice                  0.20-38    2018-11-04 [3] CRAN (R 3.6.0)
    #>    lazyeval                 0.2.2      2019-03-15 [1] CRAN (R 3.6.0)
    #>    listenv                  0.7.0      2018-01-21 [1] CRAN (R 3.6.0)
    #>    magrittr                 1.5        2014-11-22 [1] CRAN (R 3.6.0)
    #>    Matrix                   1.2-17     2019-03-22 [3] CRAN (R 3.6.0)
    #>    memoise                  1.1.0      2017-04-21 [1] CRAN (R 3.6.0)
    #>    mgcv                     1.8-28     2019-03-21 [3] CRAN (R 3.6.0)
    #>    munsell                  0.5.0      2018-06-12 [1] CRAN (R 3.6.0)
    #>    nlme                     3.1-140    2019-05-12 [3] CRAN (R 3.6.0)
    #>    pillar                   1.4.1      2019-05-28 [1] CRAN (R 3.6.0)
    #>    pkgbuild                 1.0.3      2019-03-20 [1] CRAN (R 3.6.0)
    #>    pkgconfig                2.0.2      2018-08-16 [1] CRAN (R 3.6.0)
    #>    pkgload                  1.0.2      2018-10-29 [1] CRAN (R 3.6.0)
    #>    plyr                     1.8.4      2016-06-08 [1] CRAN (R 3.6.0)
    #>    prettyunits              1.0.2      2015-07-13 [1] CRAN (R 3.6.0)
    #>    processx                 3.3.1      2019-05-08 [1] CRAN (R 3.6.0)
    #>    ps                       1.3.0      2018-12-21 [1] CRAN (R 3.6.0)
    #>    purrr                    0.3.2      2019-03-15 [1] CRAN (R 3.6.0)
    #>    R6                       2.4.0      2019-02-14 [1] CRAN (R 3.6.0)
    #>    RColorBrewer             1.1-2      2014-12-07 [1] CRAN (R 3.6.0)
    #>    Rcpp                     1.0.1      2019-03-17 [1] CRAN (R 3.6.0)
    #>    readr                  * 1.3.1      2018-12-21 [1] CRAN (R 3.6.0)
    #>    remotes                  2.0.4      2019-04-10 [1] CRAN (R 3.6.0)
    #>    reshape                  0.8.8      2018-10-23 [1] CRAN (R 3.6.0)
    #>    reshape2                 1.4.3      2017-12-11 [1] CRAN (R 3.6.0)
    #>    rlang                    0.3.4      2019-04-07 [1] CRAN (R 3.6.0)
    #>    rmarkdown                1.13       2019-05-22 [1] CRAN (R 3.6.0)
    #>    rprojroot                1.3-2      2018-01-03 [1] CRAN (R 3.6.0)
    #>    rstudioapi               0.10       2019-03-19 [1] CRAN (R 3.6.0)
    #>    scales                   1.0.0      2018-08-09 [1] CRAN (R 3.6.0)
    #>    sessioninfo              1.1.1      2018-11-05 [1] CRAN (R 3.6.0)
    #>    storr                    1.2.1      2018-10-18 [1] CRAN (R 3.6.0)
    #>    stringi                  1.4.3      2019-03-12 [1] CRAN (R 3.6.0)
    #>    stringr                  1.4.0      2019-02-10 [1] CRAN (R 3.6.0)
    #>    testthat               * 2.1.1      2019-04-23 [1] CRAN (R 3.6.0)
    #>    tibble                   2.1.1      2019-03-16 [1] CRAN (R 3.6.0)
    #>    tidyr                  * 0.8.3      2019-03-01 [1] CRAN (R 3.6.0)
    #>    tidyselect               0.2.5      2018-10-11 [1] CRAN (R 3.6.0)
    #>    usethis                  1.5.0      2019-04-07 [1] CRAN (R 3.6.0)
    #>    withr                    2.1.2      2018-03-15 [1] CRAN (R 3.6.0)
    #>    xfun                     0.7        2019-05-14 [1] CRAN (R 3.6.0)
    #>    yaml                     2.2.0      2018-07-25 [1] CRAN (R 3.6.0)
    #> 
    #> [1] /Users/shik544/R
    #> [2] /usr/local/lib/R/3.6/site-library
    #> [3] /usr/local/Cellar/r/3.6.0_2/lib/R/library
    #> 
    #>  P ── Loaded and on-disk path mismatch.

The current Git commit details are:

    #> Local:    master /Users/shik544/Box Sync/Projects/hector_project/permafrost_emit
    #> Head:     [2209bc6] 2019-05-30: Don't deploy in `make all`
