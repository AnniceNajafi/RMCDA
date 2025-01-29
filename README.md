# RMCDA

This R package has been developed to serve as a universal library in R for the application of multi-criteria decision making methods. 

<h3>Installation Guide</h3>

To install R package run the following code:

```
devtools::install_github("AnniceNajafi/RMCDA")
```
<h3>Description of Methods</h3>
This is an R package for applying MCDA methods on product data.
 
 <h3>Methods</h3>
 <table>
  <thead>
    <tr>
      <th>Method</th>
      <th>Full Name</th>
      <th>References</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>AHP</td>
      <td>Analytic Hierarchy Process</td>
      <td><a href=https://link.springer.com/article/10.1007/s11518-006-0151-5>Saaty, T. L. (2004). Decision making—the analytic hierarchy and network processes (AHP/ANP). Journal of systems science and systems engineering, 13, 1-35.</a></td>
    </tr>
    <tr>
      <td>Fuzzy AHP</td>
      <td>Fuzzy Analytic Hierarchy Process</td>
    </tr>
    <tr>
      <td>ANP</td>
      <td>Analytic Network Process</td>
      <td><a href = https://link.springer.com/book/10.1007/978-1-4614-7279-7>Saaty, T. L., & Vargas, L. G. (2006). Decision making with the analytic network process (Vol. 282). Berlin, Germany: Springer Science+ Business Media, LLC.</a></td>
    </tr>
    <tr>
      <td>ARAS</td>
      <td>Additive Ratio Assessment</td>
      <td><a href = https://www.tandfonline.com/doi/abs/10.3846/TEDE.2010.10>Zavadskas, E. K., & Turskis, Z. (2010). A new additive ratio assessment (ARAS) method in multicriteria decision‐making. Technological and economic development of economy, 16(2), 159-172.</a></td>
    </tr>
    <tr>
      <td>BORDA</td>
      <td>Borda Count Method</td>
    </tr>
    <tr>
      <td>BWM</td>
      <td>Best Worst Method</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0305048314001480?casa_token=nWrNdLNgxLsAAAAA:y-S2YVMdX8H0vL4oL9vyhUljkyPKzyeIO_De3Xds38PqhwxQZH1-P2l5wR-0I9vN2GEx2umPE7w>Rezaei, J. (2015). Best-worst multi-criteria decision-making method. Omega, 53, 49-57.</a></td>
    </tr>
    <tr>
      <td>COCOSO</td>
      <td>Combined Compromise Solution</td>
      <td><a href = https://www.emerald.com/insight/content/doi/10.1108/MD-05-2017-0458/full/html?casa_token=2tQ35vkXRNcAAAAA:jjoT6lUqI2EqO1RCa4526f865wS5t6M_KAw6yTWq2ncqOXgHMK4svbcNenM6MhwbTaCD4s_3qjBtBHNJR1YvkXubBTxjrq5CSwjsJOvSwUIfRO8218SH>Yazdani, M., Zarate, P., Kazimieras Zavadskas, E., & Turskis, Z. (2019). A combined compromise solution (CoCoSo) method for multi-criteria decision-making problems. Management decision, 57(9), 2501-2519.
</a></td>
    </tr>
    <tr>
      <td>CODAS</td>
      <td>Combinative Distance-based Assessment</td>
      <td><a href=https://etalpykla.vilniustech.lt/handle/123456789/116529>Keshavarz Ghorabaee, M., Zavadskas, E. K., Turskis, Z., & Antuchevičienė, J. (2016). A new combinative distance-based assessment (CODAS) method for multi-criteria decision-making.</a></td>
    </tr>
    <tr>
      <td>COPELAND</td>
      <td>Copeland Method</td>
    </tr>
    <tr>
      <td>COPRAS</td>
      <td>Complex Proportional Assessment</td>
    </tr>
    <tr>
      <td>CRADIS</td>
      <td>Compromise Ranking and Distance from Ideal Solution</td>
    </tr>
    <tr>
      <td>CRITIC</td>
      <td>CRiteria Importance Through Intercriteria Correlation</td>
    </tr>
    <tr>
      <td>DEMATEL</td>
      <td>Decision-Making Trial and Evaluation Laboratory</td>
    </tr>
    <tr>
      <td>EDAS</td>
      <td>Evaluation based on Distance from Average Solution</td>
    </tr>
    <tr>
      <td>ELECTRE</td>
      <td>ELimination Et Choix Traduisant la REalité</td>
    </tr>
    <tr>
      <td>ENTROPY</td>
      <td>Entropy Method</td>
    </tr>
    <tr>
      <td>GRA</td>
      <td>Grey Relational Analysis</td>
    </tr>
    <tr>
      <td>IDOCRIW</td>
      <td>Integrated Determination of Objective CRIteria Weights </td>
    </tr>
    <tr>
      <td>MABAC</td>
      <td>Multi-Attributive Border Approximation Area Comparison</td>
    </tr>
    <tr>
      <td>MACBETH</td>
      <td>Measuring Attractiveness by a Categorical Based Evaluation Technique</td>
    </tr>
    <tr>
      <td>MAIRCA</td>
      <td>Multi-Attribute Ideal Real Comparative Analysis</td>
    </tr>
    <tr>
      <td>MARA</td>
      <td>Multi-Attribute Ranking Approach</td>
    </tr>
    <tr>
      <td>MARCOS</td>
      <td>Measurement of Alternatives and Ranking based on Compromise Solution</td>
    </tr>
    <tr>
      <td>MOORA</td>
      <td>Multi-Objective Optimization by Ratio Analysis</td>
    </tr>
    <tr>
      <td>MOOSRA</td>
      <td>Multi-objective Optimization on the Basis of Simple Ratio Analysis</td>
    </tr>
    <tr>
      <td>MULTIMOORA</td>
      <td>Multi-Objective Optimization on the basis of a Ratio Analysis plus the full MULTIplicative form</td>
    </tr>
    <tr>
      <td>OCRA</td>
      <td>Outranking Compromise Ranking Approach</td>
    </tr>
    <tr>
      <td>OPA</td>
      <td>Ordered Performance Analysis</td>
    </tr>
    <tr>
      <td>ORESTE</td>
      <td>Organisation, Rangement Et Synthèse De Données Relatives À L’évaluation</td>
    </tr>
    <tr>
      <td>PIV</td>
      <td>Position Index Value</td>
    </tr>
    <tr>
      <td>PROMETHEE</td>
      <td>Preference Ranking Organization Method for Enrichment Evaluation</td>
    </tr>
    <tr>
      <td>PSI</td>
      <td>Preference Selection Index</td>
    </tr>
    <tr>
      <td>RAFSI</td>
      <td>Ranking Alternatives using Full Subset Inference</td>
    </tr>
    <tr>
      <td>REGIME</td>
      <td>Regime Analysis</td>
    </tr>
    <tr>
      <td>RIM</td>
      <td>Ranking Index Method</td>
    </tr>
    <tr>
      <td>ROV</td>
      <td>Range of Value Method</td>
    </tr>
    <tr>
      <td>SAW</td>
      <td>Simple Additive Weighting</td>
    </tr>
    <tr>
      <td>SBWM</td>
      <td>Stratified Best Worst Method</td>
    </tr>
    <tr>
      <td>SECA</td>
      <td>Simple Evaluation of Complex Alternatives</td>
    </tr>
    <tr>
      <td>SMART</td>
      <td>Simple Multi-Attribute Rating Technique</td>
    </tr>
    <tr>
      <td>SMCDM</td>
      <td>Stratified Multi-Criteria Decision Making</td>
    </tr>
    <tr>
      <td>SPOTIS</td>
      <td>Stable Preference Ordering Towards Ideal Solution</td>
    </tr>
    <tr>
      <td>SRMP</td>
      <td>Simple Ranking Method using Reference Profiles</td>
    </tr>
    <tr>
      <td>TODIM</td>
      <td>Interactive and Multicriteria Decision Making</td>
    </tr>
    <tr>
      <td>TOPSIS</td>
      <td>Technique for Order of Preference by Similarity to Ideal Solution</td>
    </tr>
    <tr>
      <td>VIKOR</td>
      <td>VIšekriterijumsko KOmpromisno Rangiranje</td>
    </tr>
    <tr>
      <td>WASPAS</td>
      <td>Weighted Aggregated Sum Product Assessment</td>
    </tr>
    <tr>
      <td>WPM</td>
      <td>Weighted Product Method</td>
    </tr>
    <tr>
      <td>WSM</td>
      <td>Weighted Sum Method</td>
    </tr>
    <tr>
      <td>WINGS</td>
      <td>Weighted Influence Nonlinear Gauge System</td>
    </tr>
    <tr>
      <td>WISP</td>
      <td>Weighted Influence Score Preference</td>
    </tr>
  </tbody>
</table>


<h3>Online Application</h3> 
Web-based application to apply methods can be accessed through <a href="https://najafiannice.shinyapps.io/AHP_app/">this link</a>.

<h3>Contact Information</h3>
For any technical questions, please contact <a href=mailto:annicenajafi27@gmail.com>Annice Najafi</a>. 

<br>
<br>
<br>
Open-source software developed in California, 🇺🇸
