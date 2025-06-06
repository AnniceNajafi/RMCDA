# RMCDA

This R package has been developed to serve as a universal library in R for the application of multi-criteria decision making methods. 

<br>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14837902.svg)](https://doi.org/10.5281/zenodo.14837902)

<h2>UPDATE (6/6/25): the RMCDA package is now available on <a href="https://cran.r-project.org/web/packages/RMCDA/index.html">CRAN</a>!</h2>

<h3>Installation Guide</h3>

<h4>CRAN version:</h4>

To install the CRAN version please simply run the following code (please check the reference manual for the CRAN version on <a href="https://cran.r-project.org/web/packages/RMCDA/index.html">CRAN's website</a>:

```
install.packages('RMCDA')
```

To install the R package from Github run the following code:

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
      <td><a href=https://www.taylorfrancis.com/chapters/edit/10.1201/9781315369884-3/comparison-methods-fahp-application-supplier-selection-nimet-yapici-pehlivan-turan-paksoy-ahmet-%C3%A7alik>Pehlivan, N. Y., Paksoy, T., & Çalik, A. (2017). Comparison of methods in FAHP with application in supplier selection. In Fuzzy analytic hierarchy process (pp. 45-76). Chapman and Hall/CRC.</a></td>
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
      <td><a href = https://cir.nii.ac.jp/crid/1570854175105600384>Borda, J. D. (1781). M'emoire sur les' elections au scrutin. Histoire de l'Acad'emie Royale des Sciences.</a></td>
    </tr>
    <tr>
      <td>BWM</td>
      <td>Best Worst Method</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0305048314001480?casa_token=nWrNdLNgxLsAAAAA:y-S2YVMdX8H0vL4oL9vyhUljkyPKzyeIO_De3Xds38PqhwxQZH1-P2l5wR-0I9vN2GEx2umPE7w>Rezaei, J. (2015). Best-worst multi-criteria decision-making method. Omega, 53, 49-57.</a></td>
    </tr>
   <tr>
      <td>CILOS</td>
      <td>Crietion Impact LOSs</td>
      <td><a href=https://www.worldscientific.com/doi/abs/10.1142/S0219622016500036>Zavadskas, E. K., & Podvezko, V. (2016). Integrated determination of objective criteria weights in MCDM. International Journal of Information Technology & Decision Making, 15(02), 267-283.</a></td>
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
      <td><a href=https://idp.springer.com/authorize/casa?redirect_uri=https://link.springer.com/article/10.1007/bf01212012&casa_token=81GHAXKFs28AAAAA:OoRU5rbnI_HQarZyUmP7heBUQoj0PW6W0vZZF6T77kz2UkVXM43y1buf2_XDa_7SVRXRPz7I4MCI4eu7>Saari, D. G., & Merlin, V. R. (1996). The copeland method: I.: Relationships and the dictionary. Economic theory, 8, 51-76.</a></td>
    </tr>
    <tr>
      <td>COPRAS</td>
      <td>Complex Proportional Assessment</td>
      <td><a href=https://journals.rtu.lv/index.php/BJRBE/article/view/1822-427X.2007.4.195%E2%80%93203>Zavadskas, E. K., Kaklauskas, A., Peldschus, F., & Turskis, Z. (2007). Multi-attribute assessment of road design solutions by using the COPRAS method. The Baltic journal of Road and Bridge engineering, 2(4), 195-203.</a></td>
    </tr>
    <tr>
      <td>CRADIS</td>
      <td>Compromise Ranking and Distance from Ideal Solution</td>
      <td><a href=https://link.springer.com/article/10.1007/s10668-021-01902-2>Puška, A., Stević, Ž., & Pamučar, D. (2022). Evaluation and selection of healthcare waste incinerators using extended sustainability criteria and multi-criteria analysis methods. Environment, Development and Sustainability, 1-31.</a></td>
    </tr>
    <tr>
      <td>CRITIC</td>
      <td>CRiteria Importance Through Intercriteria Correlation</td>
      <td><a href=https://www.sciencedirect.com/science/article/abs/pii/030505489400059H?via%3Dihub>Diakoulaki, D., Mavrotas, G., & Papayannakis, L. (1995). Determining objective weights in multiple criteria problems: The critic method. Computers & Operations Research, 22(7), 763-770.</a></td>
    </tr>
    <tr>
      <td>DEMATEL</td>
      <td>Decision-Making Trial and Evaluation Laboratory</td>
      <td><a href=https://onlinelibrary.wiley.com/doi/abs/10.1155/2018/3696457>Si, S. L., You, X. Y., Liu, H. C., & Zhang, P. (2018). DEMATEL technique: a systematic review of the state‐of‐the‐art literature on methodologies and applications. Mathematical problems in Engineering, 2018(1), 3696457.</a></td>
    </tr>
    <tr>
      <td>EDAS</td>
      <td>Evaluation based on Distance from Average Solution</td>
      <td><a href=https://content.iospress.com/articles/informatica/inf1070>Keshavarz Ghorabaee, M., Zavadskas, E. K., Olfat, L., & Turskis, Z. (2015). Multi-criteria inventory classification using a new method of evaluation based on distance from average solution (EDAS). Informatica, 26(3), 435-451.</a></td>
    </tr>
    <tr>
      <td>ELECTRE I</td>
      <td>ELimination Et Choix Traduisant la REalité</td>
      <td><a href=http://www.numdam.org/item/RO_1968__2_1_57_0.pdf>
Roy, B. (1968). Classement et choix en présence de points de vue multiples. Revue française d'informatique et de recherche opérationnelle, 2(8), 57-75.</a></td>
    </tr>
    <tr>
      <td>ENTROPY</td>
      <td>Entropy Method</td>
      <td><a href=https://ieeexplore.ieee.org/abstract/document/6773024/?casa_token=_kCZjBOKVMgAAAAA:wq8nKNnia70lL4kneInKjM5WZz5V5uv1SQz4IUo9kGnEQewUsOiaLN-Vfh3X5HoglmLG0sH8>Shannon, C. E. (1948). A mathematical theory of communication. The Bell system technical journal, 27(3), 379-423.</a></td>
    </tr>
    <tr>
      <td>GRA</td>
      <td>Grey Relational Analysis</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0360835207002732?casa_token=pzzTjhaHoTUAAAAA:QgNfcIX3BmmUaSt8oI4cpRuyPIQlAa8NvVhb9oU47PmJlvP-TcvHAdLbKeKk9XLqiGxadAUZgg>Kuo, Y., Yang, T., & Huang, G. W. (2008). The use of grey relational analysis in solving multiple attribute decision-making problems. Computers & industrial engineering, 55(1), 80-93.</a></td>
    </tr>
    <tr>
      <td>IDOCRIW</td>
      <td>Integrated Determination of Objective CRIteria Weights </td>
      <td><a href=https://www.worldscientific.com/doi/abs/10.1142/S0219622016500036>Zavadskas, E. K., & Podvezko, V. (2016). Integrated determination of objective criteria weights in MCDM. International Journal of Information Technology & Decision Making, 15(02), 267-283.</a></td>
    </tr>
    <tr>
      <td>MABAC</td>
      <td>Multi-Attributive Border Approximation Area Comparison</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0957417414007568?casa_token=0e_2p15LstQAAAAA:Piq5a90Bopn646GsqlGLmwBncig9NPtrC37S2WH7ThyDjvXlX4Zapv_1-NmHWDj2qRhDXZzmhw>Pamučar, D., & Ćirović, G. (2015). The selection of transport and handling resources in logistics centers using Multi-Attributive Border Approximation area Comparison (MABAC). Expert systems with applications, 42(6), 3016-3028.</a></td>
    </tr>
    <tr>
      <td>MACBETH</td>
      <td>Measuring Attractiveness by a Categorical Based Evaluation Technique</td>
      <td><a href=https://www.frontiersin.org/articles/10.3389/fbuil.2019.00006/full>Marcelino, P., Antunes, M. D. L., Fortunato, E., & Gomes, M. C. (2019). Development of a multi criteria decision analysis model for pavement maintenance at the network level: Application of the MACBETH approach. Frontiers in Built Environment, 5, 6.</a></td>
    </tr>
    <tr>
      <td>MAIRCA</td>
      <td>Multi-Attribute Ideal Real Comparative Analysis</td>
      <td><a href=https://www.tandfonline.com/doi/abs/10.1080/02626667.2022.2027949>Hadian, S., Shahiri Tabarestani, E., & Pham, Q. B. (2022). Multi attributive ideal-real comparative analysis (MAIRCA) method for evaluating flood susceptibility in a temperate Mediterranean climate. Hydrological Sciences Journal, 67(3), 401-418.</a></td>
    </tr>
    <tr>
      <td>MARA</td>
      <td>Multi-Attribute Ranking Approach</td>
      <td><a href=https://www.mdpi.com/2079-8954/10/6/248>Gligorić, M., Gligorić, Z., Lutovac, S., Negovanović, M., & Langović, Z. (2022). Novel hybrid MPSI–MARA decision-making model for support system selection in an underground mine. Systems, 10(6), 248.</a></td>
    </tr>
   <tr>
      <td>MAUT</td>
      <td>Multi-Attribute Utility Theory</td>
      <td><a href=https://www.tandfonline.com/doi/abs/10.1080/02626667.2022.2027949>Keeney, R. L. (1993). Decisions with multiple objectives: Preferences and value tradeoffs. Cambridge university press.</a></td>
    </tr>
    <tr>
      <td>MARCOS</td>
      <td>Measurement of Alternatives and Ranking based on Compromise Solution</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0360835219307004?casa_token=NvTwbBwCA6oAAAAA:BABhztb4Fu5dIR4eh8LSyf25PUL9xWwpK7Co-5IP9r-u2htCX-GA7GnHOzrcf9NIJ7CZ3RwNKA>Stević, Ž., Pamučar, D., Puška, A., & Chatterjee, P. (2020). Sustainable supplier selection in healthcare industries using a new MCDM method: Measurement of alternatives and ranking according to COmpromise solution (MARCOS). Computers & industrial engineering, 140, 106231.</a></td>
    </tr>
    <tr>
      <td>MOORA</td>
      <td>Multi-Objective Optimization by Ratio Analysis</td>
      <td><a href=https://bibliotekanauki.pl/articles/969961.pdf>Brauers, W. K., & Zavadskas, E. K. (2006). The MOORA method and its application to privatization in a transition economy. Control and cybernetics, 35(2), 445-469.</a></td>
    </tr>
    <tr>
      <td>MOOSRA</td>
      <td>Multi-objective Optimization on the Basis of Simple Ratio Analysis</td>
      <td><a href=https://ijret.org/volumes/2014v03/i15/IJRET20140315105.pdf>Jagadish, R. A., & Ray, A. (2014). Green cutting fluid selection using MOOSRA method. International Journal of Research in Engineering and Technology, 3(3), 559-563.</a></td>
    </tr>
    <tr>
      <td>MULTIMOORA</td>
      <td>Multi-Objective Optimization on the basis of a Ratio Analysis plus the full MULTIplicative form</td>
      <td><a href=https://www.tandfonline.com/doi/abs/10.3846/tede.2010.01>Brauers, W. K. M., & Zavadskas, E. K. (2010). Project management by MULTIMOORA as an instrument for transition economies. Technological and economic development of economy, 16(1), 5-24.</a></td>
    </tr>
    <tr>
      <td>OCRA</td>
      <td>Outranking Compromise Ranking Approach</td>
      <td><a href=https://aseestant.ceon.rs/index.php/sjm/article/view/6802>Madić, M., Petković, D., & Radovanović, M. (2015). Selection of non-conventional machining processes using the OCRA method. Serbian Journal of Management, 10(1), 61-73.</a></td>
    </tr>
    <tr>
      <td>OPA</td>
      <td>Ordered Performance Analysis</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S156849461930674X?casa_token=D-H-w4jrVoIAAAAA:Dh570QVpWFIs_LW3gAeAVrF6avGWia4fkyJYXGtGUmWmDgL_KHa9Rqq39c8r9VLVwlKrQVhMbA>Ataei, Y., Mahmoudi, A., Feylizadeh, M. R., & Li, D. F. (2020). Ordinal priority approach (OPA) in multiple attribute decision-making. Applied Soft Computing, 86, 105893.</a></td>
    </tr>
    <tr>
      <td>ORESTE</td>
      <td>Organisation, Rangement Et Synthèse De Données Relatives À L’évaluation</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/037722178290131X>Roubens, M. (1982). Preference relations on actions and criteria in multicriteria decision making. European Journal of Operational Research, 10(1), 51-55.</a></td>
    </tr>
    <tr>
      <td>PIV</td>
      <td>Position Index Value</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0360835218301360?casa_token=RCRl_SnesMYAAAAA:p39U0yEyVuX0r7vLcjz-Q8FkLadZc7xT9QNqiv-jD0alDyglHzMFFomUlt7w1v3PjO5dbYswig>Mufazzal, S., & Muzakkir, S. M. (2018). A new multi-criterion decision making (MCDM) method based on proximity indexed value for minimizing rank reversals. Computers & Industrial Engineering, 119, 427-438.</a></td>
    </tr>
    <tr>
      <td>PROMETHEE</td>
      <td>Preference Ranking Organization Method for Enrichment Evaluation</td>
      <td><a href=https://link.springer.com/chapter/10.1007/978-1-4939-3094-4_6>Brans, J. P., & De Smet, Y. (2016). PROMETHEE methods. Multiple criteria decision analysis: state of the art surveys, 187-219.</a></td>
    </tr>
    <tr>
      <td>PSI</td>
      <td>Preference Selection Index</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0261306909006396?casa_token=8muWQkZrCbwAAAAA:0h9Tly226WydBFZGe5pPycs2NwUdCQl4j_vq7yNLOn9CcE6zpV651gSaNg5hZlfVPZcOJu4VeQ>Maniya, K., & Bhatt, M. G. (2010). A selection of material using a novel type decision-making method: Preference selection index method. Materials & Design, 31(4), 1785-1789.</a></td>
    </tr>
    <tr>
      <td>RAFSI</td>
      <td>Ranking Alternatives using Full Subset Inference</td>
      <td><a href=https://www.mdpi.com/2227-7390/8/6/1015>Žižović, M., Pamučar, D., Albijanić, M., Chatterjee, P., & Pribićević, I. (2020). Eliminating rank reversal problem using a new multi-attribute model—the RAFSI method. Mathematics, 8(6), 1015.</a></td>
    </tr>
    <tr>
      <td>REGIME</td>
      <td>Regime Analysis</td>
      <td><a href=https://idp.springer.com/authorize/casa?redirect_uri=https://link.springer.com/article/10.1007/BF00221383&casa_token=1r62j3wEeE4AAAAA:Ag_NmcMx9MRhQkeIhoMvUCgBek2sksj1wFcfdWa_DPvJ74_k1wozSCiCz9lVSfyB-n2-iEHA0KxNxr63>Hinloopen, E., & Nijkamp, P. (1990). Qualitative multiple criteria choice analysis: the dominant regime method. Quality and quantity, 24, 37-56.</a></td>
    </tr>
    <tr>
      <td>RIM</td>
      <td>Ranking Index Method</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0020025515009007?casa_token=iCVf0Q8UhcEAAAAA:th2JMdtOi4qMw2a8nf17e1CtfZHSQIXByHc9jRA_u1ineiVUNKR3s1tTshKosCyTMksRhXxJEQ>Cables, E., Lamata, M. T., & Verdegay, J. L. (2016). RIM-reference ideal method in multicriteria decision making. Information Sciences, 337, 1-10.
</a></td>
    </tr>
    <tr>
      <td>ROV</td>
      <td>Range of Value Method</td>
      <td><a href=http://growingscience.com/beta/dsl/2175-application-of-the-rov-method-for-the-selection-of-cutting-fluids.html>Madić, M., Radovanović, M., & Manić, M. (2016). Application of the ROV method for the selection of cutting fluids. Decision Science Letters, 5(2), 245-254.</a></td>
    </tr>
    <tr>
      <td>SAW</td>
      <td>Simple Additive Weighting</td>
      <td><a href=https://www.neliti.com/publications/326766/simple-additive-weighting-saw-method-in-determining-beneficiaries-of-foundation>Panjaitan, M. I. (2019). Simple Additive Weighting (SAW) method in determining beneficiaries of foundation benefits. Login, 13(1), 19-25.</a></td>
    </tr>
    <tr>
      <td>SBWM</td>
      <td>Stratified Best Worst Method</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0956053X21000040?casa_token=c9pXtRPYLxEAAAAA:grSctFF2lzvwP03UXOBSw1W11KOhRR5H6nMeKnKWhBIb7kMcTg4lTNDrBh8I-SZ98xRUZT_P2A>Torkayesh, A. E., Malmir, B., & Asadabadi, M. R. (2021). Sustainable waste disposal technology selection: The stratified best-worst multi-criteria decision-making method. Waste Management, 122, 100-112.
</a></td>
    </tr>
    <tr>
      <td>SECA</td>
      <td>Simple Evaluation of Complex Alternatives</td>
      <td><a href=https://content.iospress.com/articles/informatica/inf1182>Keshavarz-Ghorabaee, M., Amiri, M., Zavadskas, E. K., Turskis, Z., & Antucheviciene, J. (2018). Simultaneous evaluation of criteria and alternatives (SECA) for multi-criteria decision-making. Informatica, 29(2), 265-280.</a></td>
    </tr>
    <tr>
      <td>SMART</td>
      <td>Simple Multi-Attribute Rating Technique</td>
      <td><a href=https://link.springer.com/chapter/10.1007/978-1-4612-3982-6_4>Olson, D. L., & Olson, D. L. (1996). Smart. Decision aids for selection problems, 34-48.
</a></td>
    </tr>
    <tr>
      <td>SMCDM</td>
      <td>Stratified Multi-Criteria Decision Making</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0950705118303502?casa_token=x3z2y4LFGbUAAAAA:wC5xtJerOtdjFB1mUoqpeLzeabHwprmzeOeeByURaz9lNJyG5p-IHBC3BI5j3HZKiHgvbTk8Ag>Asadabadi, M. R. (2018). The stratified multi-criteria decision-making method. Knowledge-Based Systems, 162, 115-123.</a></td>
    </tr>
    <tr>
      <td>SPOTIS</td>
      <td>Stable Preference Ordering Towards Ideal Solution</td>
      <td><a href=https://ieeexplore.ieee.org/abstract/document/9190347/>Dezert, J., Tchamova, A., Han, D., & Tacnet, J. M. (2020, July). The SPOTIS rank reversal free method for multi-criteria decision-making support. In 2020 IEEE 23rd International Conference on Information Fusion (FUSION) (pp. 1-8). IEEE.</a></td>
    </tr>
    <tr>
      <td>SRMP</td>
      <td>Simple Ranking Method using Reference Profiles</td>
      <td><a href=https://idp.springer.com/authorize/casa?redirect_uri=https://link.springer.com/article/10.1007/s10288-021-00487-w&casa_token=En0A95YU-8wAAAAA:8PuI3V8HS41uwkTtw_Q7NeH7oQQ6_9SIYfmsIop9srGZax_O_bzaFdEQutQmgB1Tuz57ZQ231Dvk1ayp>Khannoussi, A., Olteanu, A. L., Labreuche, C., & Meyer, P. (2022). Simple ranking method using reference profiles: incremental elicitation of the preference parameters. 4OR, 1-32.</a></td>
    </tr>
    <tr>
      <td>TODIM</td>
      <td>Interactive and Multicriteria Decision Making</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0377221707010740?casa_token=2Ydra4zgeUkAAAAA:6LEUit3fmjK6viRTi5S200bpGKq3Ftpcs8XgYeqZmqJkB58gWHTjeluxBVnlErSrK8SdF8PfnA>Gomes, L. F. A. M. (2009). An application of the TODIM method to the multicriteria rental evaluation of residential properties. European Journal of Operational Research, 193(1), 204-211.</a></td>
    </tr>
    <tr>
      <td>TOPSIS</td>
      <td>Technique for Order of Preference by Similarity to Ideal Solution</td>
      <td><a href=https://link.springer.com/chapter/10.1007/978-3-642-48318-9_3>Hwang, C. L., Yoon, K., Hwang, C. L., & Yoon, K. (1981). Methods for multiple attribute decision making. Multiple attribute decision making: methods and applications a state-of-the-art survey, 58-191.</a></td>
    </tr>
    <tr>
      <td>VIKOR</td>
      <td>VIšekriterijumsko KOmpromisno Rangiranje</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0377221703000201?casa_token=hwPAThRDUE0AAAAA:lE0gSB5M7OeYuiouhEi9IMNMnojlOPpuYduNzuqiJZ-WJ8K17ZfU-8gbE9kiZXTlWjj0FJweag>Opricovic, S., & Tzeng, G. H. (2004). Compromise solution by MCDM methods: A comparative analysis of VIKOR and TOPSIS. European journal of operational research, 156(2), 445-455.
</a></td>
    </tr>
    <tr>
      <td>WASPAS</td>
      <td>Weighted Aggregated Sum Product Assessment</td>
      <td><a href=https://www.eejournal.ktu.lt/index.php/elt/article/view/1810>Zavadskas, E. K., Turskis, Z., Antucheviciene, J., & Zakarevicius, A. (2012). Optimization of weighted aggregated sum product assessment. Elektronika ir elektrotechnika, 122(6), 3-6.</a></td>
    </tr>
    <tr>
      <td>WPM</td>
      <td>Weighted Product Method</td>
      <td><a href=https://link.springer.com/chapter/10.1007/978-1-4471-2346-0_4>San Cristóbal Mateo, J. R., & Mateo, J. R. S. C. (2012). Weighted sum method and weighted product method. Multi criteria analysis in the renewable energy industry, 19-22.</a></td>
    </tr>
    <tr>
      <td>WSM</td>
      <td>Weighted Sum Method</td>
      <td><a href=https://link.springer.com/chapter/10.1007/978-1-4471-2346-0_4>San Cristóbal Mateo, J. R., & Mateo, J. R. S. C. (2012). Weighted sum method and weighted product method. Multi criteria analysis in the renewable energy industry, 19-22.</a></td>
    </tr>
    <tr>
      <td>WINGS</td>
      <td>Weighted Influence Nonlinear Gauge System</td>
      <td><a href=https://www.sciencedirect.com/science/article/pii/S0377221713001252?casa_token=JZHx7twlgNYAAAAA:J-A5NUMoejK2EwKjjNIhl0oLISU13hi3aC848ZufJJKSeUzrROdb3uoeMlJ7Y5ZLwi1i1g05yQ>Michnik, J. (2013). Weighted Influence Non-linear Gauge System (WINGS)–An analysis method for the systems of interrelated components. European Journal of Operational Research, 228(3), 536-544.</a></td>
    </tr>
    <tr>
      <td>WISP</td>
      <td>Weighted Influence Score Preference</td>
      <td><a href=https://ieeexplore.ieee.org/abstract/document/9432404/?casa_token=Tnz3Dke0oFAAAAAA:ZJp4nuWm8nsyLei8xphQlNnBYIFtC1mev5kWS4e4qtQTYvXa8me9Z6Qdd434uZmCDZXK6Vip>Stanujkic, D., Popovic, G., Karabasevic, D., Meidute-Kavaliauskiene, I., & Ulutaş, A. (2021). An integrated simple weighted sum product method—WISP. IEEE Transactions on Engineering Management, 70(5), 1933-1944.</a></td>
    </tr>
  </tbody>
</table>


<h3>Online Application</h3> 
Web-based application to apply methods can be accessed through <a href="https://najafiannice.shinyapps.io/AHP_app/">this link</a>.

<h3>Contact Information</h3>
For any technical questions, please contact <a href=mailto:annicenajafi27@gmail.com>Annice Najafi</a>. I will try to answer your email as soon as I can - within a day. Please also open an issue on the repo. 

<h3>Modification, Additional Methods</h3>
This is an open-source project and users are encouraged to make any changes that they believe would improve the R package by forking and submitting pull requests. 
<br>
<br>
<br>
Open-source software developed in California, 🇺🇸
