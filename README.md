-# Comparative Analysis of Bus and Ridepooling Services in Wuppertal
+---
+title: “Comparative Analysis of Bus and Ridepooling Services in Wuppertal”
+---

*The purpose of this repository is to partially archive the program code related to the project ''Dynamical Route Planning for On-Demand Ride-Hailing Services'' (subproject of Bergisch Smart Mobility, 2019-2022).*

**Project Team:** Prof. Dr. Hanno Gottschalk, Prof. Dr. Kathrin Klamroth, Dr. Hayk Asatryan, Dr. Daniela Gaul, Dr. Michael Stiglmayr

**Institution:** University of Wuppertal, School of Mathematics and Natural Sciences

**Project partner:** [Aptiv](https://www.aptiv.com), [BSW](https://www.bergische-gesellschaft.de), [WSW](https://www.wsw-online.de)

**Research set-up**

The project aims to compare the service quality of time-tabled buses and on-demand ridepooling cabs in Wuppertal, Germany, during late evening hours. To facilitate this comparison, a predictive simulation framework was developed using R. This framework leverages generalized linear models for demand prediction and classification models for trip destination prediction. The resulting dynamic dial-a-ride problem is solved using a rolling-horizon algorithm with a feasible-path heuristic.

**Key Features**

* **Predictive Simulation:** Generates realistic transport requests based on historical data.
* **Demand Modeling:** Employs generalized linear models for accurate demand forecasting.
* **Destination Prediction:** Utilizes classification models to predict trip destinations.
* **Dynamic Dial-a-Ride Problem:** Solves the problem using a rolling-horizon algorithm.
* **Feasible-Path Heuristic:** Improves algorithm performance in high-demand scenarios.

**Visualization Tool**

A leaflet-based HTML map is used to visualize the simulation results. The map displays stations as markers with radii proportional to boarding numbers. Hovering over a marker provides station details, while clicking shows boarding information and destinations:

<img src="./readme_images/visualization_tool.jpg" alt="Specializations" width="500"/>

**Service Extension Analysis**

The project also includes an analysis of demand patterns and proposes potential extensions for the ride-hailing service:

<img src="./readme_images/extension_map.jpg" alt="Specializations" width="500"/>

**Publications**

- Asatryan H., Gaul D., Gottschalk H., Klamroth K., Stiglmayr M. *Ridepooling and public bus services: A comparative case-study.* – Preprint (2023), [arXiv:2302.01709](https://arxiv.org/abs/2302.01709)
