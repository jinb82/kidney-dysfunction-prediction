# Early Post-Donation Kidney Dysfunction Prediction

This repository contains the code and data for the study "Risk prediction for early post-donation kidney dysfunction in live kidney donors using a common data model". The project aims to construct a prediction model for early post-donation kidney function impairment in live kidney donors.

## Table of Contents
- [Introduction](#introduction)
- [Data](#data)
- [Model](#model)
- [Results](#results)

## Introduction
Determination of the prognosis of living kidney donors is important. Impairment of early post-donation kidney function is associated with a higher risk of long-term kidney failure. This project constructs a prediction model for early post-donation kidney function impairment in live kidney donors using data from three tertiary hospitals in Korea.

## Data
The data used in this study includes information from living kidney donors at Seoul National University Hospital, Seoul Asan Medical Center, and Seoul National University Bundang Hospital. The dataset includes variables such as age, sex, body mass index, baseline eGFR, and occurrence of postoperative AKI.

## Model
The prediction model is built using a logistic regression approach. The final model includes age, sex, body mass index, pre-donation uric acid concentration, eGFR, and the occurrence of AKI. The model's performance is assessed using the area under the receiver-operating-characteristic curve (AUC) and Hosmer-Lemeshow test.

## Results
The model shows acceptable discriminative power and calibration results in both development and validation cohorts. The developed risk calculator is available online at [https://snuhnephrology.github.io/](https://snuhnephrology.github.io/).

## Example Scenarios and Predicted Risks
The online calculator provides predictions based on various scenarios, such as age, sex, body mass index, uric acid levels, eGFR, and AKI occurrence. Here are some examples:

| Scenario | Age | Sex  | BMI | Uric Acid (mg/dL) | eGFR (mL/min/1.73 m²) | AKI | Risk eGFR < 60 (6 months) | Risk eGFR < 50 (6 months) |
|----------|-----|------|-----|-------------------|-----------------------|-----|---------------------------|---------------------------|
| 1        | 40  | Male | 25  | 5                 | 100                   | Yes | 16.7%                     | 0.5%                      |
| 2        | 25  | Female | 23  | 5                 | 100                   | No  | 1.1%                      | 0.4%                      |

This table and more detailed analysis can be found in the paper included in this repository. 
