# CHW - Atlanta On-Demand community health worker Platform

![CHW Dashboard](https://via.placeholder.com/800x400/00bfff/ffffff?text=NurseNow+Dashboard)  
*A smart, data-driven platform connecting families with qualified community health workers in Atlanta.*

---

## 🚀 Project Overview

**CHW** is an MVP platform that solves the challenge of loneliness in neigborhood and provide on-demand nursing care for elderly family members, post-op recovery, wound care, medication administration, and more.

It combines **real-time demand mapping**, **intelligent community health worker-family matching**, and a seamless booking experience within neigborhoods.

---

## ✨ Key Features

### For Families
- Browse nearby community health workers with ratings, specialties, and intro videos
- Real-time availability and GPS tracking
- Instant or scheduled booking
- audio assistant service
- dial in
- Secure payment and post-service reviews

### For Nurses
- Professional profile with license verification according to state regulation and HiPPa
- Set your own rates and availability
- Receive nearby job notifications
- Easy acceptance and payment system

### Admin & Analytics
- Live demand heatmap across Atlanta ZIP codes
- Data-powered insights using **U.S. Census + CDC PLACES**
- CHW verification dashboard

---

## 🛠️ Tech Stack

- **Frontend**: Shiny (R)
- **Mapping**: Leaflet
- **Styling**: bslib (Dark Theme)
- **Data**: U.S. Census ACS + CDC PLACES
- **Deployment**: Posit Connect Cloud / shinyapps.io

---

## 📊 Data-Driven Demand Scoring

The platform uses a weighted demand score:
```r
Demand Score = (Elderly % × 0.30) + (Loneliness % × 0.25) + 
               (Disability % × 0.20) + (Poor Health Days × 0.15) + 
               (Uninsured % × 0.10)