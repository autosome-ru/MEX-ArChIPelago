# MEX-ArChIPelago <img src='./Archipelago.png' width='55'>
This repository allows reproducing the MEX-ArChIPelago analysis. It contains scripts for sequences generation, model training/testing and plot generation for — ArChIPelago [ArChIPelago rep link] — the arrangement of multiple position weight matrices with ChIP-Seq and machine learning for prediction of transcription factor binding sites.
</br>
 
## Before you start

Make sure that you have installed:
<ul>
<li>Python 3.7 (or upper) https://www.python.org/
<li>Optionally: R 4.2.1 (or upper) and RStudio https://posit.co/download/rstudio-desktop/
</ul>
</br>

## Getting started

Please ```clone``` this directory</br></br>
```git clone https://github.com/autosome-ru/MEX-ArChIPelago/```</br></br>
Then ```cd``` in MEX-ArChIPelago </br></br>
```cd MEX-ArChIPelago```</br></br>

### Input data organization
Please download and unpack GHT-SELEX and ChIP-Seq peaks from ZENODO [doi:10.5281/zenodo.10515307] into ```Input_data``` directory and move it into ```MEX-ArChIPelago```

### Steps to reproduce the MEX-ARCHIPELAGO analysis
(1) Create the MEX-ArChIPelago environment by running ```conda env create -f environment.yml``` and activate it with ```conda activate py3_rpy2_env```</br></br>
(2) Refer to <...BIBIS...> repo for BIBIS installation, which will be used to properly generate the train-test data. Upon installation, please run ... <... XXX.sh ...></br></br>
(3) Generate sequences from the train-test data splits:
- 1
- 2
- 3
</br>

(4) Train PWM-based models and classify sequences containing TFBSs:
- 1
- 2
- 3
</br></br>

# Inner clockworks & Citing
</br></br>

# License
ArChIPelago is distributed under WTFPL. If you prefer more standard licenses, feel free to treat WTFPL as CC-BY.

--2024--
