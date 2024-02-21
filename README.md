# Optimized Schedule of satellite interaction with ground stations within specified time windows

This repository contains the first stage implementation of a two-step binary linear programming method designed to optimize task scheduling within a constellation of low-Earth-orbit satellites. Leveraging a standard mixed-integer linear programming solver, this method allocates tasks among satellites to maximize the overall mission performance metric, considering optimal task starting times and satellite-ground station communication intervals


## Getting Started
### Prerequisites
Ensure you have Python installed on your system. This code has been tested with Python 3.8. Additionally, you will need to install the following libraries:
- `numpy`
- `pandas`
- `PuLP`

You can install these packages using pip:

```bash
pip install numpy pandas pulp
```


## Usage
### 1. Import the Libraries
Before running the optimization, make sure to import the necessary libraries:

```bash
import numpy as np
import pandas as pd
from collections import defaultdict
from typing import List, Dict, Tuple
from pulp import *
```

### 2. Optimization Function
The core functionality is encapsulated in the download_interval_schedule function. This function optimizes the schedule of satellite interaction with ground stations within specified time windows.

Parameters:

* S (list): List of satellite indices.
* G (list): List of ground station indices.
* TW (dict): Dictionary of all satellite-ground station time windows.
* Z (dict): Dictionary of all download times for each satellite.
* W1 (float): Weight of objective function coefficients.

Returns:
An optimized LpProblem instance representing the scheduling model.


### 3. Example call
```bash
# Define your satellites, ground stations, time windows, and other parameters
S = ['S1', 'S2']
G = ['G1', 'G2']
TW = {('S1', 'G1', 'S1G1'): (0, 10), ...}
Z = {'S1': 5.0, 'S2': 6.0}
W1 = 0.5

# Call the optimization function
model = download_interval_schedule(S, G, TW, Z, W1)

# The model object created above can now be used to analyze the results
```
