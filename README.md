# Optimized Schedule of Satellite Interaction with Ground Stations within Specified Time Windows

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
# Set of all Satellites
S = ["s1", "s2"] 

# Set of all Ground Stations
G = ["g1", "g2"]

# Set of all Time Windows in 'minutes' assuming evenly spaced from TW1 to TW8
TW = {
    ("s1", "g1", "s1g1") : (1, 4),
    ("s1", "g2", "s1g2") : (3, 5),
    ("s2", "g1", "s2g1") : (2, 7),
    ("s2", "g2", "s2g2") : (6, 8)
}

# Set of all downloading times in 'minutes' for each satellite (Assumed values)
Z = {
    "s1" : 1.8,
    "s2" : 2.7
}

# Objective function weights
W1 = 0.5

# Call the optimization function
model = download_interval_schedule(S, G, TW, Z, W1)

# The model object created above can now be used to analyze the results
```

## References

- Cho, D.H., Kim, J.H., Choi, H.L. and Ahn, J., 2018. Optimization-based scheduling method for agile earth-observing satellite constellation. Journal of Aerospace Information Systems, 15(11), pp.611-626.
  - [Link to Paper](https://arc.aiaa.org/doi/pdf/10.2514/1.I010620)

## Contributing
We welcome contributions and suggestions to improve the algorithm and its implementation. Please feel free to fork the repository, make changes, and submit pull requests.
