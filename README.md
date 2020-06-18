# glacier-tools
Scripts and models used to identify image features, which were used for Foga et al., 2014 publication.

The citation for this work is: 

> Foga, S., Stearns, L. A., & Van der Veen, C. J. (2014). Application of satellite remote sensing techniques to quantify terminus and ice mélange behavior at Helheim Glacier, East Greenland. Marine Technology Society Journal, 48(5), 81-91.

This work can also be found in my Master's Thesis:

> Foga, S. (2016). Characterization of Ice Mélange and its Implications to Terminus Stability at Helheim Glacier, Southeast Greenland (Master's Thesis, University of Kansas). https://kuscholarworks.ku.edu/handle/1808/21829. 

These tools are provided as-is, and are not maintained.

## terminus_picker_v3_15nov2017.R
Purpose: digitize glacier terminus at the pixel level. The script makes a best guess of terminus location, based upon maximum neighboring pixel values. The user can then override the automated guesses. Produces a raster output.  

## Landsat8_membership_fxn_2.dcp
Purpose: eCognition model to detect ice mélange in Landsat 8 imagery.