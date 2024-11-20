
# Allometric brain reduction in an insular, dwarfed population of black-tailed deer
---

**"Deer Skull Data Dryad.csv"**: black-tailed deer (*Odocoileus hemionus*) cranial (skull) caliper measurements and calculations used to estimate and explore relationships between: body mass, brain mass, orbital area, and foramen magnum area, with regard to location (island vs. mainland).

**"Occipital Condyle vs Body Mass BTD WTD.csv"**: black- and white-tailed deer (*O. hemionus* and *O. virginianus*, respectively) cranial (skull) measurements used to find the relationship between occipital condyle width to body mass (mainland only).

## Description of the Data and file structure
###
**"Deer Skull Data Dryad.csv"**

*Structure*: rows for individual deer specimens measured, columns for individual details and cranial measurements.

*Usage*: this dataset can be used to relate various parts of the black-tailed deer skull to each other. This dataset was used to create Figures 3-5.

*Column definitions*:
    ID: specimen identification number
    SEX: specimen sex
    Location: specimen's location of origin; either Blakely Island ("Island") or mainland Pacific                 Northwest ("Mainland")
    CC_mL: cranial capacity, in milliliters
    BrV_mL: brain volume, in milliliters (estimated from CC_mL; see manuscript)
    BrM_g: brain mass, in grams (estimated from BrV_mL; see manuscript)
    OC_mm: occipital condyle width, in millimeters
    BM_kg: body mass, in kilograms (estimated from OC_mm; see manuscript)
    FMH_mm: foramen magnum height, in millimeters
    FMW_mm: foramen magnum width, in millimeters
    ROW_mm: right orbit width, in millimeters
    ROH_mm: right orbit height, in millimeters
    LOW_mm: left orbit width, in millimeters
    LOH_mm: left orbit height, in millimeters
    
*Sourcing*: no live animals were included in this dataset or study. All island skulls were collected opportunistically, and mainland skulls were on loan from the Burke Museum, University of Washington, WA, USA.

*Notes*: cells filled with "null" mean that the data in question was unable to be collected on that skull specimen, eg. the skull was only partially intact or too damaged for that specific measurement.

###
**"Occipital Condyle vs Body Mass BTD WTD.csv"**

*Structure*: rows for individual deer specimens measured, columns for individual details and cranial measurements

*Usage*: this dataset can be used to relate occipital condyle width and body mass in black- and white-tailed deer (mainland). This dataset was used to create Figure 2, which can be used to estimate body mass from occipital condyle width in *Odocoileus*.

*Column definitions*:
    Source: museum from which the specimen was loaned (UWBM = Burke Museum, University of Washington, WA,         USA; UBC = Beaty Museum of Biodiversity, Vancouver, British Columbia)
    ID: museum specimen identification number
    Species: specimen species (black-tailed deer = *O. hemionus*; white-tailed deer = *O. virginianus*)
    Sex: specimen sex
    OC_mm: occipital condyle width, in millimeters
    BM_kg: known body mass, in kilograms (sourced from museum databases)

*Sourcing*: no live animals were included in this dataset or study. All specimens were on museum loan from the Burke Museum, University of Washington, WA, USA, or the Beaty Biodiversity Museum, Vancouver, British Columbia.

###
**Relationship between data files**: both datasets were used to relate skull measurements to body mass. The main result from the dataset "Deer Skull Data Dryad.csv" was that brain mass was disproportionately small in island deer compared to mainland deer. The main result from the dataset "Occipital Condyle vs Body Mass BTD WTD.csv" was Figure 1, which directly links occipital condyle width to body mass.

**Missing data**: the data were carefully collected to ensure the highest precision and accuracy possible. Any missing data is the result of being unable to collect the specified data due to specimens being incomplete/partial/damaged.

## Sharing/access Information

The datasets described above are available only in Dryad, but if you are unable to access the datasets for any reason, feel free to contact myself, Claire Geiman (corresponding author), at geimanclaire@gmail.com.