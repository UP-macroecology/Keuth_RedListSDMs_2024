Preliminary results of testing different parameter settings for the virtual species approach

Problem: 
- Two of species did not reach a stable population size after the spin-up period with the currently selected parameters
- warm-adapted species did not show a proper range-shifting but had a strong dispersal limitation
- not clear if the observed pattern is a landscape effect

Ideas:
- double exponential kernel to model rare long-distance dispersal events
- play around with niche breadths to increase connectivity in landscape
- test it for three different landscapes

Parameter setting for the double exponential kernel:
- dispersal distance for the regular kernel is kept at 15000 (based on estimated distance from rodents/ birds and documentation from Fandos 2023, Journal of Animal Ecology)
- distance for rare long-distance dispersal is based on the reported dispersal distance in Fandos 2023, Journal of Animal Ecology and Paradis 1998, Journal of Animal Ecology
- dispersal probability by default is set to 0.9, Heikkinnen 2014 (PLOS ONE) & 2015 (Biological Conservation) chose either 0.9 or ranging probabilities from 0.8-0.95

Results:
When testing different niche breadths with and without the double-exponential kernel, species survive climate change a bit longer when using the double-exponential kernel and become extinct before the loss of all habitat with smaller niche breadths (0.025). (plots_nichebreadths & plots_nichebreadths_wo_longdisp)
-> double exponential kernel has a positive effect, niche breadths need to be larger
I selected the niche breadths 0.045 + 0.055 as in 0.035 long distance dispersal showed a clear dispersal limitation and also a slight one in 0.04 (especially in one of the landscapes) while short dispersal distance always showed the aspired dispersal limitation (Comparison_nichebreadths_all_land1,2,3)

For the dispersal probability I selected 0.95, since the dispersal probability did not have a strong effect on the survival of the species (only on the abundance) and with a lower dispersal probability I observed stronger scattering in the landscape. 0.95 had the smallest scattering and showed similar patterns in regards to the distribution as the other probabilities (Comparison_nichebreadths_all, occurrences in landscape, Comparison_nichebreadths_abundances)
