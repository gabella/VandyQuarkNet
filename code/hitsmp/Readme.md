# hitsmp - convert DAQ file to CSV, speed-of-muon experiment

Use the codes like<br>
**hitsmp 1 EQUIP_22JUN2021_110904.txt**<br>
where EQUIP* file is the DAQ file with the collected cosmic ray muon hits, for the speed-of-"light" experiment the CRMDs are configured as a telescope.

This is Fortran 77 code written by Med Webster and edited by Bill Gabella.<br>
To compile this code, if you are using gfortran and Linux the Makefile should work<br>
**make**<br>

which exectues the command<br>
**gfortran -o hitsmp hitsmp.f hitsaux.f fakehbk.f**<br>
creating the hitsmp executable.<br>


