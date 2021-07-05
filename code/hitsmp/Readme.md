# hitsmp - convert DAQ file to CSV, speed-of-muon experiment

Use hitsmp<br>
**hitsmp 1 EQUIP_22JUN2021_110904.txt**<br>
where EQUIP_22JUN2021_110904.txt file is the DAQ file with the collected cosmic ray muon hits, for the speed-of-"light" experiment the CRMDs are configured as a telescope.  This generates a **muspeed.csv** file that can be read into a spreadsheet.  It looks like,<br>
fedora/gabe :/tmp/billeg/hitsmp 132 >head muspeed_EQUIP_22JUN2021.csv<br>
```
  210622, 58159.609662175,   26,   -1,   24,   -1,   34,   60,   33,   -1
  210622, 58161.104561403,    6,   23,    1,   20,    6,   27,   12,   26
  210622, 58166.029890001,    1,   21,    0,   23,    9,   25,    7,   29
  210622, 58192.870475292,    8,   33,    9,   25,   16,   35,   17,   34
  210622, 58206.943891764,   21,   36,   14,   38,   23,   37,   27,   43
  210622, 58212.424382508,   23,   58,   18,   -1,   29,   57,   29,   61
...
```
This the time stamp, data and time, and then the rising and falling edges in *ticks* (for version 6000 and newer cards that is 1.25 ns per tick) for each scintillator paddle, channel 1 rising and falling edges, channel 2, etc, to channel 4.  The pulses are triangular but with a fast rising edge and slower falling edge.  Good to ask the students why they think that might be.  We use the rising edge only in the calculation of times for the pulse.<br>
We perform the experiment by setting up a telescope with 1 and 2 on top of each other, on the top of the telescope, 3 and 4 at the bottom.  Then separate by at least 2 meters/6.6 feet (have also done 6 meters/20 feet in a stairwell) take one data set, usually one hour is enough.  Then swap the 1 and 3 paddles take another set with about the same number of coincidence hits.  From those two files you can eliminate the **systematic effect** of the delays in each electronic channel being different. See the document [**Measurement_of_c_v2.pdf**](https://github.com/gabella/VandyQuarkNet/blob/master/quarknet2021/docs/Measurement_of_c_v2.pdf) in the [quarknet2021/docs/](https://github.com/gabella/VandyQuarkNet/tree/master/quarknet2021/docs) directory.



## Compiling hitsmp.f

This is Fortran 77 code written by Med Webster and edited by Bill Gabella.<br>
To compile this code, if you are using gfortran and Linux the Makefile should work<br>
**make**<br>

which exectues the command<br>
**gfortran -o hitsmp hitsmp.f hitsaux.f fakehbk.f**<br>
creating the hitsmp executable.<br>


