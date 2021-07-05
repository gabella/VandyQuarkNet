      parameter (ncntrs=45)   !number of data types 
      common/mp/ncount(ncntrs),nlines,nchar,nbufh(20),
     & nstatus(4),pres(4),degcpu(4),deggps(4),volts(4),
     & bufin,rawfile,timedate,reset
      logical reset
      character*80 bufin,rawfile
      character*24 timedate
      equivalence (ntrig,ncount(1))

