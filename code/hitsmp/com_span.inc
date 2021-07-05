c  storage for data to be recapped at end of run to compute time and rates
      parameter (nbegin=3)! beg set and sum of beginning and ending sets
      parameter (nend=6)  ! for multiplicity 
      parameter (nbegins=3)! beg set and sum of beginning and ending sets
      parameter (nends=6) ! for scalers

      common/span/locsp,nentsp(nend),ntsp(nend),nrdscsp(nend),
     & mult3sp(nend),mult4sp(nend),ippd(2,nend),ipph(2,nend),
     & spsec(2,nend),locsc,nentsc(nends),iscs(5,nends)
c in ippd,ipph, and spsec, the first arg is one for events and 2 for scaers
c  The first nbegin values are filled at the start of the run and the remaining
c  are a rotating (nbegin+1 to nend) buffer filled on every scaler read.
c  locsp points to the last fill of the buffer.
c  ntsp     trigger (coincidence) counter
c  nsdlsp   hardware coincidence counter, scaler 4
c  nrdscsp   number of scaler reads
c  ippdsp, ipphsp, spsec  day, year; hr,min; and seconds

