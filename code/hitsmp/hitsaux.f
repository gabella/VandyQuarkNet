      subroutine gpsread
      include 'com_mp.inc'
cccc      character*80 bufin
      data ngood,re,pi,alat,along/0,6.378e6,3.144423,36.15,86.8021111/
c  Longitude is East-West and Nashville is about 86:48.1267
c  Latitude is from equator and Nashville is 36:08.6654
c Above are for Stevenson, Hertle response to Maguire
c Jul 9, 2007 run says 0.0054 more lat, 0.033 more longitude and 198.9 alt
c  that is 49.5 m west of site quoted above
      data ngood,ralt,rseclat,rseclon/0,198.9,8.6708,48.1597/
      ncount(9)=ncount(9)+1
      read(2,1000)bufin  ! DG with single ctl M
 1000 format(a80)
 3000 format(i3,' |',80a1)
      read(2,1000)bufin  !1 of 10 responses from GPS for   date
      nch=lnblnk(bufin)-1
      write(6,3000)nch,(bufin(j:j),j=1,nch)
      read(2,1000)bufin  !2 of 10
      if(bufin(13:13).ne.'A')then
        do j=3,11  ! 11 for GPS on but status V and 8 for GPS off
          read(2,1000)bufin    ! so check on contents
          nch=lnblnk(bufin)-1
          if(bufin(1:nch).eq.' ChkSumErr: 0')then
            nlines=nlines+j+1
            return
          end if
        end do  !should never get here, so complain if we do
        write(6,*)' Trouble with GPS report at line ',nlines
        return
      end if
      if(ngood.eq.0)then
         dlat=2.0*pi*re/(60.*3.6e5)
         dlong=dlat*cos(pi*alat/180.)
c        write(16,2000)dlat,dlong,rseclon,rseclat,ralt
 2000    format('0.001 minute of latitude, longitude is',2f10.3,' m'/
     & ' Stevenson 6901 window is 86:',f7.4,' W Long 36:',f7.4,
     & ' N Lat, and',f7.1,' m altitude')
      end if
      ngood=ngood+1
      read(2,1000)bufin  !3 of 10
      read(2,1000)bufin  !4 of 10
      read(bufin(13:15),1004)latdeg
      read(bufin(17:23),1001)seclat
 1004 format(i3)
 1001 format(f8.4)
c  I have named these floating point numbers seclat and seclon, but they 
c   are minutes, not seconds.
      read(2,1000)bufin  !5 of 10
      read(bufin(13:15),1004)londeg
      read(bufin(17:23),1001)seclon
      read(2,1000)bufin  !6 of 10
      if(bufin(14:14).eq.'.')then ! Jul5, 2011 got  Altitude:  0.0.42m
cc        write(16,1000)bufin  ! and this avoids blow up on that first period
        nch=lnblnk(bufin)-1
        write(6,3000)nch,(bufin(j:j),j=1,nch)
        write(6,*)' Just got illegal altitude - set to 1.1'
        alt=1.1
      else
        read(bufin(12:17),1002)alt
      end if
 1002 format(f6.1)
      read(2,1000)bufin  !7 of 10
      lsat=12
c      write(6,*)'Length of nsat line',lnblnk(bufin)
c    i2 format works for old, no leading 0 for new and cntl M then blows up
      if(lnblnk(bufin).eq. 15)then ! 15 char for 2 dig nsat, 14 for 1 dig lsat
        read(bufin(13:14),1003)lsat
      else
        read(bufin(13:13),1007)lsat
      end if
 1007 format(i1)
 1003 format(i2)
      read(2,1000)bufin  !8 of 10
      read(2,1000)bufin  !9 of 10
      read(2,1000)bufin  !10 of 10
      call hf1(200,seclat-rseclat,1.) 
      call hf1(201,seclon-rseclon,1.) 
      call hf1(202,alt-ralt,1.) 
      write(6,1100)lsat,latdeg,seclat,londeg,seclon,alt,
     & seclat-rseclat,seclon-rseclon,alt-ralt 
 1100 format('No. of Sat, Latitude, Longitude, Altitude:',2i4,':',
     &  f8.4,i4,':',f8.4,f6.1/'      Observed - Reference'
     &  ,25x,f8.4,5x,f8.4,f6.1)
      read(2,1000)bufin
      nch=lnblnk(bufin)-1
      if(bufin(1:nch).ne.' ChkSumErr: 0')
     & write(6,3000)nch,(bufin(j:j),j=1,nch)
      nlines=nlines+12 
      return
      end
      subroutine status
      include 'com_mp.inc'
      read(bufin,8015,err=10)ii,ij,ik,iv  !  skip on error
 8015 format(2x,4i5)
      xxx=ii-pres(3)
      pres(1)=pres(1)+xxx
      pres(2)=pres(2)+xxx**2
      xxx=ij-degcpu(3)  ! 2.93*ij+552.  !    ij*0.1 - 25.0
      degcpu(1)=degcpu(1)+xxx
      degcpu(2)=degcpu(2)+xxx**2
      xxx=ik-deggps(3) ! *0.172 -25.0
      deggps(1)=deggps(1)+xxx
      deggps(2)=deggps(2)+xxx**2
      xxx=iv-volts(3) ! *0.00122-3.3
      volts(1)=volts(1)+xxx
      volts(2)=volts(2)+xxx**2
c      write(6,*)nstatus(1),pres(1),degcpu(1),deggps(1),volts(1)
 10   continue
      return
      end
      subroutine statusend
      include 'com_mp.inc'
      ns=ncount(8)
      if(ns.ge.1)then
        write(6,*)'   __Summary of Status Lines__'
c        write(6,*)'Air Pressure ',ns,pres(1),pres(2)
c        write(6,*)'Cpu Temp ',degcpu(1),degcpu(2)
c        write(6,*)'GPS Temp ',deggps(1),deggps(2)
c        write(6,*)'Voltage ',volts(1),volts(2)
        ba=pres(1)/ns
        badev=(pres(2)-ba**2)/ns
c        ba=ba+1.
        if(badev.gt.0.)badev=sqrt(badev)
        tcpu=degcpu(1)/ns
        tcpud=(degcpu(2)-tcpu**2)/ns
        if(tcpud.gt.0.)tcpud=sqrt(tcpud)
c        tcpu=tcpu+25.
        tgps=deggps(1)/ns
        tgpsd=(deggps(2)-tgps**2)/ns
        if(tgpsd.gt.0.)tgpsd=sqrt(tgpsd)
c        tgps=tgps+25.
        v=volts(1)/ns
        vdev=(volts(2)-v**2)/ns
        if(vdev.gt.0.)vdev=sqrt(vdev)
c        v=v+3.3
c        ba=0.
        write(6,*)'Raw values'
        write(6,9001)ns,ba,badev,tcpu,tcpud,tgps,tgpsd,v,vdev
        ba=ba+pres(3)
        tcpu=tcpu+degcpu(3)
        tgps=tgps+deggps(3)
        v=v+volts(3)
        write(6,*)'Ater nominal Calibration applied'
        write(6,9001)ns,ba,badev,tcpu,tcpud,tgps,tgpsd,v,vdev
      end if
 9001 format(i5,' status lines give mean, stdev for:',
     &'  Air Pres ',2f8.1/'Card CPU temp',2f7.1,
     &'  GPS temp',2f5.1,' nom 3.3 Volts',2f7.0)
      return
      end
      subroutine report
      include 'com_mp.inc'
      include 'com_multilines.inc'
      include 'com_analysis.inc'
      include 'com_results.inc'
      dimension ptwo(6),eptwo(6)
c summarize number of satellites detected
      gs=0.
      bs=0.
      avgs=0.
      avbs=0.
      do j=1,14
        gs=gs+nsata(j)
        bs=bs+nsatb(j)
        avgs=avgs+(j-1)*nsata(j)
        avbs=avbs+(j-1)*nsatb(j)
      end do
      if(gs.gt.0.)avgs=avgs/(gs)
      if(bs.gt.0.)avbs=avbs/(bs)
 1002 format('tot, good, bad satellites',3f10.1/
     &12x,'av good and bad sats per coinc',2f8.3/' hist, good sats'/
     &'        -0-       -1-       -2-       -3-       -4-   ',
     &'    -5-       -6-'/7i10/'        -7-       -8-       -9-',
     &'      -10-      -11-      -12-      -13-'/7i10/' hist, bad sats'/
     &'        -0-       -1-       -2-       -3-       -4-   ',
     &'    -5-       -6-'/7i10/'        -7-       -8-       -9-',
     &'      -10-      -11-      -12-      -13-'/7i10)
 1004 format('Tot, good, bad satellites',3f10.1/
     & 12x,'av good and bad sats per coinc',2f8.3)
c  seldom need full histogram, so use short form
      write(6,1004)gs+bs,gs,bs,avgs,avbs
c      write(6,1002)gs+bs,gs,bs,avgs,avbs,(nsata(j),j=1,7),
c     & (nsata(j),j=8,14),(nsatb(j),j=1,7),(nsatb(j),j=8,14)
c  study digitezer status response field.  Originally digitizer complete flag
c  always blank now as far as I can tell, so comment out (interpretation is
c  given in data_format.pdf in webstems runs directory
      write(6,1003)((ndigst(k,j),j=1,31),k=1,3)
 1003 format(' Digitizer status (probably obsolete),first,other,',
     &'last line',3(/15i2,i8,7i2,i6,7i2))
      if(nfreq.gt.1)then
        write (6,*)'Sum freq dev is',avfreq
        abvfreq=abvfreq/nfreq
        avfreq=avfreq/nfreq
        rmsfreq=(rmsfreq/nfreq)-avfreq**2
        if(rmsfreq.gt.0.)rmsfreq=sqrt(rmsfreq)
      end if
      write(6,*)'         Run Summary of oscillator variability'
      write(6,1000)fnormr,nbadf,nfreq,avfreq,rmsfreq,abvfreq
 1000 format('      Final      freq adj freq adj       avg     rms',
     &'   avg abs'/
     &'  oscill freq      fail     good         adj     adj     adj'/
     &f13.3,i10,i9,3x,f10.5,2f8.3)
c
      write(6,1001)multhit
 1001 format('Occurrences of second (or more) hits in one',
     &' digitization:'/8i9)
c
      trig=ntrig
      write(6,1509)missl,missf,missmat
 1509 format(' No Leading Edge     ',4i10/ ' No Trailing Edge    ',4i10/
     &  ' Edges Reversed Order',4i10)
      write(6,1501)(mult(j),j=1,5),((mult(j)/trig),j=1,5),
     & ((sqrt(float(mult(j)))/trig),j=1,5),(mout(j),j=1,4),
     & ((mout(j)/trig),j=1,4),((sqrt(float(mout(j)))/trig),j=1,4)
 1501 format(6x,'mult',2x,3i12,2i8/
     & 'fraction    ',3(1pe12.5),2(0pf8.5)/
     & 'err on fr   ',3(1pe12.5),2(0pf8.5)/' not in 3-fold',4i9/
     & ' fraction     ',4f9.5/' err of fract ',4f9.5)
c
      subtwo=0.
      do j=1,6
        sumtwo=sumtwo+itwo(j)
      end do
      do j=1,6
        ptwo(j)=itwo(j)/sumtwo
        eptwo(j)=sqrt(ptwo(j)*(1.-ptwo(j))/sumtwo)
      end do
      write(6,1507)sumtwo,(itwo(j),j=1,6)
      write(6,1508)ptwo,eptwo
 1507 format('Numbers, fraction, err for',f10.0,' two-fold triggers'
     &/'      0011      0101      1001      0110      1010      1100'/
     &6i10)
 1508  format(6f10.4/6f10.4)
      return
      end
      subroutine gpstime
      include 'com_multilines.inc'
      include 'com_analysis.inc'
      include 'com_mp.inc'
      integer*8 icnow,icsec,icnowp,icsecp,incrun,incsep
      real*8  x,y,z
      logical gpsback,pair
      data gpsback,pair,spair/2*.false.,0./
      data icnowp,icsecp,incrun,incsep/2*-1,0,2000/
      data nentry/0/
      nentry=nentry+1
c  05F8A627 BC 00 3B 00 3F 00 3E 00 04BE3462 153621.020 230215 A 05 0 +0078
      read(bufs(1),1002,err=15)icnow,icsec,ihr,imin,isec,imsec,iday
     & ,imo,iyr,iqual,isat,idigst,iadj
 1002 format(z8,25x,z8,1x,3i2,1x,i3,1x,3i2,1x,a1,1x,i2,1x,z1,1x,i5)
      go to 16
 15   write(6,*)'Read Err on first data line of hit:' ! Read err response
      write(6,*)bufs(1)
c        nbufs=0
      return  ! process event with previous event time
 16   if(imo.eq.0 .and. iday.eq.0)then ! GPS off on this hit
        isat=0    ! for the benefit of the shower file
        iqual='0'
        if(lgps)then ! but either it was on or this is beginning of run
          lgps=.FALSE.
          write(6,*)'GPS is off at time ',idate,itm,daysec
          ncount(ncntrs-3)=ncount(ncntrs-3)+1
        end if
      else  ! GPS is now on
        if(.not.lgps)then ! and had been off on pevious hit
          lgps=.TRUE.
          write(6,*)'GPS is back on'
          ncount(ncntrs-2)=ncount(ncntrs-2)+1
          gpsback=.true.
        end if
      end if
      if(isat.gt.13)isat=13
      if(iqual.eq.'A')then
         nsata(isat+1)=nsata(isat+1)+1
      else
         nsatb(isat+1)=nsatb(isat+1)+1
      end if
c   clock rollover - Our main clock counts up to 4294967296 = z100000000 at
c   25 MHz and thus roll over every 171.799 sec or 20.9 times per hour.  Event
c   rates are ~ 100/hr in shower config with 2 m spacing and much higher in
c   the other configurations we use.  Single rollovers occur in 1/5 of the
c   events at 100/hr, but double require a time of more than 1/5 hr while the
c   av spacing is 1/100 hr and thus have probability of ~e-20. (1/5 hr is 20
c   times av spacing.)  The average clock rate should not be calculated when
c   the time between events is more than 3 minutes.
c    compute seconds since start of day to use in clock read check
      lsec=isec+60*(imin+60*ihr)
c update running average of counts between the one second pulses to the GPS
      if(icsecp.lt.0)then
        icsecp=icsec   ! First entry initialization
        lsecp=lsec
      end if
      nsec=lsec-lsecp
      if(nsec.lt.0)nsec=nsec+86400  !add seconds in a day on day change
      incrun=fnormr
      incsep=icsec-icsecp   ! should be zero or integral multiple of fnormr
      if(incsep.lt.0)incsep=incsep+irollov
      nstp=(incsep+90000)/incrun !add 90000 so will truncate to nearest integer
      call hf1(207,0.5+float(nstp),1.0)
      df=0
      if(nstp.gt.0 .and. nsec.le.172)then
        x=float(incsep) ! introducing these intermediate real*8 numbers
        y=float(nstp)   ! appears to do a better job of maintining double
        z=x/y           ! precision than the commented out direct approach.
        df=z-fnormr
c        df=(float(incsep)/float(nstp))-fnormr
        call hf1(208,df,1.0)
c
        if(pair)then
          call hf1(203,df-spair,1.0)
          if(spair.ge.0.)then
            call hf1(204,df,1.)
          else
            call hf1(205,df,1.0)
          end if
         pair=.false.
         spair=0.
        else if(abs(df)-1.5 .gt. 0.)then
          spair=df
          pair=.true.
        end if
c
        if(abs(df)-4.7 .lt. 0.)then
c  It appears that if one interval comes in low then the next will be high, so
c  an updated average is usually wrong.  Leave the frequency at the default
c          fnormr=fnormr+0.25*df ! weighted average of new with previous value
          dff=fnormr-fnorm
          call hf1(209,dff,1.0)
          nfreq=nfreq+1
          avfreq=avfreq+dff
          abvfreq=abvfreq+abs(dff)
          rmsfreq=rmsfreq+dff**2
        else
          nbadf=nbadf+1
        end if
 1011 format('Freq Deviation on try, update, #sats',2i10,i3/
     &      i12,i14,i12,i9,f12.2,f12.4,i4)
        if(abs(df)-5.0 .gt.0.0)then
          write(6,1011)nentry,nfreq,isat,icsec,
     &    icsecp,incsep,incrun,fnormr,df,nstp
        end if
      end if
      icsecp=icsec      
      lsecp=lsec
c freq update complete, now do date and time of this hit 
      if(icnow.lt.icsec)then
        frsec=(icnow-icsec+irollov)/fnormr
      else
        frsec=(icnow-icsec)/fnormr
      end if
      kk=0.5+(float(imsec+iadj))/1000.
      isec=isec+kk
      idate=10000*iyr+100*imo+iday
      daysec=isec+60*(imin+60*ihr)
      daysec=daysec+frsec
      itm=10000*ihr+100*imin+isec
c      if(frsec.lt.0. .or. frsec.gt.0.99999)then
c        write(6,200)idate,daysec,frsec,irollov,icnow,icsec,
c     &  icnfrsec,irollov,ow-icsec,icnow-icsec+irollov,bufs(1)
      if(frsec.lt.0. .or. frsec.gt.0.99999)then
        spfrsec=frsec
        call hf1(210,spfrsec,1.0)
        write(6,200)idate,daysec,frsec,irollov,icnow,icsec,
     &  icnow-icsec,icnow-icsec+irollov,bufs(1)
        if(frsec.gt.1.0001)ncount(38)=ncount(38)+1
      end if
 200  format('frsec near 1 at date, time',i7,2f20.9/3z10,2z20/
     & a73)
c a rollover occurs every 2 min 53 sec, so double rollovers
c   will be extremely rare with coincidence rates above a few Hz =
c   a few hundred per hr.  icsec is udated every second, so there is
c   no double rollover problem in the icnow-icsec calculation above 
c      if(icnow.lt.icnowp) write(6,1000)icnow,icnowp,idate,itm,
c     & frsec,daysec
 1000 format('Clk rol',2z10,' at',2i8,f10.6,f20.2)
      icnowp=icnow
      if(gpsback)then
        gpsback=.false.
        write(6,*)'GPS is now on at time ',idate,itm,daysec
      end if
      return
 9    write(6,*)'Badly formated first line:'
      write(6,*)bufs(1)
      stop
      end
      subroutine coinc
      include 'com_multilines.inc'
      include 'com_analysis.inc'
      dimension icdr(8)
      integer*8 inow(20)
c pick up clock registers from first bufferin gpstime
      if(nchars(1).gt.70)then
        call gpstime
        ibaddig=0
        do jj=1,lbuf ! investigate idigst
          read(bufs(jj),3000,err=898)idigst
          go to 899
 898       write(6,*)'Illegal digitizer status '
          idigst=-14
 899       continue
          if (idigst.ne.0)ibaddig=ibaddig+1
 3000     format(65x,z1)
          i=idigst+16
          if(jj.eq.1)then
            ndigst(1,i)=ndigst(1,i)+1
          else if(jj.eq.lbuf)then
            ndigst(3,i)=ndigst(3,i)+1
          else
            ndigst(2,i)=ndigst(2,i)+1
          endif
        end do
        if(ibaddig.gt.0)then
          write(6,*)' Coinc with digitizer incomplete'
          do jj=1,lbuf
            write(6,3001)bufs(jj)
          end do
 3001     format(a73)
        end if
      else
        itm=-1
        idate=-1
      end if
c   Start loop over buffers for this event.
      do j=1,8
        icd(j)=-1
        icds(j)=-1
      end do
c  the small clock rise and fall times are read into icdr and corrected for
c  possible advances of the big clock from one read to the next,still in icdr
      do jj=1,lbuf
        read(bufs(jj),1001,err=98)inow(jj),icdr
 1001 format(z8,8(1x,z2))
        go to 99
 98     write(6,*)' Err reading digitizer data'
        do kk=1,lbuf
          write(6,*)bufs(kk)
        end do
c        stop  ! or clear icdr and continue
        do kk=1,8
          icdr(kk)=0
        end do
 99     continue
c  histogram values of idigst
c  now extract pulse rise and fall times relative to card clock at first read
c   first compute effect of advance of counter since first record of evt
        if(inow(jj).lt.inow(1))then  ! watch for rollover during event
          iinc=32*(inow(jj)-inow(1)+irollov)
          write(6,*)' Clock rollover in evt',icstart,inow(jj)
        else
          iinc=32*(inow(jj)-inow(1))
        end if
 1999 format(i3,1x,a80)
        kk=icdr(1)/128  ! leading or new bit has been checked in main prog
        icdr(1)=icdr(1)-kk*128 !trig bit is left of 8 bits, rest  counter 1
c number bits 0-7.  bit 5 is on if it is a real number. Then data is 0-4 and 6
c  should never be on.
        do j=1,8
           kk=icdr(j)/32  !test on good digitizer bit, on is good
           if(kk.gt.1)then
             write(6,1998)j,bufs(jj)
 1998        format('Bad digitization',i3/a80)
             icdr(j)=-1
           else if(kk.eq.1)then
c  removes high bit (kk) and corrects for increment of clock
             icdr(j)=icdr(j)-32*kk+iinc
czxzxzx next imposes ~10 microsecond limit on pulse timing
             if(icdr(j).gt.9999)then
               write(6,7010)icdr(j),icstart,iinc
               do jm=2,lbuf
                  write(6,3001)bufs(jm)
               end do
               icdr(j)=-1
             end if
 7010        format('Abnormal pulses spread',3z10)
           else
             icdr(j)=-1  
           end if
        end do
c merge hits in this record with hits from previous records in icd or icds
        do j=1,8
          if(icdr(j).ne.-1)then
            if(icd(j).eq.-1)then
              icd(j)=icdr(j)
            else
              multhit(j)=multhit(j)+1
c              write(6,1022)j,icdd(j),icd(j)
c 1022         format(' Double time entry',3i8)
              icds(j)=icdr(j)  ! last, not necessarily second event
            end if
          end if
        end do
      end do   ! end of loop over buffers  zzbuff loop
      return
      end
      subroutine spanout
      include 'com_span.inc'
      dimension intar(nend,5),idif(5),rate(5),erate(5)
      equivalence (nentsp(1),intar(1,1))
      write(6,1001)locsp
 1001 format('last entry is row ',i3/
     & 'buf index entry  trigs scl coinc 3-folds 4-folds',
     & '    date  hr-min   sec')
      do j=1,nend
        write(6,1000)j,nentsp(j),ntsp(j),nrdscsp(j),mult3sp(j),
     & mult4sp(j),ippd(1,j),ipph(1,j),spsec(1,j)
      end do
 1000 format(i6,7i8,f8.4)
      write(6,1005)
 1005  format('indices  emtries   trig rate  coinc rate   3-fold rate',
     &  ' 4-fold rate hours')
      do k=nbegin+1,nend
        do j=1,nbegin
          call runlen(1,locsp,j,hr)
          if(hr.gt.0.1)then
            do jj=1,5
              idif(jj)=intar(locsp,jj)-intar(j,jj)
              a=idif(jj)
              rate(jj)=a/hr
              erate(jj)=sqrt(a)/hr
            end do
 1002 format(2i3,5i8,f15.3)
            write(6,1003)locsp,j,(rate(jj),erate(jj),jj=1,5),hr
 1003 format(2i3,5(f7.1,f5.1),f9.3)  ! 7.3 matches ew program
          end if
        end do
        locsp=locsp-1
        if(locsp.le.nbegin)locsp=nend
      end do
      return
      end
      subroutine spanfill
      include 'com_mp.inc'
      include 'com_results.inc'
      include 'com_span.inc'
      include 'com_analysis.inc'
      data nent/0/
      nent=nent+1
      locsp=locsp+1
      if(locsp.gt.nend)locsp=nbegin+1
      ntsp(locsp)=ncount(1)
cccc      nrdscsp(locsp)=iscal(5)
      mult3sp(locsp)=mult(4)
      mult4sp(locsp)=mult(5)
      ippd(1,locsp)=idate
      ipph(1,locsp)=itm
      spsec(1,locsp)=frsec
      return
      end
      subroutine sclout
      include 'com_span.inc'
      dimension intar(nend,5),dif(5),rate(5),erate(5)
      equivalence (nentsp(1),intar(1,1))
      write(6,1001)locsc
 1001 format('last entry is row ',i3/
     & ' index                       scal 1-5              ',
     & '     date  hr-min   sec')
      do j=1,nends
        write(6,1000)j,((float(iscs(k,j))),k=1,5),ippd(2,j),ipph(2,j),
     &   spsec(2,j)
c     & ,nentsc(j)
      end do
 1000 format(i6,4(1pe10.3),0pf8.0,2i7,f8.3,i10)
      write(6,1005)
 1005  format('indices      Four scaler rates (per sec)    ',
     & '  coinc per hr  hours')
      do k=nbegins+1,nends
        do j=1,nbegins
          call runlen(2,locsc,j,hr)
          if(hr.gt.0.01)then
            seconds=3600.*hr
            do jj=1,4
              dif(jj)=iscs(jj,locsc)-iscs(jj,j)
              rate(jj)=dif(jj)/seconds
            end do
            dif(5)=iscs(5,locsc)-iscs(5,j)
            rate(5)=dif(5)/hr
            erate(5)=sqrt(dif(5))/hr
            write(6,1003)locsc,j,(rate(jj),jj=1,5),erate(5),hr
          else
            write(6,*)locsc,j,'No GPS time',hr
          end if
 1003 format(2i3,4f9.2,f10.1,f5.1,f8.3)
        end do
        locsc=locsc-1
        if(locsc.le.nbegins)locsc=nends
      end do
      return
      end
      subroutine sclfill
      include 'com_span.inc'
      include 'com_mp.inc'
      include 'com_analysis.inc'
      dimension mncnt(5),inccnt(5)
c      data mncnt/5200,5300,4400,4400,125/  ! EQUIP_4JAN2016_161737.txt  
      data mncnt/5*-1/   ! turn off search for no counts intervals
      data nentry/0/
      if(idate.eq.0)return
      if(locsc.gt.0)locscp=locsc  ! locscp set to 1 in init   
      locsc=locsc+1               ! locsc  set to 0 in init
      if(locsc.gt.nends)locsc=nbegins+1
c      write(6,*)'Characters in line is ',nchar
      if (nchar.eq.48)then
        if(reset)then
          read (bufin,1031)(inccnt(j),j=1,5)
          do j=1,5
            iscs(j,locsc)=iscs(j,locscp)+inccnt(j)
          end do
        else
          read (bufin,1031)(iscs(j,locsc),j=1,5)
        end if
      else if(nchar.eq.75)then
        if(reset)then
          read (bufin,1032)(inccnt(j),j=1,5)
          do j=1,5
            iscs(j,locsc)=iscs(j,locsc)+inccnt(j)
          end do
        else
          read (bufin,1032)(iscs(j,locsc),j=1,5)
        end if
      else
        write(6,*)nchar,' character scalar line at lines= ',nlines
      end if
      if (locsc.gt.1)then  !look for counter off intervals
        do j=1,5
          k=iscs(j,locsc)-iscs(j,locscp)
          if(k .lt. mncnt(j))write(6,1033)j,k,idate,itm,frsec
        end do
      end if
 1033 format('Counter',i2,' off?  It advanced',i6,' at',2i8,f8.5) 
 1031 format(2x,5(1x,z8))
 1032 format(2x,5(4x,z8))
      ippd(2,locsc)=idate
      ipph(2,locsc)=itm
      spsec(2,locsc)=frsec
      nentry=nentry+1
      nentsc(locsc)=nentry
      return
      end
      subroutine runlen(jw,ke,ks,hr)  ! works in leap years
c jw is 1 for event and 2 for scaler setting.  ke and ks are locations of
c stop and start data in ring buffer.
c   Accuracy in single precision is about 0,03 sec for a 24 hr run
      include 'com_span.inc'
      dimension monds(12)
      data monds/31,28,31,30,31,30,31,31,30,31,30,31/
      if(ippd(jw,ks).eq.0 .or. ippd(jw,ke).eq.0)then
        hr=0.0
        return
      end if
      iyks=ippd(jw,ks)/10000  !This is year, but not used
      mks=(ippd(jw,ks)-iyks*10000)/100  ! month
      idks=ippd(jw,ks)-100*(ippd(jw,ks)/100)  ! day
      iyke=ippd(jw,ke)/10000
      mke=(ippd(jw,ke)-iyke*10000)/100
      idke=ippd(jw,ke)-100*(ippd(jw,ke)/100)
      iday=idke-idks  !  will become number of days in run
      m=mke+12*(iyke-iyks)  ! after this calculation accounts for months
c      if(m.gt.12 .or. m.lt.1 .or. mks.gt.12 .or. mks.lt.1)write
c     & (6,*)mks,ippd(jw,ks),mke,m,ippd(jw,ke)
      iy=iyks
      do jj=mks,m-1
        mon=1+mod(jj-1,12)
        iday=iday+monds(mon)
        if(mon.eq.2 .and. mod(iy,4).eq.0)then
          iday=iday+1
          write(6,*)'Feb of leap year, add a day',jj,mon,iy,iday
        end if
        if(mon.eq.12)iy=iy+1
      end do
      ihks=ipph(jw,ks)/10000
      msks=ipph(jw,ks)-10000*ihks ! 100*min +seconds
      ihke=ipph(jw,ke)/10000
      mske=ipph(jw,ke)-10000*ihke
      hdif=ihke-ihks
      amdif=(mske/100)-(msks/100)
      isks=msks-100*(msks/100)
      iske=mske-100*(mske/100)
      sdif=spsec(jw,ke)-spsec(jw,ks)+float(iske-isks)
      hr=24.0*iday+hdif+((amdif+(sdif/60.))/60.)
      return
      end
      subroutine runlenxx(jw,ke,ks,hr) ! does not work in leap years
c jw is 1 for event and 2 for scaler setting.  ke and ks are locations of
c stop and start data in ring buffer.
      include 'com_span.inc'
      dimension monds(18)
      data monds/31,28,31,30,31,30,31,31,30,31,30,31,
     & 31,28,31,30,31,30/
c Leap year Feb is not taken into account, runs longer than 2 years not done
      hr=0.
      if(ippd(jw,ks).eq.0 .or.ippd(jw,ke).eq.0) return
      idks=ippd(jw,ks)/10000  !This is year, but not used
      mks=(ippd(jw,ks)-idks*10000)/100  ! month
      idks=ippd(jw,ks)-100*(ippd(jw,ks)/100)  ! day

      idke=ippd(jw,ke)/10000
      mke=(ippd(jw,ke)-idke*10000)/100
      idke=ippd(jw,ke)-100*(ippd(jw,ke)/100)
      iday=idke-idks  !  will become number of days in run
c
c      if(mke.lt.mks)mke=mke+12  ! after this calculation accounts for months
c      if(mke.gt.12 .or. mke.lt.1 .or. mks.gt.12 .or. mks.lt.1)write
c     & (6,*)mks,ippd(jw,ks),mke,ippd(jw,ke)
c      do jj=mks,mke-1
c        iday=iday+monds(1+mod(jj-1,12))
c      end do
c
      if(mke.lt.mks)mke=mke+12  ! after this calculation accounts for months
      if(mke.gt.22 .or. mke.lt.1 .or. mks.gt.12 .or. mks.lt.1)write
     & (6,*)mks,ippd(jw,ks),mke,ippd(jw,ke)
      do jj=mks,mke-1
        iday=iday+monds(jj)
      end do
      ihks=ipph(jw,ks)/10000
      msks=ipph(jw,ks)-10000*ihks ! 100*min +seconds
      imks=msks/100
      isks=msks-100*imks
      ihke=ipph(jw,ke)/10000
      mske=ipph(jw,ke)-10000*ihke
      imke=mske/100
      iske=mske-100*imke
      hdif=ihke-ihks
      amdif=imke-imks
      sdif=spsec(jw,ke)-spsec(jw,ks)+float(iske-isks)
      hr=24.0*iday+hdif+((amdif+(sdif/60.))/60.)
      return
      end
