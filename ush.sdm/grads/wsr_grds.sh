#!/bin/sh

# @ job_name = jwsr_grads
# @ output = /stmp/wsr_grads.o$(jobid)
# @ error = /stmp/wsr_grads.o$(jobid)
# @ shell = /bin/sh
# @ job_type = serial
# @ class = 1
# @ min_processors = 1
# @ max_processors = 1
# @ wall_clock_limit = 00:30:00
# @ notification = never
# @ queue

set -x 
### USER SETUP
export envir=prod
# ETKFOUT is the home dir where ET KF results
# are archived, if unset it will plot the results from Opr
export ETKFOUT=
export HOMEwsr=/u/wx12sd/wsr
export GESdir=/nwges/wsr
export PRINTSDM=YES
### END USER SETUP #########

export PATH=$PATH:/usrx/local/grads/bin
export GADDIR=/usrx/local/grads/dat
export GASCRP=/usrx/local/grads/gslib
export PRINTSDM=${PRINTSDM:-"NO"}
export RAWINSONDES=${RAWINSONDES:-"YES"}
export job=wsr_main
export GESdir=${GESdir:-/nw${envir}/wsr}

export pid=$$
export DATA=/ptmp/$LOGNAME/wsr/tmp/${job}.${pid}
mkdir -p $DATA
cd $DATA
rm -rf $DATA/*

PDY=`head -1 $GESdir/targdata.d`
cases=`head -2 $GESdir/targdata.d | tail -1`
. $ETKFOUT/com/wsr/${envir}/wsr.$PDY/case1.env

#cp $HOMEwsr/fix/wsr_track.* .
cp /nw${envir}/fix/wsr_track.* .
cp $HOMEwsr/grads/*.gs $DATA/.

i=1
while test ${i} -le ${cases}
do
   . $ETKFOUT/com/wsr/${envir}/wsr.$PDY/case${i}.env

   #########################################
   # GRAPHICS START UP
   #########################################
   cp ${COMIN}/case${i}.circlevr.d circlevr.d
   cp ${COMIN}/case${i}.targ1.gr targ1.d.gr 

   cat $FIXwsr/wsr_targ_flexctl1 > wsr_targ_flex.ctl
   echo "XDEF    ${nlon} linear    ${lon1} 5.000" >> wsr_targ_flex.ctl
   echo "YDEF    ${nlat} linear    ${lat1} 5.000" >> wsr_targ_flex.ctl
   cat $FIXwsr/wsr_targ_flexctl2 >> wsr_targ_flex.ctl

   if [ ${searchareacode} -eq 1 ]
   then 

       read ndrops1 < wsr_track.${fl1}
       echo $ndrops1 > dropplot1.d
       tail -n$ndrops1 wsr_track.${fl1} | awk '{ if($1 < 0) print s=$1+360.,$2;else print s=$1,$2}' >> dropplot1.d


       read ndrops2 < wsr_track.${fl2}
       echo $ndrops2 > dropplot2.d
       tail -n$ndrops2 wsr_track.${fl2} | awk '{ if($1 < 0) print s=$1+360.,$2;else print s=$1,$2}' >> dropplot2.d

       read ndrops3 < wsr_track.${fl3}
       echo $ndrops3 > dropplot3.d
       tail -n$ndrops3 wsr_track.${fl3} | awk '{ if($1 < 0) print s=$1+360.,$2;else print s=$1,$2}' >> dropplot3.d
       if [[ ${RAWINSONDES} = YES ]]
       then
       read nstations < ${COMIN}/case${i}.rawinpoints.d 
       echo $nstations > stationplot.d
       read sigmax <${COMIN}/case${i}.rawinpoints.d
       echo sigmax >> stationplot.d
       read sigmin <${COMIN}/case${i}.rawinpoints.d
       echo sigmin >> stationplot.d
       tail -n$nstations ${COMIN}/case${i}.rawinpoints.d >> stationplot.d

       fi
   fi

   if [ ${searchareacode} -eq 1 ]
   then
       if [[ ${RAWINSONDES} = YES ]]
       then
       grads -blc "wsr_targ_special5.gs ${hr1} ${hr2} ${ensdategr} ${lon1} ${lon2} ${lat1} ${lat2} ${ensemble} ${mem} ${ndrops1} ${ndrops2} ${ndrops3} ${fl1} ${fl2} ${fl3} ${vrlonewest} ${vrlat} ${radvr} ${vnormgr} ${i}"
       else 
       grads -blc "wsr_targ_special4.gs ${hr1} ${hr2} ${ensdategr} ${lon1} ${lon2} ${lat1} ${lat2} ${ensemble} ${mem} ${ndrops1} ${ndrops2} ${ndrops3} ${fl1} ${fl2} ${fl3} ${vrlonewest} ${vrlat} ${radvr} ${vnormgr} ${i}"
       fi

   else
      grads -blc "wsr_targ_notracks.2.gs ${hr1} ${hr2} ${ensdategr} ${lon1} ${lon2} ${lat1} ${lat2} ${ensemble} ${mem} ${vrlonewest} ${vrlat} ${radvr} ${vnormgr} ${i}"
   fi

   gxps -ic ec.out -o Plot${case_id}_${vrlat}N${vrlonewest}_summary_${lt1}_${lt2}.ps
   if test "$PRINTSDM" = "YES"
   then
    lpr -h -Php26_sdm Plot${case_id}_${vrlat}N${vrlonewest}_summary_${lt1}_${lt2}.ps
   fi

   mv gifimage.out Plot${case_id}_${vrlat}N${vrlonewest}_summary_${lt1}_${lt2}.gif

   if [ ${searchareacode} -eq 1 ]
   then
       cp $COMIN/case${i}.flights.d flights.d
#
# Add an edit of the NANQs from the flights.d file -
#
      cat $COMIN/case${i}.flights.d | sed 's/NaNQ/.000/' > flights.d


       if [[ ${RAWINSONDES} = YES ]]
       then
       cat $COMIN/case${i}.rawinhist.d | sed 's/NaNQ/.000/' > rawinhist.d
       grads -blc "wsr_rawhist.gs ${ensemble} ${xmin2} ${xmax2} ${ymin2} ${ymax2} ${xlow} ${xint} ${yint} ${ensdategr} ${obsdate} ${veridate} ${vrlonewest} ${vrlat} ${radvr} ${mem} ${vnormgr} ${i}"
       mv gifimage.out VR_${vrlat}N${vrlonewest}_rawinsondes_${lt1}_${lt2}.gif
       fi

       grads -blc "wsr_flihist.1.gs ${ensemble} ${xmin} ${xmax} ${ymin1} ${ymax1} ${xlow} ${xint} ${yint} ${ensdategr} ${obsdate} ${veridate} ${vrlonewest} ${vrlat} ${radvr} ${mem} ${vnormgr} ${fl1} ${fl2} ${fl3} ${i}"

       gxps -ci ec.out -o VR_${vrlat}N${vrlonewest}_flights_${lt1}_${lt2}.ps
       if test "$PRINTSDM" = "YES"
       then
          if [ $lt1 -eq 48 || $lt1 -eq 24 ] 
          then
          lpr -h -Php26_sdm VR_${vrlat}N${vrlonewest}_flights_${lt1}_${lt2}.ps
          fi
       fi

       mv gifimage.out VR_${vrlat}N${vrlonewest}_flights_${lt1}_${lt2}.gif

   fi

   i=`expr ${i} + 1`

done

if [ ${searchareacode} -eq 0 ]
then
    
i=1
while test ${i} -le ${cases}
do
    . $ETKFOUT/com/wsr/${envir}/wsr.$PDY/notwsr_case${i}.env

    ltdiffsteps=`expr ${ltdiff} / 12`
    if [ ${ltdiffsteps} -lt 7 ]
    then
        ltdiffsteps=7
    fi

    cp ${COMIN}/notwsr_case${i}.dropplot.d dropplot.d
    cp ${COMIN}/notwsr_case${i}.circlevr${i}.d circlevr${i}.d

    ctr=0
    while [ ${ctr} -le ${ltdiffsteps} ]
    do
	ctraddone=`expr ${ctr} + 1`
        cp ${COMIN}/notwsr_case${i}.sig${ctraddone}.ctl sig${ctraddone}.ctl
        cp ${COMIN}/notwsr_case${i}.sig${ctraddone}.d.gr sig${ctraddone}.d.gr
        ctr=`expr ${ctr} + 1`
    done

    grads -bpc "wsr_sigvar_notwsr_8panel.gs ${ensdate} ${obsdate} ${ltdiff} 9 ${vnormgr} ${i}"

    gxps -ic ec.out -o sigvar_case${case_id}.ps
    if test $PRINTSDM = "YES"
    then
        lpr -h -Php26_sdm sigvar_case{$case_id}.ps
    fi

    mv gifimage.out sigvar_case${case_id}.gif

    if [ ${ltdiff} -gt 84 ]
    then
         grads -bpc "wsr_sigvar_notwsr_8panel_more.gs ${ensdate} ${obsdate} ${ltdiff} ${vnormgr} ${i}"
 
         gxps -ic ec.out -o sigvar_case${case_id}_more.ps
         if test $PRINTSDM = "YES"
         then
             lpr -h -Php26_sdm sigvar_case${case_id}_more.ps
         fi

         mv gifimage.out sigvar_case${case_id}_more.gif
    fi
    
    i=`expr ${i} + 1`
done

else

. $ETKFOUT/com/wsr/${envir}/wsr.$PDY/ltinfo.env

mk[2]=`expr ${mk2} - 1`
mk[3]=`expr ${mk3} - 1`
mk[4]=`expr ${mk4} - 1`
mk[5]=`expr ${mk5} - 1`
mk[6]=`expr ${mk6} - 1`
mk[7]=`expr ${mk7} - 1`
mk[8]=`expr ${mk8} - 1`
mk[9]=`expr ${mk9} - 1`
mk[10]=`expr ${mk10} - 1`

z=1
while test ${z} -le ${cases}
do
    lt1[z]=\$lt1case${z}
    lt2[z]=\$lt2case${z}
    z=`expr ${z} + 1`
done

j=`expr ${minlt1} / 12`
finish=`expr ${maxlt1} / 12`

while test ${j} -le ${finish}
do
   lt1=`expr ${j} \* 12`
mark=0 
ct=1
maxtotlt2=0
while test ${ct} -le ${cases}
do
    lt1val=`eval echo ${lt1[$ct]}`
    lt2val=`eval echo ${lt2[$ct]}`
    if [ ${lt1} -eq $lt1val ]
    then
        mark=1
        if [ $lt2val -gt ${maxtotlt2} ]
        then
            maxtotlt2=$lt2val
        fi
    fi
ct=`expr $ct + 1`
done

if [ $mark -eq 1 ]
then

   ltdiff=`expr ${maxtotlt2} - ${lt1}`
   ltdiffsteps=`expr ${ltdiff} / 12`
   if [ ${ltdiffsteps} -lt 7 ]
   then
           ltdiffsteps=7
   fi

   k=1
   while test ${k} -le ${mk[$j]}
   do

      . $ETKFOUT/com/wsr/${envir}/wsr.$PDY/flight${k}_${lt1}.env

      ctr=0
      while [ ${ctr} -le ${ltdiffsteps} ]
      do
  	  ctraddone=`expr ${ctr} + 1`
          cp ${COMIN}/flight${k}_${lt1}.sig${ctraddone}.ctl sig${ctraddone}.ctl
	  cp ${COMIN}/flight${k}_${lt1}.sig${ctraddone}.d.gr sig${ctraddone}.d.gr
          ctr=`expr ${ctr} + 1`
      done

      cp ${COMIN}/flight${k}_${lt1}.dropplot.d dropplot.d

      i=1
      while test ${i} -le ${cases}
      do
         cp ${COMIN}/flight${k}_${lt1}.circlevr${i}.d circlevr${i}.d
         i=`expr ${i} + 1`
      done

      grads -bpc "wsr_sigvar_8panel.gs ${ensdate} ${obsdate} ${ifl} ${ndrops} ${ensemble} ${mem} ${vercase[1]} ${vercase[2]} ${vercase[3]} ${vercase[4]} ${vtime[1]} ${vtime[2]} ${vtime[3]} ${vtime[4]} ${vnormgr}"

      gxps -ic ec.out -o sigvar_${lt1}_flight${ifl}.ps
      if test $PRINTSDM = "YES"
      then
         if [[ $lt1 -eq 24 || $lt1 -eq 48 || $lt1 -eq 72 ]] 
         then
         lpr -h -Php26_sdm sigvar_${lt1}_flight${ifl}.ps
         fi
      fi

      mv gifimage.out sigvar_${lt1}_flight${ifl}.gif
 
      if [ ${ltdiff} -gt 84 ]
      then

           grads -bpc "wsr_sigvar_8panel_more.gs ${ensdate} ${obsdate} ${ifl} ${ltdiff} ${ensemble} ${mem} ${vercase[1]} ${vercase[2]} ${vercase[3]} ${vercase[4]} ${vtime[1]} ${vtime[2]} ${vtime[3]} ${vtime[4]} ${vnormgr}"

           gxps -ic ec.out -o sigvar_${lt1}_flight${ifl}_more.ps
           if test $PRINTSDM = "YES"
           then
               if [[ $lt1 -eq 24 || $lt1 -eq 48 || $lt1 -eq 72 ]]
               then
	       lpr -h -Php26_sdm sigvar_${lt1}_flight${ifl}_more.ps
               fi 
           fi

           mv gifimage.out sigvar_${lt1}_flight${ifl}_more.gif
      fi

      k=`expr ${k} + 1`
   done
 
   fi

   j=`expr ${j} + 1`
done

#lpr -h -Php26_sdm done.txt

fi

#ping -c1 emc1
#pingerr=$?
#if test $pingerr -eq 0
#then
#ftp rzdm << ftpEOF
#cd /home/people/emc/www/htdocs/gmb/targobs/target/graphics
#mkdir $PDY
#cd $PDY
#bin
#prompt
#mput *.gif
#bye
#ftpEOF

ssh -l wx12sd ncorzdm "rm -rf  /home/people/nco/www/htdocs/pmb/sdm_wsr/graphics/${PDY}"
ssh -l wx12sd ncorzdm "mkdir /home/people/nco/www/htdocs/pmb/sdm_wsr/graphics/${PDY}"
scp *.gif wx12sd@ncorzdm:/home/people/nco/www/htdocs/pmb/sdm_wsr/graphics/${PDY}
#fi

#ssh -l ysong rzdm "mkdir -p /home/people/emc/www/htdocs/gmb/tparc/special/trop1_${PDY}_${ensemble}"
#scp *.gif ysong@rzdm:/home/people/emc/www/htdocs/gmb/tparc/special/trop1_${PDY}_${ensemble}

#cp /nfsuser/g01/wx20ys/tobs/wsr/tmp/*.gr /nfsuser/g01/wx20ys/tobs/wsr/${PDY}/.
#cp /nfsuser/g01/wx20ys/tobs/wsr/tmp/*.d /nfsuser/g01/wx20ys/tobs/wsr/${PDY}/.

#rm -rf $DATA
set +x
echo "###############################################"
echo "#           Finished Processing               #"
echo "###############################################"
