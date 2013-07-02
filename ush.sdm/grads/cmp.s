for file in `ls *`
do
  echo ----- $file -----
  cmp $file /global/save/wx20ys/tobs/prod/jif/grads/$file | more
  read dummy
done

