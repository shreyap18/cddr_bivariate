#!/bin/bash
for b in {2..100}
do
        for s in 20 30 40 50 60 70 80 100 120
        do
        sleep 10
        echo "submit B=$b, S=$s"
                sbatch --export="B=$b,S=$s" subsamp_procedure.sbatch
        #       fi
        done
done


