#!/bin/bash
new_tagsfile=$1
old_tagsfile=$(echo $new_tagsfile |sed 's/.auto-update$//')
# origin_CITRE_CMD=$(sed -n '/CITRE_CMD/p' ${old_tagsfile})
origin_CITRE_CMD=$2
# origin_TAG_PROC_CWD=$(sed -n '/TAG_PROC_CWD/p' ${old_tagsfile})
origin_TAG_PROC_CWD=$3
echo ========================
date

echo old: ${old_tagsfile}
echo new: ${new_tagsfile}
echo CITRE_CMD: ${origin_CITRE_CMD}
echo TAG_PROC_CWD: ${origin_TAG_PROC_CWD}
if [ -e ${new_tagsfile} ];
then
    echo "" > /dev/null
else
    exit 0
fi

sed -i "1i !_CITRE_CMD	${origin_CITRE_CMD}	//;\"	extras:pseudo" ${new_tagsfile}
sed -i "/\!_TAG_PROC_CWD/d" ${new_tagsfile}
sed -i "1i !_TAG_PROC_CWD	${origin_TAG_PROC_CWD}	//;\"	extras:pseudo" ${new_tagsfile}

# replace file when no process use the tagsfile
proc_num=$(lsof -t ${old_tagsfile} |wc -l)
while [ $proc_num -gt 0 ]
do
    echo  Some process is reading ${old_tagsfile}, will check it again after 1 second
    sleep 1
    proc_num=$(lsof -t ${old_tagsfile} |wc -l)
done


mv ${new_tagsfile} ${old_tagsfile}
