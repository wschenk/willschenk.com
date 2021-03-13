for i in ${REPO_DIR}/*
do
    (cd ${i};git log --pretty=format:"%ae:%an")
done | sort -u > ${WORKDIR}/authors

while IFS= read -r line; do
    readarray -d : -t strarr <<< "$line"
    email=${strarr[0]}
    name=${strarr[1]::-1}
    #echo "$name $email"

    filename=${AVATAR_DIR}/${name}.png
    #echo Looking for ${filename}
    if [ ! -f "${filename}" ]; then
        md5=$(echo -n "$email" | tr '[A-Z]' '[a-z]' | md5sum | cut -d" " -f1)
        wget http://www.gravatar.com/avatar/$md5?size=512 -O "${filename}"
    fi

done < ${WORKDIR}/authors
