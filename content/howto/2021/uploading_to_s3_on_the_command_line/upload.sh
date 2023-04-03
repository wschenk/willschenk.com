if [ -z "${AWS_ACCESS_KEY_ID}" ] ; then
   echo Set AWS_ACCESS_KEY_ID
   exit 1
fi

if [ -z "${AWS_SECRET_ACCESS_KEY}" ] ; then
   echo Set AWS_SECRET_ACCESS_KEY
   exit 1
fi

if [ -z "${AWS_END_POINT}" ] ; then
   echo Set AWS_END_POINT
   exit 1
fi

if [ -z "${BUCKET_NAME}" ] ; then
   echo Set BUCKET_NAME
   exit 1
fi

if [ -z "${1}" ] ; then
  echo Usage $0 filename
  exit 1
fi

echo Good to upload ${1}

#echo $AWS_END_POINT
AWS_END_POINT=https://gratitude-public.nyc3.digitaloceanspaces.com
file=${1}
bucket="${BUCKET_NAME}"
contentType="application/octet-stream"
dateValue=`date -uR`
resource="/${bucket}/${file}"

stringToSign="PUT\n\n${contentType}\n${dateValue}\n${resource}"
echo $stringToSign
signature=`echo -en ${stringToSign} | openssl sha1 -hmac ${AWS_SECRET_ACCESS_KEY} -binary | base64`

echo $signature
echo $AWS_END_POINT


curl -X PUT -T "${file}" \
     -H "Host: ${AWS_END_POINT}" \
     -H "Date: ${dateValue}" \
     -H "Content-Type: ${contentType}" \
     -H "Authorization: AWS ${AWS_ACCESS_KEY_ID}:${signature}" \
     ${AWS_END_POINT}/${resource}
