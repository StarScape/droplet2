#!/bin/bash

# Arg 1: The Droplet version being built
# Arg 2: The destination directory to place the binary_urls.json file used by the website to get the Cloudfront URLs for downloads.
DROPLET_VERSION=$1
DEST_DIR=$2

echo -e "\033[0;36m💧 Droplet: Uploading installer files to S3 bucket...\n\033[0m"

CLOUDFRONT_URL="https://d20qwrnt30uhnl.cloudfront.net"
MAC_X64_FILE_SRCNAME="./dist/Droplet-${DROPLET_VERSION}.dmg"
MAC_ARM_FILE_SRCNAME="./dist/Droplet-${DROPLET_VERSION}-arm64.dmg"
WIN_X64_FILE_SRCNAME="./dist/Droplet Setup 0.0.1.exe"

MAC_X64_FILE_DESTNAME="droplet_${DROPLET_VERSION}_x64.dmg"
MAC_ARM_FILE_DESTNAME="droplet_${DROPLET_VERSION}_arm64.dmg"
WIN_X64_FILE_DESTNAME="droplet_${DROPLET_VERSION}_x64.exe"

aws s3 cp $MAC_X64_FILE_SRCNAME "s3://droplet-downloads/${MAC_X64_FILE_DESTNAME}"
aws s3 cp $MAC_ARM_FILE_SRCNAME "s3://droplet-downloads/${MAC_ARM_FILE_DESTNAME}"
aws s3 cp "$WIN_X64_FILE_SRCNAME" "s3://droplet-downloads/${WIN_X64_FILE_DESTNAME}"

echo -e "\033[0;36m💧 Droplet: Generating JSON file...\n\033[0m"

JSON_FILE_PATH="${DEST_DIR}/binary_urls.json"
JSON_FILE="{
  \"macX64Url\": \"${CLOUDFRONT_URL}/${MAC_X64_FILE_DESTNAME}\",
  \"macARMUrl\": \"${CLOUDFRONT_URL}/${MAC_ARM_FILE_DESTNAME}\",
  \"winX64Url\": \"${CLOUDFRONT_URL}/${WIN_X64_FILE_DESTNAME}\"
}"

pwd
echo $JSON_FILE > $JSON_FILE_PATH

echo -e "\033[0;36m💧 Droplet: Successfully outputted to '$JSON_FILE_PATH' \n\033[0m"
