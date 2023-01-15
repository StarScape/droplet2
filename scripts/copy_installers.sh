DROPLET_VERSION=$1
DEST_DIR=$2


MAC_X64="./dist/Droplet-${DROPLET_VERSION}.dmg"
MAC_ARM="./dist/Droplet-${DROPLET_VERSION}-arm64.dmg"

cp $MAC_X64 "${DEST_DIR}/droplet_latest_x64.dmg"
cp $MAC_ARM "${DEST_DIR}/droplet_latest_arm64.dmg"
