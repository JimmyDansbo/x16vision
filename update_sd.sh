#!/bin/bash
if [ "$1" = "" ]; then SDCARD="sdcard.img"; else SDCARD="$1"; fi
echo "Removing and creating new sdcard image file"
rm $SDCARD
truncate -s 34M $SDCARD
echo "Creating partition and formatting to FAT32"
/usr/sbin/parted -s $SDCARD mklabel msdos mkpart primary fat32 2048s -- -1
mformat -i $SDCARD@@1M -F
echo "Copying files..."
mcopy -v -i $SDCARD@@1M -o -m *.PRG *.BIN ::
