#!/bin/sh

set -x

INSTALLER=QuartusLiteSetup-21.1.0.842-linux.run
INSTALLER_URL=https://download.altera.com/akdlm/software/acdsinst/21.1std/842/ib_installers/QuartusLiteSetup-21.1.0.842-linux.run
INSTALLER_CHECKSUM=9B5F01AB0F5D3C591FC1802357E064D7

CYCLONEV_SUPPORT=cyclonev-21.1.0.842.qdz
CYCLONEV_SUPPORT_URL=https://download.altera.com/akdlm/software/acdsinst/21.1std/842/ib_installers/cyclonev-21.1.0.842.qdz
CYCLONEV_SUPPORT_CHECKSUM=5D3DF782AC7F408F8166E58AF030FF9B

CYCLONEIV_SUPPORT=cyclone-21.1.0.842.qdz
CYCLONEIV_SUPPORT_URL=https://download.altera.com/akdlm/software/acdsinst/21.1std/842/ib_installers/cyclone-21.1.0.842.qdz
CYCLONEIV_SUPPORT_CHECKSUM=E37015353737752218908311E2E915F5

curl -O $INSTALLER_URL
SUM=$(md5sum $INSTALLER | awk '{print toupper($1)}')
if [ "$SUM" != "$INSTALLER_CHECKSUM" ]; then echo "Checksum doesn't match."; exit 1; fi

curl -O $CYCLONEV_SUPPORT_URL
SUM=$(md5sum $CYCLONEV_SUPPORT | awk '{print toupper($1)}')
if [ "$SUM" != "$CYCLONEV_SUPPORT_CHECKSUM" ]; then echo "Checksum doesn't match."; exit 1; fi

curl -O $CYCLONEIV_SUPPORT_URL
SUM=$(md5sum $CYCLONEIV_SUPPORT | awk '{print toupper($1)}')
if [ "$SUM" != "$CYCLONEIV_SUPPORT_CHECKSUM" ]; then echo "Checksum doesn't match."; exit 1; fi

chmod +x *.run
./$INSTALLER --mode unattended --accept_eula 1 --installdir /opt/quartus

echo 'export PATH=$PATH:/opt/quartus/quartus/bin' >> /etc/bash.bashrc
echo 'export QUARTUS_PATH=/opt/quartus/quartus/' >> /etc/bash.bashrc
