#!/bin/bash
set -eu

# Install castle-view-image menu entries, icons, mime types.
# Based on ../../view3dscene/freedesktop/install.sh

APP_NAME='castle-view-image'

# For non-user install, change this to
#   SHARE_PREFIX = /usr/local/share
# For Debian package, you probably want to set this to
#   SHARE_PREFIX = $(DESTDIR)/usr/share
SHARE_PREFIX="$HOME"/.local/share

# Install icons:
mkdir -p "$SHARE_PREFIX"/icons/hicolor/scalable/apps/
cp -f "$APP_NAME".svg "$SHARE_PREFIX"/icons/hicolor/scalable/apps/

# Install desktop file:
mkdir -p "$SHARE_PREFIX"/applications/
cp -f "$APP_NAME".desktop "$SHARE_PREFIX"/applications/
if which update-desktop-database >/dev/null 2>&1 ; then update-desktop-database -q "$SHARE_PREFIX"/applications/; fi

echo "Installed $APP_NAME (menu entries, icons, mime types): all OK."
