#!/bin/bash

if [ ! -f vendor/libicu/lib/libicuuc.a ]; then
  platform="Linux"
  cd vendor/libicu/
  sh source/runConfigureICU $platform --enable-static --disable-shared
  make
  cd ../../
fi
