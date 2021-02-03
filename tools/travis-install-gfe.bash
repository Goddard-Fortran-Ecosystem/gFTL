#!/bin/bash

set -e

GFE_DIR=${HOME}/gfe
mkdir -p ${GFE_DIR}

# First install prerequisites
GFE_INSTALL_DIR=${HOME}/Software/GFE
mkdir -p ${GFE_INSTALL_DIR}

to_build=(pFUnit)
for repo in "${to_build[@]}"
do
   cd ${GFE_DIR}
   git clone https://github.com/Goddard-Fortran-Ecosystem/${repo}.git
   cd ${GFE_DIR}/${repo}
   # Necessary for consistent CMake targets
   git checkout cmake-fixes
   mkdir build && cd build
   cmake .. -DCMAKE_INSTALL_PREFIX=${GFE_INSTALL_DIR} -DCMAKE_PREFIX_PATH=${GFE_INSTALL_DIR}
   make -j$(nproc) install
done

