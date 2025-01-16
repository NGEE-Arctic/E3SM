
ORIGIN_DIR=${pwd}

echo "PETSC_DIR: $PETSC_DIR"

if [ -d "${PETSC_DIR}" ]; then
  # --------------------------------------------------------------------------------------------------------
  # pflotran src: git clone https://github.com/bsulman/pflotran-elm-interface pflotran-elm-interface_v2021
  PFLOTRAN_SRC_DIR=/projects/hpcl-cli185/proj-shared/f9y/models/pflotran-elm-interface_v2021
  # build pflotran and bgc lib, if NOT yet
    # cd $PFLOTRAN_SRC_DIR/src/pflotran
    # make PETSC_DIR=${PETSC_DIR} pflotran
    # make PETSC_DIR=${PETSC_DIR} libpflotranchem.a
 
  # pflotran installation directory, if NOT in the source directory
    # PFLOTRAN_PATH=${PFLOTRAN_SRC_DIR}/src/pflotran
  PFLOTRAN_PATH=/projects/hpcl-cli185/proj-shared/ccsi-apps/alquimia-pflotran/v2021/pflotran-elm-interface
    # cp ${PFLOTRAN_SRC_DIR}/src/pflotran/*.mod ${PFLOTRAN_PATH}/
    # cp ${PFLOTRAN_SRC_DIR}/src/pflotran/libpflotranchem.a ${PFLOTRAN_PATH}/
    # cp ${PFLOTRAN_SRC_DIR}/src/pflotran/pflotran ${PFLOTRAN_PATH}/

  # ------------------------------------------------------------------------------------------------------
  # alquimia-pflotran src: git clone -b pflotran_SOMdec_reactions https://github.com/bsulman/alquimia-dev alquimia-dev_v2021
  ALQUIMIA_SRC_DIR=/projects/hpcl-cli185/proj-shared/proj-shared/f9y/models/alquimia-dev_v2021

  ALQUIMIA_PATH=/projects/hpcl-cli185/proj-shared/ccsi-apps/alquimia-pflotran/v2021/alquimia
  PFLOTRAN_PATH=/projects/hpcl-cli185/proj-shared/ccsi-apps/alquimia-pflotran/v2021/pflotran-elm-interface

  cd $ALQUIMIA_PATH
  if [ -d "${ALQUIMIA_PATH}/build" ]; then
    rm -rf "${ALQUIMIA_PATH}/build"
  fi

  mkdir ${ALQUIMIA_PATH}/build ; cd ${ALQUIMIA_PATH}/build

cmake $ALQUIMIA_SRC_DIR/ \
  -DCMAKE_INSTALL_PREFIX=$ALQUIMIA_PATH \
  -DCMAKE_C_COMPILER=`which mpicc` \
  -DCMAKE_CXX_COMPILER=`which mpicxx` \
  -DCMAKE_Fortran_COMPILER=`which mpif90` \
  -DCMAKE_BUILD_TYPE=Debug \
  -DXSDK_WITH_PFLOTRAN=ON \
  -DXSDK_WITH_CRUNCHFLOW=OFF \
  -DTPL_PETSC_INCLUDE_DIRS="$PETSC_DIR/include" \
  -DTPL_PETSC_LDFLAGS="-L$PETSC_DIR/lib -lpetsc" \
  -DTPL_PFLOTRAN_LIBRARIES=$PFLOTRAN_SRC_DIR/src/pflotran/libpflotranchem.a \
  -DTPL_PFLOTRAN_INCLUDE_DIRS=$PFLOTRAN_SRC_DIR/src/pflotran \
  -DCMAKE_BUILD_TYPE=DEBUG \
  -DCMAKE_C_FLAGS="-W -Wall -Wextra -DPFLOTRAN_SOMDEC" \
  -DCMAKE_CXX_FLAGS="-W -Wall -Wextra -DPFLOTRAN_SOMDEC" \
  -DCMAKE_Fortran_FLAGS="-W -Wall -Wextra -DPFLOTRAN_SOMDEC"
make -j 6 VERBOSE=1
make test
make install

# above 'make install' appeared NOT copy alquimia-pflotran's *.mod *.h to installation directory
  cp -f ${ALQUIMIA_PATH}/build/alquimia/*.mod ${ALQUIMIA_PATH}/include/
  cp -f ${ALQUIMIA_PATH}/build/alquimia/*.h ${ALQUIMIA_PATH}/include/

  cd $ORIGIN_DIR

else
  echo "ERROR: $PETSC_DIR not exists, please built PETSc prior to Alquimia and PFLOTRAN"

fi

