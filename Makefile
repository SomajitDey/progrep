# This is the Makefile for progrep
# Copyright (C) 2020 Somajit Dey
# Department of Physics, University of Calcutta
# License: GPL-3.0-or-later <https://www.gnu.org/licenses/>



# CUSTOMIZABLE SECTION BEGINS
FORTRAN_COMPILER = gfortran
LOCAL_SCRATCH = /tmp    # Node-local scratch space. This must be system-wide. DO NOT make this user specific.
SIGNUM = 28    # Find the no. for SIGWINCH in your system with command: kill -l
# END OF CUSTOMIZABLE SECTION



# CHANGE THE FOLLOWING ONLY IF ABSOLUTELY NECESSARY
INSTALL_PATH = /usr/local
bin_PATH = ${INSTALL_PATH}/bin
lib_PATH = ${INSTALL_PATH}/lib
header_PATH=${INSTALL_PATH}/include
man_PATH=${INSTALL_PATH}/man/man1



# CHANGING ANY OF THE FOLLOWING MAY BREAK SOME FUNCTIONALITY
DIR = /etc/progrep
NONROOT_INSTALL_PATH = ${HOME}/progrep_installation
CONFIG_FILE = build/settings

.PHONY: all build clean install uninstall bundle nonroot

all: clean build

clean: 
	@rm -rf ./build
	
build:
	@mkdir -vp build
	@cp -f ./src/*.f90 ./build/
	@$(FORTRAN_COMPILER) -o build/syscall.o -c build/syscall_wrapper.f90
	@mv -f ./syscall.mod build/
	@$(FORTRAN_COMPILER) -o build/config.o -c build/config.f90
	@mv -f ./config.mod build/
	@$(FORTRAN_COMPILER) -o build/progrep build/progrep.f90 build/syscall.o build/config.o
	@$(FORTRAN_COMPILER) -o build/server.o -c build/progrep_driver.f90
	@mv -f ./progrep.mod build/
	@ar -rcs build/libprogrep.a build/server.o build/syscall.o build/config.o
	@rm -f build/*.o build/syscall.mod build/*.f90
	@echo ${LOCAL_SCRATCH} > ${CONFIG_FILE}
	@echo ${SIGNUM} >> ${CONFIG_FILE}
	@echo
	@echo "Build complete." 
	@echo "To install progrep, use command: make install"
	@echo "But if you do not have sudo privilege, use this instead: make nonroot"

install:
	@sudo mkdir -vp ${bin_PATH}
	@sudo mkdir -vp ${lib_PATH}
	@sudo mkdir -vp ${header_PATH}
	@sudo mkdir -vp ${DIR}
	@sudo mkdir -vp ${man_PATH}
	@sudo install -v -m u=rwxs,g=rx,o=rx build/progrep ${bin_PATH}/
	@sudo install -v build/libprogrep.a ${lib_PATH}/
	@sudo install -v -t ${header_PATH}/ headers/*.h
	@sudo install -v -t ${DIR}/ build/progrep.mod  samples/progrep_sample* COPYING headers/*.h build/libprogrep.a Makefile
	@sudo install -v -t ${DIR} version_info.txt extra/usage.txt ${CONFIG_FILE}
	@sudo install -v extra/progrep.1.gz ${man_PATH}/
	@sudo mandb -qp
	@sudo mkdir -vp /etc/profile.d
	@sudo install -v extra/progrep.sh /etc/profile.d
	@sudo mkdir -vp /etc/bash_completion.d
	@sudo install -v extra/progrep.sh /etc/bash_completion.d/progrep    
	@echo
	@echo 'Installation complete. Check with command: progrep'
	
uninstall:
	@sudo rm -vf ${bin_PATH}/progrep ${lib_PATH}/libprogrep.a
	@sudo rm -vf /etc/bash_completion.d/progrep /etc/profile.d/progrep.sh
	@sudo rm -rvf ${DIR}
	@sudo rm -vf ${header_PATH}/progrep.h ${header_PATH}/cprogrep.h ${man_PATH}/progrep.1.gz
	
bundle:
	@sh bundle.sh

nonroot:	
	@mkdir -vp ${NONROOT_INSTALL_PATH}
	@install -v build/libprogrep.a ${NONROOT_INSTALL_PATH}/
	@install -v -m u=rwxs,g=rx,o=rx build/progrep ${NONROOT_INSTALL_PATH}/
	@install -v -t ${NONROOT_INSTALL_PATH}/ build/progrep.mod  samples/progrep_sample* COPYING
	@install -v -t ${NONROOT_INSTALL_PATH} version_info.txt extra/usage.txt ${CONFIG_FILE}
	@install -v -t ${NONROOT_INSTALL_PATH}/ headers/*.h
	@cat extra/progrep.sh >> ${HOME}/.bashrc
	@echo 'export PATH=${PATH}:'${NONROOT_INSTALL_PATH} >> ${HOME}/.bashrc
	@echo 'export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:'${NONROOT_INSTALL_PATH} >> ${HOME}/.bashrc #For ld linker
	@echo 'export LIBRARY_PATH=${LIBRARY_PATH}:'${NONROOT_INSTALL_PATH} >> ${HOME}/.bashrc #For GCC
	@echo 'export C_INCLUDE_PATH=${C_INCLUDE_PATH}:'${NONROOT_INSTALL_PATH} >> ${HOME}/.bashrc #For gcc
	@echo 'export CPLUS_INCLUDE_PATH=${CPLUS_INCLUDE_PATH}:'${NONROOT_INSTALL_PATH} >> ${HOME}/.bashrc #For g++
	@echo
	@echo 'progrep has been installed for:' ${USER}
	@echo 'Installation directory:' ${NONROOT_INSTALL_PATH}
	@echo 'You need to start a new bash session for the changes to take effect.' 
	@echo 'Open a new terminal and check installation with command: progrep'	
