# This script bundles all necessary progrep files into a tarball for distribution.
# May be invoked with command: make bundle
# All files are listed below, instead of directories, to check against missing files.

#! /bin/bash
version=$(awk 'NR==2 {print $2}' version_info.txt)
dirname="progrep${version}"
filename="${dirname}.tar.gz"
rm -f ${filename}
tar -czf tmp.tgz \
./src/progrep_driver.f90 ./src/progrep.f90 ./src/syscall_wrapper.f90 ./src/config.f90 ./src/notice.txt \
./headers/progrep.h ./headers/cprogrep.h \
./samples/progrep_sample.f90 ./samples/progrep_sample.c ./samples/progrep_sample.cpp \
./samples/progrep_sample_openmp.f90 ./samples/progrep_sample_mpi.c \
./extra/usage.txt ./extra/progrep.1.gz ./extra/progrep.sh \
./bundle.sh ./COPYING ./README.md ./README.html ./Makefile ./version_info.txt
mkdir .tmp
mkdir .tmp/${dirname}
cd .tmp/${dirname}
mv ../../tmp.tgz ./
tar -xf tmp.tgz
rm tmp.tgz
cd ..
tar -czf ../${filename} ${dirname}
cd ..
rm -rf .tmp
echo ${filename} 'has been created in your current working directory'
