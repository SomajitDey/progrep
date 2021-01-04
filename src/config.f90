!@Somajit Dey <somajit@users.sourceforge.net> 14 December 2020
!
! Copyright (C) 2020 Somajit Dey
! Department of Physics, University of Calcutta
! Email: somajit@users.sourceforge.net
! 
! This file is part of progrep.
! 
! progrep is free software: you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or (at your
! option) any later version.
!   
! progrep is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
!   
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!************************************************************************

!************************************************************************
! BRIEF:
! The purpose of this module is to determine:
! The installation directory of progrep: set folder, isInstalled, isInstalled_local
! Locate and read the settings file: SIGNUM & LOCAL_SCRATCH 
! Check if LOCAL_SCRATCH permits creation of sub-directory, if yes create progrep/
! If LOCAL_SCRATCH is bad, default to /tmp and try create progrep/ therein

! The subroutine common_config is to be used by both progrep server & client so that
! they agree on install location & scratch space.
!************************************************************************

Module config
USE syscall, ONLY:f_time,f_getpid,f_chmod,f_mkdir,f_rmdir
implicit none
private
public :: common_config
character(len=256) , public :: folder,tmpdir
logical , public :: isInstalled,default_tmpdir,isInstalled_local=.FALSE.
integer , public :: SIGWINCH_num

contains

Subroutine common_config
character(len=*) , parameter :: global_install='/etc/progrep/',local_dir='progrep_installation/',settings='settings'
integer :: settingsUnit,dummy_return,testResult,trailing
character(len=256) :: HOME,local_install,from_settings,testDir,settingsFile

!Default initialization for safety
SIGWINCH_num=28
from_settings='/tmp'
default_tmpdir=.TRUE.

! Determine whether progrep installation is shared or user-specific. In case of both, proceed with system-wide installation.
CALL GET_ENVIRONMENT_VARIABLE('HOME',HOME)
local_install=TRIM(ADJUSTL(HOME))//'/'//local_dir
INQUIRE(FILE=global_install//settings,EXIST=isInstalled)
if(isInstalled)then
    folder=global_install
else
    INQUIRE(FILE=TRIM(ADJUSTL(local_install))//settings,EXIST=isInstalled)
    if(isInstalled)then
        folder=local_install
        isInstalled_local=.TRUE.
    endif
endif

! Settings file has been located by now.
settingsFile=TRIM(ADJUSTL(folder))//settings

if(isInstalled)then
    ! Read user-given scratch space and signum. IOSTAT is required so that progrep_server never crashes during any I/O.
    OPEN(NEWUNIT=settingsUnit,FILE=TRIM(ADJUSTL(settingsFile)),STATUS='old',ACCESS='sequential', &
                                                                FORM='formatted',ACTION='read',IOSTAT=dummy_return)
    READ(settingsUnit,'(A)',IOSTAT=dummy_return)from_settings
    trailing=LEN_TRIM(from_settings)
    if(from_settings(trailing:trailing)=='/')from_settings(trailing:trailing)=' ' !Get rid of any trailing slash
    READ(settingsUnit,*,IOSTAT=dummy_return)SIGWINCH_num
    CLOSE(settingsUnit,IOSTAT=dummy_return)

    ! Let's try to create a UNIQUE directory in the given tmpdir to test whether the progrep subdirectory can be created
    WRITE(testDir,'(I0,A,I0)')f_time(),'progrep',f_getpid() !This gives a unique name(as only one proc at any given time & PID)
    testDir=TRIM(ADJUSTL(from_settings))//'/'//TRIM(ADJUSTL(testDir))
    CALL f_mkdir(TRIM(ADJUSTL(testDir)),'777',testResult)
    if(testResult==0)default_tmpdir=.FALSE. !User-given directory has write permission. Keep it
    CALL f_rmdir(TRIM(ADJUSTL(testDir)),testResult)
endif

tmpdir=TRIM(ADJUSTL(from_settings))//'/'//'progrep'
! Finally create progrep subdirectory in the scratch space such that any progrep client or server may access, read or write
CALL f_mkdir(TRIM(ADJUSTL(tmpdir)),'777',dummy_return) !Create progrep dir; no side-effect if dir already exists
CALL f_chmod(TRIM(ADJUSTL(tmpdir)),'777',dummy_return) !Make dir rwx for all. r(for ls),w(for unlink),x(for file access)

End Subroutine common_config
End Module config
