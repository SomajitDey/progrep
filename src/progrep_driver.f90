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
! This module contains the progrep server or driver. Mainly, this server 
! is nothing but a set of handlers for certain signals. progrep_installer 
! installs these handlers in the simulation source code. The server module 
! also declares and initializes the global variables progrep_tot and 
! progrep_curr that are assigned values in the simulation source code. See 
! the Samples directory to know how-to integrate this server with your 
! simulation source code.
!
! At its bare minimum, progrep_installer installs the handler for SIGWINCH
! only. This is because, it is expected that a simulation routine won't 
! have any other use for the window-change signal SIGWINCH. The progrep 
! client, when invoked, sends SIGWINCH to your simulation as IPC and this 
! handler (write_to_pipe) reports the current state through a binary pipe.
!
! If the 'progrep' argument is provided by the user at run-time, then the 
! user is taken to be authenticating the use of futher signal handlers
! [SIGINT and SIGQUIT] for interactive invocation of the progrep client and 
! quit with query/pause.

! NUANCES:
! If your simulation is multi-threaded, then according to the pthreads documentation,
! an asynchronous signal is handled by an arbitrary thread. Because, progrep_curr
! is global, it must be shared between the threads. Other variables in the signal 
! handler subroutines, that do not appear in the lexical extent of the parallel 
! construct (OpenMP), would be shared by default. 
!
! pipe, register and session cookie are all in the tmpdir directory. No assumption 
! has been made as to the cleaning of tmpdir on reboot. 

! [HISTORICAL:
! Because of the sticky bit, files in /tmp cannot be unlinked or renamed by other 
! users. Hence, a new sub-directory /tmp/progrep is created with write permissions
! for all, to contain the necessary tmpfiles.]
!************************************************************************

Module progrep
USE ISO_FORTRAN_ENV, ONLY: INT64 !Fortran 2008
USE ISO_C_BINDING, ONLY:C_INT,C_PTR,C_ASSOCIATED,C_F_POINTER
USE syscall, ONLY: f_signal,f_rename,f_getpid,f_chmod,f_getcwd,f_time,f_unlink,f_mkdir
USE config
implicit none
private

integer(C_INT), bind(C), public :: progrep_curr,progrep_tot,progrep_rank
public :: progrep_init,progrep_term

integer(INT64) :: start_wclock,wclock_rate !This KIND=8 is just to avoid the wrap-around problem in system_clock intrinsic
real :: start_cpuTimer
logical :: progrep_IsAlive,interactive
character(len=256) :: pipeOut_name,pipeIn_name,sessFile
character(len=8) :: pid_char

CONTAINS

Subroutine c_progrep_init(nsteps,mpi_rank) bind(C,NAME='progrep_init') !This is C/C++ API
type(C_PTR) , value :: nsteps,mpi_rank
integer(C_INT) , pointer :: f_nsteps,f_mpi_rank

nullify(f_nsteps); nullify (f_mpi_rank)

if(C_ASSOCIATED(nsteps))CALL C_F_POINTER(nsteps,f_nsteps)
if(C_ASSOCIATED(mpi_rank))CALL C_F_POINTER(mpi_rank,f_mpi_rank)
CALL progrep_init(f_nsteps,f_mpi_rank)
End Subroutine c_progrep_init

Subroutine progrep_init(nsteps,mpi_rank) !This is FORTRAN API
integer , intent(in) , optional :: nsteps,mpi_rank

character(len=256) :: cwd,USER,started
character(len=8) :: DATE
character(len=6) :: TIME
integer :: sessUnit,chmodErr,rmErr,dummy_return

CALL common_config

CALL DATE_AND_TIME(DATE,TIME)
WRITE(started,'(A)')DATE(7:8)//'/'//DATE(5:6)//'/'//DATE(3:4)//' '//TIME(1:2)//':'//TIME(3:4)//':'//TIME(5:6)

progrep_curr=0_C_INT
if(present(nsteps))then
   progrep_tot=nsteps
else   
   progrep_tot=0_C_INT
endif
   
if(present(mpi_rank))then
   progrep_rank=mpi_rank
else   
   progrep_rank=-1_C_INT !This implies simulation is not MPI-aware
endif

CALL configure
CALL f_signal(SIGWINCH_num,write_to_pipe) !SIGWINCH
if(interactive)then
   CALL f_signal(2,progrep_launcher) !SIGINT
   CALL f_signal(3,quit_with_query) !SIGQUIT
   CALL tty_msg('pid = '//TRIM(ADJUSTL(pid_char)))
   CALL tty_msg('Press Ctrl+C to query progress, and, Ctrl+'//achar(92)//' to pause or quit')
endif   

CALL SYSTEM_CLOCK(start_wclock,wclock_rate)
CALL CPU_TIME(start_cpuTimer)

CALL f_getcwd(cwd)
CALL GET_ENVIRONMENT_VARIABLE('USER',USER)
sessFile=TRIM(ADJUSTL(tmpdir))//'/'//TRIM(ADJUSTL(pid_char))
OPEN(NEWUNIT=sessUnit,FILE=sessFile,STATUS='unknown',ACCESS='stream',FORM='unformatted',ACTION='write',IOSTAT=dummy_return)
WRITE(sessUnit,IOSTAT=dummy_return)progrep_rank,f_time(),cwd,USER,started   
CLOSE(sessUnit,IOSTAT=dummy_return)
CALL f_chmod(sessFile,'666',chmodErr) !Make session file read+write for everybody

CALL f_unlink(pipeIn_name,rmErr)
CALL f_unlink(pipeOut_name,rmErr)

End Subroutine progrep_init

Subroutine progrep_term() bind(C) !This is FORTRAN/C/C++ API
integer :: rmErr
CALL f_unlink(pipeIn_name,rmErr)
CALL f_unlink(pipeOut_name,rmErr)
CALL f_unlink(sessFile,rmErr)
End Subroutine progrep_term

Subroutine write_to_pipe(dummy) bind(C)
USE ISO_C_BINDING, ONLY: C_INT
integer(c_int) , intent(in) , value :: dummy
real :: execTime,cpuTimer,cpuSecs
integer(INT64) :: wclock
integer :: pipeUnit,dummy_return

OPEN(NEWUNIT=pipeUnit,FILE=TRIM(ADJUSTL(pipeIn_name)),STATUS='replace',ACTION='write',ACCESS='stream',FORM='unformatted', &
ASYNCHRONOUS='yes',IOSTAT=dummy_return)
WRITE(pipeUnit,ASYNCHRONOUS='yes',IOSTAT=dummy_return)progrep_curr,progrep_tot
CALL CPU_TIME(cpuTimer)
cpuSecs=cpuTimer-start_cpuTimer
WRITE(pipeUnit,ASYNCHRONOUS='yes',IOSTAT=dummy_return)cpuSecs
CALL SYSTEM_CLOCK(wclock)
execTime=REAL(wclock-start_wclock)/wclock_rate
WRITE(pipeUnit,ASYNCHRONOUS='yes',IOSTAT=dummy_return)execTime
CLOSE(pipeUnit,IOSTAT=dummy_return)
CALL f_rename(TRIM(ADJUSTL(pipeIn_name)),TRIM(ADJUSTL(pipeOut_name)),dummy_return)!Atomic rename to avoid race of write & read
End Subroutine write_to_pipe

Subroutine progrep_launcher(dummy) bind(c)
USE ISO_C_BINDING, ONLY: C_INT
integer(c_int) , intent(in) , value :: dummy
character(len=256) :: launch_cmd, title
title='"command: progrep '//TRIM(ADJUSTL(pid_char))//'"'
launch_cmd='progrep '//ADJUSTL(pid_char)
if(isInstalled_local)launch_cmd=TRIM(folder)//ADJUSTL(launch_cmd)
CALL EXECUTE_COMMAND_LINE('gnome-terminal -t '//TRIM(ADJUSTL(title))//' --geometry=66x21 -- '//TRIM(ADJUSTL(launch_cmd)), &
                                                                                                            WAIT=.false.)
End Subroutine progrep_launcher

Subroutine quit_with_query(dummy) bind(c)
USE ISO_C_BINDING, ONLY: C_INT
integer(c_int) , intent(in) , value :: dummy 
character :: userChoice
CALL tty_msg('Are you sure you want to quit? y/n')
do
   READ*,userChoice
   select case(userChoice)
      case('y')
         STOP
      case('n')
         EXIT
      case('Y')
         STOP
      case('N')
         EXIT
      case default
         CALL tty_msg('Input not recognized. Enter y to quit or n to cancel')
         CYCLE   
   endselect            
enddo      
CALL tty_msg('Quit request cancelled. Continuing run...')
End Subroutine quit_with_query

SUBROUTINE configure
character(len=1000) :: cmd
character(len=*), parameter :: substring='progrep'
logical :: handlerIsAbsent=.true.,termIsAbsent=.true.
integer :: pid_int,exitcode,cmdcode,cmdLength,procUnit,procStat

pid_int=f_getpid()
WRITE(pid_char,'(I7)')pid_int
WRITE(pipeIn_name,'(a)')TRIM(ADJUSTL(tmpdir))//'/'//'.pipe.'//TRIM(ADJUSTL(pid_char))
WRITE(pipeOut_name,'(a)')TRIM(ADJUSTL(tmpdir))//'/'//'pipe.'//TRIM(ADJUSTL(pid_char))

CALL GET_COMMAND(cmd,LENGTH=cmdLength)
if(cmdLength==0)then
   OPEN(NEWUNIT=procUnit,FILE='/proc/'//trim(adjustl(pid_char))//'/cmdline',IOSTAT=procStat)
   READ(procUnit,'(a)',IOSTAT=procStat)cmd
   CLOSE(procUnit)
   IF(procStat/=0)cmd=REPEAT(' ',LEN(cmd))
   interactive=(INDEX(cmd,ACHAR(0)//substring//ACHAR(0))/=0)
else
   interactive=(INDEX(cmd,' '//substring//' ')/=0)
endif   


if(interactive)then
   CALL EXECUTE_COMMAND_LINE('command -v progrep > /dev/null',EXITSTAT=exitcode,CMDSTAT=cmdcode) !command is a bash built-in
   if(cmdcode==0)handlerIsAbsent=(exitcode/=0)
   CALL EXECUTE_COMMAND_LINE('command -v gnome-terminal > /dev/null',EXITSTAT=exitcode,CMDSTAT=cmdcode)
   if(cmdcode==0)termIsAbsent=(exitcode/=0)
   if(handlerIsAbsent.OR.termIsAbsent)then
      interactive=.false.
      if(handlerIsAbsent)CALL tty_msg('Install progrep to avail progrep functionality')
      if(termIsAbsent)CALL tty_msg('Interactive mode unavailable (Install gnome-terminal). Use command: progrep ' &
      //TRIM(ADJUSTL(pid_char)))
   endif
endif   
END SUBROUTINE configure
 
Subroutine tty_msg(message)
character(len=*) , intent(in):: message
integer, parameter :: ttyUnit=567
character(len=*),parameter ::ttyName='/dev/tty'
OPEN(UNIT=ttyUnit,FILE=ttyName,ACCESS='stream',FORM='formatted',ACTION='write')
WRITE(ttyUnit,'(a)')new_line('x')//'progrep: '//message//new_line('x')
CLOSE(ttyUnit)
End Subroutine tty_msg

End Module progrep
