!@Somajit Dey <somajit@users.sourceforge.net> 4 January 2021
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
! This is the code for the progrep client. The client sends your simulation
! a SIGWINCH signal at 1s intervals, which is handled by the progrep server
! installed in your simulation source-code. The server writes to pipe the 
! current state of the simulation which this client reads, processes and
! displays to the user. For further information on the simulation, this
! client also accesses the ps command and/or files in the /proc/<pid> 
! directory.
!
! Much use has been made of the intrinsic procedure 'execute_command_line' 
! below in view of the low memory footprint and cpu usage of the progrep 
! client.

! WSL:
! There are a few workarounds for Windows Subsystem for Linux. Find them with
! the keyword WSL. Apparently, WSL creates a 'fort.<unitnumber>' file everytime
! there is a file opening error (such as open(newunit=<unitnumber>,file=<path>,
! status='OLD or 'UNKNOWN'',IOSTAT=)) for a file that does not yet exist, and 
! attaches unit=<unitnumber> with the fort file.

! SIGSEGV RUNTIME ERROR:
! Any such error possibly arises from type mismatch when porting from 32-bit to 
! 64-bit. E.g. equating INT32 with C_LONG variable type. C_LONG can take both 32-bit
! and 64-bit form depending on the implementation.
!************************************************************************

Program progrep
USE ISO_FORTRAN_ENV , ONLY : INT32 !Fortran 2008
USE syscall , ONLY: f_nanosleep,f_signal,f_kill,f_symlink,f_alarm,f_time,f_unlink,f_getpid,f_gethostname,f_rename,f_mkdir,f_chdir
USE config
implicit none
integer:: currStep,totSteps,mpiRank,percentage=0,ETAhrs=0,ETAmins=0,ETAsecs=0,goneHrs,goneMins,goneSecs
integer :: counter=0,oldStep=0,oldTot=0,initStep,cpuHrs,cpuMin,cpuSec,pidSim,threadNum,etime_Sim
real :: initTime,oldTime=0.,cpuSecs,old_cpuSecs=0.,cpuUsage=0.
real :: factor=0.,FPS_CPU=0.,recentFPS=0.,currFPS=0.
character(len=256) :: hostNode,storeFile,pipeName,logfileName,wdir_Sim ! working directory of the Simulation
character(len=256) :: Sim_USER
character(len=9) :: pidSim_char
integer(INT32) :: secsLeft,nsecsLeft
character(len=256) :: name_Sim,Sim_args,Sim_start_expanded
character(len=8) ::Sim_start
logical :: ssh_session=.FALSE.
integer , parameter :: display_width=66

CALL common_config

CALL f_signal(2,graceful_exit) !SIGINT
CALL f_signal(3,append_log) !SIGQUIT
CALL f_signal(14,sigwinch) !SIGALRM
CALL f_signal(13,graceful_exit) !SIGPIPE, Contingency for SSH non-interactive sessions
CALL progrep_config
do
   CALL progrep_process
   CALL progrep_display
   CALL flash_status
   CALL display_endnote
   counter=counter+1
   CALL f_nanosleep(1_INT32,0_INT32,secsLeft,nsecsLeft)
   CALL f_nanosleep(secsLeft,nsecsLeft,secsLeft,nsecsLeft) !This is to compensate for 1s sleep being interrupted by SIGALRM
   CALL clearDisplay
enddo

CONTAINS

Subroutine sigwinch(dummy) bind(C)
USE ISO_C_BINDING, ONLY:C_INT
integer(c_int) , intent(in) , value :: dummy
integer :: sigStat,dummy_return
etime_Sim=etime_Sim+1
CALL f_kill(pidSim,SIGWINCH_num,sigStat)
CALL f_alarm(1,dummy_return)
End Subroutine sigwinch

Subroutine progrep_process
real :: execTime,ETAinSecs
integer :: pipeUnit,istat,storeUnit,sigErr

OPEN(NEWUNIT=pipeUnit,FILE=TRIM(ADJUSTL(pipeName)),STATUS='old',ACTION='read',ACCESS='stream',FORM='unformatted',IOSTAT=istat)
if(istat.NE.0)then 
   if(counter==0)then
      PRINT('(A)'),'  Your process is currently paused and hence not responding. Try again later.'
   else
      CALL f_kill(pidSim,0,sigErr)
      if(sigErr/=0)then
         PRINT('(A)'),'  Your process seems to have finished.'
      else
         PRINT('(A)'),'  ERROR: tmpfile unavailable. Re-invoke progrep.'
      endif
   endif
   CALL graceful_exit(0)
endif   
READ(pipeUnit)currStep,totSteps,cpuSecs,execTime
CLOSE(pipeUnit)

if(oldTot/=totSteps)counter=0 !Change in totSteps probably means change in run-parameter hence should reset everything
oldTot=totSteps

CALL sec2HMS(REAL(etime_Sim),goneHrs,goneMins,goneSecs)
CALL sec2HMS(cpuSecs,cpuHrs,cpuMin,cpuSec)

CALL EXECUTE_COMMAND_LINE('ps -p '//pidSim_char//' -o nlwp= > '//storeFile//' 2> /dev/null')
OPEN(NEWUNIT=storeUnit,FILE=storeFile)
READ(storeUnit,'(I6)',IOSTAT=istat)threadNum
if(istat/=0) then
   write(*,*)
   PRINT('(A)'), '  Status: Complete/terminated'
   CALL graceful_exit(0)
endif
CLOSE(storeUnit)

if(counter==0)then
   initTime=execTime
   initStep=currStep
   oldStep=currStep
endif

if ((totSteps.NE.0).AND.(currStep.NE.0))then 
   factor=REAL(currStep)/totSteps
   percentage=NINT(factor*100)
   
   if(currStep.NE.oldStep)then
      currFPS=(currStep-oldStep)/(execTime-oldTime)
      recentFPS=(currStep-initStep)/(execTime-initTime)
      FPS_CPU=(currStep-oldStep)/(cpuSecs-old_cpuSecs)
      ETAinSecs=(totSteps-currStep)/recentFPS
      CALL sec2HMS(ETAinSecs,ETAhrs,ETAmins,ETAsecs)
      cpuUsage=100.*(cpuSecs-old_cpuSecs)/(execTime-oldTime)
   else
      currFPS=0. !For when simulation is not updating the pipe
      FPS_CPU=0.
      cpuUsage=0.
   endif
   oldTime=execTime
   if(counter==0)then
      recentFPS=currFPS
      CALL sec2HMS(0.,ETAhrs,ETAmins,ETAsecs)
   endif
   
   write(*,*)
elseif(currStep==0)then
   if (NINT(execTime-initTime)<5)then
      CALL EXECUTE_COMMAND_LINE('tput -Txterm smso') !Switch on stand-out mode in terminal
      PRINT('(A)'),'  Perhaps your program has not entered its main loop yet'
      CALL EXECUTE_COMMAND_LINE('tput -Txterm rmso') ! Switch off stand-out mode in terminal
   else
      CALL EXECUTE_COMMAND_LINE('tput -Txterm smso') !Switch on stand-out mode in terminal
      PRINT('(A)'),'  Timeout! progrep is quitting. Reinvoke later if necessary'
      CALL EXECUTE_COMMAND_LINE('tput -Txterm rmso') ! Switch off stand-out mode in terminal
      write(*,*)
      write(*,*)
      PRINT('(A)'),'  Either your simulation took too long to initialize'
      write(*,*)
      PRINT('(A)'),'  Or,'
      write(*,*)
      if (totSteps/=0)then
         PRINT('(A)'),'  You did not assign progrep_curr in your code'
      else
         PRINT('(A)'),'  You did not assign progrep_curr and progrep_tot in your code'
      endif   
      CALL graceful_exit(0)
   endif
else
   PRINT('(A)'),'  You did not assign progrep_tot in your code'
   CALL graceful_exit(0)
endif   
End Subroutine progrep_process

SUBROUTINE progrep_display
   CHARACTER (LEN=*) , PARAMETER :: displayFormat='(3A)' ! Declaring String parameter to hold format
   CHARACTER (LEN=60) :: displayBar
   INTEGER :: fullBarSize,i
   INTEGER :: barLength  
   
!   Configure progress bar   
   fullBarSize=LEN(displayBar)
   barLength=NINT(factor*fullBarSize)
   DO i=1,barLength-2
      displayBar(i:i)='='
   ENDDO
   displayBar(barLength-1:barLength)='>>'
   DO i=barLength+1,fullBarSize
      displayBar(i:i)=''
   ENDDO

!  Display progress bar and stats
   PRINT(displayFormat),'  [',displayBar,']'
   write(*,*)
   PRINT('(2X,I3,A,10X,I0,A,I0,1X,A)'),percentage,'% complete',currStep,'/',totSteps,'steps'
   write(*,*)
   PRINT('(2X,A,1X,I3,1X,A,2(1X,I2,1X,A))'),'Elapsed  :',goneHrs,'hrs',goneMins,'mins',goneSecs,'secs'
   write(*,*)
   PRINT('(2X,A,1X,I3,1X,A,2(1X,I2,1X,A),1X,A)'),'Remaining:',ETAhrs,'hrs',ETAmins,'mins',ETAsecs,'secs','(approx.)'
   write(*,*)
   PRINT('(2X,A,3(1X,F7.2,1X,A))'),'FPS:',currFPS,'Current ||',recentFPS,'Recent ||',FPS_CPU,'CPU'
   write(*,*)
   PRINT('(2X,A,1X,I0,10X,A,1X,F5.1,A)'),'# Threads:',threadNum,'CPU/Thread:',cpuUsage/threadNum,'%'
   write(*,*)
END SUBROUTINE progrep_display

Subroutine flash_status
if((currStep.ne.oldStep).AND.(totSteps/=0))then
   if(mod(counter,2)==1)then
      print('(A)'),'  Status: Running'
   else
      write(*,*)' Status: '   
   endif
else
   if(totSteps==0)then
      print('(A)'),'  Status: Unknown'
   else   
         if((cpuSecs-old_cpuSecs)>0.1)then !Even if 10 processes are running concurrently then each will have 0.1 in 1 sec interval  
            if(counter/=0)then
               print('(A)'),'  Status: Speed < 1 FPS'
            else
               print('(A)'),'  Status:'
            endif      
         else
            print('(A)'),'  Status: Paused / Waiting'
            counter=-1 !This is to reset initTime at the next iteration
         endif      
   endif   
endif
write(*,*)            
oldStep=currStep
old_cpuSecs=cpuSecs
End Subroutine flash_status

Subroutine display_endnote
PRINT('(A)'),'  You may close this with Ctrl+C or the X button'
PRINT('(A)'),'  Ctrl+'//achar(92)//' to log this report in your current directory & exit'
End Subroutine display_endnote

Subroutine append_log(dummy) bind(c)
USE ISO_C_BINDING, ONLY:C_INT
integer(c_int), intent(in) , value :: dummy
character(len=*) , parameter :: logFormat='(A,2X,A,I3,A,2(2X,A,I3.2,A,I2.2,A,I2.2),2(2X,A,F7.2),2X,A,1X,I0)'
character(len=8) :: DATE
character(len=6) :: TIME
character(len=256):: intro,record
integer :: logfileUnit,readStat,writeStat

WRITE(intro,'(a,1x,a)')'Started:',TRIM(ADJUSTL(Sim_start_expanded))

CALL EXECUTE_COMMAND_LINE('touch '//TRIM(ADJUSTL(logfileName))) !This is just a workaround for WSL
OPEN(NEWUNIT=logfileUnit,FILE=TRIM(ADJUSTL(logfileName)),STATUS='UNKNOWN',FORM='FORMATTED', ACCESS='SEQUENTIAL', &
ACTION='READ',IOSTAT=readStat)
READ(logfileUnit,'(a)',IOSTAT=readStat)record
CLOSE(logfileUnit)

if((readStat==0).AND.(intro==record))then
   OPEN(NEWUNIT=logfileUnit,FILE=TRIM(ADJUSTL(logfileName)),STATUS='OLD',FORM='FORMATTED', ACCESS='SEQUENTIAL', &
   ACTION='WRITE',POSITION='APPEND',IOSTAT=writeStat)
else
   OPEN(NEWUNIT=logfileUnit,FILE=TRIM(ADJUSTL(logfileName)),STATUS='OLD',FORM='FORMATTED', ACCESS='SEQUENTIAL', &
   ACTION='WRITE',POSITION='REWIND',IOSTAT=writeStat)
   WRITE(logfileUnit,'(a)',IOSTAT=writeStat)TRIM(ADJUSTL(intro))
   WRITE(logfileUnit,'(a,1x,a)',IOSTAT=writeStat)'Commmand:',TRIM(ADJUSTL(Sim_args))
   WRITE(logfileUnit,'(a,1x,a)',IOSTAT=writeStat)'Dir:',TRIM(ADJUSTL(wdir_Sim))
   WRITE(logfileUnit,'(a,1x,a)',IOSTAT=writeStat)'Owner:',TRIM(ADJUSTL(Sim_USER))
   if(mpiRank.ge.0)WRITE(logfileUnit,'(a,1x,I0)',IOSTAT=writeStat)'MPI_Rank:',mpiRank
   WRITE(logfileUnit,'(a)',IOSTAT=writeStat)REPEAT('~',72)
endif
CALL DATE_AND_TIME(DATE,TIME)
WRITE(logfileUnit,logFormat,IOSTAT=writeStat)DATE(7:8)//'/'//DATE(5:6)//'/'//DATE(3:4)//' '//&
&TIME(1:2)//':'//TIME(3:4)//':'//TIME(5:6)//'>>','Completed:',percentage,'%','CPU Time:',cpuHrs,':',cpuMin,':',cpuSec, &
'ETA:',ETAhrs,':',ETAmins,':',ETAsecs,'FPS(Real-time):',currFPS,'FPS(CPU-time):',FPS_CPU,'Threads:',threadNum
CLOSE(logfileUnit)
if(ssh_session)then
    CALL graceful_exit(3)
else
    write(*,*)
    if(writeStat==0)then
        PRINT('(A,1X,A)'),TRIM(ADJUSTL(logfileName)),'has been generated in your current directory.'
    else
        PRINT('(A)'),'FAILED to generate log: No write permission for ./'//TRIM(ADJUSTL(logfileName))
    endif        
    CALL graceful_exit(2)
endif
End Subroutine append_log

Subroutine graceful_exit(sigVal) bind(c)
USE ISO_C_BINDING, ONLY: C_INT
integer(c_int) , intent(in) , value :: sigVal
integer :: dummy_return
CALL f_alarm(0,dummy_return) !Cancels any pending alarm
CALL f_unlink(storeFile,dummy_return)
CALL EXECUTE_COMMAND_LINE('tput -Txterm cnorm') !Bring cursor back to normal

if(isInstalled.AND.default_tmpdir)then
    write(*,*)
    write(*,*)'Warning: @'//TRIM(ADJUSTL(hostNode))
    write(*,*)'progrep could not create tmpfiles in LOCAL_SCRATCH that was provided during progrep installation (in Makefile).'
    write(*,*)'Hence it used /tmp instead.'
endif

if(sigVal==0)then
   write(*,*)
   PRINT('(A)'),'  Press Enter to exit progrep'
   READ*,
elseif(sigVal==3)then
   STOP 3   
endif
STOP
End Subroutine graceful_exit

Subroutine clearDisplay
CALL EXECUTE_COMMAND_LINE('tput -Txterm cuu 17 ; tput -Txterm ed') !Move cursor up and clear whatever is below it
End Subroutine clearDisplay

Subroutine progrep_config
character(len=*) , parameter :: api='progrep_api/',prefix_cmd='export PATH=\$PATH:\$HOME/progrep_installation && progrep '
character(len=*) , parameter :: version='version_info.txt',help='usage.txt',license='COPYING'
character(len=*) , parameter :: modfile='progrep.mod',sample='progrep_sample',ext_f='.f90',ext_c='.c',ext_cpp='.cpp'
character(len=*) , parameter :: header='progrep.h',cheader='cprogrep.h',lib='libprogrep.a' 
character(len=8) :: pidSelf_char
character(len=*) , parameter :: regFmt='(A9)',listFmt='(A8,2X,A14,2X,A20,2X,A31)'
character(len=256) :: regFile,argNode
character(len=14):: pid_rank
logical :: SimExists,wasThere
integer :: argCount,sigStat,pidStat,linkStat,dummy_return,pidSelf_int,storeUnit,regUnit,sigErr,regStat,space,sshEx
integer :: logfileStat,logfileUnit,readStat
character(len=256):: ssh_cmd,sh_cmd,started_remote_log,started_local_log,num_to_char

CALL f_gethostname(hostNode)
pidSelf_int=f_getpid()
WRITE(pidSelf_char,'(I7)')pidSelf_int
WRITE(storeFile,'(a)')TRIM(ADJUSTL(tmpdir))//'/'//'store.'//TRIM(ADJUSTL(pidSelf_char))
CALL f_unlink(storeFile,dummy_return)
argCount=COMMAND_ARGUMENT_COUNT()
if(argCount.NE.0)then
   pidSim_char=REPEAT(' ',LEN(pidSim_char)) !Initialization - blank string
   CALL GET_COMMAND_ARGUMENT(1,pidSim_char)
    
   select case(pidSim_char)
      
    case('list')
     if(argCount>1)then
        if(argCount>2)PRINT('(A)'), 'Showing result for the first node only'
        CALL GET_COMMAND_ARGUMENT(2,argNode)
        ssh_cmd=prefix_cmd//'list'
        sh_cmd='PROGREP="'//TRIM(ssh_cmd)//'"; ssh -qt '//TRIM(ADJUSTL(argNode))//' $PROGREP'
        CALL EXECUTE_COMMAND_LINE(sh_cmd,EXITSTAT=sshEx)
        if(sshEx==255)PRINT('(A)'), 'Fix this first: SSH cannot connect to host: '//TRIM(ADJUSTL(argNode))
        CALL graceful_exit(2)
     else
        WRITE(regFile,'(a)')TRIM(ADJUSTL(tmpdir))//'/'//'register.'//TRIM(ADJUSTL(pidSelf_char))
        CALL EXECUTE_COMMAND_LINE('ls -t -1 '//TRIM(ADJUSTL(tmpdir))//&
                                        &' | grep -v store | grep -v pipe | grep -v register | grep -v log > '//regFile)
        OPEN(NEWUNIT=regUnit,FILE=regFile,STATUS='old',ACCESS='sequential',FORM='formatted',ACTION='read',IOSTAT=regStat)
        if(regStat==0)then
         PRINT('(A,2X,A)'),'Hostname:',TRIM(ADJUSTL(hostNode))
         CALL EXECUTE_COMMAND_LINE('tput -Txterm smso') ! Switch on stand-out mode in terminal
         WRITE(*,listFmt)'Started','PID/MPI_Rank','User','Command Line'
         CALL EXECUTE_COMMAND_LINE('tput -Txterm rmso') ! Switch off stand-out mode in terminal
         do
            READ(regUnit,regFmt,END=1)pidSim_char
            CALL validation(pidSim_char,SimExists)
            if(SimExists)then
                if(mpiRank.ge.0)then
                  WRITE(pid_rank,'(A,I0)')TRIM(ADJUSTL(pidSim_char))//'/',mpiRank
                else
                  WRITE(pid_rank,'(A)')TRIM(ADJUSTL(pidSim_char))
                endif  
                WRITE(*,listFmt)ADJUSTR(Sim_start),ADJUSTR(TRIM(pid_rank)),ADJUSTR(TRIM(Sim_USER)),ADJUSTR(TRIM(Sim_args))
            endif
         enddo   
        endif
1       CLOSE(regUnit,STATUS='delete'); CALL graceful_exit(2)
     endif
    
    case('mod')
        if(isInstalled)then
            CALL f_symlink(TRIM(ADJUSTL(folder))//modfile,modfile,linkStat)
            write(*,*)modfile//' has been created in your current working directory'
        else
            write(*,*)'Sorry cannot help'
        endif
        STOP
    
    case('api')
        if(isInstalled)then
            CALL f_mkdir(api,'777',dummy_return)
            CALL f_symlink(TRIM(ADJUSTL(folder))//modfile,api//modfile,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//header,api//header,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//cheader,api//cheader,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//lib,api//lib,linkStat)
            if(linkStat==0)then
                write(*,*)'Check out '//api//' in the current directory'
            else
                INQUIRE(FILE=api//lib,EXIST=wasThere)
                if(wasThere)then
                    write(*,*)'Come on, progrep_api/ was already there in your current directory !!'
                else
                    write(*,*)'Failed: Permission denied (write-protected directory). cd to elsewhere & retry'
                endif
            endif
        else
            write(*,*)'Sorry cannot help'
        endif        
        STOP
       
    case('sample')
        if(isInstalled)then
            CALL f_symlink(TRIM(ADJUSTL(folder))//sample//ext_f,sample//ext_f,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//sample//'_openmp'//ext_f,sample//'_openmp'//ext_f,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//sample//'_mpi'//ext_c,sample//'_mpi'//ext_c,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//sample//ext_c,sample//ext_c,linkStat)
            CALL f_symlink(TRIM(ADJUSTL(folder))//sample//ext_cpp,sample//ext_cpp,linkStat)
            write(*,*)sample//' has been created in your current working directory'
        else
            write(*,*)'Sorry cannot help'
        endif
        STOP
        
    case('help')
        if(isInstalled)then
            write(*,*)
            CALL EXECUTE_COMMAND_LINE('cat '//TRIM(ADJUSTL(folder))//help)
        else
            write(*,*)'Sorry cannot help'
        endif
        STOP
        
    case('license')
        if(isInstalled)then
            CALL EXECUTE_COMMAND_LINE('less '//TRIM(ADJUSTL(folder))//license)
        else
            write(*,*)'GNU Public License version 3 or later'
            write(*,*)'Available at <https://www.gnu.org/licenses/>'
        endif
        STOP

    case('uninstall')
        if(isInstalled)then
            if(isInstalled_local)then
                CALL EXECUTE_COMMAND_LINE('rm -vrf '//folder)
            else
                CALL f_chdir(TRIM(ADJUSTL(folder)),dummy_return)
                CALL EXECUTE_COMMAND_LINE('make uninstall')
            endif    
        else
            write(*,*)'What is there to uninstall? Have you even installed me properly!'
        endif
        STOP
      
    case default

       READ(pidSim_char,*,IOSTAT=pidStat)pidSim
       if(pidStat.NE.0)then
          PRINT('(1x,a)'),'Error: Option not recognized'
          CALL EXECUTE_COMMAND_LINE('awk ''NR==4, NR==5 {print}'' '//TRIM(ADJUSTL(folder))//help)
          STOP
       endif

     logfileName='progrep.log_'//TRIM(ADJUSTL(pidSim_char))
     if(argCount>1)then
        if(argCount>2)PRINT('(A)'), 'Showing result for the first node only'
        CALL GET_COMMAND_ARGUMENT(2,argNode)
        if(argNode=='progrep_ssh')then
            ssh_session=.TRUE.
            logfileName=TRIM(ADJUSTL(tmpdir))//'/'//TRIM(ADJUSTL(logfileName))
        else
        ssh_cmd=prefix_cmd//TRIM(ADJUSTL(pidSim_char))//' progrep_ssh'
        sh_cmd='PROGREP="'//TRIM(ssh_cmd)//'"; ssh -qt '//TRIM(ADJUSTL(argNode))//' $PROGREP'
        CALL EXECUTE_COMMAND_LINE(sh_cmd,EXITSTAT=sshEx)
            if(sshEx==255)then
                PRINT('(A)'), 'Fix this first: SSH cannot connect to host: '//TRIM(ADJUSTL(argNode))
                CALL graceful_exit(2)
            endif    
            if(sshEx==3)then
                ssh_cmd='cat '//TRIM(ADJUSTL(tmpdir))//'/'//TRIM(ADJUSTL(logfileName))//&
                                                &' && rm '//TRIM(ADJUSTL(tmpdir))//'/'//TRIM(ADJUSTL(logfileName))
                logfileName=TRIM(ADJUSTL(logfileName))//'_'//TRIM(ADJUSTL(argNode))
                sh_cmd='PROGREP="'//TRIM(ssh_cmd)//'"; ssh -qt '//TRIM(ADJUSTL(argNode))//' $PROGREP > '//TRIM(ADJUSTL(storeFile))
                PRINT('(A)'),'Generating log ...'
                CALL EXECUTE_COMMAND_LINE(sh_cmd)
                OPEN(NEWUNIT=storeUnit,FILE=storeFile)
                READ(storeUnit,'(a)')started_remote_log
                CLOSE(storeUnit)
                CALL EXECUTE_COMMAND_LINE('touch '//TRIM(ADJUSTL(logfileName))) !This is just a workaround for WSL
				OPEN(NEWUNIT=logfileUnit,FILE=logfileName,STATUS='old',IOSTAT=readStat)
                READ(logfileUnit,'(a)',IOSTAT=readStat)started_local_log
                CLOSE(logfileUnit)
                if((readStat==0).AND.(started_remote_log==started_local_log))then
                    CALL EXECUTE_COMMAND_LINE('tail -1 '//TRIM(ADJUSTL(storeFile))//' >> '//logfileName,EXITSTAT=logfileStat)
                else
					CALL f_rename(TRIM(ADJUSTL(storeFile)),TRIM(ADJUSTL(logfileName)),logfileStat)
                endif
                write(*,*)
                if(logfileStat==0)then
                    PRINT('(A,1X,A)'),TRIM(ADJUSTL(logfileName)),'has been generated in your current directory.'
                else
                    PRINT('(A)'),'There seems to be some problem regarding write permissions. cd to another directory & retry.'
                endif    
            endif
            CALL graceful_exit(2)
        endif
     endif
       CALL validation(pidSim_char,SimExists)
       if(.NOT.SimExists)then
          write(*,*)'ERROR'
          write(*,*)'Possible Reasons:' 
          write(*,*)'a) You gave the wrong PID' 
          write(*,*)'   (Get proper PID using command: progrep list)' 
          write(*,*)'b) Your simulation has terminated, or, its progrep cookie has been deleted' 
          write(*,*)'   (Is given PID shown with: progrep list)'
          write(*,*)'c) progrep server is not installed for the given process' 
          write(*,*)'   (See sample code for how-to; command: progrep sample)'
          CALL graceful_exit(2)
       endif   
       CALL f_kill(pidSim,0,sigErr)
       if(sigErr/=0)then
          write(*,*)'Permission denied. Invoke progrep as root: sudo progrep <pid>'
          CALL graceful_exit(2)
       endif   
       Sim_USER=TRIM(ADJUSTL(Sim_USER))//'@'//TRIM(ADJUSTL(hostNode))
       CALL sigwinch(0) 
   
       CALL EXECUTE_COMMAND_LINE('ps -p '//pidSim_char//' -o comm= > '//storeFile//' 2> /dev/null')
       OPEN(NEWUNIT=storeUnit,FILE=storeFile)
       READ(storeUnit,'(a)')name_Sim
       CLOSE(storeUnit)
   
       CALL EXECUTE_COMMAND_LINE('tput -Txterm smso ; tput -Txterm civis') !Switch on stand-out mode in terminal & Hide cursor
       CALL two_col_display('Name: '//ADJUSTL(name_Sim),'Started: '//ADJUSTL(Sim_start_expanded),display_width)
       if(mpiRank.ge.0)then
         WRITE(num_to_char,'(a,1X,I0)')'MPI_Rank:',mpiRank
       else
         WRITE(num_to_char,'(a,1X,a)')'MPI_Rank:','N/A'
       endif
       CALL two_col_display('Owner: '//ADJUSTL(Sim_USER),ADJUSTL(num_to_char),display_width)
       CALL two_col_display('Dir: '//ADJUSTL(wdir_Sim),' ',display_width)
       CALL EXECUTE_COMMAND_LINE('tput -Txterm rmso') ! Switch off stand-out mode in terminal
       CALL f_nanosleep(0_INT32,300000000_INT32,secsLeft,nsecsLeft) !Give progrep server a headstart in case SIGWINCH gets blocked
   endselect
else
   if(isInstalled)then
       write(*,*)
       CALL EXECUTE_COMMAND_LINE('awk ''NR==1, NR==5 {print}'' '//TRIM(ADJUSTL(folder))//help)
       write(*,*)
       CALL EXECUTE_COMMAND_LINE('awk ''NR==2, NR==7 {print}'' '//TRIM(ADJUSTL(folder))//version)
       write(*,*)
   else
       write(*,*)'Hello, this is progrep. Please install me properly using: make ; make install or make nonroot'
   endif    
   STOP
endif    
End Subroutine progrep_config

Subroutine validation(pid_char,proceed)
character(*) , intent(in) :: pid_char
logical , intent(out) :: proceed
integer(INT32) :: sessTime
character(len=256) :: sessFile
integer :: sessUnit,sessErr,timeDiff,readErr,etime_Sim_days,etime_Sim_hrs,etime_Sim_mins,etime_Sim_secs,storeUnit,rmErr
character(len=11) :: parse
sessFile=TRIM(ADJUSTL(tmpdir))//'/'//TRIM(ADJUSTL(pid_char))
OPEN(NEWUNIT=sessUnit,FILE=sessFile,STATUS='old',ACCESS='stream',FORM='unformatted',ACTION='readwrite',IOSTAT=sessErr)
if (sessErr/=0) then !This means that the given process does not have the progrep server installed
   proceed=.false.
   return
endif   
WRITE(pipeName,'(a)')TRIM(ADJUSTL(tmpdir))//'/'//'pipe.'//TRIM(ADJUSTL(pid_char))
READ(sessUnit)mpiRank,sessTime,wdir_Sim,Sim_USER,Sim_start_expanded
timeDiff=int(f_time()-sessTime,kind(timeDiff))
CALL EXECUTE_COMMAND_LINE('ps -p '//pid_char//' -o etime= > '//storeFile//' 2> /dev/null')
OPEN(NEWUNIT=storeUnit,FILE=storeFile)
READ(storeUnit,'(a)',IOSTAT=readErr)parse
CLOSE(storeUnit)
if(readErr/=0) then !This means that the given pid is inactive
   proceed=.false.
   CLOSE(sessUnit,STATUS='delete')
   CALL f_unlink(pipeName,rmErr)
   return
endif   
READ(parse,'(4(I2,1x))')etime_Sim_days,etime_Sim_hrs,etime_Sim_mins,etime_Sim_secs
etime_Sim=etime_Sim_days*86400+etime_Sim_hrs*3600+etime_Sim_mins*60+etime_Sim_secs
if(timeDiff-etime_Sim.gt.1) then !This means that sessFile belongs to an old simulation (now inactive) with same pid
   proceed=.false.
   CLOSE(sessUnit,STATUS='delete')
   CALL f_unlink(pipeName,rmErr)
   return
endif

proceed=.true.
CLOSE(sessUnit)
CALL EXECUTE_COMMAND_LINE('ps -p '//pidSim_char//' -o args= > '//storeFile//' 2> /dev/null')
OPEN(NEWUNIT=storeUnit,FILE=storeFile)
READ(storeUnit,'(a)')Sim_args
CLOSE(storeUnit)
CALL EXECUTE_COMMAND_LINE('ps -p '//pidSim_char//' -o start= > '//storeFile//' 2> /dev/null')
OPEN(NEWUNIT=storeUnit,FILE=storeFile)
READ(storeUnit,'(a)')Sim_start
CLOSE(storeUnit)

RETURN
End Subroutine validation   

Subroutine sec2HMS(time_in_secs,hrs,mins,secs)
real , intent(in) :: time_in_secs
integer, intent(out) :: hrs, mins, secs
hrs=INT(time_in_secs/3600)
mins=INT(time_in_secs/60-hrs*60.)
secs=mod(NINT(time_in_secs),60) 
End Subroutine sec2HMS

Subroutine two_col_display(string1,string2,display_width)
implicit none
character(len=*), intent(in) :: string1,string2
integer, intent(in) :: display_width
integer:: space
character(len=6) :: format_specifier

WRITE(format_specifier,'(A,I0,A)')"(A",display_width,")"
space=display_width-LEN_TRIM(ADJUSTL(TRIM(string1)//ADJUSTL(string2)))
if(space.le.0)space=1
WRITE(*,format_specifier)TRIM(ADJUSTL(string1))//REPEAT(' ',space)//TRIM(ADJUSTL(string2))
End Subroutine two_col_display
End Program progrep
