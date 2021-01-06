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
! This module contains wrappers for some user-land C system calls. This 
! module is meant to be used for making your Fortran code portable. Some
! compilers (e.g. gfortran, ifort) provide their own (i.e. non-standard) 
! extensions for these system calls.
!
! These module procedures and interfaces basically bind to functions in 
! the glibc library that ships with Linux.

! SIGSEGV RUNTIME ERROR:
! Any such error possibly arises from type mismatch when porting from 32-bit to 
! 64-bit. E.g. equating INT32 with C_LONG variable type. C_LONG can take both 32-bit
! and 64-bit form depending on the implementation.
!************************************************************************

!************************************************************************
! TODO:
! Replace trim() and len_trim() with trim(adjustl()) & len_trim(adjustl()).
! Make dummy arguments optional wherever possible by making use of present().
! Give dummy arguments meaningful names that will be used as keywords.
!************************************************************************


module syscall
use iso_c_binding
use iso_fortran_env
implicit none
private
public :: f_signal,f_alarm,f_chmod,f_getpid,f_rename,f_sleep,f_kill,f_unlink,f_symlink,f_link,f_getcwd,f_time,f_nanosleep, &
f_gethostname,f_mkdir,f_rmdir,f_chdir

type, bind(c) :: timespec
   integer(c_long) :: tv_sec
   integer(c_long) :: tv_nsec
end type

interface
   function f_getpid() bind(c,name='getpid')
   import :: c_int
   integer(c_int) :: f_getpid
   end function f_getpid
end interface   

contains

subroutine f_signal(signum,handler)
integer , intent(in) :: signum
type(c_funptr) :: iret
type(c_funptr) :: c_handler

interface
   subroutine handler(signum) bind(c)
   import :: c_int
   implicit none
   integer(c_int) , intent(in) , value :: signum
   end subroutine handler

   function c_signal(signal, sighandler) bind(c,name='signal')
   import :: c_int,c_funptr 
   implicit none
   integer(c_int) , value , intent(in) :: signal
   type(c_funptr), value , intent(in) :: sighandler
   type(c_funptr) :: c_signal
   end function c_signal
end interface

c_handler=c_funloc(handler)
iret=c_signal(signum,c_handler)
end subroutine f_signal

subroutine f_alarm(seconds,iret)
integer , intent(in) :: seconds
integer,  intent(out) :: iret

interface
   function c_alarm(sec) bind(c,name='alarm') result(ret)
   import :: c_int
   integer(c_int) , value :: sec
   integer(c_int) :: ret
   end function c_alarm
end interface

iret=c_alarm(seconds)
end subroutine f_alarm

subroutine f_chmod(fname,mode,iret)
character(len=*), intent(in) :: fname,mode
integer , intent(out) :: iret
interface
   function c_chmod(fname,mode) bind(c,name='chmod') result(ret)
   import :: c_char,c_int,c_long 
   character(c_char) , dimension(*) , intent(in) :: fname
   integer(c_long) , value :: mode
   integer(c_int) :: ret
   end function c_chmod
   
   function c_strtol(string,nullchar,base) bind(c,name='strtol') result(ret)
   import :: c_long,c_char,c_int
   character(c_char) , dimension(*) :: string,nullchar
   integer(c_int) , value :: base
   integer(c_long) :: ret
   end function c_strtol
end interface
character(len=len_trim(fname)+1) :: c_fname
character(len=len_trim(mode)+1) :: c_string
integer(c_long) :: c_mode
character(len=2)::nullchar

nullchar='0'//c_null_char
c_string=trim(mode)//c_null_char
c_mode=c_strtol(c_string,nullchar,8_c_int)
c_fname=trim(fname)//c_null_char
iret=c_chmod(c_fname,c_mode)
end subroutine f_chmod

subroutine f_rename(oldname,newname,iret)
character(len=*) , intent(in) :: oldname,newname
integer,  intent(out) :: iret
interface
   function c_rename(oldf,newf) bind(c,name='rename') result(ret)
   import :: c_char,c_int 
   character(c_char) , dimension(*) , intent(in) :: oldf,newf
   integer(c_int) :: ret
   end function c_rename
end interface
character(len=len_trim(oldname)+1) :: c_oldname
character(len=len_trim(newname)+1) :: c_newname

c_oldname=trim(oldname)//c_null_char
c_newname=trim(newname)//c_null_char
iret=c_rename(c_oldname,c_newname)
end subroutine f_rename

subroutine f_sleep(seconds,iret)
integer , intent(in) :: seconds
integer,  intent(out) :: iret
interface
   function c_sleep(sec) bind(c,name='sleep') result(ret)
   import :: c_int
   integer(c_int) , value :: sec
   integer(c_int) :: ret
   end function c_sleep
end interface   

iret=c_sleep(seconds)
end subroutine f_sleep

subroutine f_kill(pid,signal,iret)
integer , intent(in) :: pid,signal
integer,  intent(out) :: iret
interface
   function c_kill(pid,sig) bind(c,name='kill') result(ret)
   import :: c_int
   integer(c_int) , value :: pid,sig
   integer(c_int) :: ret
   end function c_kill
end interface   

iret=c_kill(pid,signal)
end subroutine f_kill

subroutine f_unlink(path,iret)
character(len=*) , intent(in) :: path
integer ,  intent(out) :: iret
interface
   function c_unlink(pathname) bind(c,name='unlink') result(ret)
   import :: c_char,c_int
   character(c_char) , dimension(*) :: pathname
   integer(c_int) :: ret
   end function c_unlink
end interface
character(len=len_trim(path)+1) :: c_path

c_path=trim(path)//c_null_char
iret=c_unlink(c_path)
end subroutine f_unlink

subroutine f_link(path,link,iret)
character(len=*) , intent(in) :: path,link
integer,  intent(out) :: iret
interface
   function c_link(path1,path2) bind(c,name='link') result(ret)
   import :: c_char,c_int 
   character(c_char) , dimension(*) , intent(in) :: path1,path2
   integer(c_int) :: ret
   end function c_link
end interface
character(len=len_trim(path)+1) :: c_path1
character(len=len_trim(link)+1) :: c_path2

c_path1=trim(path)//c_null_char
c_path2=trim(link)//c_null_char
iret=c_link(c_path1,c_path2)
end subroutine f_link

subroutine f_symlink(path,link,iret)
character(len=*) , intent(in) :: path,link
integer,  intent(out) :: iret
interface
   function c_symlink(path1,path2) bind(c,name='symlink') result(ret)
   import :: c_char,c_int 
   character(c_char) , dimension(*) , intent(in) :: path1,path2
   integer(c_int) :: ret
   end function c_symlink
end interface
character(len=len_trim(path)+1) :: c_path1
character(len=len_trim(link)+1) :: c_path2

c_path1=trim(path)//c_null_char
c_path2=trim(link)//c_null_char
iret=c_symlink(c_path1,c_path2)
end subroutine f_symlink

subroutine f_getcwd(str)
character(len=*) :: str
integer :: endChar
interface
   subroutine c_getcwd(buff,n) bind(c,name='getcwd')
   import :: c_char,c_size_t
   character(c_char) , intent(out) :: buff(*)
   integer(c_size_t) , value, intent(in) :: n
   end subroutine c_getcwd
end interface

str=repeat(' ',len(str))
call c_getcwd(str,len(str,kind=c_size_t))
endChar=len_trim(str)
str(endChar:endChar)=' ' !Because C returns a null-terminated string, we remove c_null_char with a blank character
end subroutine f_getcwd

function f_time()
integer(int32) :: f_time
integer(c_long) ::f_time_c_long 
interface
   subroutine c_time(tloc) bind(c,name='time')
   import :: c_long
   integer(c_long) :: tloc
   end subroutine c_time
end interface
call c_time(f_time_c_long)
f_time=int(f_time_c_long,kind(f_time))   
end function f_time

subroutine f_nanosleep(s,ns,rem_s,rem_ns)
integer(int32) , intent(in) :: s,ns
integer(int32) , intent(out) :: rem_s,rem_ns
type(timespec) :: f_req,f_rem
interface
   subroutine c_nanosleep(req,rem) bind(c,name='nanosleep')
   import :: timespec
   type(timespec) :: req,rem   
   end subroutine c_nanosleep   
end interface

f_req%tv_sec=int(s,kind=c_long)
f_req%tv_nsec=int(ns,kind=c_long)
call c_nanosleep(f_req,f_rem)
rem_s=int(f_rem%tv_sec,kind=int32)
rem_ns=int(f_rem%tv_nsec,kind=int32)
end subroutine f_nanosleep

subroutine f_gethostname(str)
character(len=*) :: str
integer :: endChar
interface
   subroutine c_gethostname(buff,n) bind(c,name='gethostname')
   import :: c_char,c_size_t
   character(c_char) , intent(out) :: buff(*)
   integer(c_size_t) , value, intent(in) :: n
   end subroutine c_gethostname
end interface

str=repeat(' ',len(str))
call c_gethostname(str,len(str,kind=c_size_t))
endChar=len_trim(str)
str(endChar:endChar)=' ' !Because C returns a null-terminated string, we remove c_null_char with a blank character
end subroutine f_gethostname

subroutine f_mkdir(fname,mode,iret)
character(len=*), intent(in) :: fname,mode
integer , intent(out) :: iret
interface
   function c_mkdir(fname,mode) bind(c,name='mkdir') result(ret)
   import :: c_char,c_int,c_long 
   character(c_char) , dimension(*) , intent(in) :: fname
   integer(c_long) , value :: mode
   integer(c_int) :: ret
   end function c_mkdir
   
   function c_strtol(string,nullchar,base) bind(c,name='strtol') result(ret)
   import :: c_long,c_char,c_int
   character(c_char) , dimension(*) :: string,nullchar
   integer(c_int) , value :: base
   integer(c_long) :: ret
   end function c_strtol
end interface
character(len=len_trim(fname)+1) :: c_fname
character(len=len_trim(mode)+1) :: c_string
integer(c_long) :: c_mode
character(len=2)::nullchar

nullchar='0'//c_null_char
c_string=trim(mode)//c_null_char
c_mode=c_strtol(c_string,nullchar,8_c_int)
c_fname=trim(fname)//c_null_char
iret=c_mkdir(c_fname,c_mode)
end subroutine f_mkdir

subroutine f_rmdir(fname,iret)
character(len=*), intent(in) :: fname
integer , intent(out) :: iret
interface
   function c_rmdir(fname) bind(c,name='rmdir') result(ret)
   import :: c_char,c_int 
   character(c_char) , dimension(*) , intent(in) :: fname
   integer(c_int) :: ret
   end function c_rmdir
end interface
character(len=len_trim(fname)+1) :: c_fname

c_fname=trim(fname)//c_null_char
iret=c_rmdir(c_fname)
end subroutine f_rmdir

subroutine f_chdir(fname,iret)
character(len=*), intent(in) :: fname
integer , intent(out) :: iret
interface
   function c_chdir(fname) bind(c,name='chdir') result(ret)
   import :: c_char,c_int 
   character(c_char) , dimension(*) , intent(in) :: fname
   integer(c_int) :: ret
   end function c_chdir
end interface
character(len=len_trim(fname)+1) :: c_fname

c_fname=trim(fname)//c_null_char
iret=c_chdir(c_fname)
end subroutine f_chdir
end module syscall
