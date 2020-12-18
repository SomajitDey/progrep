! This is to illustrate the 4 (+1 for scope declaration) step process of integrating progrep with your OpenMP Fortran source code.
! Think of this as the program or subroutine which forks into OpenMP threads that execute the main loop of your simulation.
! Here, just to make the program computationally expensive, we compute 250K time-consuming functions for 100K times.
!#####################################################################################################################

program test
use progrep ! STEP 1 - Include progrep API
implicit none
integer , parameter :: NStep=100000,NElements=250000
integer :: i=1,j,k
real :: r

 call progrep_init(NStep) ! STEP 2 - INITIALIZE progrep SERVER.

! ### MAIN LOOP BEGINS ###

!$omp parallel num_threads(2) default(private) shared(i,progrep_curr) ! BEING A GLOBAL VARIABLE, progrep_curr MUST BE SHARED
do while(i.lt.NStep)

! ### BEGINNING OF SOME EXPENSIVE COMPUTATION-- DISTRIBUTED OVER OpenMP THREAD(S) ###

   !$omp do
   do j=1,Nelements
      k=i*j
      r=sin(exp(sqrt(k/2.)))
   enddo
   !$omp end do

! ### END OF EXPENSIVE COMPUTATION. BELOW, ONLY A SINGLE THREAD UPDATES THE STEP COUNTER, WHILE THE OTHER THREAD(S) WAIT ###

   !$omp single
      progrep_curr=i ! STEP 3 - ASSIGN progrep_curr (DEFAULT INTEGER KIND) THE CURRENT STEP OR ITERATION NUMBER INSIDE MAIN LOOP
      i=i+1
   !$omp end single
enddo
!$omp end parallel

! ### MAIN LOOP ENDS ###

CALL progrep_term() ! STEP 4 - TERMINATE progrep SERVER.

end program test

!#####################################################################################################################
! ### COMPILATION ###
! progrep mod                                                 ### Installs progrep.mod in the build directory
! gfortran -o runme progrep_sample.f90 -lprogrep -fopenmp     ### Compile code and link with static library libprogrep.a 
!                                                             ### (Remove -fopenmp above for single-threaded version)
! nohup ./runme &                                             ### Batch mode (Access progrep with: progrep <pid>)
! ./runme &                                                   ### Terminal mode (Access progrep with: progrep <pid>)
! ./runme progrep                                             ### Interactive mode (Access progrep with: Ctrl+C)
