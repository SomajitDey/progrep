! This is to illustrate the 4 step process of integrating progrep with your Fortran source code.
! Think of this as the program or subroutine that hosts the main loop of your simulation.
! Here, just to make the program computationally expensive, we generate 500x500 random numbers for 100K steps.

Program test
USE progrep ! Step 1 - Include progrep API

implicit none
integer , parameter :: NStep=100000
integer :: i
REAL :: r(500,500)

 CALL progrep_init ! Step 2 - Initialize progrep server. ALTERNATIVE: CALL progrep_init(NStep)

! ONLY IF NStep HAS NOT BEEN PASSED IN STEP 2: Assign progrep_tot the total number of iterations before entering main loop
progrep_tot=NStep 

! # MAIN LOOP BEGINS #
do i=1,NStep
	progrep_curr=i ! Step 3 - Assign progrep_curr (integer kind) the current step or iteration number inside main loop
	CALL RANDOM_NUMBER(r)
enddo
! # MAIN LOOP ENDS #

CALL progrep_term ! Step 4 - Terminate progrep server.

End Program test

! # COMPILATION #
! progrep mod                                         # Installs progrep.mod in the build directory
! gfortran -o runme progrep_sample.f90 -lprogrep      # Compile code and link with static library libprogrep.a
! nohup ./runme &                                     # Batch mode (Access progrep with: progrep <pid>)
! ./runme &                                           # Terminal mode (Access progrep with: progrep <pid>)
! ./runme progrep                                     # Interactive mode (Access progrep with: Ctrl+C)
