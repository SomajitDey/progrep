/* 
This is to illustrate the 4 step process of integrating progrep with your MPI C source code.

For MPMD (a single-simulation with parallel processes spread over multiple cores/nodes), think of this as the code for the 
master process that tracks the timestep of your simulation.

For SPMD (embarassingly parallel simulations with different run-parameters/initializations), think of this as the code for your sim.

Here, just to make the program computationally expensive, we generate 250K random numbers for 100K steps (for each MPI process).

To keep things simple, no message passing has been done.

Also see instructions at the end of this file for how to use progrep to track progress/status/stats when running this code.
*/


#include <progrep.h> //Step 1 - Include progrep header for C code. For C++, replace progrep.h with cprogrep.h

#include <mpi.h>
#include <stdlib.h>
#include <time.h>

void main(int argc, char *argv[])
{
// # Launch the MPI processes #
   MPI_Init(&argc,&argv);

// # Initialization & stuff #
   srand(time(0));
   int r,i,j,nstep,tid;
   nstep=100000;
   MPI_Comm_rank(MPI_COMM_WORLD, &tid);
   
   progrep_init(&nstep, &tid); //Step 2 - Initialize progrep server. ALTERNATIVE: progrep_init(&nstep, NULL); progrep_rank=tid;

// # Main loop begins #
   for(i=1; i<=nstep; i++)
   {

      progrep_curr=i; //Step 3 - Assign progrep_curr the current step or iteration number inside main loop

      for (j=1; j<=250000; j++)
      {      
         r=rand();
      }   
   }   
// # Main loop ends #
   MPI_Finalize();
   
   progrep_term(); //Step 4 - Initialize progrep server
}

// # COMPILATION #
// mpicc -o runme progrep_sample_mpi.c -lprogrep -lgfortran  # Compile code and link with progrep and gfortran libraries      
//                                                             (assuming progrep was built using gfortran) 

// # EXECUTE #
// mpirun -np 2 runme &                                      # Replace 2 with desired number of parallel processes

// # TRACK SIMULATION #
// progrep list                                              # Lists Linux PID of master(for MPMD) or each(for SPMD) MPI process
// progrep <PID>
