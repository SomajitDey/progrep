// This is to illustrate the 4 step process of integrating progrep with your C++ source code.
// Think of this as the program or function that hosts the main loop of your simulation.
// Here, just to make the program computationally expensive, we generate 250K random numbers for 100K steps.


#include <cprogrep.h> //Step 1 - Include progrep API (NOTE: For C code, header will be progrep.h)

#include <stdlib.h>
#include <time.h>

int main()
{
   srand(time(0));
   int r,i,j,total_steps;
   total_steps=100000;

   progrep_init(&total_steps,NULL); //Step 2 - Initialize progrep server. ALTERNATIVE: progrep_init(NULL,NULL);

// ONLY IF FIRST PARAMETER IN STEP 2 IS NULL - Assign progrep_tot the total number of iterations before entering main loop as:
//   progrep_tot=total_steps;

// # Main loop begins #
   for(i=1; i<=total_steps; i++)
   {

      progrep_curr=i; //Step 3 - Assign progrep_curr the current step or iteration number inside main loop

      for (j=1; j<=250000; j++)
      {      
         r=rand();
      }   
   }
// # Main loop ends #

   progrep_term(); //Step 4 - Terminate progrep server  

   return 0;
}

// # COMPILATION #
// g++ -o runme progrep_sample.c -lprogrep -lgfortran  # Compile code and link with progrep and gfortran libraries (assuming prog-      
//                                                     rep was built using gfortran) 
// nohup ./runme &                                     # Batch mode (Access progrep with: progrep <pid>)
// ./runme &                                           # Terminal mode (Access progrep with: progrep <pid>)
// ./runme progrep                                     # Interactive mode (Access progrep with: Ctrl+C)
