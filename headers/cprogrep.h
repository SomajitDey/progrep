// Include this header file in your C++ source code to avail progrep utility

extern "C" {
   int progrep_tot, progrep_curr,progrep_rank;
   void progrep_init(int *nsteps, int *mpi_rank);
   void progrep_term(void);
}
