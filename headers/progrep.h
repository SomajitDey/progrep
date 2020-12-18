// Include this header file in your C source code to avail progrep utility

extern int progrep_tot, progrep_curr,progrep_rank;
extern void progrep_init(int *nsteps, int *mpi_rank);
extern void progrep_term(void);
