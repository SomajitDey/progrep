
# WHAT IS *progrep*

**progrep (= progRESS+repORT)** is a utility to show live progress, status and 
stats of a running simulation or any compute job that executes a given number
of iterations.

*progrep* can report for both serial (single-core) and parallel (multi-core/multi-node -
e.g. OpenMP/MPI) jobs. *progrep* is cluster-friendly and can access jobs running on a 
remote machine.

*progrep* comes in two parts - an executable or command (client) + an API library (server).
*progrep* command can only report for those jobs that have the server installed through the
API. Installation of the server in your source code takes only 4 extra lines (described below).
To eliminate run-time dependencies, the server/API library is static. Thus, once built/compiled
on a system with *progrep* installed, your program would run on another system even if *progrep*
is not installed there; only there would be no *progrep* client to take advantage of the linked
server. 

*progrep* is lightweight and does not interfere with or slow down your simulation/job
in any significant way. The *progrep* server is installed in your code merely as a
handler for the SIGWINCH signal. The *progrep* command, when invoked, queries this 
server as a client by sending a SIGWINCH signal at 1s intervals. The corresponding 
overhead is insignificant. When the command is not invoked, your simulation 
is not even aware of the *progrep* server and hence remains unaffected.

__Copyright (C) 2020 Somajit Dey__
Department of Physics, University of Calcutta;
Email: <somajit@users.sourceforge.net>

**progrep is free software**: you can redistribute it and/or modify it
under the terms of the **GNU General Public License** as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.
  
progrep is distributed in the hope that it will be useful, but
**WITHOUT ANY WARRANTY**; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
  
You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.



# HOW TO BUILD/INSTALL/UNINSTALL *progrep*

**Installation:** 

- Download and extract (command: tar xzf) the progrep tarball (.tar.gz).

- cd to the extracted directory.

- If needed, set the first 3 variables in the customizable section of the 
   **Makefile** provided. Then use the command:

		make

    and follow the on-screen instructions.

**Other relevant commands:**

	make clean		# cleaning any previous build
   
	make		# building progrep.mod + libprogrep.a + progrep
   
	make install		# installation
   
	progrep uninstall		# uninstallation
   
Check your installed version with command:

	progrep		# invoke progrep

**Note:** If you have a non-admin user account, you cannot install progrep
with *make install*, as above. If *progrep* is not already installed, it's best
to ask your system administrator to install the latest version of *progrep*.
However, if that is not possible, then you can install *progrep* for your 
exclusive use with the command: 

	make nonroot 	# do this only as last resort
	
System-wide installation with *make install* is the most desirable as *progrep* 
functions best that way. In any case, for *progrep* to access tasks running on
a remote node, *progrep* needs to be installed (with *make install* or, atleast
*make nonroot*) in that node.

In case *progrep* has been installed with both *make install* and *make nonroot*,
only the system-wide installation is taken into account.



# HOW TO INTEGRATE *progrep* WITH YOUR CODE

**progrep API** consists of just two function/procedure calls and three global variables.

The following **4 steps** are for integration with Fortran/C/C++ code.

[If your code is in Julia, Python etc., please write your own wrapper. Also consider 
contributing the same to the progrep code-base (see end-of-file).]

**Step 1: Include module or header** 

FORTRAN -

	USE progrep

C -

	#include <progrep.h>

C++ -

	#include <cprogrep.h>
   
**Step 2: Initialize progrep server before main loop**

Below, nsteps=total number of steps or iterations.

mpi_rank may be ignored (FORTRAN) or passed as NULL (C/C++) for non-MPI jobs.

FORTRAN -

	CALL progrep_init(nsteps=,mpi_rank=)	! Arguments are optional & of default integer kind 

C/C++ - 

	progrep_init(int *nsteps, int *mpi_rank);	// Parameters may be NULL

**Global variable assignment before entering the main loop**

If arguments/parameters were not passed in Step 2 above, then

	progrep_tot = total number of simulation steps or iterations (default integer kind);

Also, for MPI jobs:

	progrep_rank = MPI_Rank (default integer kind);

**Step 3: Global variable assignment inside the main loop**

	progrep_curr = serial number(index) of current step (iteration) (default integer kind);

**Step 4: Terminate progrep server after main loop**

FORTRAN -

	CALL progrep_term

C/C++ - 

	progrep_term();

See progrep_sample codes for illustration.
To get the sample code in your current working directory, use command: progrep sample

**When building executable from your code, follow these simple steps:**

   > (*For Fortran only*) Make sure the file 'progrep.mod' is in the same directory as your source code. 
   *To get the .mod file in your current working directory, use command*: progrep mod
   
   > Link the static library libprogrep.a with -lprogrep
   
   > (*For C/C++ only*) Assuming progrep was built with gfortran, link the gfortran library with -lgfortran

**Example**- Compile the sample codes provided in the samples directory:
   
	gfortran -o runme samples/progrep_sample.f90 -lprogrep
	
	gcc -o runme samples/progrep_sample.c -lprogrep -lgfortran
	
	g++ -o runme samples/progrep_sample.cpp -lprogrep -lgfortran

	gfortran -o runme_omp samples/progrep_sample.f90 -lprogrep -fopenmp

	mpicc -o runme_mpi samples/progrep_sample_mpi.c -lprogrep -lgfortran

**Note:** If *progrep* has been installed properly (i.e. using *make install* or *make nonroot*), then 
*progrep.h*, *cprogrep.h* and *libprogrep.a* should be in the default search path of your system. 
However, if needed, the header files and the library can also be accessed using the command:

	progrep api 	# create sub-directory progrep_api containing soft-links to progrep headers, library and modfile



# HOW TO CHECK PROGRESS REPORT OF YOUR SIMULATION/JOB

This section describes the key uses of **progrep command**. Help on the command can be accessed anytime with:

	man progrep

	progrep help

Get the process ID (PID) of your simulation/job with command:
 
	progrep list	#List PID of all active simulations that have the progrep server installed.
					#An empty list means all such simulations/jobs are complete/terminated.
						  
	progrep list | grep <user name>		#Lists only those jobs that are run by the specified user

	progrep list | grep <name of simulation>	#Lists only those jobs that have the specified name

With this PID, invoke progrep using the command line:

	progrep <PID>

**Example**-

	./runme &    # or: ./runme_omp ; or: mpirun -np <#> runme_mpi
 
	progrep list
   
	progrep <PID of runme>
   
**Cluster Jobs / Remote Computing:**

- For batch jobs submitted via a cluster workload manager (such as SLURM, PBS/Torque etc.), 
get the list of nodes allocated to your job, from the node-list file whose name is stored in
the appropriate environment variable, such as: $PBS_NODEFILE ; $SLURM_JOB_NODELIST ;
$PE_HOSTFILE (for SGE).

- Assuming progrep is installed (using *make install* or *make nonroot*) on a remote node, you 
can then simply use,

		progrep list [user@]nodename	#Show PID of simulations running on the given node
	
		progrep <PID> [user@]nodename	#Report for a node-local PID
	
progrep accesses the given node through SSH. It therefore, asks for the login password, if 
password-less login has not been setup. (To set up password-less login, use the following 
commands in succession: ssh-keygen ; ssh-copy-id user@nodename).

**Example**-

	progrep list localhost
	
	progrep <PID of runme> $USER@localhost

**Note:** To run commands in remote hosts, *progrep* executes the command-line

	ssh -qt [user@]nodename command

Therefore, it will be affected by changes in ~/.ssh/config or /etc/ssh/ssh_config
You may thus use these files to configure how the command:

	progrep [list|<PID>] [user@]nodename

behaves. For example, you may set ports in the remote node that ssh shall connect to.
Or, you may set an alias for a bunch of hostnames so that *progrep list alias*
will show progrep-aware processes running in those nodes (password-less login needs
to be set up for this).
See SSH_CONFIG(5) [command: man ssh_config] for details on SSH settings.



# COMMAND LINE ARGUMENTS WHILE EXECUTING YOUR SIMULATION/JOB

When executing your simulation from a terminal in the foreground, you can optionally provide the 
*progrep* argument that is recognized by the progrep server installed in your code. *This argument 
can be given in any order with respect to any other argument that your program might take. DO NOT 
use this argument for MPI jobs.*

**Argument option:** 

	progrep		---enables interactive mode. progrep can be invoked simply by pressing Ctrl and C together

**Example**: 

	./runme progrep
		 


# FRAMES PER SECOND (FPS) and TIME REMAINING (ETA)

progrep shows three types   of FPS: Current, Recent and CPU. 

**Current** shows average FPS sampled over the last 1 second duration. 

**Recent** shows average FPS since progrep was invoked. 

**CPU** shows average FPS sampled over total cpu time (as opposed to wall-clock time) for the last 1s (wall-clock) duration. 

*Two competing algorithms may be compared for speed (performance) in terms of the Current FPS and/or the CPU FPS.* 

**ETA**, or the time remaining for the simulation to complete, is estimated based on the Recent FPS and the 
number of frames remaining. The estimate is reasonably accurate only when the simulation is expected to 
run with almost uniform speed, i.e. if Current FPS does not fluctuate widely with time.



# STATS & STATUS

*progrep* shows the following for a running process:

**Percentage Completed**

**Time Elasped** (Wall-clock time elapsed since process called progrep_init)

**ETA**

**FPS**

**Number Of Threads** (Useful for simulations using openmp, posix threads etc.)

**Mpi_Rank (If Any)** (Useful for MPI simulations)

**Cpu Usage (%)** (Per thread for multithreaded processes)

**Current Status Of Your Simulation** (Running, Puased/Waiting, Complete/Terminated)

**Start Time & Date**

**Name Of The Process**

**User@Hostname Under Which Process Is Running**

**Working Directory Of The Process** (Shows current working directory of the process when it called progrep_init)

In addition, the logfile (see below) includes the following information:

**Command-Line Used To Launch The Process**

**Total Cpu Time Taken** (Maybe useful in evaluating scaling benefits for multithreaded processes)

If more features are desired, feel free to send a feature request (see at the end of this file).



# LOGFILE

progrep's report can be logged against time-stamp with the **hotkey**: Ctrl+\ (i.e. press Ctrl and \ keys together)

In the log file, the total processor time taken so far by the simulation is provided. 
For multi-threaded (parallel) simulations, this time roughly corresponds to its single-threaded (serial) counterpart.

*The logfile is created in the current working directory of the invoker and is titled progrep.log_



# BUG-REPORT / QUERY / FEEDBACK / CONTRIBUTION

**Please report bugs at** <somajit@users.sourceforge.net> with the subject line: 
	
	progrep <version number>
	
Submit feature requests with the subject line: 

	progrep feature

**For any query** related to this software, email with the subject line: 

	progrep query <version number>

Users' feedback is the developers' reward. If progrep helps you in your work, kindly drop a line at the above-mentioned email address. Constructive criticism is also welcome.

**To contribute** to the code-base, please refer to notice.txt in the src directory.
