#include <iostream>
#include <sstream>
#include "Clock.h"
#include "Solver.h"
using namespace std;

int main(int argc, char** argv)
{
	if(argc != 4)
	{
		cerr << "Invalid number of arguments.\n"
		        "Usage:\n"
		        "clock <max-hour> <start-hour> <end-hour>" << endl;
		exit(1);
	}
	
	stringstream args;
	args << argv[1] << " " << argv[2] << " " << argv[3];

	int maxhour(0), starthour(0), endhour(0);
	args >> maxhour >> starthour >> endhour;

	if(args.fail() || maxhour < 1 || starthour < 1 || endhour < 1)
	{
		cerr << "Invalid arguments.  All arguments "
		     << "must be positive integers."
		     << endl;
		exit(1);
	}
	if(starthour > maxhour || endhour > maxhour)
	{
		cerr << "Invalid arguments.  Start and end "
		     << "hours must be in the range [1,maxhours]."
		     << endl;
		exit(1);
	}


	Clock c(maxhour, starthour, endhour);
	solver::solve(c);
	return 0;
}
