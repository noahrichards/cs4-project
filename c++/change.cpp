#include <iostream>
#include <vector>
#include <functional>
#include "Change.h"
#include "Solver.h"
using namespace std;

int main(int argc, char** argv)
{
	// Ignore first argument (program name)
	argc--;
	argv++;

	if(argc < 2)
	{
		cerr << "Usage:\n"
		     << "change <amount> <denomination1> [denominations...]\n"
			 << endl;
		exit(1);
	}

	// Grab desired amount
	int amt = atoi(argv[0]);

	argc --;
	argv ++;

	// Grab all denominations of coins
	vector<int> denominations;

	while(argc)
	{
		stringstream s(argv[0]);
		int denomination(0);
		s >> denomination;
		if(s.fail() || denomination < 1)
		{
			cerr << "Invalid argument: " << argv[0] << endl;
			exit(1);
		}

		denominations.push_back(denomination);

		argv++;
		argc--;
	}

	assert(!denominations.empty());

    // Put the denominations in descending order, so we take the biggest
    // steps first and include the biggest jumps at the beginning of the 
    // solution.
    sort(denominations.begin(), denominations.end(), greater<int>());

	Change c(amt, denominations);
	solver::solve(c);
	return 0;
}
