#include <iostream>
#include <fstream>
#include "Lloyd.h"
#include "Solver.h"
using namespace std;

int main(int argc, char** argv)
{
	if(argc != 3)
	{
		cerr << "Illegal number of arguments.\n"
		     << "Usage:\n"
		     << "lloyd <input-file> <output-file>\n"
			 << "If input-file is -, input taken from standard input.\n"
			 << "If output-file is -, output directed to standard out.\n"
			 << endl;
		exit(1);
	}

	ifstream infile;
	ofstream outfile;
	istream* in;
	ostream* out;

	if(!strcmp(argv[1], "-"))
		in = &cin;
	else
	{
		infile.open(argv[1]);
		if(!infile)
		{
			cerr << "Error opening input file: " << argv[1] << endl;
			exit(1);
		}
		in = &infile;
	}

	if(!strcmp(argv[2], "-"))
		out = &cout;
	else
	{
		outfile.open(argv[2]);
		if(!outfile)
		{
			cerr << "Error opening output file: " << argv[2] << endl;
			exit(1);
		}
		out = &outfile;
	}


	Lloyd l(*in);
	solver::solve(l, *out);
	return 0;
}
