/*
 * Lloyd.h
 *
 * This header contains the declaration of the Lloyd puzzle class,
 *
 * Author: Noah Richards
 *
 * $Id: Lloyd.h,v 1.3 2006/04/10 15:50:42 noah Exp $
 */


#ifndef LLOYD_H
#define LLOYD_H

#include <vector>
#include <iostream>
#include <sstream>
#include "Puzzle.h"
using namespace std;

class Lloyd : public Puzzle<vector<char> >
{
public:
	typedef vector<char> config_type;

	// Exceptions
	class StreamException {};
	class InvalidStreamException : public std::exception, public StreamException
	{
		const char* what() const throw()
		{
			return "Input stream passed to constructor is not valid.\n";
		}
	};

	class InvalidDataException : public std::exception, public StreamException
	{
		const char* what() const throw()
		{
			return "Input stream data is invalid.\n";
		}
	};

	Lloyd(istream& in);

	vector<config_type> getNeighbors(const config_type&) const;
	bool isSolution(const config_type&) const;
	config_type getStartConfig() const;
	string stringFromConfig(const config_type&) const;

private:

	inline int intFromRC(int row, int col) const
	{
		return row * _cols + col;
	};

	inline void swap(config_type& cfg, int to, int from) const
	{
		int temp = cfg[to];
		cfg[to] = cfg[from];
		cfg[from] = temp;
	};

	
	config_type _start;
	config_type _end;
	int _rows;
	int _cols;
};

Lloyd::Lloyd(istream& in) :
	_rows(0), _cols(0)
{
	if(!in)
		throw InvalidStreamException();

	in >> _cols >> _rows;

	_start.resize(_rows * _cols);
	_end.resize(_rows * _cols);

	for(int r = 0; r < _rows; r++)
	{
		for(int c = 0; c < _cols; c++)
		{
			in >> _start[intFromRC(r, c)];
		}
	}

	for(int r = 0; r < _rows; r++)
	{
		for(int c = 0; c < _cols; c++)
		{
			in >> _end[intFromRC(r, c)];
		}
	}

	if(!in)
		throw InvalidDataException();
}



vector<Lloyd::config_type> Lloyd::getNeighbors(const config_type& cfg) const
{
	vector<config_type> neighbors;

	// For each 0 in the vector, move it around
	for(int r = 0; r < _rows; r++)
	{
		for(int c = 0; c < _cols; c++)
		{
			int index = intFromRC(r, c);

			if(cfg[index] != '.')
				continue;

			// Move up
			if(r != 0)
			{
				config_type next = cfg;
				swap(next, index, intFromRC(r - 1, c));
				neighbors.push_back(next);
			}

			// Move down
			if(r != _rows - 1)
			{
				config_type next = cfg;
				swap(next, index, intFromRC(r + 1, c));
				neighbors.push_back(next);
			}

			// Move left
			if(c != 0)
			{
				config_type next = cfg;
				swap(next, index, intFromRC(r, c - 1));
				neighbors.push_back(next);
			}

			// Move right
			if(c != _cols - 1)
			{
				config_type next = cfg;
				swap(next, index, intFromRC(r, c + 1));
				neighbors.push_back(next);
			}
		}
	}

	return neighbors;
}	

bool Lloyd::isSolution(const config_type& cfg) const
{
	return cfg == _end;
}

Lloyd::config_type Lloyd::getStartConfig() const
{
	return _start;
}

string Lloyd::stringFromConfig(const config_type& cfg) const
{
	stringstream s;
	for(int r = 0; r < _rows; r++)
	{
		for(int c = 0; c < _cols; c++)
		{
			s << cfg[intFromRC(r, c)] << " ";
		}
		s << "\n";
	}
	return s.str();
}

#endif

/* Change log:
 *
 * $Log: Lloyd.h,v $
 * Revision 1.3  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.2  2006/03/27 04:59:04  noah
 * Replaced Solver class with single templated function.
 * Created a templated Puzzle base class for the templated function
 * to use, which formalizes requirements on the puzzle types and
 * allows the templated solve function to guess its arguments.
 *
 * Revision 1.1.1.1  2006/03/27 00:52:30  noah
 * CS4 Project
 *
 * Revision 1.3  2006/01/28 05:12:49  noah
 * Change Puzzle
 *
 * Revision 1.2  2006/01/26 18:01:00  noah
 * Modified to take characters instead of integers
 *
 * Revision 1.1  2006/01/26 04:49:35  noah
 * Initial revision
 */
