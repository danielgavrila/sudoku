#include <iostream>
#include <chrono>
#include "cell.hpp"
#include "input.hpp"

using namespace std;
using namespace std::chrono;
void solve(const TVctCellInput &contents)
{
    high_resolution_clock::time_point t1 = high_resolution_clock::now();
    auto t=toTable(contents);
    const auto startNode=Node{t,{}};
    backtracking(startNode);
    high_resolution_clock::time_point t2 = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>( t2 - t1 ).count();
    cout <<"---duration milisesconds:"<< duration/1000<< std::endl;
}

int main()
{



    const auto contents= readContentFile("/usr/local/ssd/projects/Labor/Sudoku/Inputs/expert01.txt");
    solve(contents);
    return 0;
}
