#include <iostream>
#include <chrono>
#include "cell.hpp"
#include "input.hpp"

using namespace std;
using namespace std::chrono;
Node solve(const TVctCellInput &contents)
{
    high_resolution_clock::time_point t1 = high_resolution_clock::now();
    auto t=toTable(contents);
    const auto startNode=Node{t,{},{}};
    auto res=solve(startNode);

    high_resolution_clock::time_point t2 = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>( t2 - t1 ).count();
    cout <<"---duration milisesconds:"<< duration/1000<< std::endl;
    return res;
}

int main(int argc, char **argv)
{
if (argc !=2)
{
    std::cout << "Invalid arguments numbers\n. Missing path file!";
    exit (1);
}
    std::cout << "File name: " << argv[1]<<"\n";
//"/usr/local/ssd/projects/Labor/Sudoku/Inputs/expert01.txt"
    const auto contents= readContentFile(argv[1]);
    auto res=solve(contents);
    print(res);
    return 0;
}
