#pragma once
#include "datastructs.hpp"
#include <filesystem>
#include <fstream>
#include <string_view>
#include <string>
using TVctCellInput= std::vector<CellInput>;

inline TVctCellInput readContentFile (const std::string &sFile)
{
TVctCellInput ret;
std::string line;
std::ifstream infile(sFile);
while (std::getline(infile, line))
{
    std::istringstream iss(line);
    int a, b;
    if ((iss >> a >> b)) {
        auto c=CellInput{a,b};
        ret.emplace_back(c);
    }


}
return ret;
}

inline Cell fillCell(const CellInput &x)
{
    return Cell{x.p,Value{x.v}};
}

inline Cell fillCandidate(int i)
{
//    auto c=Candidates (sizeTbl);
//    std::iota(c.begin(),c.end(),1);
    auto c = ranges::view::take(ranges::view::ints(1), sizeTbl);
    return Cell{i,Value{c}};
}

inline Grid fill(const TVctCellInput &xs)
{

const auto lng = ranges::size(xs);
auto ret= Grid(lng);
ranges::transform(xs,
                  ranges::begin(ret),[](auto &o)->Cell
{
   return fillCell(o);
});
return ret;
}
inline bool isFilled(int i,const Grid &t)
{
    const auto lng = ranges::size(t);
    auto idxs= TVctInt(lng);
    ranges::transform(t,ranges::begin(idxs),[](auto &o)->int
    {
       return o.idx;
    });

    auto it = ranges::find(idxs, i);
    if(it == ranges::end(idxs))
        return false;
    else
        return true;

}


inline std::optional<Cell> paddingCell(int i,const Grid &t)
{
    auto ret=std::optional<Cell>();
if (false==isFilled(i,t))
    ret=fillCandidate(i);
return ret;

}

inline Grid padding( const TVctInt &xs,const Grid &t)
{
    const auto lng = ranges::size(xs);
    auto t0= GridOpt(lng);
    ranges::transform(xs,
                      ranges::begin(t0),
                      [t=t](auto &i)->std::optional<Cell>
    {
       return paddingCell(i,t);
    });
    auto t1= catMaybes(t0);

    ranges::copy(t,ranges::back_inserter(t1));
    assert(ranges::size(t1)==sizeTbl*sizeTbl);
    return t1;
}

inline Grid toGrid(const TVctCellInput &c)
{

    const auto t0 = fill(c);
    auto t1= padding(indexes(),t0);
    ranges::sort(t1,[](auto &x,auto &y)->bool
    {
        return x.idx < y.idx;
    });

    assert(t1[0].idx==0);
    assert(t1[sizeTbl*sizeTbl-1].idx==sizeTbl*sizeTbl-1);
    return cleanGrid(t1);
}

