#pragma once
#include "datastructs.hpp"
template <typename F>
auto flip( F &&f) { return std::bind(f,std::placeholders::_2,std::placeholders::_1); }

template <typename F, typename T, typename C>
T foldr( F &&f,const  T &z, C &&c) {

    return ranges::accumulate(c, z,flip(f));

}

inline void print (const Table &t)
{
    for (auto &o:t)
    {
        auto idx = o.idx;
        std::cout << "idx= "<< idx ;
        switch (o.val.index())
        {
        case 0:
        {
            auto fil=std::get<0>(o.val);
            std::cout << " ,F= "<<fil;
        }
            break;
        case 1:
        {
            auto lst=std::get<1>(o.val);
            std::cout << " ,Cand= [" ;
            bool bFst=true;
            for (auto &x:lst)
            {
                if(false==bFst)
                    std::cout <<", ";
                std::cout <<x;
                bFst=false;
            }
            std::cout <<"]";

        }
            break;

        }
        std::cout <<std::endl;
    }
}


inline void print (const Node &n)
{
    std::cout << "Node:\n";
    assert(n.solutions.size()==1);
    print (n.solutions[0]);
    //std::cout << "size contenders :" << n.contenders.size()<< std::endl;
}

inline Table rplCell(const Cell &newCell, const Table &t)
{
    auto ret=t;
    const auto i = newCell.idx;
    ret[i]=newCell;
    return ret;
}


inline Table rplTable(const Table &news, const Table &t)
{
    auto ret= t;
    return foldr(rplCell,ret,news);

}


inline Cell cleanCandidate(const Cell &pivot,const Cell &c )
{
    auto ret =Cell();
    const auto v= c.val;
    const auto a= pivot.val;
    switch(a.index())
    {

    case 0:
    {
        const auto p=std::get<0>(a);
        switch(v.index())
        {

        case 0:
            ret=c;
            break;
        case 1:
            auto x = std::get<1>(v);
            auto it=ranges::partition(x,[p=p](auto &o)
            {
                return o!=p;
            });
            auto aa={TVctInt(x.begin(),it)};
            ret = Cell{c.idx,{TVctInt(x.begin(),it)}};
            break;
        }
    }
        break;
    case 1:
        ret=c;
        break;
    }
    return ret;
}

inline Table cleanRegion(const Cell &p,const Table &reg )
{
    const auto lng = ranges::size(reg);
    auto r= Table(lng);
    ranges::transform(reg,ranges::begin(r),
                      [p=p](auto &o)->Cell
    {
        return cleanCandidate(p,o);
    });
    return r;
}
inline Neighbours cleanNeighbours (const Neighbours &n)
{
    const auto p= n.pivot;
    const auto r=cleanRegion(p,n.rows);
    const auto c=cleanRegion(p,n.cols);
    const auto b=cleanRegion(p,n.block);
    return Neighbours{p,r,c,b};
}


inline TVctInt indexesRow(int r)
{
    //indexesRow r = [size*r+x| x<-[0..size - 1 ]]

    auto ct = ranges::view::take(ranges::view::ints(0), sizeTbl);

    auto row= ct | ranges::view::for_each([r=r](int x)
    {
        return ranges::yield(sizeTbl*r+x);
    });
    return row;
}

inline TVctInt indexesCol(int c)
{

    //indexesCol c = [x*size+c| x<-[0..size -1 ]]
    auto ct = ranges::view::take(ranges::view::ints(0), sizeTbl);
    auto col= ct | ranges::view::for_each([c=c](int x)
    {
        return ranges::yield(sizeTbl*x+c);
    });
    return col;


}

inline TVctInt indexesBlock(int b)
{
    //indexesBlock b = [b+size*x+y| x<-[0.. sqrtsize-1], y<-[0.. sqrtsize-1]]

    auto pc = ranges::view::take(ranges::view::ints(0), sqrtsize);
    const auto pairs=ranges::view::cartesian_product (pc,pc);

    auto block=TVctInt();

    ranges::transform(pairs,ranges::back_inserter(block),
                      [b=b](auto && p)->int
    {

        return (b+std::get<0>(p)*sizeTbl+std::get<1>(p));
    }
    );
    return block;

}

inline TVctInt upperLeftCorners()
{
    // all indexes for top left corner for a block  =[sqrtsize*size*x+ sqrtsize*y| x<-[0.. sqrtsize-1], y<-[0.. sqrtsize-1]]
    auto pc = ranges::view::take(ranges::view::ints(0), sqrtsize);
    const auto pairs=ranges::view::cartesian_product (pc,pc);

    auto block=TVctInt();

    ranges::transform(pairs,ranges::back_inserter(block),
                      [](auto && p)->int
    {

        return (std::get<0>(p)*sqrtsize*sizeTbl+sqrtsize*std::get<1>(p));
    }
    );
    return block;
}

inline std::vector<TVctInt> allRegions()
{
    //lsRows= map (\x -> indexesRow x )  [0..size -1]
    std::vector<TVctInt> lsRows,lsCols,lsBlocks;
    const auto ct = ranges::view::take(ranges::view::ints(0), sizeTbl);
    ranges::transform(ct,ranges::back_inserter(lsRows),
                      [](auto && p)->TVctInt
    {

        return indexesRow(p);
    }
    );

    ranges::transform(ct,ranges::back_inserter(lsCols),
                      [](auto && p)->TVctInt
    {

        return indexesCol(p);
    }
    );


    const auto block=upperLeftCorners();
    ranges::transform(block,ranges::back_inserter(lsBlocks),
                      [](auto && p)->TVctInt
    {

        return indexesBlock(p);
    }
    );


    ranges::copy(lsCols,ranges::back_inserter(lsRows));
    ranges::copy(lsBlocks,ranges::back_inserter(lsRows));
    return lsRows;
}


//valOfCell :: Int ->[Cell]->Maybe Int
inline std::optional <int> valOfCell ( int i,const Table& t )
{
    auto res=std::optional <int>();
    if(0==t[i].val.index())
    {
        res=std::get<0>(t[i].val);
    }
    return res;
}

//valRegion::[Int]->[Cell]->[Int]
inline TVctInt valRegion (const TVctInt &is,const Table &t)
{
    auto temp=TVctIntOpt();
    ranges::transform(is,ranges::back_inserter(temp),
                      [t=t](auto && i)->std::optional <int>
    {

        return valOfCell(i,t);
    }
    );
    return catMaybes(temp);
}
//valRow::Int->[Cell]->[Int]
inline TVctInt valRow ( int r,const Table &t)
{
    return valRegion(indexesRow(r),t);
}

inline TVctInt valCol ( int c,const Table &t)
{
    return valRegion(indexesCol(c),t);
}


inline TVctInt valBlock ( int b,const Table &t)
{
    return valRegion(indexesBlock(b),t);
}

//return a list of values (list of ints) in a reagion
//valsInRegions::( Int->[Cell]->[Int] )-> [Int]-> [Cell]-> [[Int]]

template <typename F>
inline std::vector<TVctInt> valsInRegions(F &&f,const TVctInt& xs,const Table & table )
{
    auto temp=std::vector<TVctInt> ();
    ranges::transform(xs,ranges::back_inserter(temp),
                      [t=table,f=f](auto && x)->TVctInt
    {

        return f(x,t);
    }
    );
    return temp;

}


inline bool hasDuplicates(const TVctInt &xs)
{
    auto tmp=xs;
    ranges::sort(tmp);
    auto un=xs |ranges::view::unique;
    auto l1=ranges::size(tmp);
    auto l2=ranges::size(TVctInt{ranges::begin(un),ranges::end(un)});
    if (l1!=l2)
        return true;
    else
        return false;

}


//check if the region has duplicates used in foldl
//checkRegion ::Bool->[Int]->Bool

inline bool checkRegion (bool b,const TVctInt& xs)
{
    return b && ! hasDuplicates(xs);
}


//checkTable::[Cell]->Bool
inline bool checkTable (const Table& t)
{
    const auto ct = ranges::view::take(ranges::view::ints(0), sizeTbl);

    auto lsRows= valsInRegions( valRow ,ct ,t   );
    auto br=ranges::accumulate( lsRows,true,  checkRegion);

    auto lsCols= valsInRegions( valCol ,ct ,t   );
    auto bc=ranges::accumulate( lsRows,true,  checkRegion);

    auto block=upperLeftCorners();
    auto lsBlocks= valsInRegions (valBlock ,block, t);
    auto bb=ranges::accumulate( lsBlocks,true,  checkRegion);

    return br && bc && bb;
}


//check to see if the cell is filled
//cellFilled :: Bool->Cell->Bool

inline bool cellFilled(bool b,const Cell &c)
{
    if(c.val.index()==0)
        return b;
    else
        return false;
}


//check to see if the table is filled
//tableFilled::[Cell]->Bool
inline bool tableFilled(const Table &t)
{
    return ranges::accumulate(t,true,cellFilled);
}

//check to see if  we've found the solution
//isSolved :: [Cell] -> Bool
inline bool isSolved(const Table &t)
{
    return tableFilled(t) && checkTable(t);
}


inline Pos getPos(int i)
{
    assert(0<=i && i < sizeTbl*sizeTbl);

    return Pos{i/sizeTbl,i%sizeTbl};
}

//return a list of cells given the list of indexes
//getCells :: [Int] -> [Cell]-> [Cell]
inline Table getCells (const TVctInt &xs, const Table &t)
{
    //getCells  xs t = map (\x-> t!!x) xs

    auto temp=Table ();
    ranges::transform(xs,ranges::back_inserter(temp),
                      [t=t](auto &&i)->Cell
    {
        return t[i];
    }
    );
    return temp;

}


//getRows::Int->[Int]
inline TVctInt getRows(int idx)
{
    const auto p=getPos(idx);
    auto line = indexesRow(p.row);
    auto it=ranges::partition(line,[idx=idx](auto &o)
    {
        return o!=idx;
    });
    return TVctInt(ranges::begin(line),it);

}


inline TVctInt getCols(int idx)
{
    const auto p=getPos(idx);
    auto line = indexesCol(p.col);
    auto it=ranges::partition(line,[idx=idx](auto &o)
    {
        return o!=idx;
    });
    return TVctInt{ranges::begin(line),it};

}


inline TVctInt getBlock(int idx)
{
    const auto p=getPos(idx);

    const auto a1 = p.row  / sqrtsize;
    const auto a2 = p.col  / sqrtsize;
    const auto ul =a1*sqrtsize*sizeTbl+ a2*sqrtsize ;//upper left corner

    auto line = indexesBlock(ul);
    auto it=ranges::partition(line,[idx=idx](auto &o)
    {
        return o!=idx;
    });
    return TVctInt{ranges::begin(line),it};

}
inline Table neighboursCells(const Neighbours &n)
{
    auto ret= n.rows;
    ranges::copy(n.cols,ranges::back_inserter(ret));
    ranges::copy(n.block,ranges::back_inserter(ret));
    return ret;
}
//neighboursCells n = (rows n) ++ (cols n) ++ (block n)


//getNeighbours::Cell->[Cell]->Neighbours
inline Neighbours getNeighbours(const Cell &c,const Table &t)
{
    const auto p1 = c.idx;


    return Neighbours {c, getCells (getRows (p1),t),
                getCells (getCols (p1),t) ,
                getCells (getBlock (p1),t)
    };
}

//validCell::Bool->Cell->Bool
//return false if the list of candidates is empty
inline bool validCell(bool b, const Cell &c)
{
    if(c.val.index()==1)
    {
        const auto x= std::get<1>(c.val);
        if(x.empty())
            return false;
    }


    return b;
}

//validTable::[Cell]->Bool
inline bool validTable(const Table &t)
{
    if (t.size () ==sizeTbl*sizeTbl)
        return ranges::accumulate(t,true,validCell);
    return false;
}

//cleanCell::Cell->[Cell]->[Cell]
inline Table cleanCell(const Cell &c,const Table &t)
{
    const auto cs = neighboursCells (cleanNeighbours (getNeighbours (c, t)));
    return rplTable (cs ,t);
}

//cleanTable::[Cell]->[Cell]
//cleanTable t = foldr cleanCell t t
inline Table cleanTable(const Table &t )
{
    auto ret= t;
    return  foldr(cleanCell,ret,t);
    return ret;

}

//candidatesRegion::[Int]->[Cell]->[Int]
inline TVctInt candidatesRegion(const TVctInt &r, const Table &t)
{
    auto tmp1=valRegion( r, t );

    ranges::sort(tmp1);
    TVctInt tmp2;
    tmp2.resize(sizeTbl);
    std::iota(tmp2.begin(), tmp2.end(), 1);

    TVctInt diff;
    std::set_difference(std::begin(tmp2),std::end(tmp2),
                        std::begin(tmp1),std::end(tmp1),
                        std::back_inserter(diff)
                        );

    return diff;
}


//addIndexCandidat::Int->[Cell]->Int->Maybe Int
inline std::optional<int> addIndexCandidat(int r,const Table &t, int v)
{
    auto res = std::optional<int>();
    const auto c = t[r];

    if (1==c.val.index())
    {
        auto l = std::get<1>(c.val);
        auto it=ranges::find(l,v);
        if (it != ranges::end(l))
            res = c.idx;

    }
    return res;
}
//indexCandidatRegion::[Int]->[Cell]->Int->[Int]
inline TVctInt indexCandidatRegion(const TVctInt &rs,const Table &t, int v)
{
    auto temp= TVctIntOpt();
    ranges::transform(rs,ranges::back_inserter(temp),
                      [t=t,v=v](auto && i)->std::optional <int>
    {

        return addIndexCandidat(i,t,v);
    }
    );
    auto ret=catMaybes(temp);
    return ret;
}
//indexesCandidates :: [Int]->[Cell]->[CandidatIndexes]
inline TVctCandidatIndexes indexesCandidates(const TVctInt &r,const Table &t )
{
    const auto cd = candidatesRegion (r, t);
    auto temp= TVctCandidatIndexes();
    ranges::transform(cd,ranges::back_inserter(temp),
                      [r=r,t=t](auto && x)->CandidatIndexes
    {

        return CandidatIndexes{x,indexCandidatRegion(r,t,x)};
    }
    );
    return temp;
}

//functor from foldl a->b->a
//a=TVctCandidatIndexes
//b=TVctInt
//the extra parameter will be "curryed"
inline TVctCandidatIndexes joinCandidates(const TVctCandidatIndexes &lst,const TVctInt &r ,const Table &t)
{
    auto tmp=indexesCandidates(r,t );
    ranges::copy(lst,ranges::back_inserter(tmp));
    return tmp;
}

//allCandidatIndexes:: [[Int]]->[Cell]->[CandidatIndexes]
inline TVctCandidatIndexes allCandidatIndexes(const std::vector<TVctInt> &xs , const Table &t)
{
    auto res=TVctCandidatIndexes();
    auto curryjoinCandidates= std::bind(joinCandidates,std::placeholders::_1,std::placeholders::_2,t);
    auto ll =ranges::accumulate (xs,res,curryjoinCandidates) ;
    ranges::sort(ll,[](auto &x, auto &y)->bool
    {
        return x.idxs.size()< y.idxs.size();
    });
    return ll;
}
//uniqCandidate :: [CandidatIndexes] -> Maybe CandidatIndexes

inline std::optional<CandidatIndexes> uniqCandidate (const TVctCandidatIndexes &x)
{
    auto res=std::optional<CandidatIndexes>();
    if (x.size() > 0)
    {
        auto f=x[0];
        if(f.idxs.size()==1)
            res=f;
    }
    return res;
}


//toContenders:: Cell->[Cell]
inline Table toContenders(const Cell &c)
{
    auto res=Table();
    if (c.val.index()==1)
    {
        const auto tmp = std::get<1>(c.val);
        ranges::transform(tmp,ranges::back_inserter(res),
                          [c=c](auto &x)->Cell
        {
            return Cell{c.idx,x};
        });
    }
    return res;
}
//backToNode:: Node->Node
inline Node backToNode(const Node &n)
{
    const auto ct = n.contenders;
    if (ct.size()==0)
    {
        std::cout << "No solution\n";
        exit(1);
    }

    auto tmp=std::vector<Table>(ct.begin()+1,ct.end());
    return Node{ct[0],tmp};
}


//nextNode::Node->a->Node
inline Node nextNode(const Node &n,const CandidatIndexes &dc )
{
    const auto newcell= Cell{ dc.idxs[0],dc.candidate};
const auto t =cleanTable (rplCell( newcell, n.act ));
return Node {t,n.contenders};

}

inline Node nextNode(const Node &n,int i )
{
    auto res =n;
    const auto c= n.act[ i];
    if(1==c.val.index())
    {
        auto l= std::get<1>(c.val);
        const auto ct = toContenders(c);
        const auto a = cleanTable (rplCell (ct[0], n.act ) );

        auto tail=Table(ct.begin()+1,ct.end());
        std::vector<Table> tmp;
        ranges::transform(tail,ranges::back_inserter(tmp),
                          [n=n](auto &&x)->Table
        {
            return cleanTable ( rplCell (x ,n.act ));
        });

        ranges::copy(n.contenders,ranges::back_inserter(tmp));
        res = Node {a,tmp};
    }

    return res;
}

//buildOrdCandidates :: Cell -> OrdCandidates
inline OrdCandidates buildOrdCandidates(const Cell &c)
{
    auto res=OrdCandidates();
    switch(c.val.index())
    {
    case 0:
        res=OrdCandidates{c.idx,0};
        break;
    case 1:
    {
        auto x = std::get<1>(c.val);
        res=OrdCandidates{c.idx,x.size()};
    }
        break;

    }

    return res;
}

//orderedCandidates :: [Cell]-> [OrdCandidates]
inline VctOrdCandidates orderedCandidates(const Table &t)
{
    auto tmp=VctOrdCandidates();
    ranges::transform(t,ranges::back_inserter(tmp),buildOrdCandidates);
    auto it = ranges::partition(tmp,[](auto &x)->bool
    {
        return x.lng!=0;
    });

    auto ret=VctOrdCandidates(tmp.begin(),it);
    ranges::sort(ret,[](auto &x, auto &y)->bool
    {
        return x.lng < y.lng;
    });
    return ret;
}
//backtracking :: Node-> Node
inline void backtracking(const Node &n)
{
    auto t = n.act;
    if (isSolved(t))
    {
        std::cout << "solved:\n";
        print(n);
        //print(t);
        //exit(1);
        return ;
    }
    auto oc = orderedCandidates( t);
    if(oc.size()==0)
    {
        std::cout <<"backtracking -> backToNode\n";
        backtracking(backToNode(n));
    }
    else
    {

        const auto reg = allRegions();
        const auto uq= uniqCandidate(allCandidatIndexes(reg,t));
        if(!uq)
        {
            const auto i= oc[0].indx;
            //std::cout <<"orderedCandidates,idx= "<< i << " length: "<<  oc[0].lng<< std::endl;
            backtracking(nextNode(n,i));
        }
        else
        {
            //std::cout <<"uniqueCandidate,idx= "<< (*uq).idxs[0] << std::endl;
            backtracking(nextNode(n,*uq));
        }
    }
}
//unitNode:: [Cell] -> Node
inline Node unitNode(const Table &t)
{
    return Node {t ,{},{}};
}

inline Node returnNode(const Table &t)
{
    return Node {t ,{},{t}};
}


//guard::Node ->Bool
inline bool guard(const Node &n)
{
    if (0==n.act.size() && 0==n.contenders.size())
        return true;
    else
        return false;

}


//nextNode::Node->a->Node
inline Node cellToNode(const Table &t,const CandidatIndexes &dc )
{
    const auto newcell= Cell{ dc.idxs[0],dc.candidate};
const auto t1 =cleanTable (rplCell( newcell, t ));
if (validTable (t1))
   return unitNode (t1);
else
   return unitNode (t);

}

inline Node cellToNode(const Table &t,int i )
{
    auto res=unitNode(t);
    const auto c= t[ i];
    if(1==c.val.index())
    {
        auto l= std::get<1>(c.val);
        const auto ct = toContenders(c);
        std::vector<Table> ts1;
        ranges::transform(ct,ranges::back_inserter(ts1),
                          [t=t](auto &&x)->Table
        {
            return cleanTable ( rplCell (x ,t ));
        });

        auto it = ranges::partition(ts1,[](auto &&x)->bool
        {
            return validTable ( x );
        });

        auto ts2 = std::vector<Table>(ts1.begin(),it);
        if(ts2.size() > 0)
        {
            auto tail = std::vector<Table>(ts2.begin()+1,ts2.end());
            res=Node{ts2[0],tail,{}};
    }
}

return res;
}


//contBind::[Cell]->Node
inline Node contBind(const Table & t)
{
    if (isSolved (t) == true)
       return returnNode(t);
    else
    {
        auto oc = orderedCandidates( t);
        if(oc.size()==0)
        {

            return unitNode(t);
        }
        else
        {

            const auto reg = allRegions();
            const auto uq= uniqCandidate(allCandidatIndexes(reg,t));
            if(!uq)
            {
                const auto i= oc[0].indx;
                //std::cout <<"orderedCandidates,idx= "<< i << " length: "<<  oc[0].lng<< std::endl;
                return cellToNode(t,i);
            }
            else
            {
                //std::cout <<"uniqueCandidate,idx= "<< (*uq).idxs[0] << std::endl;
                return cellToNode(t,*uq);
            }
        }


    }
}

template <typename F>
inline Node bind (const Node &n, F &&f)
{
    auto comp = f (n.act);

    ranges::copy(n.solutions,ranges::back_inserter(comp.solutions));
    if (comp.act==n.act)
    {
        if (n.contenders.size() > 0)// backtracking   , continues with the head of contenders list
        {
    //        std::cout << "backtracking\n";
            auto head = n.contenders[0];
            auto tail = std::vector<Table>(n.contenders.begin()+1,n.contenders.end());
            return Node {head,tail,comp.solutions};

        }
        else
        {
//            std::cout << "end search\n";
            return Node {{},{},comp.solutions};
        }
    }
    else
    {
  //      std::cout << "continues\n";
        ranges::copy(n.contenders,ranges::back_inserter(comp.contenders));
        return Node {comp.act,comp.contenders,comp.solutions};
    }
}


//solve ::Node-> Node
inline Node solve(const Node &n)
{
    const auto cont = bind (n,contBind);
    if (guard(cont))
        return cont;
    else
       return solve (cont);

}
