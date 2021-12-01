#include <iostream>
#include <vector>
#include <utility>
#include <set>

using namespace std;

typedef pair<pair<int,char>,int> transition; //qi,x -> qj
typedef vector<set<int> > part;              //partition = list of sets

int getSetInd(int, part);                    //get the index of the partition in which an element is
void show(part);                             //print a partition
void showset(set<int>);                      //print a show

struct DFA          //structure to represent DFA
{
    int n;      //no. of states
    int a;      //no. of accept states
    int k;      //no. of transitions

    set<int> f;                //set of accept states
    vector<transition> delta;  //list of transitions

    DFA (int, int, int, set<int>, vector<transition>); //constructor

    int distinguish (int, int, part);   //checks if two states are distinguishable according to partition
    int to_state(int, char);            //looks up destination state in delta

    part get_partition();               //repeatedly partitions Q until all partitions have only equivt states
    void reduce();                      //calls get_partition() and creates new DFA

    void showDFA();                     //print all information related to the DFA
};

DFA::DFA (int nQ, int nT, int nA, set<int> accept, vector<transition> trans)
{
    n = nQ;
    a = nA;
    k = nT;
    f = accept;                   //assignments
    delta.resize(k);
    delta = trans;
}

int DFA::distinguish (int qi, int qj, part p)
{
    //cout << "    Checking between " << qi << " and " << qj << endl;

    int f = 0, q1, q2;
    for (char s = 'a'; s <= 'z'; s++)                               //for each symbol in alphabet
    {
        q1 = to_state(qi,s); q2 = to_state(qj,s);                   //get destination sets
        //cout << "    Symbol " << s << endl;
        //cout << "    qi to " << q1 << ", qj to " << q2 << endl;
        for (int i = 0; i < p.size(); i++)
            if ((p[i].count(q1) != 0) !=                            //and check if they are
                (p[i].count(q2) != 0))                              //in the same partition
            {
                //cout << "      Discr in set with " << *(p[i].begin()) << endl;
                f = 1;                                              //if they are not
                break;                                              //flag and break
            }
      if (f == 1)
        {
            //cout << "    Discr with symbol " << s << endl;
            break;
        }
    }
    //if (f == 0) cout << "    No discr" << endl;
    return f;
}

int DFA::to_state(int q, char x)
{
    pair<int,char> key = make_pair(q,x);
    for (int i = 0; i < k; i++)         //iterate through delta
        if (delta[i].first == key)      //looking for q,x
            return (delta[i].second);   //return
    return -1;
}

part DFA::get_partition ()
{
    set<int> nonacc;                //find out all
    for (int i = 0; i < n; i++)     //non-accepting states
    {
        if (f.count(i) == 0)
            nonacc.insert(i);
    }
    //cout << "Nonacc: ";
    //for (set<int>::iterator it = nonacc.begin(); it != nonacc.end(); it++) cout << *it << " "; cout << endl;

    part partition;
    partition.push_back(nonacc); partition.push_back(f);        //create partition with
    //cout << "Partition size = " << partition.size() << endl;  //accepting and non-accepting states

    /* The partition is treated as a queue of sets.
       An element is popped from the front (beginning)
       and if it can be divided, the new sub-
       partitions are pushed into the back (end).
       A variable keeps track of how many partitions
       have been iterated over without any change;
       when it is equal to the queue size, the loop
       terminates.                                  */

    int no_change = 0;                              //keeps track of unchanged partitions
    set<int> curr, temp; vector<set<int> > repl;

    //cout << "Initial: "; show(partition);

    while (no_change < partition.size())            //termination condition
    {
        curr = partition[0];        //get first set
        
        //cout << "Curr is "; showset(curr); cout << endl;

        if (curr.size() == 1)       //if singleton, pop, push, increment no_change, and continue
        {
            partition.erase(partition.begin());
            partition.push_back(curr);
            no_change++;
            //cout << "Continuing" << endl;
            continue;
        }

        repl.clear(); temp.clear();                         //repl is a list of replacement partitions
        temp.insert(*(curr.begin())); repl.push_back(temp); //starts with a singleton set

        /* For each element in the current set,
           check if it is equivalent to any of
           the sets already in repl. If it is
           not, create a new one and add it to repl. */

        for (set<int>::iterator it = ++curr.begin(); it != curr.end(); it++)    //for each element after the first
        {
            //cout << "  Checking " << (*it) << endl;
            for (vector<set<int> >::iterator s_it = repl.begin(); s_it != repl.end(); s_it++)  //check if
                if (distinguish(*it, *((*s_it).begin()), partition) == 0)                      //it is equi-
                {                                                                              //valent to
                    (*s_it).insert(*it);                                                       //any set in 
                    //cout << "  Inserting with " << *((*s_it).begin()) << endl;               //repl
                    goto next_it;
                }

            //cout << "  Creating new" << endl;
            temp.clear(); temp.insert(*it);     //else create new set
            repl.push_back(temp);               //and push to repl

next_it: ;
        }
        partition.erase(partition.begin());     //pop curr from partition
        for (int i = 0; i < repl.size(); i++)
            partition.push_back(repl[i]);       //and push all sets in repl

        if (*(repl.begin()) == curr)            //check and update no_change
            no_change++;
        else
            no_change = 0;

        //cout << "Now no_change = " << no_change << endl;
        //cout << "Partition "; show(partition);
    }

    return partition;
}

void DFA::reduce ()
{
    part p = get_partition();           //get the partition of the states
    for (vector<set<int> >::iterator it = p.begin(); it != p.end(); it++)
        if ((*it).count(0) > 0)             //move the set
        {                                   //containing 0
            set<int> start = *it;           //to the beginning
            p.erase(it);                    //so that it is
            p.insert(p.begin(),start);      //marked as the start
            break;                          //state.
        }


    set<int> f2;
    vector<transition> delta2;          //new variables
    //cout << "Size " << delta2.size() << endl;

    int s, i = 0, j;
    for (vector<set<int> >::iterator it = p.begin(); it != p.end(); it++, i++)  //iterate over all sets in the partition
    {
        s = *((*it).begin());                           //the first element is taken as representative
        //cout << "In set "; showset(*it); cout << endl;
        //cout << "Rep element " << s << ", index " << i << endl;

        if (f.count(s) > 0)
        {
            f2.insert(i);                               //check if accepting
            //cout << "Adding to f2" << endl;
        }

        for (char x = 'a'; x <= 'z'; x++)                        //for each input symbol
        {
            j = getSetInd(to_state(s,x),p);                     //get the index of the set containing destination state
            delta2.push_back(make_pair(make_pair(i,x),j));      //make and push transition
            //cout << "On input " << x << ", to " << j << endl;
            //cout << "Size " << delta2.size() << endl;
        }
    }

    f = f2;

    n = p.size();
    a = f.size();
    k = delta2.size();

    delta = delta2;     //assignments
}
    
void DFA::showDFA()
{
    //cout << "Number of states in d = " << n << endl;

    //cout << "Accept states of d: ";
    //for (set<int>::iterator it = f.begin(); it != f.end(); it++) cout << *it << " "; cout << endl;
    //
    //cout << "Transitions of d: ";
    //for (int i = 0; i < k; i++) cout << "(" << delta[i].first.first
    //                                 << "," << delta[i].first.second
    //                                 << "," << delta[i].second << ") "; cout << endl;

    cout << n << " " << k << " " << a << endl;

    for (set<int>::iterator it = f.begin(); it != f.end(); it++)
        cout << (*it) << " ";
    cout << endl;

    for (vector<transition>::iterator it = delta.begin(); it != delta.end(); it++)
        cout << (*it).first.first << " "
             << (*it).first.second << " "
             << (*it).second << " " << endl;
}

int main ()
{
    int n, k, a; int s1, s2; char x;

    set<int> accept;
    vector<transition> trans;

    cin >> n >> k >> a;             //input n, k, a

    for (int i = 0; i < a; i++)
    {
        cin >> s1;
        accept.insert(s1);          //input accept states
    }

    for (int i = 0; i < k; i++)
    {
        cin >> s1 >> x >> s2;       //input transitions
        trans.push_back(make_pair(make_pair(s1,x),s2));
    }

    DFA d = DFA(n, k, a, accept, trans);        //create DFA object

    //d.showDFA();

    d.reduce();                                 //reduce it

    d.showDFA();                                //and print it
}


int getSetInd (int q, part p)
{
    for (vector<set<int> >::iterator it = p.begin(); it != p.end(); it++)   //iterate over sets
        if ((*it).count(q) != 0)                                            //check for membership
            return distance(p.begin(),it);                                  //and return index
    return -1;
}

void show (part p)
{
    cout << "{";
    for (int i = 0; i < p.size(); i++)
    {
        showset(p[i]);
        cout << ",";
    }
    cout << "}" << endl;
}

void showset (set<int> s)
{
    cout << "{";
    for (set<int>::iterator it = s.begin(); it != s.end(); it++)
        cout << *it << ",";
    cout << "}";
}
