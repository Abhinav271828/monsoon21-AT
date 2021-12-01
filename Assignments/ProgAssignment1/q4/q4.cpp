#include <iostream>
#include <fstream>
#include <vector>
#include <utility>
#include <set>
using namespace std;
typedef pair<pair<int,char>,int> transition;

struct Regex                //Regex datatype
{
    char *symb;             //single symbol OR ε
    Regex *star;            //Kleene star of a regex
    vector<Regex> *plus;    //union of n regexes
    vector<Regex> *conc;    //concat of n regexes

    Regex (char*, Regex*, vector<Regex>*, vector<Regex>*);  //constructor
    void show();                                            //printer
};

Regex::Regex (char* s, Regex* st, vector<Regex>* pl, vector<Regex>* c)
{
    symb = s; star = st; plus = pl; conc = c;   //assignment
}

void Regex::show ()
{
    if (star != NULL)
    {
        cout << '(';
        (*star).show();
        cout << ")*";       //print sub-regex in brackets followed by *
    }
    else if (plus != NULL)
    {
        cout << '(';
        (*((*plus).begin())).show(); //print first sub-regex
        for (vector<Regex>::iterator it = ++((*plus).begin()); it != (*plus).end(); it++)
        {
            cout << '+';            //print + followed by
            (*it).show();           //each subseq regex
        }
        cout << ')';                //enclosed in brackets
    }
    else if (conc != NULL)
    {
        cout << '(';
        for (vector<Regex>::iterator it = (*conc).begin(); it != (*conc).end(); it++)
            (*it).show();           //print list of regexes
        cout << ')';                //enclosed in brackets
    }
    else if (symb != NULL)
        cout << (*symb);            //single character
    else
        cout << "ε";                //ε
}

/*The parser function works by iterating
  over the characters in the string.
  Single characters are pushed on the stack;
  for stars, the last regex is popped off, con-
  verted to a star, and pushed back.
  At an opening bracket, a regex containing
  only the bracket is pushed, and the parser
  continues. When a closing bracket is en-
  countered, regexes are popped one by one
  till the opening bracket is found, and they
  are all concatenated and pushed back.
  When a plus is found, the last regex is nested
  in a singleton union structure and pushed back,
  followed by a regex consisting only of a plus.
  This acts as an indicator that more operands are
  required.*/
Regex parse (string s)          //Parses string into Regex datatype
{
    vector<Regex> stack;        //global stack to maintain list of regexes
    Regex r(NULL, NULL, NULL, NULL);
    char c; char* cptr; Regex* rptr; vector<Regex>* vptr;

    for (int i = 0; i < s.size(); i++)      //for each character in the string
    {
        c = s[i];
        if (c >= 'a' && c <= 'z')           //single character
        {
            cptr = new char; (*cptr) = c;
            r = Regex(cptr, NULL, NULL, NULL);  //create regex
            if (!stack.empty())
            {
                Regex last = stack.back();
                if (last.conc != NULL && s[i+1] != '*')
                    (last.conc)->push_back(r);
                else if (i < s.size()-1 && ((s[i+1] >= 'a' && s[i+1] <= 'z') ||
                                            (s[i+1] == '(') ||
                                            (s[i+1] == 'E')) &&
                         (s[i+1] != '*'))
                {
                    vptr = new vector<Regex>; vptr->push_back(r);
                    r = Regex(NULL, NULL, NULL, vptr);
                    stack.push_back(r);
                }
                else if (last.symb != NULL && *(last.symb) == '+' &&
                         i < s.size()-1 && s[i+1] != '*')    //if plus is on top,
                {                                            //and current regex is not
                    stack.pop_back();                        //star, add to plus
                    last = stack.back();
                    (last.plus)->push_back(r);
                }
                else                                    //else directly push
                    stack.push_back(r);
            }
            else if (i < s.size()-1 && ((s[i+1] >= 'a' && s[i+1] <= 'z') ||
                                        (s[i+1] == '(') ||
                                        (s[i+1] == 'E')) &&
                     (s[i+1] != '*'))
            {
                vptr = new vector<Regex>; vptr->push_back(r);
                r = Regex(NULL, NULL, NULL, vptr);
                stack.push_back(r);
            }
            else
                stack.push_back(r);
        }
        else if (c == 'E')          //epsilon treated similar to character
        {
            r = Regex(NULL, NULL, NULL, NULL);
            if (!stack.empty())
            {
                Regex last = stack.back();
                if (last.symb != NULL && *(last.symb) == '+')
                {
                    stack.pop_back();
                    last = stack.back();
                    (last.plus)->push_back(r);
                }
                else
                    stack.push_back(r);
            }
            else
                stack.push_back(r);
        }
        else if (c == '*')              //Kleene star
        {
            Regex last = stack.back(); stack.pop_back();    //get the last regex
            rptr = new Regex(NULL, NULL, NULL, NULL); (*rptr) = last;
            r = Regex(NULL, rptr, NULL, NULL);              //wrap it in a star
            if (!stack.empty())
            {
                last = stack.back();
                if (last.symb != NULL && *(last.symb) == '+')       //check for plus
                {
                    stack.pop_back();
                    last = stack.back();
                    (last.plus)->push_back(r);
                }
                else                                        //and push
                    stack.push_back(r);
            }
            else
                stack.push_back(r);
        }
        else if (c == '+')              //union
        {
            Regex last = stack.back();
            if (last.plus == NULL)          //if the last regex is
            {                               //not a union, then create
                stack.pop_back();           //one and push it
                vector<Regex> plus; plus.push_back(last);
                vptr = new vector<Regex>; (*vptr) = plus;
                r = Regex(NULL, NULL, vptr, NULL);
                cptr = new char; (*cptr) = '+';
                Regex p = Regex(cptr, NULL, NULL, NULL);
                stack.push_back(r); stack.push_back(p);
            }
            else                            //else add to the existing union
            {
                cptr = new char; (*cptr) = '+';
                Regex p = Regex(cptr, NULL, NULL, NULL);
                stack.push_back(p);
            }
        }
        else if (c == '(')              //push opening bracket
        {
            cptr = new char; (*cptr) = '(';
            r = Regex(cptr, NULL, NULL, NULL);
            stack.push_back(r);
        }
        else if (c == ')')              //for closing bracket
        {
            vector<Regex> conc;
            Regex b = stack.back();
            while (b.symb == NULL || *(b.symb) != '(')  //pop all regexes until
            {                                           //opening bracket
                conc.insert(conc.begin(), b);           //and add to conc
                stack.pop_back(); b = stack.back();     //in reverse order
            }
            stack.pop_back();
            if (conc.size() > 1)                        //if more than one,
            {                                           //wrap in concat
                vptr = new vector<Regex>; (*vptr) = conc;
                r = Regex(NULL, NULL, NULL, vptr);
            }
            else                                        //else keep single
                r = conc[0];
            if (!stack.empty())
            {
                Regex last = stack.back();
                if (last.symb != NULL && *(last.symb) == '+')   //check for plus
                {
                    stack.pop_back();
                    last = stack.back();
                    (last.plus)->push_back(r);
                }
                else
                    stack.push_back(r);
            }
            else
                stack.push_back(r);
        }
        else continue;
    }
    
    if (stack.size() > 1)               //check for size of stack
    {                                   //and concat or push directly
        vptr = new vector<Regex>; (*vptr) = stack;
        r = Regex(NULL, NULL, NULL, vptr);
    }
    else
        r = stack[0];
    return r;
}

transition trans (int, char, int); //create transition out of , x, d

struct NFA
{
    int n, k, a;

    set<int> f;
    vector<transition> delta;

    int countS(Regex);      //counts states needed to simulate regex
    int makeDel(int,Regex); //creates and inserts transitions
    void getN(Regex);       //calls both above fns to make NFA
};


int NFA::countS (Regex r)
{
    if (r.star != NULL)
        return (2 + countS(*(r.star))); //done using two extra states
    else if (r.plus != NULL)
    {
        int c = 0;
        for (vector<Regex>::iterator it = (r.plus)->begin(); it != (r.plus)->end(); it++)
            c += countS(*it);
        return (2 + c);                 //total intermediate plus two extra
    }
    else if (r.conc != NULL)
    {
        int c = 0;
        for (vector<Regex>::iterator it = (r.conc)->begin(); it != (r.conc)->end(); it++)
            c += countS(*it);
        return c;                       //strings all together
    }
    else return 2;                      //two states for 1 character
}

int NFA::makeDel (int i, Regex r)   //starts numbering states from i
{                                   //and returns last number used
    Regex s(NULL, NULL, NULL, NULL); int j;
    if (r.star != NULL)                       //State i leads via epsilon
    {                                         //to state i+1. From state
        s = *(r.star);                        //i+1, the sub-regex s is
        delta.push_back(trans(i,'E',i+1));    //recognised up to state j.
        j = makeDel(i+1,s);                   //State j leads via epsilon
        delta.push_back(trans(j,'E',j+1));    //to state j+1. States i and
                                              //j+1 have epsilon transitions
        delta.push_back(trans(i,'E',j+1));    //in both directions.
        delta.push_back(trans(j+1,'E',i));
        return (j+1);
    }
    else if (r.plus != NULL)
    {
        vector<int> eps;                //State i is the starting state.
        j = i;                          //The vector eps holds the destination
                                        //states of each sub-regex in the union.
        for (vector<Regex>::iterator it = (r.plus)->begin(); it != (r.plus)->end(); it++)
        {                               
            s = *it;                    
            delta.push_back(trans(i,'E',j+1));
            j = makeDel(j+1, s);
            eps.push_back(j);
        }
        for (vector<int>::iterator it = eps.begin(); it != eps.end(); it++)
            delta.push_back(trans(*it,'E',j+1)); //Then each of these destination states
        return (j+1);                              //leads via epsilon to the end state. 
    }
    else if (r.symb != NULL)                //A single character takes
    {                                       //two states, a source and
        delta.push_back(trans(i,*(r.symb),i+1));
        return (i+1);                       //a destination.
    }
    else if (r.conc != NULL)
    {
        s = *((r.conc)->begin());           //The NFAs for each sub-regex
        j = makeDel(i, s);                  //are attached via epsilon
        for (vector<Regex>::iterator it = ++((r.conc)->begin()); it != (r.conc)->end(); it++)
        {                                   //transitions from the end of
            s = *it;                        //each to the beginning of the next.
            delta.push_back(trans(j,'E',j+1));
            j = makeDel(j+1, s);
        }
        return j;
    }
    else
    {
        delta.push_back(trans(i,'E',i+1));  //Similarly for epsilon.
        return (i+1);
    }
}

void NFA::getN (Regex r)
{
    n = makeDel(0, r); n++;     //Set all the transitions and the no. of states.

    a = 1;
    f.insert(n-1);              //One accept state which is the final one.

    k = delta.size();           //No. of transitions

}

int main (int argc, char* argv[])
{
    string inp_f = argv[1], outp_f = argv[2], p; //Get CL args
    ifstream fin; ofstream fout;
    fin.open(inp_f);                             //Open input file
    getline(fin,p);                              //Read from file
    fin.close();                                 //Close file
    
    Regex r = parse(p);             //Parse regex

    NFA m; m.getN(r);               //Construct NFA

    fout.open(outp_f);                          //Open output file
    fout << m.n << " " << m.k << " " << m.a << endl;
    for (set<int>::iterator it = m.f.begin(); it != m.f.end(); it++)
        fout << (*it) << " ";
    fout << endl;
    for (vector<transition>::iterator it = m.delta.begin(); it != m.delta.end(); it++)
        fout << (*it).first.first << " "
             << (*it).first.second << " "
             << (*it).second << endl;           //Print NFA
    fout.close();                               //Close file
}

transition trans (int i, char x, int j)
{
    return make_pair(make_pair(i,x),j);     //Transition from i to j on input x
}
