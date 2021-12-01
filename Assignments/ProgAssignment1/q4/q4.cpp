#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <utility>
#include <set>
#include <stack>

using namespace std;

typedef pair<pair<int,char>,int> transition;


struct Regex
{
    string symb;

    Regex* star;
    vector<Regex>* concat;
    vector<Regex>* plus;

    Regex (string);
    Regex ();

    pair<vector<Regex>,string> parse (vector<Regex>, string);
    
    int isEmpty();
    int isStar();
    int isPlus();
    int isConc();
    int isChar();
    void show();

    void reduce (vector<Regex>);

};
void showvec(vector<Regex>);

void Regex::reduce (vector<Regex> st)
{
    cout << "In reduce" << endl;
    
    cout << "Passed "; showvec(st); cout << endl;
    reverse(st.begin(),st.end());
    cout << "Reversed? "; showvec(st); cout << endl;
    concat = &st;
}

Regex::Regex ()
{
    symb = ""; star = NULL; concat = NULL; plus = NULL;
}

Regex::Regex (string r)
{
    cout << "Creating new with " << r << "; parsing" << endl;

    vector<Regex> st;
    pair<vector<Regex>,string> ret = parse(st,r);
    cout << "Parsed" << endl;

    vector<Regex> p = ret.first;
    if (ret.second != "")
    {
        cout << "Error; no parse" << endl;
        return;
    }

    symb = ""; star = NULL; plus = NULL;

    cout << "About to reduce" << endl;
    showvec(p); cout << endl;
    reduce(p);
    cout << "Reduced" << endl;
}

pair<vector<Regex>,string> Regex::parse (vector<Regex> st, string r)
{

    cout << "Parsing;" << endl
         << "first symbol " << r[0] << endl
         << "stack " << endl << "  "; showvec(st);

    if (r == "")
    {
        cout << "R empty; returning stack ";
        showvec(st); cout << endl;
        return make_pair(st,"");
    }

    if ((r[0] >= 'a' && r[0] <= 'z') || (r[0] == 'E'))
    {
        Regex p; p.symb.append(1,r[0]);
        st.push_back(p); r.erase(r.begin());

        cout << "Now stack is " << endl; showvec(st);
        cout << endl << "and r is " << r << endl;

        pair<vector<Regex>,string> ret = parse(st, r);
        cout << "Returning stack "; showvec(ret.first); cout << endl;
        cout << "with string " << ret.second << endl;
        return ret;
    }
    else if (r[0] == '*')
    {
        Regex c = st.back(); st.pop_back();
        Regex p; p.star = &c;
        st.push_back(p); r.erase(r.begin());

        cout << "Now stack is " << endl; showvec(st);
        cout << endl << "and r is " << r << endl;

        pair<vector<Regex>,string> ret = parse(st, r);
        cout << "Returning stack "; showvec(ret.first); cout << endl;
        cout << "with string " << ret.second << endl;
        return ret;
    }
    else if (r[0] == '+')
    {
        Regex c = st.back(); st.pop_back();

        r.erase(r.begin()); vector<Regex> emp;
        pair<vector<Regex>,string> op = parse(emp,r);

        Regex p; p.reduce(op.first);

        if (c.isPlus())
        {
            (*(c.plus)).push_back(p);
            st.push_back(c);
        }
        else
        {
            Regex pl; vector<Regex> v;
            v.push_back(c); v.push_back(p);
            pl.plus = &v;
            st.push_back(pl);
        }

        pair<vector<Regex>,string> ret = parse(st, op.second);
        cout << "Returning stack "; showvec(ret.first); cout << endl;
        cout << "with string " << ret.second << endl;
        return ret;
    }
    else if (r[0] == '(')
    {
        r.erase(r.begin()); vector<Regex> emp;
        pair<vector<Regex>,string> op = parse(emp,r);

        Regex p; p.reduce(op.first);
        st.push_back(p);
        if (op.second[0] == ')')
        {
            pair<vector<Regex>,string> ret = parse(st, op.second);
            cout << "Returning stack "; showvec(ret.first); cout << endl;
            cout << "with string " << ret.second << endl;
            return ret;
        }
    }
    else if (r[0] == ')')
    {
        r.erase(r.begin());
        cout << "Closing bracket, returning stack "; showvec(st); cout << endl;
        cout << "with r " << r << endl;
        return make_pair(st, r);
    }

    r.erase(r.begin());
    cout << "Otherwise returning stack "; showvec(st); cout << endl;
    cout << "with r " << r << endl;
    return parse(st, r);
}

int Regex::isEmpty()
{
    if (symb == "" && star == NULL &&
        concat == NULL && plus == NULL)
            return 1;
    return 0;
}
int Regex::isStar() { if (star != NULL) return 1; return 0; }
int Regex::isPlus() { if (plus != NULL) return 1; return 0; }
int Regex::isConc() { if (concat != NULL) return 1; return 0; }
int Regex::isChar() { if (symb != "")   return 1; return 0; }

void Regex::show()
{
    //cout << "Showing" << endl;
    //cout << "isEmpty() = " << isEmpty() << "; "
    //     << "isChar() = " << isChar() << "; "
    //     << "isStar() = " << isStar() << "; "
    //     << "isPlus() = " << isPlus() << "; "
    //     << "isConc() = " << isConc() << endl;

    if (isEmpty() == 1) cout << "";
    else if (isChar() == 1)
        cout << symb;
    else if (isStar() == 1)
    {
        cout << "(";
        (*star).show();
        cout << ")*";
    }
    else if (isPlus() == 1)
    {
        cout << "(";
        (*((*plus).begin())).show(); cout << " + ";
        for (vector<Regex>::iterator it = ++(*plus).begin(); it != (*plus).end(); it++)
        {
            (*it).show();
            cout << "+";
        }
        cout << ")";
    }
    else if (isConc() == 1)
    {
        cout << "IsConc print" << endl;
        cout << "Size " << (*concat).size() << endl;
        cout << "(";
        for (vector<Regex>::iterator it = (*concat).begin(); it != (*concat).end(); it++)
            (*it).show();
        cout << ")";
    }
}


struct NFA
{
    int n, k, a;

    set<int> f;
    vector<transition> delta;

    NFA (Regex);
    void show();
};

void NFA::show ()
{
    cout << n << " " << k << " " << a << endl;
    for (set<int>::iterator it = f.begin(); it != f.end(); it++)
        cout << (*it) << " ";
    cout << endl;
    for (vector<transition>::iterator it = delta.begin(); it != delta.end(); it++)
        cout << (*it).first.first << " "
             << (*it).first.second << " "
             << (*it).second << endl;
}

int main (int argc, char* argv[])
{
    string inp_f = argv[1], outp_f = argv[2], p;
    ifstream fin; ofstream fout;
    fin.open(inp_f);
    getline(fin,p);
    cout << "Took input " << p << endl;

    Regex r = Regex(p);

    r.show(); cout << endl;

    //NFA m = NFA(r);

    //m.show();
}

void showvec(vector<Regex> v)
{
    for (vector<Regex>::iterator it = v.begin(); it != v.end(); it++)
        (*it).show(); cout << " ";
    cout << endl;
}
