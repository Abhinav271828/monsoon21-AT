#include <iostream>
#include <fstream>
#include <vector>
#include <utility>
using namespace std;

struct Regex
{
    char *symb;
    Regex *star;
    vector<Regex> *plus;
    vector<Regex> *conc;

    Regex (char*, Regex*, vector<Regex>*, vector<Regex>*);
    void show();
};

Regex::Regex (char* s, Regex* st, vector<Regex>* pl, vector<Regex>* c)
{
    symb = s; star = st; plus = pl; conc = c;
}

void Regex::show ()
{
    if (symb != NULL) cout << (*symb);
    else if (star != NULL)
    {
        cout << '(';
        (*star).show();
        cout << ')';
    }
    else if (plus != NULL)
    {
        cout << '(';
        (*((*plus).begin())).show();
        for (vector<Regex>::iterator it = ++((*plus).begin()); it != (*plus).end(); it++)
        {
            cout << '+';
            (*it).show();
        }
        cout << ')';
    }
    else if (conc != NULL)
    {
        cout << '(';
        for (vector<Regex>::iterator it = (*conc).begin(); it != (*conc).end(); it++)
            (*it).show();
        cout << ')';
    }
}

Regex parse (string s)
{
    vector<Regex> stack; Regex r(NULL, NULL, NULL, NULL);
    char c; char* cptr; Regex* rptr; vector<Regex>* vptr;

    for (int i = 0; i < s.size(); i++)
    {
        c = s[i];
        if (c >= 'a' && c <= 'z')
        {
            cptr = new char; (*cptr) = c;
            r = Regex(cptr, NULL, NULL, NULL);
            Regex last = stack.back();
            if (*(last.symb) == '+' && i < s.size()-1 && s[i+1] != '*')
            {
                stack.pop_back();
                last = stack.back();
                (last.plus)->push_back(r);
                stack.push_back(last);
            }
            else
                stack.push_back(r);
        }
        else if (c == 'E')
        {
            r = Regex(NULL, NULL, NULL, NULL);
            Regex last = stack.back();
            if (*(last.symb) == '+')
            {
                stack.pop_back();
                last = stack.back();
                (last.plus)->push_back(r);
                stack.push_back(last);
            }
            else
                stack.push_back(r);
        }
        else if (c == '*')
        {
            Regex last = stack.back(); stack.pop_back();
            rptr = new Regex(NULL, NULL, NULL, NULL); (*rptr) = last;
            r = Regex(NULL, rptr, NULL, NULL);
            last = stack.back();
            if (*(last.symb) == '+')
            {
                stack.pop_back();
                last = stack.back();
                (last.plus)->push_back(r);
                stack.push_back(last);
            }
            else
                stack.push_back(r);
        }
        else if (c == '+')
        {
            Regex last = stack.back(); stack.pop_back();
            if (last.plus == NULL)
            {
                vector<Regex> plus; plus.push_back(last);
                vptr = new vector<Regex>; (*vptr) = plus;
                r = Regex(NULL, NULL, vptr, NULL);
                cptr = new char; (*cptr) = '+';
                Regex p = Regex(cptr, NULL, NULL, NULL);
                stack.push_back(r); stack.push_back(p);
            }
            else
            {
                cptr = new char; (*cptr) = '+';
                Regex p = Regex(cptr, NULL, NULL, NULL);
                stack.push_back(p);
            }
        }
        else if (c == '(')
        {
            cptr = new char; (*cptr) = '(';
            r = Regex(cptr, NULL, NULL, NULL);
            stack.push_back(r);
        }
        else if (c == ')')
        {
            vector<Regex> conc;
            Regex b = stack.back();
            while (b.symb != NULL && *(b.symb) == '(')
            {
                conc.insert(conc.begin(), b);
                stack.pop_back(); b = stack.back();
            }
            vptr = new vector<Regex>; (*vptr) = conc;
            r = Regex(NULL, NULL, NULL, vptr);
            Regex last = stack.back();
            if (*(last.symb) == '+')
            {
                stack.pop_back();
                last = stack.back();
                (last.plus)->push_back(r);
                stack.push_back(last);
            }
            else
                stack.push_back(r);
        }
        else continue;
    }

    vptr = new vector<Regex>; (*vptr) = stack;
    r = Regex(NULL, NULL, NULL, vptr);
    return r;
}

int main (int argc, char* argv[])
{
    string inp_f = argv[1], outp_f = argv[2], p;
    ifstream fin; ofstream fout;
    fin.open(inp_f);
    getline(fin,p);
    cout << "Took input " << p << endl;
    
    Regex r = parse(p);

    r.show(); cout << endl;
}
