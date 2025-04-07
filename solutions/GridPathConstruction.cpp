// https://cses.fi/problemset/task/2418/
// https://www.researchgate.net/publication/220616693_Hamilton_Paths_in_Grid_Graphs
#include <bits/stdc++.h>
#pragma GCC optimize("Ofast,no-stack-protector,unroll-loops")
using namespace std;
static inline char myGetc() {
    static const int BUFSZ = 1<<20;
    static char buf[BUFSZ];
    static int idx=0, nread=0;
    if(idx>=nread){
        nread=fread(buf,1,BUFSZ,stdin);
        if(!nread) return EOF;
        idx=0;
    }
    return buf[idx++];
}
static inline long long readInt(){
    long long x=0; bool neg=false; char c;
    do{ c=myGetc(); } while(c!='-' && (c<'0'||c>'9') && c!=EOF);
    if(c=='-'){ neg=true; c=myGetc(); }
    for(; c>='0' && c<='9'; c=myGetc()) x=x*10+(c-'0');
    return neg? -x : x;
}
static string outBuf;
static string solvePath(int n, int m, int i1, int j1, int i2, int j2,
                        char u='U', char d='D', char l='L', char r='R');
static inline void reverseIfNeeded(string &s, bool needRev) {
    if(needRev) reverse(s.begin(), s.end());
}
static inline string doSplitInline(int &n,int &m,int &i1,int &j1,int &i2,int &j2,
                                   char &u, char &d, char &l, char &r,
                                   bool &needRev,
                                   int row, int col)
{
    {
        int nn=n, mm=col, ii1=i1, jj1=j1, ii2=row, jj2=col;
        char uu=u, dd=d, ll=l, rr=r;
        string a = solvePath(nn,mm,ii1,jj1,ii2,jj2,uu,dd,ll,rr);
        if(a.empty()) return a;
        nn=n; mm=m-col; ii1=row; jj1=1; ii2=i2; jj2=j2-col;
        uu=u; dd=d; ll=l; rr=r;
        string b = solvePath(nn,mm,ii1,jj1,ii2,jj2,uu,dd,ll,rr);
        if(b.empty()) return b;
        string res; res.reserve(a.size()+1+b.size());
        res.append(a);
        res.push_back(r);
        res.append(b);
        if(needRev) reverse(res.begin(),res.end());
        return res;
    }
}
static string solvePath(int n, int m, int i1, int j1, int i2, int j2,
                        char u, char d, char l, char r)
{
    bool needRev = false;
    auto flipI = [&](int &N,int &i1,int &i2,char &u,char &d){
        i1=N+1-i1; i2=N+1-i2; swap(u,d);
    };
    auto flipJ = [&](int &M,int &j1,int &j2,char &l,char &r){
        j1=M+1-j1; j2=M+1-j2; swap(l,r);
    };
    auto flip12 = [&](int &i1,int &j1,int &i2,int &j2,char &u,char &d,char &l,char &r,bool &needRev){
        swap(i1,i2); swap(j1,j2);
        swap(u,d); swap(l,r);
        needRev=!needRev;
    };
    auto transpose = [&](int &N,int &M,int &i1,int &j1,int &i2,int &j2,char &u,char &d,char &l,char &r){
        swap(N,M); swap(i1,j1);
        swap(i2,j2); swap(u,l);
        swap(d,r);
    };
    auto normalize = [&](int &n,int &m,int &i1,int &j1,int &i2,int &j2,char &u,char &d,char &l,char &r,bool &needRev){
        if(j1>j2) flipJ(m,j1,j2,l,r);
        if(j1>m+1-j1){ flipJ(m,j1,j2,l,r); flip12(i1,j1,i2,j2,u,d,l,r,needRev); }
        if(i1>n+1-i1) flipI(n,i1,i2,u,d);
        if(j1==j2){
            if(i1>i2) flip12(i1,j1,i2,j2,u,d,l,r,needRev);
            if(i1>n+1-i2){
                flipI(n,i1,i2,u,d);
                flip12(i1,j1,i2,j2,u,d,l,r,needRev);
            }
        }
        if((i2==1||i2==n)&&(j2==1||j2==m)){
            flip12(i1,j1,i2,j2,u,d,l,r,needRev);
            if(i1==n) flipI(n,i1,i2,u,d);
            if(j1==m) flipJ(m,j1,j2,l,r);
        }
    };
    if(n>m) transpose(n,m,i1,j1,i2,j2,u,d,l,r);
    normalize(n,m,i1,j1,i2,j2,u,d,l,r,needRev);
    if(n%2==1 && m%2==1){
        if( ((i1+j1)&1) || ((i2+j2)&1) ) return "";
    } else {
        if( ((i1+j1)&1) == ((i2+j2)&1) ) return "";
    }
    if(n==1){
        if(i1==1 && j1==1 && i2==1 && j2==m){
            string rr(m-1, r);
            if(needRev) reverse(rr.begin(),rr.end());
            return rr;
        }
        return "";
    }
    if(i1==1 && j1==1){
        string full; full.reserve(n*m -1);
        while(true){
            if(n>m) transpose(n,m,i1,j1,i2,j2,u,d,l,r);
            if(n==1){
                full.append(m-1, r);
                break;
            }
            if(j2==1 || (j2==2 && i2==n)){
                transpose(n,m,i1,j1,i2,j2,u,d,l,r);
            }
            full.append(n-1, d);
            full.push_back(r);
            i1=n; j2--; m--;
            flipI(n,i1,i2,u,d);
        }
        if(needRev) reverse(full.begin(),full.end());
        return full;
    }
    if(n==2){
        if(j1==j2 && j1>1 && j1<m) return "";
        if(j1>1){
            string ans = doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,2,j1);
            return ans;
        }
    }
    if(n==3){
        if(j2-j1>1){
            if(((i1+j1)&1)==1) return "";
            string ans = doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,3,j1);
            return ans;
        }
        if(j2==j1+1){
            if(((i1+j1)&1)==1){
                if(i1==2||i2==2) return "";
                string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,2,j1);
                return ans;
            } else {
                if(i2!=3){
                    string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,3,j1);
                    return ans;
                } else if(i1==1){
                    string rA(2,r);
                    string partA=solvePath(n,m-j1-1,1,1,2,1,u,d,l,r);
                    if(partA.empty()) { return ""; }
                    string L3(3,l);
                    string partB=solvePath(n,j1-1,2,j1-1,3,j1-1,u,d,l,r);
                    if(partB.empty()) { return ""; }
                    string R2(2,r);
                    string combined; 
                    combined.reserve(rA.size()+partA.size()+L3.size()+partB.size()+R2.size());
                    combined.append(rA);
                    combined.append(partA);
                    combined.append(L3);
                    combined.append(partB);
                    combined.append(R2);
                    if(needRev) reverse(combined.begin(),combined.end());
                    return combined;
                } else {
                    string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,1,j1);
                    return ans;
                }
            }
        }
        if(j2==j1){
            if(i2==2){
                if((j1&1)==0) flipJ(m,j1,j2,l,r);
                string pA=solvePath(n,m-j1,1,1,3,1,u,d,l,r);
                if(pA.empty()) return "";
                string L2(2,l);
                string pB=solvePath(n,j1-1,3,j1-1,2,j1-1,u,d,l,r);
                if(pB.empty()) return "";
                string final; final.reserve(1 + pA.size() + 2 + pB.size() + 1);
                final.push_back(r); final.append(pA); final.append(L2); final.append(pB); final.push_back(r);
                if(needRev) reverse(final.begin(),final.end());
                return final;
            }
            if(i2==3){
                string pA=solvePath(n,m-j1,1,1,2,1,u,d,l,r);
                if(pA.empty()) return "";
                string L2(2,l);
                string pB=solvePath(n,j1-1,2,j1-1,3,j1-1,u,d,l,r);
                if(pB.empty()) return "";
                string final; final.reserve(1 + pA.size() + 2 + pB.size() + 1);
                final.push_back(r); final.append(pA); final.append(L2); final.append(pB); final.push_back(r);
                if(needRev) reverse(final.begin(),final.end());
                return final;
            }
        }
    }
    if(n%2==1 && m%2==1){
        if(j1==j2 || (j1==1&&j2==2) || (i1==1&&i2==m)){
            transpose(n,m,i1,j1,i2,j2,u,d,l,r);
            normalize(n,m,i1,j1,i2,j2,u,d,l,r,needRev);
        }
        string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,(i1!=1 && i2!=1)?1:n, max(j1,2));
        return ans;
    }
    if(n%2==1){
        transpose(n,m,i1,j1,i2,j2,u,d,l,r);
        normalize(n,m,i1,j1,i2,j2,u,d,l,r,needRev);
    }
    if(j1!=j2){
        if(j1==1 && j2==2 && i1==i2){
            string a=solvePath(i1-1,3,i1-1,1,1,3,u,d,l,r);
            if(a.empty()) return "";
            string b=solvePath(n,m-3,1,1,n,1,u,d,l,r);
            if(b.empty()) return "";
            string c=solvePath(n-i1,3,n-i1,3,1,3,u,d,l,r);
            if(c.empty()) return "";
            string final; final.reserve(1+a.size()+1+b.size()+1+c.size()+1+1);
            final.push_back(u);
            final.append(a);
            final.push_back(r);
            final.append(b);
            final.push_back(l);
            final.append(c);
            final.push_back(u);
            final.push_back(l);
            if(needRev) reverse(final.begin(),final.end());
            return final;
        }
        if(j2>j1+1 || i1==i2){
            string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,((i1 + (j1==1))%2==1 ? n:1),max(j1,2));
            return ans;
        }
    }
    transpose(n,m,i1,j1,i2,j2,u,d,l,r);
    normalize(n,m,i1,j1,i2,j2,u,d,l,r,needRev);
    if(n%2==0){
        if(j1==1 && j2==2 && i1==i2){
            string a=solvePath(i1-1,3,i1-1,1,1,3,u,d,l,r);
            if(a.empty()) return "";
            string b=solvePath(n,m-3,1,1,n,1,u,d,l,r);
            if(b.empty()) return "";
            string c=solvePath(n-i1,3,n-i1,3,1,3,u,d,l,r);
            if(c.empty()) return "";
            string final; final.reserve(1+a.size()+1+b.size()+1+c.size()+1+1);
            final.push_back(u);
            final.append(a);
            final.push_back(r);
            final.append(b);
            final.push_back(l);
            final.append(c);
            final.push_back(u);
            final.push_back(l);
            if(needRev) reverse(final.begin(),final.end());
            return final;
        }
        string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,((i1+(j1==1))%2==1? n:1), max(j1,2));
        return ans;
    }
    if(((i1+j1)&1)==0 && (j1>1||j2>2)){
        string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,n,max(j1,2));
        return ans;
    }
    if((j2-j1>1) || ((j1&1)==0)){
        string ans=doSplitInline(n,m,i1,j1,i2,j2,u,d,l,r,needRev,n-((i1+j1)&1), j1+(j1&1));
        return ans;
    }
    int blockSize=(m/2)*2 + (n-i1+1) + (m-1) + ( (m/2-1)*2*(n-i1-2) ) + (n-i1-1) + (m-1-j2);
    string res; res.reserve(blockSize+10);
    for(int i=0;i<j1-1;i++) res.push_back(l);
    res.push_back(u);
    for(int k=0;k<m/2;k++){
        for(int x=0;x<i1-2;x++) res.push_back(u);
        res.push_back(r);
        for(int x=0;x<i1-2;x++) res.push_back(d);
        res.push_back(r);
    }
    if(!res.empty()) res.pop_back();
    for(int i=0;i<n-i1+1;i++) res.push_back(d);
    for(int i=0;i<m-1;i++) res.push_back(l);
    res.push_back(u);
    for(int k=1;k<m/2;k++){
        for(int x=0;x<n-i1-2;x++) res.push_back(u);
        res.push_back(r);
        for(int x=0;x<n-i1-2;x++) res.push_back(d);
        res.push_back(r);
    }
    for(int i=0;i<n-i1-1;i++) res.push_back(u);
    for(int i=0;i<m-1-j2;i++) res.push_back(l);
    if(needRev) reverse(res.begin(),res.end());
    return res;
}
int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t=(int)readInt();
    outBuf.reserve(500000);
    while(t--){
        int n=(int)readInt(), m=(int)readInt();
        int y1=(int)readInt(), x1=(int)readInt();
        int y2=(int)readInt(), x2=(int)readInt();
        string path = solvePath(n,m,y1,x1,y2,x2,'U','D','L','R');
        if(path.empty()) {
            outBuf += "NO\n";
        } else {
            outBuf += "YES\n";
            outBuf += path;
            outBuf.push_back('\n');
        }
    }
    fwrite(outBuf.data(),1,outBuf.size(),stdout);
    return 0;
}