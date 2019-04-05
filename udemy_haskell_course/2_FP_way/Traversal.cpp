#include <iostream>
#include <vector>
using namespace std;

int main() {
    vector<int> xs{1,2,3};
    vector<int> ys;
    for (auto x : xs) {
        ys.emplace_back(x + 10);
    }

    for (auto y : ys) {
        cout << y << endl;
    }
}
