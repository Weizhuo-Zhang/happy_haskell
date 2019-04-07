#include <iostream>
#include <vector>
using namespace std;

int main() {
    vector<int> xs { 1, 2, 3, 4, 5, 6 };
    vector<int> ys;
    for (auto x : xs) {
        if (x > 3) {
            ys.emplace_back(x);
        }
    }

    for (auto y : ys) {
        cout << y << endl;
    }
}
