#include <iostream>
#include <vector>
using namespace std;

int main() {
    vector<int> xs{ 1, 2, 3 };
    int total = 0;
    for (auto x : xs) {
        total += x;
    }

    cout << total << endl;
}
